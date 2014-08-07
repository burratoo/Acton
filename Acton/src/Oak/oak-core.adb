------------------------------------------------------------------------------
--                                                                          --
--                              OAK COMPONENTS                              --
--                                                                          --
--                                 OAK.CORE                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                 Copyright (C) 2010-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

with Oak.Processor_Support_Package.Interrupts;
use Oak.Processor_Support_Package.Interrupts;

with Ada.Cyclic_Tasks;
with Oak.Agent.Interrupts;        use Oak.Agent.Interrupts;
with Oak.Agent.Kernel;            use Oak.Agent.Kernel;
with Oak.Agent.Oak_Agent;         use Oak.Agent.Oak_Agent;
with Oak.Agent.Schedulers;        use Oak.Agent.Schedulers;
with Oak.Agent.Tasks;             use Oak.Agent.Tasks;
with Oak.Agent.Tasks.Cycle;       use Oak.Agent.Tasks.Cycle;
with Oak.Agent.Tasks.Activation;  use Oak.Agent.Tasks.Activation;

with Oak.Brokers;                   use Oak.Brokers;
with Oak.Brokers.Protected_Objects; use Oak.Brokers.Protected_Objects;

with Oak.Interrupts;        use Oak.Interrupts;
with Oak.Protected_Objects; use Oak.Protected_Objects;
with Oak.Scheduler;         use Oak.Scheduler;

with Oak.Core_Support_Package.Interrupts;
with Oak.Core_Support_Package.Task_Support;
use Oak.Core_Support_Package.Task_Support;

with Oak.Processor_Support_Package; use Oak.Processor_Support_Package;
with Oak.Core_Support_Package.Call_Stack;
use Oak.Core_Support_Package.Call_Stack;

with System; use System;

package body Oak.Core is

   ----------------
   -- Initialise --
   ----------------

   procedure Initialise is
      Kid : Kernel_Id;
   begin
      --  Setup storage

      Setup_Storage;

      --  Setup kernel data structures

      for Processor in Processors'Range loop
         New_Kernel_Agent (Agent => Kid);
      end loop;

      --  Setup Sleep Agent

      New_Agent
        (Agent                => Agent.Sleep_Agent,
         Name                 => "Sleep",
         Call_Stack_Address   => Null_Address,
         Call_Stack_Size      => Sleep_Stack_Size,
         Run_Loop             => Sleep_Agent_Run_Loop'Address,
         Run_Loop_Parameter   => Null_Address,
         Normal_Priority      => Priority'First,
         Initial_State        => Runnable,
         Scheduler_Agent      => No_Agent,
         Wake_Time            => Time_First,
         When_To_Charge_Agent => Only_While_Running);

      --  Setup Timers

      Setup_Timers;

      Oak.Core_Support_Package.Interrupts.Set_Up_Interrupts;
      Oak.Core_Support_Package.Task_Support.Initialise_Task_Enviroment;

      Global_Start_Time := Clock + Global_Start_Time_Offset;
   end Initialise;

   -----------------------------
   -- Complete_Initialisation --
   -----------------------------

   procedure Complete_Initialisation is
   begin
      Processor_Support_Package.Interrupts.Complete_Interrupt_Initialisation;
   end Complete_Initialisation;

   -------------------------
   -- Request_Oak_Service --
   -------------------------

   procedure Request_Oak_Service
     (Reason_For_Run : in Run_Reason;
      Message        : in out Oak_Message)
      renames Context_Switch_To_Oak;

   --------------
   -- Run_Loop --
   --------------

   procedure Run_Loop is
      My_Kernel_Id   : constant Kernel_Id := This_Oak_Kernel;

      Current_Agent  : Oak_Agent_Id := No_Agent;
      Current_Timer  : Oak_Timer_Id := No_Timer;

      Next_Agent     : Oak_Agent_Id := No_Agent;
      Next_Timer     : Oak_Timer_Id := No_Timer;

      Agent_Message  : Oak_Message;
      Message_Is_Bad : constant Oak_Message :=
                         (Message_Type => Invalid_Message);

      Reason_For_Run : Run_Reason;

      procedure Handle_External_Interrupt;

      procedure Handle_External_Interrupt is
         Id : constant External_Interrupt_Id :=
                Get_External_Interrupt_Id;

         P               : constant Interrupt_Priority :=
                             Current_Interrupt_Priority;
         Interrupt_Agent : constant Agent.Interrupt_Id :=
                             Interrupt_For_Priority
                               (Oak_Kernel => My_Kernel_Id,
                                Priority   => P);
      begin
         Set_State (Interrupt_Agent, Handling_Interrupt);
         Set_Interrupt_Kind
           (For_Agent => Interrupt_Agent, Kind => External);
         Set_External_Id
           (For_Agent => Interrupt_Agent, Id => Id);
         Activate_Interrupt_Agent
           (Oak_Kernel => My_Kernel_Id, Interrupt => Interrupt_Agent);
      end Handle_External_Interrupt;

   begin

      --        Invoke_Reason_Table :=
      --          (Reason_For_Run => (others => 0),
      --           Message_Reason => (others => 0),
      --           Timer_Kind     => (others => 0),
      --           Early_Fire     => 0);

      First_Run_Actions : declare

         --  First time the kernel instance has run. Initialise Scheduler
         --  Agents.

         Master_Task   : constant Task_Id     := Task_Id'First;
      begin
         Flush_Scheduler_Ops_Stack (Oak_Kernel => My_Kernel_Id);
         pragma Warnings (Off, "*False*");
         if My_Kernel_Id = 1 then
            Push_Scheduler_Op
              (Oak_Kernel => My_Kernel_Id,
               Scheduler  => Scheduler_Agent_For_Agent (Master_Task),
               Operation  => (Message_Type => Adding_Agent,
                              Agent_To_Add => Master_Task,
                              Place_At     => Front));
         end if;
         pragma Warnings (On, "*False*");

         Push_Scheduler_Op
           (Oak_Kernel => My_Kernel_Id,
            Scheduler  => Top_Level_Schedulers (My_Kernel_Id),
            Operation  => (Message_Type   => Initialising_Agents,
                           Agents_To_Init =>
                             Top_Level_Schedulers (My_Kernel_Id)));
      end First_Run_Actions;

      loop
         --  Oddly we start here since the first thing to do after the
         --  initialisation occurs is to dispatch the above scheduler
         --  operations.

         --  Pick a next Agent to run

         if Has_Scheduler_Operations_Pending (My_Kernel_Id) then
            Pop_Scheduler_Op
              (Oak_Kernel => My_Kernel_Id,
               Scheduler  => Next_Agent,
               Operation  => Agent_Message);

            --  Deal with the fact that we can only add one agent at a time
            --  to a scheduler agent.

            case Agent_Message.Message_Type is
               when Adding_Agents =>
                  declare
                     A : constant Oak_Agent_Id := Agent_Message.Agents_To_Add;
                     B : constant Oak_Agent_Id := Oak_Agent.Next_Agent (A);
                  begin
                     --  Push the remaining agents back onto the scheduler ops
                     --  stack

                     if B /= No_Agent then
                        Push_Scheduler_Op
                          (Oak_Kernel => My_Kernel_Id,
                           Scheduler  => Scheduler_Agent_For_Agent (B),
                           Operation  => (Message_Type  => Adding_Agents,
                                          Agents_To_Add => B));
                     end if;

                     Set_Next_Agent (A, No_Agent);
                     Agent_Message := (Message_Type => Adding_Agent,
                                       Agent_To_Add => A,
                                       Place_At     => Back);
                  end;

               when Initialising_Agents =>
                  declare
                     A : constant Oak_Agent_Id := Agent_Message.Agents_To_Init;
                     B : constant Oak_Agent_Id := Oak_Agent.Next_Agent (A);
                  begin
                     --  Push the remaining agents back onto the scheduler ops
                     --  stack

                     if B /= No_Agent then
                        Push_Scheduler_Op
                          (Oak_Kernel => My_Kernel_Id,
                           Scheduler  => B,
                           Operation  =>
                             (Message_Type   => Initialising_Agents,
                              Agents_To_Init => B));
                     end if;

                     Agent_Message := (Message_Type => No_Message);
                  end;

               when others =>
                  null;
            end case;

            Set_Oak_Message (For_Agent => Next_Agent,
                             Message   => Agent_Message);

            Set_Current_Priority
              (Oak_Kernel => My_Kernel_Id,
               Priority   => Any_Priority'Last);

            --  Remove the scheduler agent from the charge list since it will
            --  be added back below. This is done since the scheduler agent may
            --  already be on the charge list and we can safely call the remove
            --  procedure even if the agent is not on the charge list. Cost of
            --  searching vs cost of deleting then adding do not differ to much
            --  since the scheduler will be added back to the front of the
            --  charge list.

            --  OPTIMISATION: Could conditionalise this to exclude the case
            --  of the scheduler running again, but as this is a case that does
            --  not occur that often, its probably best to forgo the if
            --  statement.

            Remove_Agent_From_Charge_List
              (Oak_Kernel => My_Kernel_Id, Agent => Current_Agent);
            if Scheduler_Agent_For_Agent (Next_Agent) /= No_Agent then
               Remove_Agent_From_Charge_List
                 (Oak_Kernel => My_Kernel_Id,
                  Agent      => Next_Agent);
            end if;
            Add_Agent_To_Charge_List
              (Oak_Kernel => My_Kernel_Id,
               Agent      => Next_Agent);

            Next_Timer := No_Timer;

         else

            --  Pick next agent to run

            declare
               P               : Any_Priority;
               Interrupt_Agent : constant Interrupt_Id_With_No :=
                                   Find_Top_Active_Interrupt (My_Kernel_Id);
               Protected_Agent : constant Protected_Id_With_No :=
                                   Next_Protected_Agent_To_Run (My_Kernel_Id);
            begin
               Check_Sechduler_Agents_For_Next_Agent_To_Run
                 (Next_Agent_To_Run => Next_Agent,
                  Top_Priority      => P);

               --  Check to see if any interrupt or protected agents are active
               --  and have a priority equal to and above the agent selected
               --  above.

               --  Select the interrupt agent if it has a priority equal to or
               --  higher than the agent selected above. Note that it does not
               --  matter if either Next_Agent or Interrut_Agent is No_Agent as
               --  it maps to the sleep agent that has a value of
               --  Priority'First. Same applies to protected agents

               if Protected_Agent /= No_Protected_Object
                 and then Ceiling_Priority (Protected_Agent) >= P
               then
                  P := Ceiling_Priority (Protected_Agent);
                  Next_Agent := Task_Within (Protected_Agent);
               end if;

               if Normal_Priority (Interrupt_Agent) >= P then
                  P := Normal_Priority (Interrupt_Agent);
                  Next_Agent := Interrupt_Agent;
               end if;

               --  Check to see if there are any pending external interrupts to
               --  save switching unnecessarily to another agent and then
               --  imediately back through here again.

               if Has_Outstanding_Interrupts (Above_Priority => P) then
                  Handle_External_Interrupt;
                  Next_Agent := Find_Top_Active_Interrupt (My_Kernel_Id);
                  P := Normal_Priority (Next_Agent);
               end if;

               --  Set the current priority the core is running at now (which
               --  may not correspond to the current agent's priority due to
               --  the correction that follows).

               Set_Current_Priority
                 (Oak_Kernel => My_Kernel_Id,
                  Priority   => P);

               --  Correct Next Agent. Needed to cover the case where a
               --  protected or an activitor task has been selected.

               --  If it is an activator call the Continue_Activation
               --  subprogram to discover the task to run.

               if State (Next_Agent) = Activation_Pending then
                  --  This call will select the correct task to run.
                  Continue_Activation
                    (Activator        => Next_Agent,
                     Next_Task_To_Run => Next_Agent);
               end if;
            end;

            ---------------
            --  After we run the Scheduler Agents, all that is left to do is
            --  set the time for the Run_Loop's next activation and record the
            --  reason for it's activation. Then we can use the kernel's
            --  scheduler data structure to load the next scheduled task into
            --  the processor. If there are no tasks available to run we put
            --  the processor into a processor define sleep or halt state,
            --  (or god forbid a busy wait loop). Note that the kernel jumps to
            --  the task in question, rather than call a procedure.
            -------------------

            --  OPTIMISATION: no need to look for new timer if the agent to
            --  run is the same as the agent that entered Oak.

            --  OPTIMISATION: Conditional this since there is no point pulling
            --  off the current agent if it is going to be put back on.

            if Next_Agent /= Current_Agent then
               Next_Timer :=
                 Earliest_Timer_To_Fire
                   (Above_Priority => Current_Priority (My_Kernel_Id));
               Remove_Agent_From_Charge_List
                 (Oak_Kernel => My_Kernel_Id, Agent => Current_Agent);
               Add_Agent_To_Charge_List
                 (Oak_Kernel => My_Kernel_Id,
                  Agent      => Next_Agent);
            else
               Next_Timer := Current_Timer;
            end if;
         end if;

         --  Execution timers do not live in the active timer store since they
         --  are only relevant for the currently executing task and only apply
         --  if the timer will fire before any timer that is currently managed.
         --  Because of that, each kernel has a timer of their own which they
         --  fill in with the details of the currently active execution timer.

         --  A timer is not set for scheduler agents, but are for tasks and
         --  interrupt agents. The following code also sets up a timer to
         --  trigger on budget exhaustion if this event will occur before the
         --  selected timer.

         --  OPTIMISATION: If the next timer is already the same as the current
         --  timer then there has been no changes to the timing system. This is
         --  because any timer changes would also cause a scheduler agent to
         --  run, in which case the current timer would be No_Timer. Note that
         --  there is an edge case if the task decides to top up its execution
         --  budget. In this case it sets the current timer to No_Timer to
         --  force a refresh. An alternative approach would be to compare
         --  Current_Agent to Next_Agent. Or compare times.

         if Next_Agent not in Scheduler_Id
           and then Current_Timer /= Next_Timer
         then
            declare
               Budget_Task    : constant Oak_Agent_Id :=
                                  Earliest_Expiring_Budget
                                    (Charge_List (My_Kernel_Id),
                                     Current_Priority (My_Kernel_Id));
               Budget_Expires : Time;
            begin
               if Budget_Task /= No_Agent then
                  --  Earliest_Expiring_Budget only returns a non No_Agent only
                  --   if the remaining budget is less than Time_Span_Last.

                  Budget_Expires := Clock + Remaining_Budget (Budget_Task);

                  if Next_Timer = No_Timer
                    or else Budget_Expires < Firing_Time (Next_Timer)
                  then
                     if Budget_Task in Task_Id then
                        Next_Timer := Budget_Timer (Budget_Task);
                     elsif Budget_Task in Scheduler_Id then
                        Next_Timer := Timer_For_Scheduler_Agent (Budget_Task);
                     else
                        raise Program_Error;
                     end if;

                     Update_Timer
                       (Timer    => Next_Timer,
                        New_Time => Budget_Expires);
                  end if;
               end if;
            end;

            --  These called functions are responsible for enabling the sleep
            --  timer.

            if Next_Timer /= No_Timer then
               Core_Support_Package.Task_Support.Set_Oak_Wake_Up_Timer
                 (Wake_Up_At => Firing_Time (Next_Timer));
            end if;
         end if;

         --  Store the value of Next_Agent and Next_Timer into the kernel
         --  data structure.

         Set_Current_Agent (Oak_Kernel => My_Kernel_Id, Agent => Next_Agent);
         Set_Current_Timer (Oak_Kernel => My_Kernel_Id, Timer => Next_Timer);
         Set_Hardware_Priority (Current_Priority (Oak_Kernel => My_Kernel_Id));

         Current_Agent := Next_Agent;
         Current_Timer := Next_Timer;

         --  If a timer needs servicing we do not switch to the agent, instead
         --  we service the timer straight away (removes needless context
         --  switching).

         if Firing_Time (Current_Timer) <= Clock then
            Agent_Message := (Message_Type => No_Message);
            Reason_For_Run := Timer;
         else
            --  Exit Oak and switch to selected agent

            if Is_Agent_Interrupted (Next_Agent) then
               Context_Switch_Will_Be_To_Interrupted_Task;
            else
               Context_Switch_Will_Be_To_Agent;
            end if;

            Update_Exit_Stats (Oak_Kernel => This_Oak_Kernel);

            --  Switch to the selected agent.

            Context_Switch_To_Agent : declare
               Received_Message_Address : Address;
            begin
               Context_Switch_From_Oak
                 (Reason_For_Oak_To_Run => Reason_For_Run,
                  Message_Address       => Received_Message_Address);
               if Reason_For_Run = Agent_Request
                 and then Received_Message_Address /= Null_Address
               then
                  --  Copy message into kernel space

                  Copy_Oak_Message (Destination => Agent_Message'Address,
                                    Source      => Received_Message_Address);
               else
                  Agent_Message := (Message_Type => No_Message);
               end if;
               Set_Agent_Message_Address
                 (For_Agent       => Current_Agent,
                  Message_Address => Received_Message_Address);
            end Context_Switch_To_Agent;

            -------------------------------
            -- Start of the OAK RUN LOOP --
            -------------------------------

            --  Note the start is down here instead of logically up the top
            --  because the first time the run loop is invoked, none of the
            --  code below here is relevant to that first run through. This
            --  saves a few needless comparisions to a first run flag.

            --  First thing is to calculate run-time statistics and remove the
            --  current task from the charge list. Does not apply when this is
            --  the first run.

            Update_Entry_Stats (Oak_Kernel => My_Kernel_Id);

            --  Flag if the current agent was interrupted (affects how the
            --  context switch back is handled.

            case Reason_For_Run is
               when Agent_Request =>
                  Set_Agent_Interrupted (Current_Agent, False);

               when Timer | External_Interrupt =>
                  Set_Agent_Interrupted (Current_Agent);
            end case;
         end if;

         --  Check to see why we are running.
         --
         --  Activation of the run loop can be due to four (five?) things:
         --   1. Intial activation of the run loop,
         --   2. Timer expiration due to a task activating (via a Scheduler
         --  Agent),
         --   3. Timer expiration due to a missed deadline,
         --   4. Turns out we forgot this one. Turns out that the run-loop can
         --  be
         --  activated by the completion of a task.
         --  (5. An interrupt may release a task why it is sleeping. This maybe
         --   covered by the general Proected Object mechanism.)
         --
         --  So we can record in a variable before the system sleeps (or busy
         --  wait if we do not have a sleep option (could we just halt the
         --  processor?)) (or during the setup routine) or we can check at wake
         --  up.
         --  Better to record before sleep because we already know which of the
         --  three (really 2 options) has caused us to wake up.

         --  ????? Check to see if the above is valid and makes sense.

--           Invoke_Reason_Table.Reason_For_Run (Reason_For_Run) :=
--             Invoke_Reason_Table.Reason_For_Run (Reason_For_Run) + 1;

         case Reason_For_Run is
         when Agent_Request =>
--         Invoke_Reason_Table.Message_Reason (Agent_Message.Message_Type) :=
--           Invoke_Reason_Table.Message_Reason (Agent_Message.Message_Type)
            --                + 1;

            --  The task has yielded to tell or ask Oak something. The agent
            --  in question is stored in Current_Agent.

            case Agent_Message.Message_Type is
               when Activation_Pending =>
                  --  Only applies to task agents

                  if Current_Agent in Task_Id then
                     Agent.Tasks.Activation.Start_Activation
                       (Activator        => Current_Agent,
                        Activation_List  =>
                          Agent_Message.Activation_List);
                  else
                     Set_Oak_Message (Current_Agent, Message_Is_Bad);
                  end if;

               when Activation_Complete =>
                  --  Activation_Complete Message only applies to task agents.

                  if Current_Agent in Task_Id then
                     Agent.Tasks.Activation.Finish_Activation
                       (Activator        => Current_Agent);
                  else
                     Set_Oak_Message (Current_Agent, Message_Is_Bad);
                  end if;

               when Activation_Successful =>
                  --  Activation_Successful message only applies to task agents

                  if Current_Agent in Task_Id then
                     Set_State
                       (For_Agent => Current_Agent,
                        State     => Activation_Successful);
                  else
                     Set_Oak_Message (Current_Agent, Message_Is_Bad);
                  end if;

               when Scheduler_Agent_Done =>
                  Post_Run_Scheduler_Agent
                    (Agent   => Current_Agent,
                     Message => Agent_Message);

               when Sleeping =>
                  --  Sleeping only applies to task agents.
                  --  ??? Should we allow other agents to use this message?

                  if Current_Agent in Task_Id then
                     Set_State (Current_Agent, Sleeping);
                     Set_Wake_Time (Current_Agent,
                                    Agent_Message.Wake_Up_At);
                     Inform_Scheduler_Agent_Has_Changed_State
                       (Changed_Agent => Current_Agent);
                  else
                     Set_Oak_Message (Current_Agent, Message_Is_Bad);
                  end if;

               when Setup_Cycles =>
                  --  Setup_Cycles only appies to task agents.

                  if Current_Agent in Task_Id then
                     Setup_Cyclic_Section (For_Task => Current_Agent);
                  else
                     Set_Oak_Message (Current_Agent, Message_Is_Bad);
                  end if;

               when New_Cycle =>
                  --  New_Cycles only appies to task agents.

                  if Current_Agent in Task_Id then
                     New_Cycle (For_Task => Current_Agent);
                  else
                     Set_Oak_Message (Current_Agent, Message_Is_Bad);
                  end if;

               when Release_Task =>
                  --  Applies to task and interrupt agents. No test needed
                  --  since they are the only agents who run Oak.

                  Release_Task
                    (Task_To_Release   => Agent_Message.Task_To_Release,
                     Releasing_Agent   => Current_Agent);

               when Update_Task_Property =>
                  --  Applies only to task agents

                  if Current_Agent in Task_Id then
                     Update_Task_Property
                       (For_Task           => Agent_Message.Update_Task,
                        Property_To_Update =>
                          Agent_Message.Property_To_Update);
                  else
                     Set_Oak_Message (Current_Agent, Message_Is_Bad);
                  end if;

               when Entering_PO =>
                  --  Applies only to task agents.

                  if Current_Agent in Task_Id then
                     Protected_Objects.Process_Enter_Request
                       (Entering_Agent  => Current_Agent,
                        PO              => Agent_Message.PO_Enter,
                        Subprogram_Kind => Agent_Message.Subprogram_Kind,
                        Entry_Id        => Agent_Message.Entry_Id_Enter);
                  else
                     Set_Oak_Message (Current_Agent, Message_Is_Bad);
                  end if;

               when Exiting_PO =>
                  --  Applies only to task agents.

                  if Current_Agent in Task_Id then
                     Protected_Objects.Process_Exit_Request
                       (Exiting_Agent => Current_Agent,
                        PO            => Agent_Message.PO_Exit);
                  else
                     Set_Oak_Message (Current_Agent, Message_Is_Bad);
                  end if;

               when Attach_Interrupt_Handler =>
                  --  Applies only to task agents.

                  if Current_Agent in Task_Id then
                     Attach_Handler
                       (Handler => Agent_Message.Attach_Handler);
                  else
                     Set_Oak_Message (Current_Agent, Message_Is_Bad);
                  end if;

               when Interrupt_Done =>
                  --  Only applies to interrupt agents.

                  if Current_Agent in Agent.Interrupt_Id then
                     Interrupt_Done
                       (Kernel        => My_Kernel_Id,
                        Current_Agent => Current_Agent);
                  else
                     Set_Oak_Message (Current_Agent, Message_Is_Bad);
                  end if;

               when Adding_Agent =>
                  --  Only applies to task agents.

                  Set_State (Current_Agent, Runnable);
                  Add_Agent_To_Scheduler (Agent_Message.Agent_To_Add);
               when others =>
                  null;
            end case;

         when External_Interrupt =>
            Handle_External_Interrupt;

         when Timer =>

--              Invoke_Reason_Table.Timer_Kind (Timer_Kind (Current_Timer)) :=
--             Invoke_Reason_Table.Timer_Kind (Timer_Kind (Current_Timer)) + 1;
            --
            --              if not Has_Timer_Fired (Current_Timer) then
            --                 Invoke_Reason_Table.Early_Fire :=
            --                   Invoke_Reason_Table.Early_Fire + 1;
            --              end if;

            if Current_Timer = No_Timer
              or else not Has_Timer_Fired (Current_Timer)
            then
               --  False alarm, go back to what we were doing. Occurs in cases
               --  where the size of the timer used is smaller that the size
               --  of the clock.

               null;

            elsif Timer_Kind (Current_Timer) = Scheduler_Timer then
               --  A scheduler needs handling.

               Service_Scheduler_Agent_Timer
                 (Scheduler_Agent (Timer => Current_Timer));

            elsif Timer_Kind (Current_Timer) = Event_Timer then
               --  An event timer wishes to run.

               --  Need to deactivate the timer to stop it from firing again.
               --  Note that this does not stop timers associated with
               --  execution budgets from firing again.

               Deactivate_Timer (Timer => Current_Timer);

               --  HACK: To shut up execution budgets for now
               Set_Remaining_Budget
                 (For_Agent => Agent_To_Handle (Current_Timer),
                  To_Amount => Time_Span_Last);

               --  Handle the different timer handler responses.
               --  ??? Should this move to Oak.Timers?

               case Timer_Action (Current_Timer) is
                  when Ada.Cyclic_Tasks.Handler =>
                     Handle_Event : declare
                        P              : constant Any_Priority :=
                                           Timer_Priority (Current_Timer);
                        Interrup_Agent : constant Agent.Interrupt_Id :=
                                           Interrupt_For_Priority
                                             (Oak_Kernel => My_Kernel_Id,
                                              Priority   => P);
                     begin
                        Set_State (Interrup_Agent, Handling_Interrupt);
                        Set_Interrupt_Kind (Interrup_Agent,
                                            Kind => Timer_Action);
                        Set_Timer_To_Handle
                          (Agent => Interrup_Agent,
                           Timer => Current_Timer);
                        Activate_Interrupt_Agent
                          (Oak_Kernel => My_Kernel_Id,
                           Interrupt  => Interrup_Agent);
                     end Handle_Event;

                  when others =>
                     null;
               end case;

            end if;
         end case;
      end loop;
   end Run_Loop;

   -----------
   -- Start --
   -----------

   procedure Start is
   begin
      --  We'll loop here for multiprocessors to load an individual copy of the
      --  kernel onto each physical processor. Will require calling procesor
      --  dependent code to launch the kernel on each processor.
      Start_Oak_Kernel;
   end Start;

   -----------------------
   -- Start_Oak_Kernel --
   -----------------------

   procedure Start_Oak_Kernel is
   begin
      Run_Loop;
   end Start_Oak_Kernel;

   ------------------------
   -- Update_Entry_Stats --
   ------------------------

   procedure Update_Entry_Stats (Oak_Kernel : in Kernel_Id)
   is
      Current_Time : constant Time := Clock;
      Charge_Time  : constant Time_Span :=
                       Current_Time - Entry_Exit_Stamp (Oak_Kernel);
   begin
      Charge_Execution_Time_To_List
        (List             => Charge_List (Oak_Kernel),
         Exec_Time        => Charge_Time,
         Current_Agent    => Current_Agent (Oak_Kernel),
         Current_Priority => Current_Priority (Oak_Kernel));
      Set_Entry_Exit_Stamp (Oak_Kernel, Time => Current_Time);
   end Update_Entry_Stats;

   -----------------------
   -- Update_Exit_Stats --
   -----------------------

   procedure Update_Exit_Stats (Oak_Kernel : in Kernel_Id)
   is
      Current_Time : constant Time := Clock;
      Charge_Time  : constant Time_Span :=
                       Current_Time - Entry_Exit_Stamp (Oak_Kernel);
   begin
      Charge_Execution_Time
        (To_Agent  => Oak_Kernel,
         Exec_Time => Charge_Time);
      Increment_Execution_Cycle_Count (Oak_Kernel);
      Set_Entry_Exit_Stamp (Oak_Kernel, Time => Current_Time);
   end Update_Exit_Stats;

end Oak.Core;
