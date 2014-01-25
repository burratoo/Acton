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

with Ada.Cyclic_Tasks;
with Oak.Processor_Support_Package.Interrupts;
use Oak.Processor_Support_Package.Interrupts;

with Oak.Agent.Interrupts;        use Oak.Agent.Interrupts;
with Oak.Agent.Kernel;            use Oak.Agent.Kernel;
with Oak.Agent.Oak_Agent;         use Oak.Agent.Oak_Agent;
with Oak.Agent.Protected_Objects; use Oak.Agent.Protected_Objects;
with Oak.Agent.Schedulers;        use Oak.Agent.Schedulers;
with Oak.Agent.Tasks;             use Oak.Agent.Tasks;
with Oak.Agent.Tasks.Cycle;       use Oak.Agent.Tasks.Cycle;
with Oak.Agent.Tasks.Activation;  use Oak.Agent.Tasks.Activation;

with Oak.Interrupts;        use Oak.Interrupts;
with Oak.Protected_Objects; use Oak.Protected_Objects;
with Oak.Scheduler;         use Oak.Scheduler;
with Oak.States;            use Oak.States;
with Oak.Timers;            use Oak.Timers;

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
      Setup_Storage;
      for Processor in Processors'Range loop
         New_Kernel_Agent (Agent => Kid);
      end loop;

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

   ---------------------------
   -- Request_Agent_Service --
   ---------------------------

   --  Stores the pointer of the agent's outgoing message into a register
   --  and then context is switched to the recieving agent which will pick it
   --  up. The other agent will do likewise and we pick up its message location
   --  and copy its contents into our current message store.

   procedure Request_Agent_Service (Message : in out Oak_Message) is
      Message_Pointer : Message_Access := Message'Unrestricted_Access;
   begin
      Context_Switch_Save_Callee_Registers (Message_Pointer);
      if Message_Pointer /= null then
         Message := Message_Pointer.all;
      end if;
   end Request_Agent_Service;

   -------------------------
   -- Request_Oak_Service --
   -------------------------

   procedure Request_Oak_Service
     (Reason_For_Run : in Run_Reason;
      Message        : in out Oak_Message)
   is
      pragma Unreferenced (Reason_For_Run, Message);
   begin
      Context_Switch_Save_Callee_Registers;
   end Request_Oak_Service;

   --------------
   -- Run_Loop --
   --------------

   --  This procedure should not use any callee save registers since they will
   --  get trashed during the context switch.

   procedure Run_Loop is
   begin
      --  A block is used here so No_Message_Here does not sit forever unused
      --  on Oak Kernel's stack.

      declare
         No_Message_Here : Oak_Message := (Message_Type => No_Message);
      begin
         Run_Oak
           (Reason_For_Run => First_Run, Message => No_Message_Here);
      end;
      loop
         declare
            Agent_Message   : Message_Access;
            Reason_For_Run  : Run_Reason;
         begin
            Context_Switch_From_Oak
              (Reason_For_Oak_To_Run => Reason_For_Run,
               Message               => Agent_Message);

            if Agent_Message /= null then
               Run_Oak
                 (Reason_For_Run => Reason_For_Run,
                  Message        => Agent_Message.all);
            end if;
         end;
      end loop;
   end Run_Loop;

   -------------
   -- Run_Oak --
   -------------

   --  Run_Oak exists seperate from Run_Loop for the simple reason that this
   --  way there is no need to save any of Oak's registers.

   procedure Run_Oak
     (Reason_For_Run : in      Run_Reason;
      Message        : in out  Oak_Message)
   is
      My_Kernel_Id   : constant Kernel_Id := This_Oak_Kernel;
      --  The Id of this kernel.

      Current_Agent  : Oak_Agent_Id := Kernel.Current_Agent (My_Kernel_Id);
      --  The currently selected Agent.

      Current_Timer  : Oak_Timer_Id := Kernel.Current_Timer (My_Kernel_Id);
      --  The currently selected timer.

      Next_Agent     : Oak_Agent_Id := No_Agent;
      --  The next agent to run.

      Next_Timer     : Oak_Timer_Id := No_Timer;
      --  The next firing timer.

      Message_Is_Bad : constant Oak_Message :=
                         (Message_Type => Invalid_Message);
      --  An invalid message.

      Oak_Running_Because : Run_Reason := Reason_For_Run;
      --  Variable to hold the run reason since will be updated if we find a
      --  timer that can be serviced straight away.

   begin
      --  First thing is to calculate run-time statistics and remove the
      --  current task from the charge list. Does not apply when this is the
      --  first run.

      if Reason_For_Run /= First_Run then
         Update_Entry_Stats (Oak_Kernel => My_Kernel_Id);
         Remove_Agent_From_Charge_List
           (Oak_Kernel => My_Kernel_Id, Agent => Current_Agent);
      end if;

      --  Flag if the current agent was interrupted (affects how the context
      --  switch back is handled.

      case Oak_Running_Because is
         when First_Run | Agent_Request =>
            Set_Agent_Interrupted (Current_Agent, False);

         when Timer | External_Interrupt =>
            Set_Agent_Interrupted (Current_Agent);
      end case;

      loop

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

         case Oak_Running_Because is
         when First_Run =>
            --  First time the kernel instance has run. Simply check for
            --  the first task to run – which would have been already placed
            --  in a scheduler agent.

            Check_Sechduler_Agents_For_Next_Agent_To_Run
              (From_Scheduler_Agent => Top_Level_Schedulers (My_Kernel_Id),
               Next_Agent_To_Run    => Next_Agent);

         when Agent_Request =>
            --  The task has yielded to tell or ask Oak something. The agent
            --  in question is stored in Current_Agent.

            case Message.Message_Type is
               when Activation_Pending =>
                  --  Only applies to task agents

                  if Current_Agent in Task_Id then
                     Agent.Tasks.Activation.Start_Activation
                       (Activator        => Current_Agent,
                        Activation_List  => Message.Activation_List,
                        Next_Task_To_Run => Next_Agent);

                  else
                     Message := Message_Is_Bad;
                     Next_Agent     := Current_Agent;
                  end if;

               when Activation_Complete =>
                  --  Activation_Complete Message only applies to task agents.

                  if Current_Agent in Task_Id then
                     Agent.Tasks.Activation.Finish_Activation
                       (Activator        => Current_Agent,
                        Next_Task_To_Run => Next_Agent);

                  else
                     Message := Message_Is_Bad;
                     Next_Agent     := Current_Agent;
                  end if;

               when Sleeping =>
                  --  Sleeping only applies to task agents.
                  --  ??? Should we allow other agents to use this message?

                  if Current_Agent in Task_Id then
                     Set_State (Current_Agent, Sleeping);
                     Set_Wake_Time (Current_Agent, Message.Wake_Up_At);
                     Inform_Scheduler_Agent_Has_Changed_State
                       (Changed_Agent     => Current_Agent,
                        Next_Agent_To_Run => Next_Agent);
                  else
                     Message := Message_Is_Bad;
                     Next_Agent     := Current_Agent;
                  end if;

               when Setup_Cycles =>
                  --  Setup_Cycles only appies to task agents.

                  if Current_Agent in Task_Id then
                     Setup_Cyclic_Section
                       (For_Task          => Current_Agent,
                        Next_Agent_To_Run => Next_Agent);
                  else
                     Message    := Message_Is_Bad;
                     Next_Agent := Current_Agent;
                  end if;

               when New_Cycle =>
                  --  New_Cycles only appies to task agents.

                  if Current_Agent in Task_Id then
                     New_Cycle
                       (For_Task          => Current_Agent,
                        Next_Agent_To_Run => Next_Agent);
                  else
                     Message    := Message_Is_Bad;
                     Next_Agent := Current_Agent;
                  end if;

               when Release_Task =>
                  --  Applies to task and interrupt agents. No test needed
                  --  since they are the only agents who run Oak.

                  Release_Task
                    (Task_To_Release   => Message.Task_To_Release,
                     Releasing_Agent   => Current_Agent,
                     Next_Agent_To_Run => Next_Agent);

               when Update_Task_Property =>
                  --  Applies only to task agents

                  if Current_Agent in Task_Id then
                     Update_Task_Property
                       (For_Task           => Message.Update_Task,
                        Property_To_Update => Message.Property_To_Update,
                        Next_Task_To_Run   => Next_Agent);
                  else
                     Message := Message_Is_Bad;
                     Next_Agent     := Current_Agent;
                  end if;

               when Entering_PO =>
                  --  Applies only to task agents.

                  if Current_Agent in Task_Id then
                     Protected_Objects.Process_Enter_Request
                       (Entering_Agent    => Current_Agent,
                        PO                => Message.PO_Enter,
                        Subprogram_Kind   => Message.Subprogram_Kind,
                        Entry_Id          => Message.Entry_Id_Enter,
                        Next_Agent_To_Run => Next_Agent);
                  else
                     Message := Message_Is_Bad;
                     Next_Agent     := Current_Agent;
                  end if;

               when Exiting_PO =>
                  --  Applies only to task agents.

                  if Current_Agent in Task_Id then
                     Protected_Objects.Process_Exit_Request
                       (Exiting_Agent     => Current_Agent,
                        PO                => Message.PO_Exit,
                        Next_Agent_To_Run => Next_Agent);
                  else
                     Message := Message_Is_Bad;
                     Next_Agent     := Current_Agent;
                  end if;

               when Attach_Interrupt_Handler =>
                  --  Applies only to task agents.

                  if Current_Agent in Task_Id then
                     Attach_Handler
                       (Handler           => Message.Attach_Handler,
                        Current_Agent     => Current_Agent,
                        Next_Agent_To_Run => Next_Agent);
                  else
                     Message := Message_Is_Bad;
                     Next_Agent     := Current_Agent;
                  end if;

               when Interrupt_Done =>
                  --  Only applies to interrupt agents.

                  if Current_Agent in Interrupt_Id then
                     Deactivate_Interrupt_Agent
                       (Oak_Kernel => My_Kernel_Id,
                        Interrupt  => Current_Agent);

                     if Interrupt_Kind (Current_Agent) = Timer_Action then
                        Release_Protected_Object_For_Interrupt
                          (Protected_Object_From_Access (
                           Handler (Timer_To_Handle (Current_Agent))));
                     end if;

                     Check_Sechduler_Agents_For_Next_Agent_To_Run
                       (From_Scheduler_Agent =>
                          Top_Level_Schedulers (My_Kernel_Id),
                        Next_Agent_To_Run    => Next_Agent);

                  else
                     Message := Message_Is_Bad;
                     Next_Agent     := Current_Agent;
                  end if;

               when Adding_Agent =>
                  --  Only applies to task agents.

                  Set_State (Current_Agent, Runnable);
                  Add_Agent_To_Scheduler (Message.Agent_To_Add);
                  Check_Sechduler_Agents_For_Next_Agent_To_Run
                    (From_Scheduler_Agent =>
                        Top_Level_Schedulers (My_Kernel_Id),
                     Next_Agent_To_Run    => Next_Agent);
               when others =>
                  null;
            end case;

         when External_Interrupt =>
            Handle_External_Interrupt : declare
               Interrupt_Id : constant External_Interrupt_Id :=
                                Get_External_Interrupt_Id;

               P : constant Interrupt_Priority := Current_Interrupt_Priority;

            begin
               Next_Agent := Interrupt_For_Priority
                 (Oak_Kernel => My_Kernel_Id, Priority   => P);
               Set_State (Next_Agent, Handling_Interrupt);
               Set_Interrupt_Kind
                 (For_Agent => Next_Agent, Kind => External);
               Set_External_Id
                 (For_Agent => Next_Agent, Id => Interrupt_Id);
               Activate_Interrupt_Agent
                 (Oak_Kernel => My_Kernel_Id, Interrupt => Next_Agent);
            end Handle_External_Interrupt;

         when Timer =>

            if Current_Timer = No_Timer
              or else not Has_Timer_Fired (Current_Timer)
            then
               --  False alarm, go back to what we were doing. Occurs in cases
               --  where the size of the timer used is smaller that the size
               --  of the clock.

               Next_Agent := Current_Agent;

            elsif Timer_Kind (Current_Timer) = Scheduler_Timer then
               --  A scheduler wants to run.

               case Scheduler_Action (Current_Timer) is
                  when Service =>
                     Run_The_Bloody_Scheduler_Agent_That_Wanted_To_Be_Woken
                       (Scheduler         =>
                           Scheduler_Agent (Timer => Current_Timer),
                        Current_Agent     => Current_Agent,
                        Next_Agent_To_Run => Next_Agent);

                  when End_Cycle =>
                     New_Scheduler_Cycle
                       (Scheduler         =>
                           Scheduler_Agent (Timer => Current_Timer),
                        Next_Agent_To_Run => Next_Agent);
               end case;

            elsif Timer_Kind (Current_Timer) = Event_Timer then
               --  An event timer wishes to run.

               --  Need to deactivate the timer to stop it from firing again.
               Deactivate_Timer (Timer => Current_Timer);

               --  Handle the different timer handler responses.
               --  ??? Should this move to Oak.Timers?

               case Timer_Action (Current_Timer) is
                  when Ada.Cyclic_Tasks.Handler =>
                     Handle_Event : declare
                        P : constant Any_Priority :=
                              Timer_Priority (Current_Timer);
                     begin
                        Next_Agent := Interrupt_For_Priority
                          (Oak_Kernel => My_Kernel_Id, Priority   => P);
                        Set_State (Next_Agent, Handling_Interrupt);
                        Set_Interrupt_Kind (Next_Agent, Kind => Timer_Action);
                        Set_Timer_To_Handle
                          (Agent => Next_Agent, Timer => Current_Timer);
                        Activate_Interrupt_Agent
                          (Oak_Kernel => My_Kernel_Id,
                           Interrupt  => Next_Agent);
                        Acquire_Protected_Object_For_Interrupt
                          (Protected_Object_From_Access (
                           Handler (Current_Timer)));
                     end Handle_Event;

                  when others =>
                     Check_Sechduler_Agents_For_Next_Agent_To_Run
                       (From_Scheduler_Agent =>
                           Top_Level_Schedulers (My_Kernel_Id),
                        Next_Agent_To_Run    => Next_Agent);
               end case;

            end if;
         end case;

         --  Check to see if any interrupt agents are active and have a
         --  priority equal to and above the agent selected above.

         Handle_Active_Interrupts : declare
            Interrupt_Agent : constant Interrupt_Id_With_No :=
                                Find_Top_Active_Interrupt (My_Kernel_Id);
         begin
            --  Select the interrupt agent if it has a priority equal to or
            --  higher than the agent selected above. Note that it does not
            --  matter if either Next_Agent or Interrut_Agent is No_Agent as it
            --  maps to the sleep agent that has a value of Priority'First.

            if Normal_Priority (Interrupt_Agent) >=
              Normal_Priority (Next_Agent)
            then
               Next_Agent := Interrupt_Agent;
            end if;
         end Handle_Active_Interrupts;

         --  Set the current priority the core is running at now (which may not
         --  correspond to the current agent's priority due to the correction
         --  that follows).

         Set_Current_Priority
           (Oak_Kernel => My_Kernel_Id,
            Priority   => Normal_Priority (Next_Agent));

         --  Correct Next Agent. Needed to cover the case where a protected
         --  agent has been selected which cannot be executed directly and for
         --  an activator task which is activating child tasks.

         if Next_Agent in Protected_Id then
            Next_Agent := Task_Within (Next_Agent);
         elsif State (Next_Agent) = Activation_Pending then
            --  This call will select the correct task to run.

            Continue_Activation
              (Activator        => Next_Agent,
               Next_Task_To_Run => Next_Agent);
         end if;

         ---------------
         --  After we run the Scheduler Agents, all that is left to do is set
         --  the time for the Run_Loop's next activation and record the reason
         --  for it's activation. Then we can use the kernel's scheduler data
         --  structure to load the next scheduled task into the processor. If
         --  there are no tasks available to run we put the processor into a
         --  processor define sleep or halt state, (or god forbid a busy wait
         --  loop). Note that the kernel jumps to the task in question, rather
         --  than call a procedure.
         -------------------

         Next_Timer :=
           Earliest_Timer_To_Fire
             (Above_Priority => Normal_Priority (Next_Agent));

         --  If the selected timer has fired, we service it now by setting the
         --  run reason to timer and run the loop again. Otherwise we exit and
         --  run the selected task.

         exit when not Has_Timer_Fired (Next_Timer);

         --  Update current agent and current timer variables used by the loop.
         --  No need to update the kernel data structure since these are not
         --  referenced here.

         Current_Timer := Next_Timer;
         Current_Agent := Next_Agent;
         Oak_Running_Because := Timer;
      end loop;

      --  Execution timers do not live in the active timer store since they are
      --  only relevant for the currently executing task and only apply if the
      --  timer will fire before any timer that is currently managed. Because
      --  of that, each kernel has a timer of their own which they fill in with
      --  the details of the currently active execution timer.

      Add_Agent_To_Charge_List
        (Oak_Kernel => My_Kernel_Id,
         Agent      => Next_Agent);

      declare
         Budget_Task    : constant Oak_Agent_Id :=
                            Earliest_Expiring_Budget
                              (Charge_List (My_Kernel_Id));
         Budget_Expires : Time;
      begin
         if Budget_Task /= No_Agent then
            --  Earliest_Expiring_Budget only returns a non No_Agent only if
            --  the remaining budget is less than Time_Span_Last.

            Budget_Expires := Clock + Remaining_Budget (Budget_Task);

            if Next_Timer = No_Timer
              or else Budget_Expires < Firing_Time (Current_Timer)
            then
               if Budget_Task in Task_Id then
                  Next_Timer := Kernel_Timer (My_Kernel_Id);
                  Set_Timer_Event_Data
                    (Timer => Next_Timer,
                     Data  => Budget_Action (Budget_Task));

               elsif Budget_Task in Scheduler_Id then
                  Next_Timer := Timer_For_Scheduler_Agent (Budget_Task);
                  Update_Timer
                    (Timer    => Next_Timer,
                     New_Time => Budget_Expires);
                  Set_Timer_Scheduler_Action
                    (Timer            => Next_Timer,
                     Scheduler_Action => End_Cycle);
               else
                  raise Program_Error;
               end if;

               Update_Timer (Next_Timer, New_Time => Budget_Expires);
            end if;
         end if;
      end;

      --  These called functions are responsible for enabling the sleep
      --  timer.

      if Next_Timer /= No_Timer then
         Core_Support_Package.Task_Support.Set_Oak_Wake_Up_Timer
           (Wake_Up_At => Firing_Time (Next_Timer));
      end if;

      --  Store the value of Next_Agent and Next_Timer into the kernel
      --  data structure.

      Set_Current_Agent (Oak_Kernel => My_Kernel_Id, Agent => Next_Agent);
      Set_Current_Timer (Oak_Kernel => My_Kernel_Id, Timer => Next_Timer);
      Set_Hardware_Priority (Current_Priority (Oak_Kernel => My_Kernel_Id));

      if Is_Agent_Interrupted (Next_Agent) then
         Context_Switch_Will_Be_To_Interrupted_Task;
      else
         Context_Switch_Will_Be_To_Agent;
      end if;

      Update_Exit_Stats (Oak_Kernel => This_Oak_Kernel);

   end Run_Oak;

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
      Set_Entry_Exit_Stamp (Oak_Kernel, Time => Current_Time);
   end Update_Exit_Stats;

end Oak.Core;
