with Oak.Agent.Schedulers;   use Oak.Agent.Schedulers;
with Oak.Agent.Tasks.Activation;
with Oak.Agent.Tasks.Cycle; use Oak.Agent.Tasks.Cycle;
with Oak.Agent.Tasks.Protected_Objects; use Oak.Agent.Tasks.Protected_Objects;
with Oak.Atomic_Actions;
with Oak.Core_Support_Package.Task_Support;
use Oak.Core_Support_Package.Task_Support;
with Oak.Interrupts; use Oak.Interrupts;
with Oak.Protected_Objects;
with Oak.Core_Support_Package.Interrupts;
with Ada.Cyclic_Tasks;
with Oak.Core_Support_Package.Call_Stack;
use Oak.Core_Support_Package.Call_Stack;
with Oak.Processor_Support_Package.Interrupts;
use Oak.Processor_Support_Package.Interrupts;

with Oak.Message; use Oak.Message;
with Oak.States;  use Oak.States;

package body Oak.Core is

   ----------------
   -- Initialise --
   ----------------

   procedure Initialise is
   begin
      for Kernel_Agent of Processor_Kernels loop
         Initialise_Agent
           (Agent => Kernel_Agent'Access,
            Name  => "Kernel",
            Call_Stack_Size =>
              Core_Support_Package.Call_Stack.Oak_Call_Stack_Size);

         Kernel_Agent.Woken_By         := First_Run;
         Kernel_Agent.Current_Priority := System.Any_Priority'First;
         Kernel_Agent.Current_Agent    := Main_Task_OTCR'Access;

         for P in Interrupt_Priority'Range loop
            Initialise_Interrupt_Agent
              (Agent    => Kernel_Agent.Interrupt_Agents (P)'Access,
               Priority => P);
         end loop;

         for State of Kernel_Agent.Interrupt_States loop
            State := Inactive;
         end loop;

         Initialise_Agent
           (Agent              => Kernel_Agent.Sleep_Agent'Access,
            Name               => "Sleep",
            Call_Stack_Address => Null_Address,
            Call_Stack_Size    => Sleep_Stack_Size,
            Run_Loop           => Sleep_Agent'Address,
            Run_Loop_Parameter => Null_Address,
            Normal_Priority    => Priority'First,
            Initial_State      => Runnable,
            Wake_Time          => Time_Last);
      end loop;

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

   -----------
   -- Start --
   -----------

   procedure Start is
   begin
      --  We'll loop here for multiprocessors to load an individual copy of the
      --  kernel onto each physical processor. Will require calling procesor
      --  dependent code to launch the kernel on each processor.
      Start_Oak_Instance
        (Oak_Instance => Processor_Kernels (Processor_Kernels'First));
   end Start;

   ------------------------
   -- Start_Oak_Instance --
   ------------------------

   procedure Start_Oak_Instance (Oak_Instance : in out Oak_Data) is
   begin
      --  Set up Scheduler Agents and load tasks.

      Oak_Instance.Woken_By := First_Run;

      Run_Loop (Oak_Instance => Oak_Instance);
   end Start_Oak_Instance;

   --------------
   -- Run_Loop --
   --------------

   procedure Run_Loop (Oak_Instance : in out Oak_Data) is
      Active_Timer  : access Oak.Timers.Oak_Timer'Class;
      Next_Agent    : Agent_Handler := null;
      Charge_List   : access Oak_Agent'Class renames
                        Oak_Instance.Budgets_To_Charge;
      Current_Agent : not null access Oak_Agent'Class renames
                        Oak_Instance.Current_Agent;
      P             : Any_Priority;
      Interrupt_Id  : Oak_Interrupt_Id;

      Task_Message  : Oak_Message := (Message_Type => No_State);
   begin
      loop
      --  Actually the first step should be to mask the timer interrupt we use
      --  to wake up so that the run loop isn't interrupted.

      --  Then we should check to see if in between the wakeup timer going
      --  off and the run-loop starting that a deadline may have been
      --  missed. This means that we can act on the missed dealine much
      --  quicker than if we had waited until the end of the run-loop to
      --  check to see if any deadlines have been missed. That said, what is
      --  the latency between the triggering of an interrupt and reaching
      --  this point? Probably what we'll catch are deadlines that fall just
      --  after a Scheduler Agent requesting servicing. We'll be talking
      --  something very short here. Maybe we will need to test.

      --  First step: Check to see why we have been activated.
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
      --

      --  Second Step: Beak into 2 code paths to handle normal scheduling
      --  and
      --  missed deadlines? Possible a good idea as it saves having two
      --  large
      --  blocks within an if-else statement.
      --
      --  What we expect to happen when we run the Run_Scheduler_Agents
      --  subprogram:
      --  Each scheduler agent is called one by one. Here they have the
      --  opportunity to manage their queues and to nominate a task to run.
      --  They should also set the time that they wish to be activated next.
      --  On return, Oak's scheduler should have recorded which task should
      --  run now, if there are any eligible tasks to run at all.

         case Oak_Instance.Woken_By is
            when First_Run =>
               Check_Sechduler_Agents_For_Next_Task_To_Run
                 (Scheduler_Info   => Oak_Instance.Scheduler,
                  Next_Task_To_Run => Next_Agent);
            when Task_Yield =>
               case Task_Message.Message_Type is
                  when Activation_Pending =>
                     --  The activation pending state is handled specially
                     --  outside of this block.
                     null;

                  when Activation_Complete =>
                     if Current_Agent.all in Task_Agent'Class then
                        Agent.Tasks.Activation.Finish_Activation
                          (Activator => Task_Agent (Current_Agent.all));
                        Check_Sechduler_Agents_For_Next_Task_To_Run
                          (Scheduler_Info   => Oak_Instance.Scheduler,
                           Next_Task_To_Run => Next_Agent);
                     end if;

                  when Sleeping =>
                     if Current_Agent.all in Task_Agent'Class then
                        Current_Agent.Set_Wake_Time
                          (WT => Task_Message.Wake_Up_At);
                        Inform_Scheduler_Agent_Task_Has_Changed_State
                          (Changed_Task     => Task_Handler (Current_Agent),
                           Next_Task_To_Run => Next_Agent);
                     end if;

                  when Setup_Cycles =>
                     if Current_Agent.all in Task_Agent'Class then
                        Setup_Cyclic_Section (Task_Agent (Current_Agent.all));
                     end if;

                  when New_Cycle =>
                     if Current_Agent.all in Task_Agent'Class then
                        New_Cycle
                          (T                => Task_Handler (Current_Agent),
                           Next_Task_To_Run => Next_Agent);
                     end if;

                  when Release_Task =>
                     Release_Task
                       (Task_To_Release  => Task_Message.Task_To_Release,
                        Releasing_Task   => Current_Agent,
                        Next_Task_To_Run => Next_Agent);

                  when Change_Cycle_Period =>
                     declare
                        Target_Task : constant access Task_Agent'Class :=
                                        Task_Message.Cycle_Period_Task;
                     begin
                        Target_Task.Set_Cycle_Period
                           (Task_Message.New_Cycle_Period);
                        Inform_Scheduler_Agent_Task_Has_Changed_State
                          (Changed_Task     => Target_Task,
                           Next_Task_To_Run => Next_Agent);
                     end;

                  when Change_Relative_Deadline =>
                     declare
                        Target_Task : constant access Task_Agent'Class :=
                                        Task_Message.Cycle_Period_Task;
                     begin
                        Target_Task.Set_Relative_Deadline
                           (Task_Message.New_Cycle_Period);
                        Inform_Scheduler_Agent_Task_Has_Changed_State
                          (Changed_Task     => Target_Task,
                           Next_Task_To_Run => Next_Agent);
                     end;

                  when Entering_PO =>
                     Protected_Objects.Process_Enter_Request
                       (Scheduler_Info    => Oak_Instance.Scheduler,
                        Entering_Agent    => Current_Agent,
                        PO                => Task_Message.PO_Enter,
                        Subprogram_Kind   => Task_Message.Subprogram_Kind,
                        Entry_Id          => Task_Message.Entry_Id_Enter,
                        Next_Agent_To_Run => Next_Agent);

                  when Exiting_PO =>
                     Protected_Objects.Process_Exit_Request
                       (Scheduler_Info    => Oak_Instance.Scheduler,
                        Exiting_Agent     => Current_Agent,
                        PO                => Task_Message.PO_Exit,
                        Next_Agent_To_Run => Next_Agent);

                  when Attach_Interrupt_Handlers =>
                     Interrupts.Attach_Handlers
                       (Handlers          => Task_Message.Attach_Handlers,
                        Handler_PO        => Task_Message.Attach_Handler_PO,
                        Current_Agent     => Current_Task,
                        Next_Agent_To_Run => Next_Agent);

                  when Entering_Atomic_Action =>
                     Atomic_Actions.Process_Enter_Request
                       (AO                => Task_Message.AA_Enter,
                        T                 => Current_Task,
                        Scheduler_Info    => Oak_Instance.Scheduler,
                        Action_Id         => Task_Message.Action_Id_Enter,
                        Next_Agent_To_Run => Next_Agent);

                  when Entering_Exit_Barrier =>
                     Atomic_Actions.Exit_Barrier
                       (AO                => Task_Message.AA_EB,
                        T                 => Current_Task,
                        Scheduler_Info    => Oak_Instance.Scheduler,
                        Action_Id         => Task_Message.Action_Id_EB,
                        Exception_Raised  => Task_Message.Exception_Raised,
                        Next_Agent_To_Run => Next_Agent);

                  when Exiting_Atomic_Action =>
                     Atomic_Actions.Process_Exit_Request
                       (AO                => Task_Message.AA_Exit,
                        T                 => Current_Task,
                        Scheduler_Info    => Oak_Instance.Scheduler,
                        Action_Id         => Task_Message.Action_Id_Exit,
                        Exception_Raised  => Task_Message.Atomic_Exception,
                        Next_Agent_To_Run => Next_Agent);

                  when Interrupt_Done =>
                     if Current_Agent.all in Interrupt_Agent'Class then
                        Oak_Instance.Interrupt_States
                          (Next_Agent.Normal_Priority) := Inactive;
                        if Interrupt_Agent (Next_Agent.all).Interrupt_Kind =
                          Timer_Action
                        then
                           Oak.Protected_Objects.
                             Release_Protected_Object_For_Interrupt
                               (Protected_Object_From_Access
                                  (Interrupt_Agent
                                     (Next_Agent.all).Timer_To_Handle.Handler)
                               );
                        end if;

                        Check_Sechduler_Agents_For_Next_Task_To_Run
                          (Scheduler_Info   => Oak_Instance.Scheduler,
                           Next_Task_To_Run => Next_Agent);
                     end if;

                  when Adding_Agent_To_Scheduler =>
                     Current_Agent.Set_State (Runnable);
                     Add_Agent_To_Scheduler
                       (Current_Agent.Agent_Message.Agent_To_Add_To_Scheduler);
                     Check_Sechduler_Agents_For_Next_Task_To_Run
                       (Scheduler_Info   => Oak_Instance.Scheduler,
                        Next_Task_To_Run => Next_Agent);
                  when others =>
                     null;
               end case;

            when External_Interrupt =>
               Interrupt_Id := External_Interrupt_Id;
               P := Current_Interrupt_Priority;
               Next_Agent :=
                 Oak_Instance.Interrupt_Agents (P)'Unchecked_Access;
               Next_Agent.Set_State (Handling_Interrupt);
               Set_Interrupt_Kind
                 (Interrupt_Agent (Next_Agent.all), Kind => External);
               Set_External_Id
                 (Interrupt_Agent (Next_Agent.all), Interrupt_Id);
               Oak_Instance.Interrupt_States (P) := Active;

            when Timer =>

               --  False alarm, go back to what we were doing. Occurs in cases
               --  where the size of the timer used is smaller that the size
               --  of the clock.

               if Active_Timer = null
                 or else Active_Timer.Firing_Time > Clock
               then
                  null;

               elsif Active_Timer.all in Timers.Scheduler_Timer then
                  Run_The_Bloody_Scheduler_Agent_That_Wanted_To_Be_Woken
                    (Agent       =>
                       Timers.Scheduler_Timer
                         (Active_Timer.all).Timer_Scheduler_Agent,
                     Current_Agent    => Current_Agent,
                     Next_Task_To_Run => Next_Agent);

               elsif Active_Timer.all in Timers.Action_Timer then
                  Active_Timer.Remove_Timer;

                  --  Disable execution timer if fired. This is done by setting
                  --  the property Remaining_Budget to Time_Span_Last.

                  declare
                     A : constant Agent_Handler :=
                           Timers.Action_Timer'Class
                             (Active_Timer.all).Agent_To_Handle;
                  begin
                     if A.Remaining_Budget <= Time_Span_Zero then
                        A.Set_Remaining_Budget (Time_Span_Last);
                     end if;
                  end;

                  case Timers.Action_Timer'Class
                    (Active_Timer.all).Timer_Action is
                     when Ada.Cyclic_Tasks.Handler =>
                        P := Active_Timer.Priority;
                        Next_Agent :=
                          Oak_Instance.Interrupt_Agents (P)'Unchecked_Access;
                        Next_Agent.Set_State (Handling_Interrupt);
                        Set_Interrupt_Kind
                          (Interrupt_Agent (Next_Agent.all),
                           Kind => Timer_Action);
                        Set_Timer_To_Handle
                          (Interrupt_Agent (Next_Agent.all),
                           Timers.Action_Timer (Active_Timer.all)'Access);
                        Oak_Instance.Interrupt_States (P) := Active;
                        Oak.Protected_Objects.
                          Acquire_Protected_Object_For_Interrupt
                            (Protected_Object_From_Access
                              (Oak.Timers.Handler
                                (Timers.Action_Timer (Active_Timer.all))));
                     when others =>
                        Check_Sechduler_Agents_For_Next_Task_To_Run
                          (Scheduler_Info   => Oak_Instance.Scheduler,
                           Next_Task_To_Run => Next_Agent);
                  end case;

               end if;
         end case;

         --  Find any active interrupts

         P := Interrupt_Priority'Last;
         for Interrupt_State of reverse Oak_Instance.Interrupt_States loop
            if Interrupt_State = Active then
               if (Next_Agent /= null and then
                     P >= Next_Agent.Normal_Priority) or Next_Agent = null
               then
                  Next_Agent :=
                    Oak_Instance.Interrupt_Agents (P)'Unchecked_Access;
               end if;

               exit;
            end if;

            P := P - 1;
         end loop;

         --  Handle special states

         if Next_Agent /= null then
            Oak_Instance.Current_Priority := Next_Agent.Normal_Priority;

            if Next_Agent.all in Protected_Agent'Class then
               Next_Agent := Protected_Agent (Next_Agent.all).Task_Within;
            elsif Next_Agent.State = Activation_Pending then
               declare
                  Activating_Task : constant Agent_Handler :=
                     Agent.Tasks.Activation.Continue_Activation
                       (Activator => Task_Handler (Next_Agent));
               begin
                  Next_Agent := (if Activating_Task /= null then
                                    Activating_Task else Next_Agent);
               end;
            end if;
         else
            --  No task selected, so we choose the sleep agent

            Oak_Instance.Current_Priority := Oak_Priority'First;
            Next_Agent := Oak_Instance.Sleep_Agent'Unchecked_Access;
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

         Active_Timer :=
           Oak_Timer_Store.Earliest_Timer_To_Fire
             (Above_Priority => Oak_Instance.Current_Priority);

         --  Execution timers are not placed under the control of the Timer
         --  Manager since they are only relevant for the currently executing
         --  task and only apply if the timer will fire before any timer
         --  that is currently managed.

         Next_Agent.Add_Agent_To_Exec_Charge_List
           (Agent_Handler (Charge_List));

         declare
            Budget_Task    : constant access Oak_Agent'Class :=
                               Earliest_Expiring_Budget (Charge_List);
            Budget_Expires : Time;
         begin
            if Budget_Task /= null then
               Budget_Expires := Clock + Budget_Task.Remaining_Budget;
               if Active_Timer = null
                 or else Budget_Expires < Active_Timer.Firing_Time
               then
                  if Budget_Task.all in Task_Agent'Class then
                     Active_Timer := Task_Handler (Budget_Task).Budget_Timer;

                  elsif Budget_Task.all in Scheduler_Agent'Class then
                     Active_Timer :=
                       Scheduler_Handler (Budget_Task).Scheduler_Timer;
                  else
                     raise Program_Error;
                  end if;

                  Active_Timer.Update_Timer (New_Time => Budget_Expires);
               end if;
            end if;
         end;

         --  These called functions are responsible for enabling the sleep
         --  timer.

         if Active_Timer /= null then
            Core_Support_Package.Task_Support.Set_Oak_Wake_Up_Timer
              (Wake_Up_At => Active_Timer.Firing_Time);
         end if;

         --  Next_Agent becomes Current_Agent

         Current_Agent := Next_Agent;

         --  Service any timers that may have fired
         if Active_Timer /= null and then Active_Timer.Firing_Time < Clock then
            Oak_Instance.Woken_By := Timer;

         else

            --  Otherwise run the selected task

            --   Set MMU is applicable.

            --  Switch registers and enable Wake Up Interrupt.
            Context_Switch_To_Agent (Current_Agent);

            --  Clean up after task has return via a context switch and
            --  determine the reason why the task did.
            Task_Message := Current_Agent.Agent_Message;

            case Current_Agent.Agent_Yield_Status is
               when Voluntary =>
                  --  If the task yielded voluntary update the task state
                  --  with the state provided by the message the task sent as
                  --  part of its yield.

                  Oak_Instance.Woken_By := Task_Yield;
                  Current_Agent.Set_State (State => Task_Message.Message_Type);

               when Timer =>
                  Current_Agent.Set_Agent_Yield_Status (Yielded  => Voluntary);
                  Oak_Instance.Woken_By := Timer;

               when Interrupt =>
                  Current_Agent.Set_Agent_Yield_Status (Yielded  => Voluntary);
                  Oak_Instance.Woken_By := External_Interrupt;

            end case;
         end if;

         Current_Agent.Remove_Agent_From_Exec_Charge_List
           (Agent_Handler (Charge_List));
      end loop;
   end Run_Loop;

   procedure Add_Agent_To_Charge_List
     (Oak_Instance : in out Oak_Data'Class;
      Agent        : not null access Oak_Agent'Class) is
   begin
      Agent.Add_Agent_To_Exec_Charge_List (Oak_Instance.Budgets_To_Charge);
   end Add_Agent_To_Charge_List;

   procedure Context_Switch_To_Agent (Agent : not null access Oak_Agent'Class)
   is
      Charge_List : access Oak_Agent'Class renames
                      Oak_Instance.Budgets_To_Charge;

      procedure Charge_Exec_Time
        (To             : not null access Oak_Agent'Class;
         To_Charge_List : Boolean);

      procedure Charge_Exec_Time
        (To             : not null access Oak_Agent'Class;
         To_Charge_List : Boolean)
      is
         Current_Time : constant Time      := Clock;
         Charge_Time  : constant Time_Span :=
                          Current_Time - Oak_Instance.Entry_Exit_Stamp;
      begin
         if To_Charge_List then
            Charge_Execution_Time_To_List
              (List             => To,
               Exec_Time        => Charge_Time,
               Current_Priority => Oak_Instance.Current_Priority);
         else
            Charge_Execution_Time
              (To_Agent  => To.all,
               Exec_Time => Charge_Time);
         end if;

         Oak_Instance.Entry_Exit_Stamp := Current_Time;
      end Charge_Exec_Time;
   begin
      Charge_Exec_Time (Oak_Instance, To_Charge_List => False);
      Oak_Instance.Current_Agent := Agent;
      Core_Support_Package.Task_Support.Context_Switch_To_Agent;
      Charge_Exec_Time
        (Charge_List, To_Charge_List => True);
   end Context_Switch_To_Agent;

   procedure Remove_Agent_From_Charge_List
     (Oak_Instance : in out Oak_Data'Class;
      Agent        : not null access Oak_Agent'Class) is
   begin
      Agent.Remove_Agent_From_Exec_Charge_List
        (Oak_Instance.Budgets_To_Charge);
   end Remove_Agent_From_Charge_List;

   procedure Set_Current_Agent_Stack_Pointer (SP : Address) is
   begin
      Set_Stack_Pointer
        (Agent           =>
           Processor_Kernels (Processor.Proccessor_Id).Current_Agent.all,
         Stack_Pointer => SP);
   end Set_Current_Agent_Stack_Pointer;

   procedure Set_Oak_Stack_Pointer (SP : Address) is
   begin
      Set_Stack_Pointer
        (Agent         => Processor_Kernels (Processor.Proccessor_Id),
         Stack_Pointer => SP);
   end Set_Oak_Stack_Pointer;

end Oak.Core;
