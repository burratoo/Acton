with Oak.Agent.Tasks.Activation;
with Oak.Agent.Tasks.Cycle; use Oak.Agent.Tasks.Cycle;
with Oak.Agent.Tasks.Protected_Objects; use Oak.Agent.Tasks.Protected_Objects;
with Oak.Atomic_Actions;
with Oak.Core_Support_Package.Task_Support;
with Oak.Interrupts; use Oak.Interrupts;
with Oak.Protected_Objects;
with Oak.Core_Support_Package.Interrupts;
with Ada.Cyclic_Tasks;
with Oak.Core_Support_Package.Call_Stack;
with Oak.Processor_Support_Package.Interrupts;
use Oak.Processor_Support_Package.Interrupts;

package body Oak.Core is

   ----------------
   -- Initialise --
   ----------------

   procedure Initialise is
      C : Activation_Chain;
   begin
      for Kernel_Agent of Processor_Kernels loop
         Initialise_Agent
           (Agent => Kernel_Agent'Access,
            Name  => "Kernel",
            Call_Stack_Size =>
              Core_Support_Package.Call_Stack.Oak_Call_Stack_Size);

         for P in Interrupt_Priority'Range loop
            Initialise_Task_Agent
              (Agent             => Kernel_Agent.Interrupt_Agents (P)'Access,
               Stack_Address     => Null_Address,
               Stack_Size        => Interrupt_Stack_Size,
               Name              => "Interrupt Agent",
               Run_Loop          => Interrupt_Run_Loop'Address,
               Task_Value_Record => Null_Address,
               Normal_Priority   => P,
               Cycle_Behaviour   => Ada.Cyclic_Tasks.Normal,
               Cycle_Period      => Oak_Time.Time_Span_Last,
               Phase             => Oak_Time.Time_Span_Zero,
               Execution_Budget  => Oak_Time.Time_Span_Last,
               Budget_Action     => Ada.Cyclic_Tasks.No_Action,
               Budget_Handler    => null,
               Relative_Deadline => Oak_Time.Time_Span_Last,
               Deadline_Action   => Ada.Cyclic_Tasks.No_Action,
               Deadline_Handler  => null,
               Execution_Server  => null,
               Chain             => C,
               Elaborated        => null);
            Kernel_Agent.Interrupt_Agents (P).Set_State (Interrupt_Done);
         end loop;

         for State of Kernel_Agent.Interrupt_States loop
            State := Inactive;
         end loop;

         Initialise_Sleep_Agent
           (Kernel_Agent.Sleep_Agent'Access,
            Core_Support_Package.Task_Support.Sleep_Agent'Address);
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
      Active_Timer : access Oak.Timers.Oak_Timer'Class;
      Next_Task    : Task_Handler := null;
      P            : Any_Priority;
      Interrupt_Id : Oak_Interrupt_Id;

      Task_Message   : Oak_Task_Message := (Message_Type => No_State);
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
               Check_With_Scheduler_Agents_On_Which_Task_To_Run_Next
                 (Scheduler_Info => Oak_Instance.Scheduler,
                  Chosen_Task    => Next_Task);
            when Task_Yield =>
               case Task_Message.Message_Type is
                  when Activation_Pending =>
                     Inform_Scheduler_Agent_Task_Has_Changed_State
                       (Chosen_Task => Next_Task);

                  when Activation_Complete =>
                     Agent.Tasks.Activation.Finish_Activation
                       (Activator => Current_Task.all);
                     Check_With_Scheduler_Agents_On_Which_Task_To_Run_Next
                       (Scheduler_Info => Oak_Instance.Scheduler,
                        Chosen_Task    => Next_Task);

                  when Sleeping =>
                     Next_Task.Set_Wake_Time (WT => Task_Message.Wake_Up_At);
                     Inform_Scheduler_Agent_Task_Has_Changed_State
                       (Chosen_Task => Next_Task);

                  when Setup_Cycles =>
                     Setup_Cyclic_Section (Next_Task);

                  when New_Cycle =>
                     New_Cycle (Next_Task);

                  when Release_Task =>
                     Release_Task
                       (Task_To_Release => Task_Message.Task_To_Release,
                        Releasing_Task  => Current_Task,
                        Next_Task       => Next_Task);

                  when Change_Cycle_Period =>
                     Current_Task.Set_Cycle_Period
                        (Task_Message.New_Cycle_Period);
                     Inform_Scheduler_Agent_Task_Has_Changed_State
                       (Chosen_Task => Next_Task);

                  when Change_Relative_Deadline =>
                     Current_Task.Set_Relative_Deadline
                        (Task_Message.New_Deadline_Span);
                     Inform_Scheduler_Agent_Task_Has_Changed_State
                       (Chosen_Task => Next_Task);

                  when Entering_PO =>
                     Protected_Objects.Process_Enter_Request
                       (Scheduler_Info  => Oak_Instance.Scheduler,
                        T               => Current_Task,
                        PO              => Task_Message.PO_Enter,
                        Subprogram_Kind => Task_Message.Subprogram_Kind,
                        Entry_Id        => Task_Message.Entry_Id_Enter,
                        Chosen_Task => Next_Task);

                  when Exiting_PO =>
                     Protected_Objects.Process_Exit_Request
                       (Scheduler_Info    => Oak_Instance.Scheduler,
                        T                 => Current_Task,
                        PO                => Task_Message.PO_Exit,
                        Chosen_Task       => Next_Task);

                  when Attach_Interrupt_Handlers =>
                     Interrupts.Attach_Handlers
                       (Handlers    => Task_Message.Attach_Handlers,
                        Handler_PO  => Task_Message.Attach_Handler_PO,
                        T           => Current_Task,
                        Chosen_Task => Next_Task);

                  when Entering_Atomic_Action =>
                     Atomic_Actions.Process_Enter_Request
                       (AO             => Task_Message.AA_Enter,
                        T              => Current_Task,
                        Scheduler_Info => Oak_Instance.Scheduler,
                        Action_Id      => Task_Message.Action_Id_Enter,
                        Chosen_Task    => Next_Task);

                  when Entering_Exit_Barrier =>
                     Atomic_Actions.Exit_Barrier
                       (AO               => Task_Message.AA_EB,
                        T                => Current_Task,
                        Action_Id        => Task_Message.Action_Id_EB,
                        Exception_Raised => Task_Message.Exception_Raised,
                        Chosen_Task      => Next_Task);

                  when Exiting_Atomic_Action =>
                     Atomic_Actions.Process_Exit_Request
                       (AO               => Task_Message.AA_Exit,
                        T                => Current_Task,
                        Scheduler_Info   => Oak_Instance.Scheduler,
                        Action_Id        => Task_Message.Action_Id_Exit,
                        Exception_Raised => Task_Message.Atomic_Exception,
                        Chosen_Task      => Next_Task);

                  when Interrupt_Done =>
                     Oak_Instance.Interrupt_States
                       (Next_Task.Normal_Priority) := Inactive;
                     if Interrupt_Agent (Next_Task.all).Interrupt_Kind =
                       Timer_Action
                     then
                        Oak.Protected_Objects.
                          Release_Protected_Object_For_Interrupt
                            (Protected_Object_From_Access
                                 (Interrupt_Agent
                                      (Next_Task.all).Timer_To_Handle.Handler)
                            );
                     end if;

                     Check_With_Scheduler_Agents_On_Which_Task_To_Run_Next
                       (Scheduler_Info => Oak_Instance.Scheduler,
                        Chosen_Task    => Next_Task);

                  when others =>
                     null;
               end case;

            when External_Interrupt =>
               Interrupt_Id := External_Interrupt_Id;
               P := Current_Interrupt_Priority;
               Next_Task := Oak_Instance.Interrupt_Agents (P)'Unchecked_Access;
               Next_Task.Set_State (Handling_Interrupt);
               Set_Interrupt_Kind
                 (Interrupt_Agent (Next_Task.all), Kind => External);
               Set_External_Id
                 (Interrupt_Agent (Next_Task.all), Interrupt_Id);
               Oak_Instance.Interrupt_States (P) := Active;

            when Timer =>
               if Active_Timer = null then
                  raise Program_Error;
               elsif Active_Timer.Firing_Time > Clock then
                  null;
               elsif Active_Timer.all in Timers.Scheduler_Timer then
                  Run_The_Bloody_Scheduler_Agent_That_Wanted_To_Be_Woken
                    (Scheduler_Info => Oak_Instance.Scheduler,
                     Chosen_Task    => Next_Task);
               elsif Active_Timer.all in Timers.Action_Timer then
                  case Timers.Action_Timer'Class
                    (Active_Timer.all).Timer_Action is
                     when Ada.Cyclic_Tasks.Handler =>
                        Active_Timer.Remove_Timer;
                        P := Active_Timer.Priority;
                        Next_Task :=
                          Oak_Instance.Interrupt_Agents (P)'Unchecked_Access;
                        Next_Task.Set_State (Handling_Interrupt);
                        Set_Interrupt_Kind
                          (Interrupt_Agent (Next_Task.all),
                           Kind => Timer_Action);
                        Set_Timer_To_Handle
                          (Interrupt_Agent (Next_Task.all),
                           Timers.Action_Timer (Active_Timer.all)'Access);
                        Oak_Instance.Interrupt_States (P) := Active;
                        Oak.Protected_Objects.
                          Acquire_Protected_Object_For_Interrupt
                            (Protected_Object_From_Access
                              (Oak.Timers.Handler
                                (Timers.Action_Timer (Active_Timer.all))));
                     when others =>
                        Check_With_Scheduler_Agents_On_Which_Task_To_Run_Next
                          (Scheduler_Info => Oak_Instance.Scheduler,
                           Chosen_Task    => Next_Task);
                  end case;

               end if;
         end case;

         --  Find any active interrupts

         P := Interrupt_Priority'Last;
         for Interrupt_State of reverse Oak_Instance.Interrupt_States loop
            if Interrupt_State = Active then
               if (Next_Task /= null and then
                     P >= Next_Task.Normal_Priority) or Next_Task = null
               then
                  Next_Task :=
                    Oak_Instance.Interrupt_Agents (P)'Unchecked_Access;
               end if;

               exit;
            end if;

            P := P - 1;
         end loop;

         if Next_Task /= null then
            Oak_Instance.Current_Priority := Next_Task.Normal_Priority;

            if Next_Task.all in Protected_Agent'Class then
               Next_Task := Protected_Agent (Next_Task.all).Task_Within;
            end if;

            case Next_Task.State is
               when Shared_State =>
                  if Next_Task.Shared_State = Entering_PO then
                     declare
                        M : constant Oak_Task_Message
                          := Next_Task.Task_Message;
                     begin
                        Protected_Objects.Process_Enter_Request
                         (Scheduler_Info  => Oak_Instance.Scheduler,
                          T               => Next_Task,
                          PO              => M.PO_Enter,
                          Subprogram_Kind => M.Subprogram_Kind,
                          Entry_Id        => M.Entry_Id_Enter,
                          Chosen_Task => Next_Task);
                     end;
                  end if;

               when others =>
                  null;
            end case;
         else
            Oak_Instance.Current_Priority := Any_Priority'First;
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
           Oak_Timer_Store.Earliest_Timer_To_Fire (Above_Priority => P);

         --  Execution timers are not placed under the control of the Timer
         --  Manager since they are only relevant for the currently executing
         --  task and only apply if the timer will fire before any timer
         --  that is currently managed.

         if Next_Task /= null
           and then Next_Task.Execution_Budget /= Oak_Time.Time_Span_Last
         then
            declare
               T : constant Time := Clock + Next_Task.Remaining_Budget;
            begin
               if Active_Timer = null
                 or else T < Active_Timer.Firing_Time
               then
                     Active_Timer := Next_Task.Budget_Timer;
                     Active_Timer.Update_Timer (New_Time => T);
               end if;
            end;
         end if;

         --  These called functions are responsible for enabling the sleep
         --  timer.
         Core_Support_Package.Task_Support.Set_Oak_Wake_Up_Timer
           (Wake_Up_At => Active_Timer.Firing_Time);

         if Next_Task = null then
            Next_Task := Oak_Instance.Sleep_Agent'Unchecked_Access;
         end if;

         --   Set MMU is applicable.

         --  Switch registers and enable Wake Up Interrupt.
         Context_Switch_To_Agent (Next_Task);

         --  Clean up after task has return via a context switch and
         --  determine the reason why the task did.
         Task_Message := Next_Task.Task_Message;

         case Next_Task.Task_Yield_Status is
            when Voluntary =>
               --  If the task yielded voluntary update the task state
               --  with the state provided by the message the task sent as
               --  part of its yield.

               Oak_Instance.Woken_By := Task_Yield;
               Next_Task.Set_State (State => Task_Message.Message_Type);

            when Timer =>
               Next_Task.Store_Task_Yield_Status (Yielded  => Voluntary);
               Oak_Instance.Woken_By := Timer;

            when Interrupt =>
               Next_Task.Store_Task_Yield_Status (Yielded  => Voluntary);
               Oak_Instance.Woken_By := External_Interrupt;

         end case;

      end loop;
   end Run_Loop;

   procedure Context_Switch_To_Agent (Agent : access Oak_Agent'Class) is
      procedure Charge_Exec_Time (To_Agent : access Oak_Agent'Class);

      procedure Charge_Exec_Time (To_Agent : access Oak_Agent'Class) is
         Current_Time : constant Time := Clock;
      begin
         Charge_Execution_Time
           (To_Agent  => To_Agent.all,
            Exec_Time => Current_Time - Oak_Instance.Entry_Exit_Stamp);
         Oak_Instance.Entry_Exit_Stamp := Current_Time;
      end Charge_Exec_Time;
   begin
      Charge_Exec_Time (Oak_Instance);
      Oak_Instance.Current_Agent := Agent;
      Core_Support_Package.Task_Support.Context_Switch_To_Agent;
      Charge_Exec_Time (Agent);
   end Context_Switch_To_Agent;

   procedure Set_Current_Agent_Stack_Pointer (SP : Address) is
   begin
      Set_Stack_Pointer
        (Agent           =>
           Processor_Kernels (Processor.Proccessor_Id).Current_Agent.all,
         Stack_Pointer => SP);
   end Set_Current_Agent_Stack_Pointer;

   procedure Set_Current_Agent (Agent : access Oak_Agent'Class) is
   begin
      Processor_Kernels (Processor.Proccessor_Id).Current_Agent := Agent;
   end Set_Current_Agent;

   procedure Set_Oak_Stack_Pointer (SP : Address) is
   begin
      Set_Stack_Pointer
        (Agent         => Processor_Kernels (Processor.Proccessor_Id),
         Stack_Pointer => SP);
   end Set_Oak_Stack_Pointer;

end Oak.Core;
