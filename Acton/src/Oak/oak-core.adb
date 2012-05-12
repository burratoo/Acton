with Oak.Agent.Tasks.Activation;
with Oak.Agent.Tasks.Internal;
with Oak.Agent.Tasks.Protected_Object; use Oak.Agent.Tasks.Protected_Object;
with Oak.Real_Time;                                 use Oak.Real_Time;
with Oak.Memory.Call_Stack.Ops;
with Oak.Core_Support_Package.Task_Support;
use  Oak.Core_Support_Package.Task_Support;
with Oak.Core_Support_Package.Task_Interrupts;
with Oak.Core_Support_Package.Call_Stack;
with Oak.Interrupts;
with Oak.Protected_Object;
with Oak.Processor_Support_Package.Interrupts;

package body Oak.Core is

   ----------------
   -- Initialise --
   ----------------

   procedure Initialise is
   begin
      for K of Processor_Kernels loop
         Oak.Memory.Call_Stack.Ops.Allocate_Call_Stack
           (Stack            => K.Call_Stack,
            Size_In_Elements =>
              Oak.Core_Support_Package.Call_Stack.Oak_Call_Stack_Size);
      end loop;
      Oak.Core_Support_Package.Task_Interrupts.Initialise_Task_Enviroment;
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
      Wake_Up_Time, Earliest_Deadline : Time;
      Earliest_Scheduler_Time   : Time;
      Next_Task                       : Task_Handler := null;

      Task_Message : Oak_Task_Message := (Message_Type => No_State);
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
                     Inform_Scheduler_Agent_Task_Has_Yielded
                       (Chosen_Task => Next_Task);

                  when Activation_Complete =>
                     Agent.Tasks.Activation.Finish_Activation
                       (Activator => Current_Task.all);
                     Check_With_Scheduler_Agents_On_Which_Task_To_Run_Next
                       (Scheduler_Info => Oak_Instance.Scheduler,
                        Chosen_Task    => Next_Task);

                  when Sleeping =>
                     Agent.Tasks.Set_Wake_Time
                       (T  => Next_Task.all,
                        WT => Task_Message.Wake_Up_At);
                     Inform_Scheduler_Agent_Task_Has_Yielded
                       (Chosen_Task => Next_Task);

                  when Cycle_Completed =>
                     Agent.Tasks.Internal.Next_Run_Cycle
                       (T => Current_Task.all);
                     Inform_Scheduler_Agent_Task_Has_Yielded
                       (Chosen_Task => Next_Task);

                  when Change_Cycle_Period =>
                     Agent.Tasks.Internal.Set_Cycle_Period
                       (T  => Current_Task.all,
                        CP => Task_Message.New_Cycle_Period);
                     Inform_Scheduler_Agent_Task_Has_Yielded
                       (Chosen_Task => Next_Task);

                  when Change_Relative_Deadline =>
                     Agent.Tasks.Internal.Set_Relative_Deadline
                       (T  => Current_Task.all,
                        RD => Task_Message.New_Deadline_Span);
                     Inform_Scheduler_Agent_Task_Has_Yielded
                       (Chosen_Task => Next_Task);

                  when Entering_PO =>
                     Oak.Protected_Object.Process_Enter_Request
                       (Scheduler_Info  => Oak_Instance.Scheduler,
                        T               => Current_Task.all,
                        PO              => Task_Message.PO_Enter.all,
                        Subprogram_Kind => Task_Message.Subprogram_Kind,
                        Entry_Id        => Task_Message.Entry_Id_Enter,
                        Chosen_Task => Next_Task);

                  when Exiting_PO =>
                     Oak.Protected_Object.Process_Exit_Request
                       (Scheduler_Info    => Oak_Instance.Scheduler,
                        T                 => Current_Task.all,
                        PO                => Task_Message.PO_Exit.all,
                        Chosen_Task       => Next_Task);
                  when Attach_Interrupt_Handlers =>
                     Oak.Interrupts.Attach_Handlers
                       (Handlers    => Task_Message.Attach_Handlers,
                        Handler_PO  => Task_Message.Attach_Handler_PO,
                        T           => Current_Task,
                        Chosen_Task => Next_Task);
                  when others =>
                     null;
               end case;

            when Scheduler_Agent | Missed_Deadline =>
               Run_The_Bloody_Scheduler_Agent_That_Wanted_To_Be_Woken
                 (Scheduler_Info => Oak_Instance.Scheduler,
                  Chosen_Task    => Next_Task);
               --  when Missed_Deadline =>
               --                 Handle_Missed_Deadline
               --                   (Scheduler_Info => Oak_Instance.Scheduler,
               --                    Chosen_Task    => Next_Task);
               --   null;
         end case;

         if Next_Task /= null and then
           Next_Task.all in Protected_Agent'Class then
            Next_Task := Task_Within (Protected_Agent'Class (Next_Task.all));
         end if;

         if Next_Task /= null then
            case State (T => Next_Task.all) is
               when Shared_State =>
                  if Shared_State (Next_Task.all) = Entering_PO then
                     declare
                        M : constant Oak_Task_Message
                          := Agent.Tasks.Task_Message
                            (For_Task => Next_Task.all);
                     begin
                        Oak.Protected_Object.Process_Enter_Request
                         (Scheduler_Info  => Oak_Instance.Scheduler,
                          T               => Next_Task.all,
                          PO              => M.PO_Enter.all,
                          Subprogram_Kind => M.Subprogram_Kind,
                          Entry_Id        => M.Entry_Id_Enter,
                          Chosen_Task => Next_Task);
                     end;
                  end if;

               when others =>
                  null;
            end case;
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
         Earliest_Scheduler_Time :=
            Earliest_Scheduler_Agent_Time
              (Scheduler_Info => Oak_Instance.Scheduler);
         Earliest_Deadline             := Time_Last;
         --              Get_Earliest_Deadline (Scheduler_Info =>
         --  Oak_Instance.Scheduler);

         if Earliest_Deadline <= Earliest_Scheduler_Time then
            Oak_Instance.Woken_By := Missed_Deadline;
            Wake_Up_Time          := Earliest_Deadline;
         else
            Oak_Instance.Woken_By := Scheduler_Agent;
            Wake_Up_Time          := Earliest_Scheduler_Time;
         end if;

         --  These called functions are responsible for enabling the sleep
         --  timer.
         Set_Oak_Wake_Up_Timer (Wake_Up_At => Wake_Up_Time);

         if Next_Task = null then
            Sleep_Kernel;
         else
            --   Set MMU is applicable.

            --  Switch registers and enable Wake Up Interrupt.
            Oak_Instance.Current_Agent := Next_Task;
            Context_Switch_To_Task;
            Task_Message := Agent.Tasks.Task_Message
                              (For_Task => Next_Task.all);
            case Internal.Task_Yield_Status (For_Task => Next_Task.all) is
               when Voluntary =>
                  Oak_Instance.Woken_By := Task_Yield;
                  Set_State
                    (T     => Task_Handler (Oak_Instance.Current_Agent).all,
                     State => Task_Message.Message_Type);
               when Forced =>
                  Internal.Store_Task_Yield_Status
                    (For_Task => Next_Task.all,
                     Yielded  => Voluntary);
            end case;
         end if;

      end loop;
   end Run_Loop;

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
end Oak.Core;
