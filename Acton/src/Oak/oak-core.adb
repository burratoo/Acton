with Ada.Real_Time;                                 use Ada.Real_Time;
with Oak.Memory.Call_Stack.Ops;
with Oak.Processor_Support_Package.Task_Support;
use  Oak.Processor_Support_Package.Task_Support;
with Oak.Processor_Support_Package.Processor;
with Oak.Oak_Task.Internal;                         use Oak.Oak_Task.Internal;
with Oak.Oak_Task.Data_Access;
with Oak.Oak_Task.Activation;
with Oak.Processor_Support_Package.Task_Interrupts;
with Oak.Processor_Support_Package.Call_Stack;

package body Oak.Core is

   package Processor renames Oak.Processor_Support_Package.Processor;

   ----------------
   -- Initialise --
   ----------------

   procedure Initialise is
   begin
      for J in Processor_Kernels'Range loop
         Oak.Memory.Call_Stack.Ops.Allocate_Call_Stack
           (Stack            => Processor_Kernels (J).Call_Stack,
            Size_In_Elements =>
              Oak.Processor_Support_Package.Call_Stack.Oak_Call_Stack_Size);
      end loop;
      Oak.Processor_Support_Package.Task_Interrupts.Initialise_Task_Enviroment;
   end Initialise;

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
      Earliest_Scheduler_Agent_Time   : Time;
      Next_Task                       : Oak_Task_Handler;

      Task_Return_State : Task_Requested_State :=
        (Type_State => Sleeping,
         State      => Sleeping,
         Wake_Up_At => Time_Last);
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
               case Task_Return_State.State is
                  when Sleeping =>
                     Oak.Oak_Task.Data_Access.Set_Wake_Time
                       (T  => Get_Current_Task,
                        WT => Task_Return_State.Wake_Up_At);

                  when Cycle_Completed =>
                     Next_Run_Cycle (T => Get_Current_Task);

                  when Activation_Complete =>
                     Oak.Oak_Task.Activation.Finish_Activation
                       (Activator => Get_Current_Task);

                  when Change_Cycle_Period =>
                     Oak.Oak_Task.Internal.Set_Cycle_Period
                       (T  => Get_Current_Task,
                        CP => Task_Return_State.New_Time_Span);

                  when Change_Relative_Deadline =>
                     Oak.Oak_Task.Internal.Set_Relative_Deadline
                       (T  => Get_Current_Task,
                        RD => Task_Return_State.New_Time_Span);

                  when others =>
                     null;
               end case;

               case Task_Return_State.State is
                  when Activation_Complete =>
                     Check_With_Scheduler_Agents_On_Which_Task_To_Run_Next
                       (Scheduler_Info => Oak_Instance.Scheduler,
                        Chosen_Task    => Next_Task);
                  when others =>
                     Run_Current_Task_Scheduler_Agent
                       (Scheduler_Info => Oak_Instance.Scheduler,
                        Chosen_Task    => Next_Task);
               end case;

            when Scheduler_Agent =>
               Run_The_Bloody_Scheduler_Agent_That_Wanted_To_Be_Woken
                 (Scheduler_Info => Oak_Instance.Scheduler,
                  Chosen_Task    => Next_Task);
            when Missed_Deadline =>
               --                 Handle_Missed_Deadline
               --                   (Scheduler_Info => Oak_Instance.Scheduler,
               --                    Chosen_Task    => Next_Task);
               null;
         end case;

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
         Earliest_Scheduler_Agent_Time :=
            Get_Earliest_Scheduler_Agent_Time
              (Scheduler_Info => Oak_Instance.Scheduler);
         Earliest_Deadline             := Time_Last;
         --              Get_Earliest_Deadline (Scheduler_Info =>
         --  Oak_Instance.Scheduler);

         if Earliest_Deadline <= Earliest_Scheduler_Agent_Time then
            Oak_Instance.Woken_By := Missed_Deadline;
            Wake_Up_Time          := Earliest_Deadline;
         else
            Oak_Instance.Woken_By := Scheduler_Agent;
            Wake_Up_Time          := Earliest_Scheduler_Agent_Time;
         end if;

         --  These called functions are responsible for enabling the sleep
         --  timer.
         Set_Oak_Wake_Up_Timer (Wake_Up_At => Wake_Up_Time);

         if Next_Task = null then
            Sleep_Kernel;
         else
            --   Set MMU is applicable.

            --  Switch registers and enable Wake Up Interrupt.
            Oak_Instance.Current_Task := Next_Task;
            Context_Switch_To_Task (Task_Return_State => Task_Return_State);
            if Task_Return_State.State /= Runnable then
               Oak_Instance.Woken_By := Task_Yield;
               Set_State
                (T         => Oak_Instance.Current_Task,
                 New_State => Task_Return_State.State);
            end if;
         end if;

      end loop;
   end Run_Loop;

   function Get_Current_Task_Stack_Pointer return Address is
   begin
      return (Get_Stack_Pointer
                 (T =>
                    Processor_Kernels (Processor.Get_Proccessor_Id).
        Current_Task));
   end Get_Current_Task_Stack_Pointer;

   procedure Set_Current_Task_Stack_Pointer (SP : Address) is
   begin
      Set_Stack_Pointer
        (T             =>
           Processor_Kernels (Processor.Get_Proccessor_Id).Current_Task,
         Stack_Pointer => SP);
   end Set_Current_Task_Stack_Pointer;

   procedure Set_Current_Task (T : Oak_Task_Handler) is
   begin
      Processor_Kernels (Processor.Get_Proccessor_Id).Current_Task := T;
   end Set_Current_Task;

   function Get_Current_Task return Oak_Task_Handler is
   begin
      return Processor_Kernels (Processor.Get_Proccessor_Id).Current_Task;
   end Get_Current_Task;

   function Get_Oak_Instance return access Oak_Data is
   begin
      return Processor_Kernels (Processor_Kernels'First)'Access;
   end Get_Oak_Instance;

   function Get_Scheduler_Info
     (Oak_Instance : access Oak_Data)
      return         access Oak_Scheduler_Info is
   begin
      return Oak_Instance.Scheduler'Access;
   end Get_Scheduler_Info;

   function Get_Main_Task return Oak_Task_Handler is
   begin
      return Main_Task_OTCR'Access;
   end Get_Main_Task;
end Oak.Core;
