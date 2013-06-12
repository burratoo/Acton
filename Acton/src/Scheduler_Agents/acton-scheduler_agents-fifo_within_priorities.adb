with Oak.Agent.Tasks.Activation;
with Oak.Core;
with Oak.Memory.Call_Stack;
with Oak.Memory.Call_Stack.Ops;
with Oak.Scheduler.Agent_List;

with Oak.Agent.Tasks;        use Oak.Agent.Tasks;
with Oak.Agent.Tasks.Queues; use Oak.Agent.Tasks.Queues;
with Oak.Core_Support_Package.Task_Support;
use  Oak.Core_Support_Package.Task_Support;
with Oak.Oak_Time;         use Oak.Oak_Time;

package body Acton.Scheduler_Agents.FIFO_Within_Priorities is

   package Task_Queues renames Oak.Agent.Tasks.Queues.General;

   type Task_Array is
     array (System.Any_Priority range <>) of access Task_Agent'Class;

   procedure Create_Agent
     (Agent        : in out Scheduler_Agent'Class;
      Min_Priority : in Any_Priority;
      Max_Priority : in Any_Priority)
   is
      Call_Stack : Oak.Memory.Call_Stack.Call_Stack_Handler;

      OI : constant access Oak.Core.Oak_Data := Oak.Core.Oak_Instance;

      Scheduler : constant access Oak.Scheduler.Oak_Scheduler_Info :=
         Oak.Core.Scheduler_Info (OI);

   begin
      Oak.Memory.Call_Stack.Ops.Allocate_Call_Stack
        (Stack            => Call_Stack,
         Size_In_Elements => Stack_Size);

      Initialise_Scheduler_Agent
        (Agent        => Agent'Unchecked_Access,
         Name         => Agent_Name,
         Call_Stack   => Call_Stack,
         Max_Priority => Max_Priority,
         Min_Prioirty => Min_Priority,
         Run_Loop     => Run_Loop'Address);

      Oak.Scheduler.Agent_List.Add_Scheduler_Agent
        (Scheduler_Info => Scheduler.all,
         New_Agent      => Agent'Unchecked_Access);

   end Create_Agent;

   -----------------
   -- Remove_Task --
   -----------------

   procedure Remove_Task is
   begin
      --  Generated stub: replace with real body! pragma Compile_Time_Warning
      --  (True, "Remove_Task unimplemented");
      raise Program_Error with "Unimplemented procedure Remove_Task";
   end Remove_Task;

   --------------------------
   -- Change_Task_Priority --
   --------------------------

   procedure Change_Task_Priority is
   begin
      --  Generated stub: replace with real body!
      --  pragma Compile_Time_Warning
      --  (True,
      --   "Change_Task_Priority unimplemented");
      raise Program_Error with "Unimplemented procedure Change_Task_Priority";
   end Change_Task_Priority;

   --------------
   -- Run_Loop --
   --------------

   procedure Run_Loop is
      Self            : constant access Scheduler_Agent'Class :=
                          Scheduler_Handler (Oak.Core.Current_Agent);
      Run_Reason      : Reason_For_Run;
      Runnable_Queues : Task_Array
        (Self.Lowest_Priority .. Self.Highest_Priority);
      Sleeping_Queue  : access Task_Agent'Class;

      Scheduler_Error1 : exception;

      procedure Select_Next_Task;
      procedure Task_Yielded;
      procedure Add_Task;
      procedure Remove_Task;

      procedure Insert_Into_Sleeping_Queue
        (T     : access Task_Agent'Class);

      procedure Add_Task_To_End_Of_Runnable_Queue
        (Task_To_Add : access Task_Agent'Class);
      procedure Move_Woken_Tasks_To_Runnable_Queue;

      --------------------------
      -- Select_Next_Task     --
      --------------------------

      procedure Select_Next_Task is
         Selected_Task : access Task_Agent'Class := null;
         Head_Task     : access Task_Agent'Class := null;
      begin
         Move_Woken_Tasks_To_Runnable_Queue;

         for Queue_Head of reverse Runnable_Queues loop
            Head_Task := Queue_Head;
            if Head_Task /= null then
               Selected_Task := Head_Task;
               while Selected_Task.State = Shared_State
                 and then Selected_Task.Shared_State in Waiting
               loop
                  Selected_Task := Next_Task (Selected_Task);

                  --  We have already checked the Head_Task, so if we reach it
                  --  again it means we have hit the end of the queue and thus
                  --  there are no runnable tasks in the queue
                  if Selected_Task = Head_Task then
                     Selected_Task := null;
                     exit;
                  end if;
               end loop;
            end if;
            exit when Selected_Task /= null;
         end loop;

         if Selected_Task /= null
           and then Selected_Task.State = Activation_Pending then
            declare
               Activating_Task : constant access Task_Agent'Class :=
                  Activation.Continue_Activation
                    (Activator => Selected_Task);
            begin
               Selected_Task := (if Activating_Task /= null then
                                 Activating_Task
                                 else
                                 Selected_Task);
            end;
         end if;

         Self.Set_Chosen_Task (Selected_Task);

         if Sleeping_Queue = null then
            Self.Set_Desired_Run_Time (Time_Last);
         else
            Self.Set_Desired_Run_Time (Sleeping_Queue.Wake_Time);
         end if;
      end Select_Next_Task;

      ------------------
      -- Task_Yielded --
      ------------------

      procedure Task_Yielded is
         Yielded_Task : constant access Task_Agent'Class :=
                          Self.Task_To_Manage;
         T_Priority   : constant Any_Priority := Yielded_Task.Normal_Priority;
      begin
         case Yielded_Task.State is
            when Sleeping =>
               Task_Queues.Remove_Agent
                 (Queue => Runnable_Queues (T_Priority),
                  Agent => Yielded_Task);
               Insert_Into_Sleeping_Queue
                 (T     => Yielded_Task);

            when Activation_Pending    |
                 Activation_Complete   |
                 Activation_Successful =>
               null;
            when Change_Cycle_Period | Change_Relative_Deadline =>
               null;
            when others =>
               raise Scheduler_Error1;
         end case;
         Select_Next_Task;
      end Task_Yielded;

      --------------
      -- Add_Task --
      --------------

      procedure Add_Task is
         Task_To_Add  : constant access Task_Agent'Class :=
                          Self.Task_To_Manage;
         Current_Time : constant Time := Clock;
      begin
         if Task_To_Add.Wake_Time < Current_Time then
            Add_Task_To_End_Of_Runnable_Queue (Task_To_Add => Task_To_Add);
         else
            Insert_Into_Sleeping_Queue (T => Task_To_Add);
            Self.Set_Desired_Run_Time (Sleeping_Queue.Wake_Time);
         end if;
      end Add_Task;

      -----------------
      -- Remove_Task --
      -----------------

      procedure Remove_Task is
         Task_To_Remove  : constant access Task_Agent'Class :=
                             Self.Task_To_Manage;
      begin
         case Task_To_Remove.State is
            when Runnable | Entering_PO | Entering_Atomic_Action =>
               declare
                  Task_Priority : constant Any_Priority :=
                           Task_To_Remove.Normal_Priority;
               begin
                  Task_Queues.Remove_Agent
                    (Queue => Runnable_Queues (Task_Priority),
                     Agent => Task_To_Remove);
               end;

            when Sleeping =>
               Task_Queues.Remove_Agent
                 (Queue => Task_Queues.Agent_Handler (Sleeping_Queue),
                  Agent => Task_To_Remove);
            when others =>
               raise Scheduler_Error1;
         end case;
      end Remove_Task;

      procedure Insert_Into_Sleeping_Queue
        (T     : access Task_Agent'Class)
      is
         Current        : access Task_Agent'Class := Sleeping_Queue;
         Task_Wake_Time : constant Time           := T.Wake_Time;
         Queue_End      : Task_Queues.Queue_End_Point  := Task_Queues.Head;
      begin
         if Sleeping_Queue = null then
            Task_Queues.Add_Agent_To_Head
              (Queue => Task_Queues.Agent_Handler (Sleeping_Queue),
               Agent => T);
         else
            while Task_Wake_Time > Current.Wake_Time loop
               Current := Next_Task (T => Current);
               if Current = Sleeping_Queue then
                  Queue_End := Task_Queues.Tail;
                  exit;
               end if;
            end loop;
            Task_Queues.Add_Agent_Before
              (Queue  => Task_Queues.Agent_Handler (Sleeping_Queue),
                Agent => T,
                Before => Current,
                Queue_End => Queue_End);
         end if;
      end Insert_Into_Sleeping_Queue;

      procedure Add_Task_To_End_Of_Runnable_Queue
        (Task_To_Add : access Task_Agent'Class)
      is
         Task_Priority : System.Any_Priority;
      begin
         Task_To_Add.Set_State (Runnable);
         Task_Priority := Task_To_Add.Normal_Priority;
         Task_Queues.Add_Agent_To_Tail
           (Queue => Runnable_Queues (Task_Priority),
            Agent => Task_To_Add);
      end Add_Task_To_End_Of_Runnable_Queue;

      procedure Move_Woken_Tasks_To_Runnable_Queue is
         Current_Time : constant Time    := Clock;
         T            : access Task_Agent'Class := Sleeping_Queue;
      begin
         while Sleeping_Queue /= null
           and then Current_Time > Sleeping_Queue.Wake_Time
         loop
            T := Sleeping_Queue;
            Task_Queues.Remove_Agent
              (Queue => Task_Queues.Agent_Handler (Sleeping_Queue),
               Agent => T);
            Add_Task_To_End_Of_Runnable_Queue (Task_To_Add => T);
         end loop;
      end Move_Woken_Tasks_To_Runnable_Queue;

   begin
      loop
         Run_Reason := Self.Run_Reason;
         case Run_Reason is
            when Task_State_Change =>
               Task_Yielded;
            when Select_Next_Task =>
               Select_Next_Task;
            when Add_Task =>
               Add_Task;
            when Remove_Task =>
               Remove_Task;
         end case;
         Yield_Processor_To_Kernel;
      end loop;
   end Run_Loop;

end Acton.Scheduler_Agents.FIFO_Within_Priorities;
