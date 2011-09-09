with Oak.Core;
with Oak.Processor_Support_Package.Task_Support;
use  Oak.Processor_Support_Package.Task_Support;
with Oak.Oak_Task.Scheduler_Agent;               use
  Oak.Oak_Task.Scheduler_Agent;
with Oak.Oak_Task.Data_Access;
with Ada.Real_Time;
with Oak.Oak_Task.Activation;
with Oak.Memory.Call_Stack;
with Oak.Memory.Call_Stack.Ops;
with Oak.Scheduler.Agent_List;

package body Acton.Scheduler_Agent.FIFO_Within_Priorities is
   use type Ada.Real_Time.Time;
   package Task_Data renames Oak.Oak_Task.Data_Access;

   type Oak_Task_Array is
     array (System.Any_Priority range <>) of Oak_Task_Handler;

   procedure Create_Agent
     (Agent                      : Oak_Task_Handler;
      Min_Priority, Max_Priority : Any_Priority)
   is
      Call_Stack : Oak.Memory.Call_Stack.Call_Stack_Handler;

      OI : constant access Oak.Core.Oak_Data := Oak.Core.Get_Oak_Instance;

      Scheduler : constant access Oak.Scheduler.Oak_Scheduler_Info :=
         Oak.Core.Get_Scheduler_Info (OI);

   begin
      Oak.Memory.Call_Stack.Ops.Allocate_Call_Stack
        (Stack            => Call_Stack,
         Size_In_Elements => Stack_Size);

      Initialise_Agent
        (Agent        => Agent,
         Name         => Agent_Name,
         Call_Stack   => Call_Stack,
         Max_Priority => Max_Priority,
         Min_Prioirty => Min_Priority,
         Run_Loop     => Run_Loop'Address);

      Oak.Scheduler.Agent_List.Add_Scheduler_Agent
        (Scheduler_Info => Scheduler.all,
         New_Agent      => Agent);

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

   --------------------------
   -- Run_Loop            --
   --------------------------

   procedure Run_Loop is
      Self            : constant Oak_Task_Handler :=
         Oak.Core.Get_Current_Task;
      Run_Reason      : Reason_For_Run;
      Runnable_Queues : Oak_Task_Array (
         Get_Lowest_Priority (Self) .. Get_Highest_Priority (Self));
      Sleeping_Queue  : Oak_Task_Handler;

      Scheduler_Error1, Scheduler_Error2 : exception;

      procedure Select_Next_Task;
      procedure Task_Yielded;
      procedure Add_Task;

      procedure Insert_Into_Queue
        (Queue : in out Oak_Task_Handler;
         T     : in Oak_Task_Handler);
      procedure Remove_From_Queue
        (Queue : in out Oak_Task_Handler;
         T     : in Oak_Task_Handler);

      procedure Add_Task_To_End_Of_Runnable_Queue
        (Task_To_Add : Oak_Task_Handler);
      procedure Remove_Head_Task_From_Runnable_Queue (P : System.Priority);
      procedure Move_Woken_Tasks_To_Runnable_Queue;

      --------------------------
      -- Select_Next_Task     --
      --------------------------

      procedure Select_Next_Task is
         Selected_Task : Oak_Task_Handler := null;
      begin
         Move_Woken_Tasks_To_Runnable_Queue;

         for T_Priority in reverse Runnable_Queues'Range loop
            Selected_Task := Runnable_Queues (T_Priority);
            exit when Selected_Task /= null;
         end loop;

         if Selected_Task /= null
           and then Task_Data.Get_State (T => Selected_Task) =
                    Activation_Pending
         then
            declare
               Activating_Task : constant Oak_Task_Handler :=
                  Oak.Oak_Task.Activation.Continue_Activation
                    (Activator => Selected_Task);
            begin
               Selected_Task := (if Activating_Task /= null then
                                 Activating_Task
                                 else
                                 Selected_Task);
            end;
         end if;

         Set_Chosen_Task (Agent => Self, T => Selected_Task);

         if Sleeping_Queue = null then
            Set_Desired_Run_Time
              (Agent    => Self,
               Run_Time => Ada.Real_Time.Time_Last);
         else
            Set_Desired_Run_Time
              (Agent    => Self,
               Run_Time => Task_Data.Get_Wake_Time (T => Sleeping_Queue));
         end if;
      end Select_Next_Task;

      --------------------------
      -- Task_Yielded         --
      --------------------------
      procedure Task_Yielded is
         T_Priority   : System.Priority;
         Yielded_Task : constant Oak_Task_Handler :=
            Get_Task_To_Run (Agent => Self);
      begin
         T_Priority := Task_Data.Get_Normal_Priority (T => Yielded_Task);
         if Runnable_Queues (T_Priority) = Yielded_Task then --  Sanity check.
            case Task_Data.Get_State (T => Yielded_Task) is
               when Cycle_Completed =>
                  Task_Data.Set_State (T => Yielded_Task, State => Sleeping);
                  --  Remove task from Runnable Queues
                  Remove_Head_Task_From_Runnable_Queue (T_Priority);
                  Insert_Into_Queue
                    (Queue => Sleeping_Queue,
                     T     => Yielded_Task);
               when Blocked =>
                  null;
               when Sleeping =>
                  Remove_Head_Task_From_Runnable_Queue (T_Priority);
                  Insert_Into_Queue
                    (Queue => Sleeping_Queue,
                     T     => Yielded_Task);
               when Activation_Pending    |
                    Activation_Complete   |
                    Activation_Successful =>
                  null;
               when Change_Cycle_Period | Change_Relative_Deadline =>
                  null;
               when others =>
                  raise Scheduler_Error1;
            end case;
         else
            raise Scheduler_Error2;
         end if;
         Select_Next_Task;
      end Task_Yielded;

      --------------
      -- Add_Task --
      --------------

      procedure Add_Task is
         Task_To_Add  : Oak_Task_Handler;
         Current_Time : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
      begin
         Task_To_Add := Get_Task_To_Manage (Agent => Self);
         if Task_Data.Get_Wake_Time (T => Task_To_Add) < Current_Time then
            Add_Task_To_End_Of_Runnable_Queue (Task_To_Add => Task_To_Add);
         else
            Insert_Into_Queue (Queue => Sleeping_Queue, T => Task_To_Add);
            Set_Desired_Run_Time
              (Agent    => Self,
               Run_Time => Task_Data.Get_Wake_Time (T => Sleeping_Queue));
         end if;
      end Add_Task;

      procedure Insert_Into_Queue
        (Queue : in out Oak_Task_Handler;
         T     : in Oak_Task_Handler)
      is
         Current        : Oak_Task_Handler            := Queue;
         Previous       : Oak_Task_Handler            := null;
         Task_Wake_Time : constant Ada.Real_Time.Time :=
            Task_Data.Get_Wake_Time (T => T);
      begin
         while Current /= null
           and then Task_Wake_Time > Task_Data.Get_Wake_Time (T => Current)
         loop
            Previous := Current;
            Current  := Get_Next_In_Queue (T => Current);
         end loop;

         if Previous /= null then
            Set_Next_In_Queue (T => Previous, Next => T);
         else
            Queue := T;
         end if;
         Set_Queue_Link (T => T, Prev => Previous, Next => Current);
         if Current /= null then
            Set_Prev_In_Queue (T => Current, Prev => T);
         end if;

      end Insert_Into_Queue;

      procedure Remove_From_Queue
        (Queue : in out Oak_Task_Handler;
         T     : in Oak_Task_Handler)
      is
         Previous : constant Oak_Task_Handler := Get_Prev_In_Queue (T => T);
         Next     : constant Oak_Task_Handler := Get_Next_In_Queue (T => T);
      begin
         if T = Queue then
            Queue := Next;
         else
            Set_Next_In_Queue (T => Previous, Next => Next);
         end if;
         if Next /= null then
            Set_Prev_In_Queue (T => Next, Prev => Previous);
         end if;
      end Remove_From_Queue;

      procedure Add_Task_To_End_Of_Runnable_Queue
        (Task_To_Add : Oak_Task_Handler)
      is
         Task_Priority : System.Priority;
      begin
         Task_Data.Set_State (T => Task_To_Add, State => Runnable);
         Task_Priority := Task_Data.Get_Normal_Priority (Task_To_Add);
         if Runnable_Queues (Task_Priority) = null then
            Runnable_Queues (Task_Priority) := Task_To_Add;
            Set_Queue_Link
              (T    => Task_To_Add,
               Prev => Task_To_Add,
               Next => Task_To_Add);
         else
            declare
               Head : constant Oak_Task_Handler :=
                  Runnable_Queues (Task_Priority);
               Tail : constant Oak_Task_Handler := Get_Prev_In_Queue (Head);
            begin
               Set_Prev_In_Queue (T => Head, Prev => Task_To_Add);
               Set_Queue_Link (T => Task_To_Add, Next => Head, Prev => Tail);
               Set_Next_In_Queue (T => Tail, Next => Task_To_Add);
            end;
         end if;
      end Add_Task_To_End_Of_Runnable_Queue;

      procedure Remove_Head_Task_From_Runnable_Queue (P : System.Priority) is
         Old_Head : constant Oak_Task_Handler := Runnable_Queues (P);
         New_Head : constant Oak_Task_Handler := Get_Next_In_Queue (Old_Head);
         Tail     : constant Oak_Task_Handler := Get_Prev_In_Queue (Old_Head);
      begin
         if Old_Head = New_Head then
            Runnable_Queues (P) := null;
         else
            Set_Prev_In_Queue (T => New_Head, Prev => Tail);
            Set_Next_In_Queue (T => Tail, Next => New_Head);
            Runnable_Queues (P) := New_Head;
         end if;
         Set_Queue_Link (T => Old_Head, Prev => null, Next => null);
      end Remove_Head_Task_From_Runnable_Queue;

      procedure Move_Woken_Tasks_To_Runnable_Queue is
         Current_Time : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
         T, Move_T    : Oak_Task_Handler            := Sleeping_Queue;
      begin
         while T /= null
           and then Current_Time > Task_Data.Get_Wake_Time (T => T)
         loop
            Move_T := T;
            T      := Get_Next_In_Queue (T => T);
            Remove_From_Queue (Queue => Sleeping_Queue, T => Move_T);
            Add_Task_To_End_Of_Runnable_Queue (Task_To_Add => Move_T);
         end loop;
      end Move_Woken_Tasks_To_Runnable_Queue;

   begin
      loop
         Run_Reason := Get_Run_Reason (Agent => Self);
         case Run_Reason is
            when Task_Yield =>
               Task_Yielded;
            when Select_Next_Task =>
               Select_Next_Task;
            when Add_Task =>
               Add_Task;
            when others =>
               null;
         end case;
         Yield_Processor_To_Kernel;
      end loop;
   end Run_Loop;

end Acton.Scheduler_Agent.FIFO_Within_Priorities;
