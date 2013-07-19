with Oak.Agent; use Oak.Agent;
with Oak.Agent.Queue;
with Oak.Core_Support_Package.Task_Support;
use Oak.Core_Support_Package.Task_Support;
with Oak.Message; use Oak.Message;
with Oak.Oak_Time;         use Oak.Oak_Time;
with Oak.States; use Oak.States;
with Oak.Scheduler; use Oak.Scheduler;
with Oak.Core;

package body Acton.Scheduler_Agents.FIFO_Within_Priorities is

   package Queue renames Oak.Agent.Queue;

   procedure Initialise_Scheduler_Agent
     (Agent : in out FIFO_Within_Priorities) is
   begin
      Initialise_Scheduler_Agent
        (Agent           => Agent'Unchecked_Access,
         Name            => Agent_Name,
         Call_Stack_Size => Stack_Size,
         Run_Loop        => Run_Loop'Address);

      Agent.Runnable_Queues := (others => null);
      Agent.Sleeping_Queues := (others => null);

      Add_Scheduler_To_Scheduler_Table
        (Scheduler_Info => Oak.Core.Oak_Instance.Scheduler_Info.all,
         Scheduler      => Agent'Unchecked_Access);
   end Initialise_Scheduler_Agent;

   --------------
   -- Run_Loop --
   --------------

   procedure Run_Loop  (Self : in out FIFO_Within_Priorities) is
      Run_Reason      : Agent_State;
      Runnable_Queues : Agent_Array renames Self.Runnable_Queues;
      Sleeping_Queues : Agent_Array renames Self.Sleeping_Queues;

      Scheduler_Error1 : exception;

      procedure Select_Next_Task;
      procedure Task_Yielded;
      procedure Add_Task;
      procedure Remove_Task;

      procedure Insert_Into_Sleeping_Queue
        (T     : access Oak_Agent'Class);

      procedure Add_Task_To_End_Of_Runnable_Queue
        (Task_To_Add : access Oak_Agent'Class);
      procedure Move_Woken_Tasks;

      --------------------------
      -- Select_Next_Task     --
      --------------------------

      procedure Select_Next_Task is
         Selected_Agent : access Oak_Agent'Class := null;
         Wake_Time      : Time := Time_Last;
      begin
         Move_Woken_Tasks;

         for Queue_Head of reverse Runnable_Queues loop
            Selected_Agent := Queue_Head;
            exit when Selected_Agent /= null;
         end loop;

         for P in reverse Sleeping_Queues'Range loop
            exit when Selected_Agent /= null
              and then P <= Selected_Agent.Normal_Priority;

            if Sleeping_Queues (P) /= null
              and then Sleeping_Queues (P).Wake_Time < Wake_Time
            then
               Wake_Time := Sleeping_Queues (P).Wake_Time;
            end if;
         end loop;

         Self.Set_Agent_Message
           (Message => (Message_Type      => Scheduler_Agent_Done,
                        Next_Agent        => Selected_Agent,
                        Wake_Scheduler_At => Wake_Time));
      end Select_Next_Task;

      ------------------
      -- Task_Yielded --
      ------------------

      procedure Task_Yielded is
         Yielded_Task : constant access Oak_Agent'Class :=
                          Self.Agent_Message.Agent_That_Changed;
         T_Priority   : constant Any_Priority := Yielded_Task.Normal_Priority;
      begin
         case Yielded_Task.State is
            when Sleep =>
               Queue.Remove_Agent
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
            when Runnable =>
               if Runnable_Queues (T_Priority) = Yielded_Task then
                  Queue.Move_Head_To_Tail (Runnable_Queues (T_Priority));
               end if;

            when others =>
               raise Scheduler_Error1;
         end case;
      end Task_Yielded;

      --------------
      -- Add_Task --
      --------------

      procedure Add_Task is
         Task_To_Add  : constant access Oak_Agent'Class :=
                          Self.Agent_Message.Agent_To_Add;
         Current_Time : constant Time := Clock;
      begin
         if Task_To_Add.Wake_Time < Current_Time then
            Add_Task_To_End_Of_Runnable_Queue (Task_To_Add => Task_To_Add);
         else
            Insert_Into_Sleeping_Queue (T => Task_To_Add);
         end if;
      end Add_Task;

      -----------------
      -- Remove_Task --
      -----------------

      procedure Remove_Task is
         Task_To_Remove  : constant access Oak_Agent'Class :=
                             Self.Agent_Message.Agent_To_Remove;
      begin
         case Task_To_Remove.State is
         --  Probably best to move this case to the others.

            when Runnable | Entering_PO | Entering_Atomic_Action |
                 Waiting_For_Event =>
               declare
                  Task_Priority : constant Any_Priority :=
                           Task_To_Remove.Normal_Priority;
               begin
                  Queue.Remove_Agent
                    (Queue => Runnable_Queues (Task_Priority),
                     Agent => Task_To_Remove);
               end;

            when Sleeping =>
               Queue.Remove_Agent
                 (Queue => Sleeping_Queues (Task_To_Remove.Normal_Priority),
                  Agent => Task_To_Remove);
            when others =>
               raise Scheduler_Error1;
         end case;
      end Remove_Task;

      procedure Insert_Into_Sleeping_Queue
        (T     : access Oak_Agent'Class)
      is
         Sleeping_Queue : access Oak_Agent'Class renames
                            Sleeping_Queues (T.Normal_Priority);
         Current        : access Oak_Agent'Class := Sleeping_Queue;
         Task_Wake_Time : constant Time          := T.Wake_Time;
         Queue_End      : Queue.Queue_End_Point  := Queue.Head;
      begin
         if Sleeping_Queue = null then
            Queue.Add_Agent_To_Head
              (Queue => Sleeping_Queue,
               Agent => T);
         else
            while Task_Wake_Time > Current.Wake_Time loop
               Current := Queue.Next_Agent (Current);
               if Current = Sleeping_Queue then
                  Queue_End := Queue.Tail;
                  exit;
               end if;
            end loop;
            Queue.Add_Agent_Before
              (Queue  => Sleeping_Queue,
                Agent => T,
                Before => Current,
                Queue_End => Queue_End);
         end if;
      end Insert_Into_Sleeping_Queue;

      procedure Add_Task_To_End_Of_Runnable_Queue
        (Task_To_Add : access Oak_Agent'Class)
      is
         Task_Priority : System.Any_Priority;
      begin
         Task_To_Add.Set_State (Runnable);
         Task_Priority := Task_To_Add.Normal_Priority;
         Queue.Add_Agent_To_Tail
           (Queue => Runnable_Queues (Task_Priority),
            Agent => Task_To_Add);
      end Add_Task_To_End_Of_Runnable_Queue;

      procedure Move_Woken_Tasks is
         Current_Time : constant Time    := Clock;
         T            : access Oak_Agent'Class;
      begin
         for Sleeping_Queue of Sleeping_Queues loop
            while Sleeping_Queue /= null
              and then Current_Time > Sleeping_Queue.Wake_Time
            loop
               T := Sleeping_Queue;
               Queue.Remove_Agent
                 (Queue => Sleeping_Queue,
                  Agent => T);
               case T.Destination_On_Wake_Up is
                  when Run_Queue =>
                     Add_Task_To_End_Of_Runnable_Queue (Task_To_Add => T);
                  when Remove =>
                     null;
               end case;
            end loop;
         end loop;
      end Move_Woken_Tasks;

   begin
      loop
         Run_Reason := Self.Agent_Message.Message_Type;
         case Run_Reason is
            when Agent_State_Change =>
               Task_Yielded;
            when Adding_Agent =>
               Add_Task;
            when Removing_Agent =>
               Remove_Task;
            when others =>
               null;
         end case;
         Select_Next_Task;
         Yield_Processor_To_Kernel;
      end loop;
   end Run_Loop;

end Acton.Scheduler_Agents.FIFO_Within_Priorities;
