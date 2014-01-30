------------------------------------------------------------------------------
--                                                                          --
--                           ACTON SCHEDULER AGENT                          --
--                                                                          --
--                  ACTON.SCHEDULER_AGENTS.PRIORITY_SERVER                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                 Copyright (C) 2013-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

with Oak.Message;  use Oak.Message;
with Oak.States;   use Oak.States;

with Oak.Agent.Oak_Agent;  use Oak.Agent.Oak_Agent;
with Oak.Agent.Schedulers; use Oak.Agent.Schedulers;

with Oak.Core;      use Oak.Core;
with Oak.Storage;   use Oak.Storage;
with Oak.Scheduler; use Oak.Scheduler;

package body Acton.Scheduler_Agents.Priority_Server is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Run_Loop with No_Return;

   -------------------------
   -- New_Scheduler_Agent --
   -------------------------

   procedure New_Scheduler_Agent
     (Agent             : out Scheduler_Id;
      Min_Priority      : in Any_Priority;
      Max_Priority      : in Any_Priority;
      Budget            : in Time_Span;
      Period            : in Time_Span;
      Phase             : in Time_Span;
      Relative_Deadline : in Time_Span;
      CPU               : in CPU_Range)
   is
      pragma Unreferenced (Max_Priority);
   begin
      New_Scheduler_Agent
        (Agent                 => Agent,
         Name                  => Agent_Name,
         Call_Stack_Size       => Stack_Size,
         Run_Loop              => Run_Loop'Address,
         Lowest_Priority       => Min_Priority,
         Highest_Priority      => Min_Priority,
         Scheduler_Agent       =>
           Find_Scheduler_For_System_Priority (Min_Priority, CPU),
         When_To_Charge_Agent  => Below_Priority,
         Interpret_No_Agent_As => As_Is,
         Charge_While_No_Agent => True,
         Agent_Active_Till     => Budget_Exhaustion,
         Cycle_Period          => Period,
         Cycle_Phase           => Phase,
         Relative_Deadline     => Relative_Deadline,
         Execution_Budget      => Budget);
   end New_Scheduler_Agent;

   --------------
   -- Run_Loop --
   --------------

   procedure Run_Loop is

      Scheduler : Scheduler_Storage;

      Scheduler_Error1 : exception;

      --------------------------
      -- Run Loop Subprograms --
      --------------------------

      procedure Add_Agent_To_End_Of_Runnable_Queue
        (Agent_Sid : in Storage_Id);

      procedure Add_Agent_To_Scheduler (Agent : in Oak_Agent_Id);

      procedure Agent_Changed (Agent : in Oak_Agent_Id);

      procedure Insert_Into_Sleeping_Queue (Agent_Sid : in Storage_Id);

      procedure Move_Woken_Tasks;

      procedure Remove_Agent_From_Scheduler (Agent : in Oak_Agent_Id);

      procedure Select_Next_Task
        (Message : out Oak_Message);

      procedure Service_Agent (Message : in out Oak_Message);

      ----------------------------------------
      -- Add_Agent_To_End_Of_Runnable_Queue --
      ----------------------------------------

      procedure Add_Agent_To_End_Of_Runnable_Queue
        (Agent_Sid : in Storage_Id)
      is

         Queue_Head_Id  : Storage_Id renames
                            Scheduler.Runnable_Queue.Head;

         Queue_Tail_Id  : Storage_Id renames
                            Scheduler.Runnable_Queue.Tail;

         Queue_Tail     : Scheduler_Element renames
                            Scheduler.Pool (Queue_Tail_Id);

      begin
         Scheduler.Pool (Agent_Sid).Next := No_Node;

         if Queue_Head_Id = No_Node then
            Queue_Head_Id := Agent_Sid;
         else
            Queue_Tail.Next := Agent_Sid;
         end if;

         Queue_Tail_Id   := Agent_Sid;

      end Add_Agent_To_End_Of_Runnable_Queue;

      ----------------------------
      -- Add_Agent_To_Scheduler --
      ----------------------------

      procedure Add_Agent_To_Scheduler (Agent : in Oak_Agent_Id) is
         Agent_Sid : Storage_Id;
      begin
         --  Storage allocation follows the Storage.Time_Priority_Pool.

         if Scheduler.Free_List = No_Node then
            --  The free list is empty so we allocate a node from the bulk free
            --  store.

            if Scheduler.Bulk_Free = No_Node then
               --  No more room in the pool!
               raise Pool_Capacity_Error with "No room in the pool!";
            end if;

            Agent_Sid      := Scheduler.Bulk_Free;
            Scheduler.Bulk_Free := Scheduler.Bulk_Free + 1;

         else
            --  Extract a node from the free list.

            Agent_Sid           := Scheduler.Free_List;
            Scheduler.Free_List := Scheduler.Pool (Scheduler.Free_List).Next;
         end if;

         Scheduler.Pool (Agent_Sid).Agent := Agent;

         if Wake_Time (Agent) <= Clock then
            Add_Agent_To_End_Of_Runnable_Queue (Agent_Sid);
         else
            Insert_Into_Sleeping_Queue (Agent_Sid);
         end if;
      end Add_Agent_To_Scheduler;

      -------------------
      -- Agent_Changed --
      -------------------

      procedure Agent_Changed (Agent : in Oak_Agent_Id)
      is
         Queue_Head_Id  : Storage_Id renames
                            Scheduler.Runnable_Queue.Head;

         Queue_Head     : Scheduler_Element renames
                            Scheduler.Pool (Queue_Head_Id);

         Queue_Tail_Id  : Storage_Id renames
                            Scheduler.Runnable_Queue.Tail;

         Queue_Tail     : Scheduler_Element renames
                            Scheduler.Pool (Queue_Tail_Id);

      begin
         --  Assumes that the task was in a runnable state before and is the
         --  head of its runnable queue.

         pragma Assert (Agent = Queue_Head.Agent);

         case State (Agent) is
            when Sleep =>
               --  Remove agent from its runnable queue and insert it into the
               --  sleep queue.

               Move_Agent_From_Runnable_To_Sleep_Queues : declare
                  Agent_Sid : constant Storage_Id := Queue_Head_Id;
               begin
                  if Queue_Head_Id = Queue_Tail_Id then
                     Queue_Head_Id := No_Node;
                     Queue_Tail_Id := No_Node;
                  else
                     Queue_Head_Id := Queue_Head.Next;
                  end if;
                  Insert_Into_Sleeping_Queue (Agent_Sid => Agent_Sid);
               end Move_Agent_From_Runnable_To_Sleep_Queues;

            when Activation_Pending    |
                 Activation_Complete   |
                 Activation_Successful =>
               null;
            when Update_Task_Property =>
               null;
            when Runnable =>
               --  Move agent from the head of its queue to its tail.

               Rotate_Queue : declare
                  New_Head : constant Storage_Id := Queue_Head.Next;
               begin
                  Queue_Tail.Next := Queue_Head_Id;
                  Queue_Head.Next := No_Node;

                  Queue_Tail_Id := Queue_Head_Id;
                  Queue_Head_Id := New_Head;
               end Rotate_Queue;

            when others =>
               raise Scheduler_Error1;
         end case;
      end Agent_Changed;

      --------------------------------
      -- Insert_Into_Sleeping_Queue --
      --------------------------------

      procedure Insert_Into_Sleeping_Queue (Agent_Sid : in Storage_Id)
      is
         Agent : Oak_Agent_Id renames Scheduler.Pool (Agent_Sid).Agent;

         Sleeping_Queue : Queue renames
                            Scheduler.Sleeping_Queue;

         Node_Id         : Storage_Id        := Sleeping_Queue.Head;
         Prev_Id         : Storage_Id        := No_Node;
         Agent_Wake_Time : constant Time     := Wake_Time (Agent);
      begin
         if Sleeping_Queue.Head = No_Node then
            Sleeping_Queue.Head             := Agent_Sid;
            Scheduler.Pool (Agent_Sid).Next := No_Node;
         else
            while Node_Id /= No_Node
              and then Agent_Wake_Time >
                Wake_Time (Scheduler.Pool (Node_Id).Agent)
            loop
               Prev_Id := Node_Id;
               Node_Id := Scheduler.Pool (Node_Id).Next;
            end loop;

            Scheduler.Pool (Agent_Sid).Next := Node_Id;

            if Prev_Id /= No_Node then
               Scheduler.Pool (Prev_Id).Next := Agent_Sid;
            else
               --  If Prev_Id is No_Node it means that the sleeping node is
               --  in front of the head node.
               Sleeping_Queue.Head := Agent_Sid;
            end if;
         end if;
      end Insert_Into_Sleeping_Queue;

      ----------------------
      -- Move_Woken_Tasks --
      ----------------------

      procedure Move_Woken_Tasks is
         Current_Time   : constant Time := Clock;
         Sleeping_Queue : Queue renames
                            Scheduler.Sleeping_Queue;
         Node_Id        : Storage_Id    := Sleeping_Queue.Head;
      begin
         while Node_Id /= No_Node
           and then Wake_Time (Scheduler.Pool (Node_Id).Agent) <=
             Current_Time
         loop
            --  Remove Agent from sleeping queue...

            Sleeping_Queue.Head := Scheduler.Pool (Node_Id).Next;

            --  ... and add it to its runnable queue.

            Add_Agent_To_End_Of_Runnable_Queue (Node_Id);

            Node_Id := Sleeping_Queue.Head;

         end loop;
      end Move_Woken_Tasks;

      ---------------------------------
      -- Remove_Agent_From_Scheduler --
      ---------------------------------

      procedure Remove_Agent_From_Scheduler (Agent : in Oak_Agent_Id) is

      begin
         --  The state of the agent determines which queue it is on.

         case State (Agent) is
            when Runnable | Entering_PO | Waiting_For_Event =>
               Remove_Agent_And_Deallocate_Storage : declare
                  Queue_Head_Id : Storage_Id renames
                                    Scheduler.Runnable_Queue.Head;

                  Queue_Head    : Scheduler_Element renames
                                    Scheduler.Pool (Queue_Head_Id);

                  Queue_Tail_Id  : Storage_Id renames
                                     Scheduler.Runnable_Queue.Tail;

                  Old_Storage_Id : constant Storage_Id := Queue_Head_Id;
               begin
                  --  For now we can only delete an agent at the head of its
                  --  queue (the only possible way for an agent to be deleted
                  --  at the moment).

                  pragma Assert (Agent = Queue_Head.Agent);

                  --  Remove from runnable queue

                  Queue_Head_Id := Queue_Head.Next;

                  if Queue_Tail_Id = Old_Storage_Id then
                     Queue_Tail_Id := No_Node;
                  end if;

                  --  Deallocate storage, follows Storage.Time_Priority_Pool.

                  if Old_Storage_Id + 1 = Scheduler.Bulk_Free then
                     Scheduler.Pool (Old_Storage_Id).Agent := No_Agent;
                     Scheduler.Pool (Old_Storage_Id).Next := No_Node;
                     Scheduler.Bulk_Free := Old_Storage_Id;

                  else
                     Scheduler.Pool (Old_Storage_Id).Next :=
                       Scheduler.Free_List;
                     Scheduler.Free_List := Old_Storage_Id;
                  end if;

               end Remove_Agent_And_Deallocate_Storage;

            when Sleeping =>
               --  Not supported at this point. Support for it would need
               --  a double linked list or a tree to ease its removal.
               raise Scheduler_Error1;
            when others =>
               raise Scheduler_Error1;
         end case;
      end Remove_Agent_From_Scheduler;

      ----------------------
      -- Select_Next_Task --
      ----------------------

      procedure Select_Next_Task
        (Message : out Oak_Message)
      is
         Selected_Agent : Oak_Agent_Id := No_Agent;
         Wake_Time      : Time         := Time_Last;
      begin
         Move_Woken_Tasks;

         --  Find next agent to run

         Selected_Agent :=
           Scheduler.Pool (Scheduler.Runnable_Queue.Head).Agent;

         --  Find next time to wake up

         if Scheduler.Sleeping_Queue.Head /= No_Node
           and then Oak_Agent.Wake_Time
             (Scheduler.Pool (Scheduler.Sleeping_Queue.Head).Agent) <=
               Wake_Time
         then
            --  Test here against No_Agent short circuits the case where
            --  the sleeping queue is empty, preventing the need to do the
            --  64 bit memory fetch and comparision.

            Wake_Time :=
              Oak_Agent.Wake_Time
                (Scheduler.Pool (Scheduler.Sleeping_Queue.Head).Agent);
         end if;

         Message :=
           (Message_Type        => Scheduler_Agent_Done,
            Next_Agent          => Selected_Agent,
            Wake_Scheduler_At   => Wake_Time,
            Keep_In_Charge_List => False);
      end Select_Next_Task;

      -------------------
      -- Service_Agent --
      -------------------

      procedure Service_Agent (Message : in out Oak_Message) is
      begin
         case Message.Message_Type is
            when Agent_State_Change =>
               Agent_Changed (Message.Agent_That_Changed);
            when Adding_Agent =>
               Add_Agent_To_Scheduler (Message.Agent_To_Add);
            when Removing_Agent =>
               Remove_Agent_From_Scheduler (Message.Agent_To_Remove);
            when others =>
               null;
         end case;
         Select_Next_Task (Message);
      end Service_Agent;

      Message : Oak_Message := (Message_Type => Selecting_Next_Agent);

   begin
      --  Initialise the No_Node element of the scheduler's storage pool
      Scheduler.Pool (No_Node) := (Agent => No_Agent, Next => No_Node);

      loop
         Service_Agent (Message);
         Request_Agent_Service (Message);
      end loop;
   end Run_Loop;

end Acton.Scheduler_Agents.Priority_Server;
