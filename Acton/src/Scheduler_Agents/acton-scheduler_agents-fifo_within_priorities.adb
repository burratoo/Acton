------------------------------------------------------------------------------
--                                                                          --
--                           ACTON SCHEDULER AGENT                          --
--                                                                          --
--              ACTON.SCHEDULER_AGENTS.FIFO_WITHIN_PRIORITIES               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                 Copyright (C) 2010-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

with Oak.Message;  use Oak.Message;
with Oak.Oak_Time; use Oak.Oak_Time;
with Oak.States;   use Oak.States;

with Oak.Agent.Kernel;     use Oak.Agent.Kernel;
with Oak.Agent.Oak_Agent;  use Oak.Agent.Oak_Agent;
with Oak.Agent.Schedulers; use Oak.Agent.Schedulers;

with Oak.Core;      use Oak.Core;
with Oak.Storage;   use Oak.Storage;

package body Acton.Scheduler_Agents.FIFO_Within_Priorities is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Run_Loop with No_Return;

   -------------------------
   -- New_Scheduler_Agent --
   -------------------------

   procedure New_Scheduler_Agent
     (Min_Priority : Any_Priority;
      Max_Priority : Any_Priority;
      Oak_Kernel   : Kernel_Id)
   is
      Agent : Scheduler_Id;
   begin
      New_Scheduler_Agent
        (Agent                => Agent,
         Name                 => Agent_Name,
         Call_Stack_Size      => Stack_Size,
         Run_Loop             => Run_Loop'Address,
         Lowest_Priority      => Min_Priority,
         Highest_Priority     => Max_Priority);

      Add_Scheduler_To_Scheduler_Table
        (Oak_Kernel =>  Oak_Kernel,
         Scheduler  => Agent);
   end New_Scheduler_Agent;

   --------------
   -- Run_Loop --
   --------------

   procedure Run_Loop is

      Me : constant Scheduler_Id := Current_Agent (This_Oak_Kernel);

      Scheduler : Scheduler_Storage (Lowest_Resposible_Priority (Me),
                                     Highest_Resposible_Priority (Me));

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

      ---------------------------------------
      -- Add_Task_To_End_Of_Runnable_Queue --
      ---------------------------------------

      procedure Add_Agent_To_End_Of_Runnable_Queue
        (Agent_Sid : in Storage_Id)
      is
         Agent : constant Oak_Agent_Id := Scheduler.Pool (Agent_Sid).Agent;
         P     : constant Any_Priority := Normal_Priority (Agent);

         Queue_Head_Id  : Storage_Id renames
                            Scheduler.Runnable_Queues (P).Head;

         Queue_Tail_Id  : Storage_Id renames
                            Scheduler.Runnable_Queues (P).Tail;

         Queue_Tail     : Scheduler_Element renames
                            Scheduler.Pool (Queue_Tail_Id);

      begin
         Scheduler.Pool (Agent_Sid).Next := No_Node;
         Queue_Tail.Next := Agent_Sid;
         Queue_Tail_Id   := Agent_Sid;

         if Queue_Head_Id = No_Node then
            Queue_Head_Id := Agent_Sid;
         end if;

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

         if Wake_Time (Agent) < Clock then
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
         Agent_Priority : constant Any_Priority :=  Normal_Priority (Agent);

         Queue_Head_Id  : Storage_Id renames
                            Scheduler.Runnable_Queues (Agent_Priority).Head;

         Queue_Head     : Scheduler_Element renames
                            Scheduler.Pool (Queue_Head_Id);

         Queue_Tail_Id  : Storage_Id renames
                            Scheduler.Runnable_Queues (Agent_Priority).Tail;

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
                  Queue_Head_Id := Queue_Head.Next;
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
                            Scheduler.Sleeping_Queues
                              (Normal_Priority (Agent));

         Node_Id         : Storage_Id        := Sleeping_Queue.Head;
         Prev_Id         : Storage_Id        := No_Node;
         Current_Node    : Scheduler_Element renames Scheduler.Pool (Node_Id);
         Current_Agent   : Oak_Agent_Id      renames Current_Node.Agent;
         Agent_Wake_Time : constant Time     := Wake_Time (Agent);
      begin
         if Sleeping_Queue.Head = No_Node then
            Sleeping_Queue.Head := Agent_Sid;
         else
            while Node_Id /= No_Node
              and then Agent_Wake_Time > Wake_Time (Current_Agent)
            loop
               Prev_Id := Node_Id;
               Node_Id := Current_Node.Next;
            end loop;

            Scheduler.Pool (Agent_Sid).Next := Node_Id;

            if Prev_Id /= No_Node then
               Scheduler.Pool (Agent_Sid).Next := Agent_Sid;
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
         Current_Time : constant Time := Clock;
      begin
         for Sleeping_Queue of Scheduler.Sleeping_Queues loop
            declare
               Node_Id      : Storage_Id    := Sleeping_Queue.Head;
               Current_Node : Scheduler_Element renames
                                Scheduler.Pool (Node_Id);
            begin

               while Node_Id /= No_Node
                 and then Current_Time > Wake_Time (Current_Node.Agent)
               loop
                  --  Remove Agent from sleeping queue...

                  Sleeping_Queue.Head := Scheduler.Pool (Node_Id).Next;

                  --  ... and add it to its runnable queue.

                  Add_Agent_To_End_Of_Runnable_Queue (Node_Id);

                  Node_Id := Sleeping_Queue.Head;

               end loop;
            end;
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
                  P : constant Any_Priority :=
                        Normal_Priority (Agent);

                  Queue_Head_Id : Storage_Id renames
                                    Scheduler.Runnable_Queues (P).Head;

                  Queue_Head    : Scheduler_Element renames
                                     Scheduler.Pool (Queue_Head_Id);

                  Queue_Tail_Id  : Storage_Id renames
                                     Scheduler.Runnable_Queues (P).Tail;

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

         for Queue of reverse Scheduler.Runnable_Queues loop
            Selected_Agent := Scheduler.Pool (Queue.Head).Agent;
            exit when Selected_Agent /= No_Agent;
         end loop;

         --  Find next time to wake up

         for P in reverse Scheduler.Sleeping_Queues'Range loop
            exit when P <= Normal_Priority (Selected_Agent);

            if Scheduler.Sleeping_Queues (P).Head /= No_Node
              and then Oak_Agent.Wake_Time
                (Scheduler.Pool (Scheduler.Sleeping_Queues (P).Head).Agent) <
              Wake_Time
            then
               --  Test here against No_Agent short circuits the case where
               --  the sleeping queue is empty, preventing the need to do the
               --  64 bit memory fetch and comparision.

               Wake_Time :=
                 Oak_Agent.Wake_Time
                   (Scheduler.Pool (Scheduler.Sleeping_Queues (P).Head).Agent);
            end if;
         end loop;

         Message :=
           (Message_Type        => Scheduler_Agent_Done, L => 0,
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

      Message : Oak_Message :=
                  (Message_Type        => Scheduler_Agent_Done, L => 0,
                   Next_Agent          => No_Agent,
                   Wake_Scheduler_At   => Time_Last,
                   Keep_In_Charge_List => False);

   begin
      loop
         Perform_Quick_Switch (Message);
         Service_Agent (Message);
      end loop;
   end Run_Loop;

end Acton.Scheduler_Agents.FIFO_Within_Priorities;
