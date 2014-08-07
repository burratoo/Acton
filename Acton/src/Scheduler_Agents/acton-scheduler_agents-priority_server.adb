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
with Oak.Scheduler;

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
           Oak.Scheduler.Find_Scheduler_For_System_Priority
             (Min_Priority, CPU),
         When_To_Charge_Agent  => At_Or_Below_Priority,
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

      Runnable_Queue : Heap_Type;
      Sleep_Queue    : Heap_Type;

      --------------------------
      -- Run Loop Subprograms --
      --------------------------

      procedure Add_Agent_To_End_Of_Runnable_Queue (Agent : in Oak_Agent_Id);

      procedure Add_Agent_To_Scheduler (Agent : in Oak_Agent_Id);

      procedure Agent_Changed (Agent : in Oak_Agent_Id);

      procedure Insert_Into_Sleep_Queue (Agent : in Oak_Agent_Id);

      procedure Move_Woken_Tasks;

      procedure Remove_Agent_From_Scheduler (Agent : in Oak_Agent_Id);

      procedure Select_Next_Task
        (Message : out Oak_Message);

      procedure Service_Agent (Message : in out Oak_Message);

      ----------------------------------------
      -- Add_Agent_To_End_Of_Runnable_Queue --
      ----------------------------------------

      procedure Add_Agent_To_End_Of_Runnable_Queue
        (Agent : in Oak_Agent_Id) is
      begin
         Add_Item (To_Heap    => Runnable_Queue,
                   Item        => Agent);
      end Add_Agent_To_End_Of_Runnable_Queue;

      ----------------------------
      -- Add_Agent_To_Scheduler --
      ----------------------------

      procedure Add_Agent_To_Scheduler (Agent : in Oak_Agent_Id) is
      begin
         if Wake_Time (Agent) <= Clock then
            Add_Agent_To_End_Of_Runnable_Queue (Agent);
         else
            Insert_Into_Sleep_Queue (Agent);
         end if;
      end Add_Agent_To_Scheduler;

      -------------------
      -- Agent_Changed --
      -------------------

      procedure Agent_Changed (Agent : in Oak_Agent_Id) is
         Pulled_Agent : Oak_Agent_Id;
      begin
         --  Assumes that the task was in a runnable state before and is the
         --  head of its runnable queue.

         pragma Assert (Agent = First_Item (Runnable_Queue));

         case State (Agent) is
            when Sleeping =>
               Remove_Top (From_Heap => Runnable_Queue,
                           Item      => Pulled_Agent);
               Add_Agent_To_Scheduler (Agent);

            when Activation_Pending    |
                 Activation_Complete   |
                 Activation_Successful |
                 Update_Task_Property =>
               null;

            when Runnable =>
               Remove_Top (From_Heap => Runnable_Queue,
                           Item      => Pulled_Agent);
               Add_Agent_To_Scheduler (Agent);

            when others =>
               raise Scheduler_Error;
         end case;
      end Agent_Changed;

      --------------------------------
      -- Insert_Into_Sleep_Queue --
      --------------------------------

      procedure Insert_Into_Sleep_Queue (Agent : in Oak_Agent_Id) is
      begin
         Add_Item (To_Heap => Sleep_Queue,
                   Item    => Agent);
      end Insert_Into_Sleep_Queue;

      ----------------------
      -- Move_Woken_Tasks --
      ----------------------

      procedure Move_Woken_Tasks is
         Current_Time : constant Time := Clock;
         Agent        : Oak_Agent_Id;
      begin
         while First_Item (Sleep_Queue) /= No_Agent
           and then Wake_Time (First_Item (Sleep_Queue)) <= Current_Time
         loop
            Remove_Top (From_Heap => Sleep_Queue, Item => Agent);
            Add_Agent_To_End_Of_Runnable_Queue (Agent);
         end loop;
      end Move_Woken_Tasks;

      ---------------------------------
      -- Remove_Agent_From_Scheduler --
      ---------------------------------

      procedure Remove_Agent_From_Scheduler (Agent : in Oak_Agent_Id) is
         Removed_Agent : Oak_Agent_Id;
      begin
         --  The state of the agent determines which queue it is on.

         case State (Agent) is
            when Runnable | Entering_PO | Waiting_For_Event | Inactive |
                 Waiting_For_Protected_Object | Allowance_Exhausted =>

               --  For now we can only delete an agent at the head of its
               --  queue (the only possible way for an agent to be deleted
               --  at the moment). If the agent pulled off the runnable queue
               --  is not the agent we wanted we have a little problem

               Remove_Top (Runnable_Queue, Removed_Agent);
            when Sleeping =>
               --  Not supported at this point. Support for it would need
               --  a double linked list or a tree to ease its removal.
               raise Scheduler_Error;
            when others =>
               raise Scheduler_Error;
         end case;
      end Remove_Agent_From_Scheduler;

      ----------------------
      -- Select_Next_Task --
      ----------------------

      procedure Select_Next_Task
        (Message : out Oak_Message)
      is
         Next_Agent_To_Wake : Oak_Agent_Id;
         WT                 : Time;
      begin
         Move_Woken_Tasks;

         Next_Agent_To_Wake := First_Item (From_Heap => Sleep_Queue);

         if Next_Agent_To_Wake = No_Agent then
            WT := Time_Last;
         else
            WT := Wake_Time (Next_Agent_To_Wake);
         end if;

         Message :=
           (Message_Type        => Scheduler_Agent_Done,
            Next_Agent          => First_Item (Runnable_Queue),
            Wake_Priority       => 1,
            Wake_Scheduler_At   => WT);
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
      loop
         Service_Agent (Message);
         Request_Oak_Service
           (Reason_For_Run => Agent_Request,
            Message        => Message);
      end loop;
   end Run_Loop;

   function Wake_Greater_Than (Left, Right : in Oak_Agent_Id)
                               return Boolean is
     (Wake_Time (Left) > Wake_Time (Right));

end Acton.Scheduler_Agents.Priority_Server;
