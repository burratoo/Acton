------------------------------------------------------------------------------
--                                                                          --
--                              OAK COMPONENTS                              --
--                                                                          --
--                           OAK.AGENT.TASK_AGENT                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                 Copyright (C) 2010-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

with Ada.Cyclic_Tasks; use Ada.Cyclic_Tasks;

with Oak.Agent.Oak_Agent; use Oak.Agent.Oak_Agent;
with Oak.Scheduler;       use Oak.Scheduler;
with Oak.States;          use Oak.States;

package body Oak.Agent.Tasks is

   --------------------
   -- New_Task_Agent --
   --------------------

   procedure New_Task_Agent
     (Agent             : out Task_Id;
      Stack_Address     : in Address;
      Stack_Size        : in Storage_Count;
      Name              : in String;
      Run_Loop          : in Address;
      Task_Value_Record : in Address;
      Normal_Priority   : in Integer;
      Cycle_Behaviour   : in Ada.Cyclic_Tasks.Behaviour;
      Cycle_Period      : in Oak_Time.Time_Span;
      Phase             : in Oak_Time.Time_Span;
      Execution_Budget  : in Oak_Time.Time_Span;
      Budget_Response   : in Ada.Cyclic_Tasks.Event_Response;
      Budget_Handler    : in Ada.Cyclic_Tasks.Response_Handler;
      Relative_Deadline : in Oak_Time.Time_Span;
      Deadline_Response : in Ada.Cyclic_Tasks.Event_Response;
      Deadline_Handler  : in Ada.Cyclic_Tasks.Response_Handler;
      Scheduler_Agent   : in Scheduler_Id_With_No := No_Agent;
      Chain             : in out Task_List;
      Elaborated        : in Address)
   is
      P  : Any_Priority;
   begin
      Allocate_An_Agent (Agent);

      Setup_Oak_Agent : declare
         SA : Scheduler_Id_With_No := Scheduler_Agent;
      begin

         --  Determine task priority

         if Normal_Priority in Any_Priority then
            P := System.Any_Priority (Normal_Priority);
         elsif Normal_Priority = Unspecified_Priority then
            P := Default_Priority;
         else
            raise Program_Error with "Priority out of range";
         end if;

         --  Find Scheduler Agent if needed
         if SA = No_Agent then
            SA := Scheduler.Find_Scheduler_For_System_Priority (P, 1);
         end if;

         New_Agent
           (Agent              => Agent,
            Name               => Name,
            Call_Stack_Address => Stack_Address,
            Call_Stack_Size    => Stack_Size,
            Run_Loop           => Run_Loop,
            Run_Loop_Parameter => Task_Value_Record,
            Normal_Priority    => P,
            Initial_State      => Activation_Pending,
            Scheduler_Agent    => SA,
            Wake_Time          => Time_Last);
      end Setup_Oak_Agent;

      Setup_Task : declare
         T : Task_Agent_Record renames Agent_Pool (Agent);
      begin
         T.Cycle_Behaviour   := Cycle_Behaviour;
         T.Cycle_Period      := Cycle_Period;
         T.Phase             := Phase;

         T.Execution_Budget  := Execution_Budget;
         T.Relative_Deadline := Relative_Deadline;
         T.Next_Run_Cycle    := Oak_Time.Time_Last;
         T.Event_Raised      := False;
         T.Elaborated        := Elaborated;

         case Budget_Response is
            when No_Response =>
               null;

            when Handler =>
               T.Budget_Action := Fill_Event_Timer_Data
                 (Timer_Action     => Handler,
                  Handler_Priority =>
                     Protected_Object_From_Access
                    (Budget_Handler).Normal_Priority,
                  Agent_To_Handle  => Agent,
                  Handler          => Budget_Handler);

            when Abort_Cycle | Abort_And_Raise_Exception =>
               T.Budget_Action := Fill_Event_Timer_Data
                 (Timer_Action     => Budget_Response,
                  Handler_Priority => P + 1,
                  Agent_To_Handle  => Agent);
         end case;

         case Deadline_Response is
            when No_Response =>
               null;

            when Handler =>
               New_Event_Timer
                 (Timer        => T.Deadline_Timer,
                  Priority     =>
                     Protected_Object_From_Access
                    (Deadline_Handler).Normal_Priority,
                  Timer_Action => Handler,
                  Agent        => Agent,
                  Handler      => Deadline_Handler);

            when Abort_Cycle | Abort_And_Raise_Exception =>
               New_Event_Timer
                 (Timer        => T.Deadline_Timer,
                  Priority     => P + 1,
                  Timer_Action => Deadline_Response,
                  Agent        => Agent,
                  Handler      => Deadline_Handler);
         end case;
      end Setup_Task;

      --  Add task to activation chain
      Set_Next_Agent (For_Agent => Agent, Next_Agent => Chain);
      Chain := Agent;
   end New_Task_Agent;

   ----------------
   -- Next_Queue --
   ----------------

   function Next_Queue
     (For_Task : in Task_Id; Entry_Id : out Entry_Index)
      return Task_Id_With_No
   is
      T : Task_Agent_Record renames Agent_Pool (For_Task);
   begin
      if T.Next_Queue = No_Agent then
         Entry_Id := No_Entry;
      else
         Entry_Id := Agent_Pool (T.Next_Queue).Id_Of_Entry;
      end if;

      return T.Next_Queue;
   end Next_Queue;

   -----------------------
   -- Set_Cycle_Periood --
   -----------------------

   procedure Set_Cycle_Period
     (For_Task     : in Task_Id;
      Cycle_Period : in Oak_Time.Time_Span) is
   begin
      Agent_Pool (For_Task).Cycle_Period := Cycle_Period;
   end Set_Cycle_Period;

   ---------------------
   -- Set_Id_Of_Entry --
   ---------------------

   procedure Set_Id_Of_Entry
     (For_Task : Task_Id;
      Entry_Id : Entry_Index) is
   begin
      Agent_Pool (For_Task).Id_Of_Entry := Entry_Id;
   end Set_Id_Of_Entry;

   --------------------------------
   -- Set_Next_Deadline_For_Task --
   --------------------------------

   procedure Set_Next_Deadline_For_Task
     (For_Task : in Task_Id;
      Using    : in Deadline_Base)
   is
      T            : Task_Agent_Record renames Agent_Pool (For_Task);
      New_Deadline : Oak_Time.Time;
   begin
      if T.Deadline_Timer /= No_Timer then
         case Using is
            when Wake_Up_Time =>
               New_Deadline := Wake_Time (For_Task) + T.Relative_Deadline;

            when Clock_Time =>
               New_Deadline := Clock + T.Relative_Deadline;

         end case;

         Set_Absolute_Deadline (For_Task, New_Deadline);
         Update_Timer
           (Timer => T.Deadline_Timer, New_Time => New_Deadline);

         if New_Deadline /= Oak_Time.Time_Last then
            Activate_Timer (T.Deadline_Timer);
         end if;

      end if;
   end Set_Next_Deadline_For_Task;

   --------------------
   -- Set_Next_Queue --
   --------------------

   procedure Set_Next_Queue
     (For_Task   : Task_Id;
      Next_Queue : Task_Id_With_No) is
   begin
      Agent_Pool (For_Task).Next_Queue := Next_Queue;
   end Set_Next_Queue;

   ---------------------------
   -- Set_Relative_Deadline --
   ---------------------------

   procedure Set_Relative_Deadline
     (For_Task          : in Task_Id;
      Relative_Deadline : in Oak_Time.Time_Span) is
   begin
      Agent_Pool (For_Task).Relative_Deadline := Relative_Deadline;
   end Set_Relative_Deadline;

   --------------------------
   -- Update_Task_Property --
   --------------------------

   procedure Update_Task_Property
     (For_Task           : in Task_Id;
      Property_To_Update : in Task_Property;
      Next_Task_To_Run   : out Oak_Agent_Id)
   is
      T : Task_Agent_Record renames Agent_Pool (For_Task);
   begin
      case Property_To_Update.Property is
         when Cycle_Period =>
            T.Cycle_Period := Property_To_Update.Cycle_Period;

         when Relative_Deadline =>
            T.Relative_Deadline := Property_To_Update.Deadline_Span;
      end case;

      Scheduler.Inform_Scheduler_Agent_Task_Has_Changed_State
        (Changed_Task     => For_Task,
         Next_Task_To_Run => Next_Task_To_Run);
   end Update_Task_Property;

end Oak.Agent.Tasks;
