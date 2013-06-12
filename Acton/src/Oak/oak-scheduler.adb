with Oak.Agent.Tasks.Queues;
with Oak.Core;
with Oak.Core_Support_Package.Task_Support;

with Oak.Core_Support_Package; use Oak.Core_Support_Package;
with System;                   use System;

package body Oak.Scheduler is

   package Inactive_Queue renames Oak.Agent.Tasks.Queues.General;

   procedure Activate_Task
     (Scheduler_Info : in out Oak_Scheduler_Info;
      T              : access Task_Agent'Class)
   is
      Agent : constant access Scheduler_Agent'Class :=
                T.Scheduler_Agent_For_Task;
   begin
      Inactive_Queue.Remove_Agent
        (Queue => Scheduler_Info.Inactive_Task_List,
         Agent => T);
      T.Set_State (Runnable);
      Agent.Set_Task_To_Manage (MT => T);
      Run_Scheduler_Agent (Agent => Agent, Reason => Add_Task);
   end Activate_Task;

   procedure Add_New_Task_To_Inactive_List
     (Scheduler_Info : in out Oak_Scheduler_Info;
      T              : access Task_Agent'Class)
   is
      Task_Priority : constant Any_Priority := T.Normal_Priority;
      Agent         : access Scheduler_Agent'Class :=
                        Scheduler_Info.Scheduler_Agent_Table;
   begin
      while Agent /= null
        and then Task_Priority < Agent.Lowest_Priority
      loop
         Agent := Agent.Next_Agent;
      end loop;
      T.Set_Scheduler_Agent_For_Task (Agent);
      T.Set_State (Inactive);
      Inactive_Queue.Add_Agent_To_Head
        (Queue => Scheduler_Info.Inactive_Task_List,
         Agent => T);
   end Add_New_Task_To_Inactive_List;

   procedure Add_Task_To_Scheduler
     (Scheduler_Info : in out Oak_Scheduler_Info;
      T              : access Task_Agent'Class)
   is
      Task_Priority : constant Any_Priority := T.Normal_Priority;
      Agent         : access Scheduler_Agent'Class :=
        Scheduler_Info.Scheduler_Agent_Table;
   begin
      while Agent /= null
        and then Task_Priority < Agent.Lowest_Priority
      loop
         Agent := Agent.Next_Agent;
      end loop;
      T.Set_Scheduler_Agent_For_Task (Agent);
      Agent.Set_Task_To_Manage (T);
      Run_Scheduler_Agent (Agent => Agent, Reason => Add_Task);
   end Add_Task_To_Scheduler;

   procedure Deactivate_Task
     (Scheduler_Info : in out Oak_Scheduler_Info;
      T              : access Task_Agent'Class)
   is
      Agent : constant access Scheduler_Agent'Class :=
                T.Scheduler_Agent_For_Task;
   begin
      Agent.Set_Task_To_Manage (T);
      Run_Scheduler_Agent (Agent => Agent, Reason => Remove_Task);
      T.Set_State (Inactive);
      Inactive_Queue.Add_Agent_To_Head
        (Queue => Scheduler_Info.Inactive_Task_List,
         Agent => T);
   end Deactivate_Task;

   function Earliest_Scheduler_Agent_Time
     (Scheduler_Info : Oak_Scheduler_Info)
      return           Time
   is
      Earliest_Time : Time := Time_Last;

      Agent : access Scheduler_Agent'Class :=
                Scheduler_Info.Scheduler_Agent_Table;
   begin
      while Agent /= null loop
         if Earliest_Time > Agent.Desired_Run_Time then
            Earliest_Time := Agent.Desired_Run_Time;
         end if;
         Agent := Agent.Next_Agent;
      end loop;
      return Earliest_Time;
   end Earliest_Scheduler_Agent_Time;

   procedure Check_With_Scheduler_Agents_On_Which_Task_To_Run_Next
     (Scheduler_Info : in out Oak_Scheduler_Info;
      Chosen_Task    : out Task_Handler)
   is
   begin
      Check_With_Scheduler_Agents_On_Which_Task_To_Run_Next
        (Chosen_Task          => Chosen_Task,
         From_Scheduler_Agent => Scheduler_Info.Scheduler_Agent_Table);
   end Check_With_Scheduler_Agents_On_Which_Task_To_Run_Next;

   procedure Check_With_Scheduler_Agents_On_Which_Task_To_Run_Next
     (From_Scheduler_Agent : access Scheduler_Agent'Class;
      Chosen_Task          : out Task_Handler)
   is
      Agent : access Scheduler_Agent'Class := From_Scheduler_Agent;
   begin
      Chosen_Task := null;

      while Agent /= null and then Chosen_Task = null loop
         --  Context switch to Manage Queues Routine.
         Chosen_Task :=
           Run_Scheduler_Agent (Agent => Agent, Reason => Select_Next_Task);
         Agent       := Agent.Next_Agent;
      end loop;
   end Check_With_Scheduler_Agents_On_Which_Task_To_Run_Next;

   procedure Handle_Missed_Deadline
     (Scheduler_Info : in out Oak_Scheduler_Info;
      Chosen_Task    : out Task_Handler)
   is
      pragma Unreferenced (Scheduler_Info, Chosen_Task);
   begin
      --  Generated stub: replace with real body!
      --  pragma Compile_Time_Warning
      --  (True,
      --   "Handle_Missed_Deadline unimplemented");
      null;
   end Handle_Missed_Deadline;

   procedure Inform_Scheduler_Agent_Task_Has_Changed_State
     (Chosen_Task : in out Task_Handler)
   is
      Agent : constant access Scheduler_Agent'Class :=
         Chosen_Task.Scheduler_Agent_For_Task;
   begin
      Agent.Set_Task_To_Manage (Chosen_Task);
      Chosen_Task :=
         Run_Scheduler_Agent (Agent => Agent, Reason => Task_State_Change);

      if Chosen_Task = null then
         if Agent.Next_Agent /= null then
            Check_With_Scheduler_Agents_On_Which_Task_To_Run_Next
              (From_Scheduler_Agent => Agent.Next_Agent,
               Chosen_Task          => Chosen_Task);
         else
            Chosen_Task := null;
         end if;
      end if;
   end Inform_Scheduler_Agent_Task_Has_Changed_State;

   procedure Insert_Task_Into_Dealine_List
     (Scheduler_Info : in out Oak_Scheduler_Info;
      Task_To_Add    : access Task_Agent'Class)
   is
   begin
      null;
   end Insert_Task_Into_Dealine_List;

   procedure Remove_Task_From_Deadline_List
     (Scheduler_Info : in out Oak_Scheduler_Info;
      Task_To_Remove : access Task_Agent'Class)
   is
   begin
      null;
   end Remove_Task_From_Deadline_List;

   procedure Remove_Task_From_Scheduler
     (T : access Task_Agent'Class)
   is
      Agent : constant access Scheduler_Agent'Class :=
                T.Scheduler_Agent_For_Task;
   begin
      Agent.Set_Task_To_Manage (T);
      Run_Scheduler_Agent (Agent => Agent, Reason => Remove_Task);
      T.Set_Scheduler_Agent_For_Task (null);
   end Remove_Task_From_Scheduler;

   function Run_Scheduler_Agent
     (Agent  : access Scheduler_Agent'Class;
      Reason : in Reason_For_Run)
      return access Task_Agent'Class is
   begin
      Run_Scheduler_Agent (Agent => Agent, Reason => Reason);
      return Agent.Task_To_Run;
   end Run_Scheduler_Agent;

   procedure Run_Scheduler_Agent
     (Agent  : access Scheduler_Agent'Class;
      Reason : in Reason_For_Run) is
   begin
      Agent.Set_Run_Reason (Reason);
      Core.Set_Current_Agent (Agent => Agent);
      Core_Support_Package.Task_Support.Context_Switch_To_Scheduler_Agent;
   end Run_Scheduler_Agent;

   ------------------------------------------------------------
   -- Run_The_Bloody_Scheduler_Agent_That_Wanted_To_Be_Woken --
   --
   --  Note that we check from the highest priority agent down
   --  as a higher priority agent may wake up inbetween the
   --  wake up alarm and the lower priority agent actually
   --  running
   ------------------------------------------------------------
   procedure Run_The_Bloody_Scheduler_Agent_That_Wanted_To_Be_Woken
     (Scheduler_Info : in out Oak_Scheduler_Info;
      Chosen_Task    : in out Task_Handler)
   is
      Current_Time : constant Time         := Oak_Time.Clock;
      Current_Task : constant Task_Handler := Chosen_Task;
      Agent        : access Scheduler_Agent'Class :=
        Scheduler_Info.Scheduler_Agent_Table;
   begin
      Chosen_Task := null;
      while Agent /= null and Chosen_Task = null loop
         if Agent.Desired_Run_Time < Current_Time then
            Chosen_Task :=
               Run_Scheduler_Agent
                 (Agent  => Agent,
                  Reason => Select_Next_Task);
         end if;
         Agent := Agent.Next_Agent;
         exit when (Current_Task /= null and Agent /= null)
           and then Current_Task.Normal_Priority > Agent.Highest_Priority;
      end loop;

      if Chosen_Task = null then
         Chosen_Task := Current_Task;
      end if;
   end Run_The_Bloody_Scheduler_Agent_That_Wanted_To_Be_Woken;

   procedure Task_Deadline_Updated
     (Scheduler_Info : in out Oak_Scheduler_Info;
      Updated_Task   : access Task_Agent'Class) is
   begin
      null;
   end Task_Deadline_Updated;

end Oak.Scheduler;
