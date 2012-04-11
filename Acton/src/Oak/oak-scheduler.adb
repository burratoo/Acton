with Oak.Oak_Task.Deadline_List;
with Oak.Oak_Task.Internal;                      use Oak.Oak_Task.Internal;
with Oak.Core_Support_Package;
use Oak.Core_Support_Package;
with Oak.Oak_Task.Data_Access;                   use Oak.Oak_Task.Data_Access;
with Oak.Core_Support_Package.Task_Support;
with Oak.Oak_Task.Scheduler_Agent;
with Oak.Core;
with System;                                     use System;
with Oak.Oak_Task.Queue;

package body Oak.Scheduler is
   package Tasks renames Oak.Core_Support_Package.Task_Support;
   package SA_Ops renames Oak.Oak_Task.Scheduler_Agent;
   package Inactive_Queue renames Oak.Oak_Task.Queue;

   -----------------------------------
   -- Insert_Task_Into_Dealine_List --
   -----------------------------------

   procedure Insert_Task_Into_Dealine_List
     (Scheduler_Info : in out Oak_Scheduler_Info;
      Task_To_Add    : Oak_Task_Handler)
   is
   begin
      Deadline_List.Insert_Task
        (List_Head   => Scheduler_Info.Task_Deadline_List,
         Task_To_Add => Task_To_Add);
   end Insert_Task_Into_Dealine_List;

   ------------------------------------
   -- Remove_Task_From_Deadline_List --
   ------------------------------------

   procedure Remove_Task_From_Deadline_List
     (Scheduler_Info : in out Oak_Scheduler_Info;
      Task_To_Remove : Oak_Task_Handler)
   is
   begin
      Deadline_List.Remove_Task
        (List_Head      => Scheduler_Info.Task_Deadline_List,
         Task_To_Remove => Task_To_Remove);
   end Remove_Task_From_Deadline_List;

   ---------------------------
   -- Task_Deadline_Updated --
   ---------------------------

   procedure Task_Deadline_Updated
     (Scheduler_Info : in out Oak_Scheduler_Info;
      Updated_Task   : Oak_Task_Handler)
   is
   begin
      Deadline_List.Task_Deadline_Updated
        (List_Head    => Scheduler_Info.Task_Deadline_List,
         Updated_Task => Updated_Task);
   end Task_Deadline_Updated;

   ---------------------------
   -- Get_Earliest_Deadline --
   ---------------------------

   function Get_Earliest_Deadline
     (Scheduler_Info : in Oak_Scheduler_Info)
      return           Time
   is
   begin
      return Deadline_List.Get_Earliest_Deadline
               (List_Head => Scheduler_Info.Task_Deadline_List);
   end Get_Earliest_Deadline;

   -----------------------------------------------------------
   -- Check_With_Scheduler_Agents_On_Which_Task_To_Run_Next --
   -----------------------------------------------------------
   procedure Check_With_Scheduler_Agents_On_Which_Task_To_Run_Next
     (Scheduler_Info : in out Oak_Scheduler_Info;
      Chosen_Task    : out Oak_Task_Handler)
   is
   begin
      Check_With_Scheduler_Agents_On_Which_Task_To_Run_Next
        (Chosen_Task          => Chosen_Task,
         From_Scheduler_Agent => Scheduler_Info.Scheduler_Agent_Table);
   end Check_With_Scheduler_Agents_On_Which_Task_To_Run_Next;

   -----------------------------------------------------------
   -- Check_With_Scheduler_Agents_On_Which_Task_To_Run_Next --
   -----------------------------------------------------------
   procedure Check_With_Scheduler_Agents_On_Which_Task_To_Run_Next
     (From_Scheduler_Agent : in Oak_Task_Handler;
      Chosen_Task          : out Oak_Task_Handler)
   is
      Agent : Oak_Task_Handler := From_Scheduler_Agent;
   begin
      Chosen_Task := null;

      while Agent /= null and then Chosen_Task = null loop
         --  Context switch to Manage Queues Routine.
         Chosen_Task :=
            Run_Scheduler_Agent (Agent => Agent, Reason => Select_Next_Task);
         Agent       := SA_Ops.Get_Next_Agent (T => Agent);
      end loop;

   end Check_With_Scheduler_Agents_On_Which_Task_To_Run_Next;

   ---------------------------------------------
   -- Inform_Scheduler_Agent_Task_Has_Yielded --
   ---------------------------------------------
   procedure Inform_Scheduler_Agent_Task_Has_Yielded
     (Chosen_Task : in out Oak_Task_Handler)
   is
      Agent : constant Oak_Task_Handler :=
         Get_Scheduler_Agent_For_Task (T => Chosen_Task);
   begin
      Chosen_Task :=
         Run_Scheduler_Agent (Agent => Agent, Reason => Task_Yield);

      if Chosen_Task = null then
         if SA_Ops.Get_Next_Agent (T => Agent) /= null then
            Check_With_Scheduler_Agents_On_Which_Task_To_Run_Next
              (From_Scheduler_Agent => SA_Ops.Get_Next_Agent (T => Agent),
               Chosen_Task          => Chosen_Task);
         else
            Chosen_Task := null;
         end if;
      end if;
   end Inform_Scheduler_Agent_Task_Has_Yielded;

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
      Chosen_Task    : in out Oak_Task_Handler)
   is
      Current_Time : constant Time             := Ada.Real_Time.Clock;
      Current_Task : constant Oak_Task_Handler := Chosen_Task;
      Agent        : Oak_Task_Handler          :=
        Scheduler_Info.Scheduler_Agent_Table;
   begin
      Chosen_Task := null;
      while Agent /= null and Chosen_Task = null loop
         if SA_Ops.Get_Desired_Run_Time (Agent) < Current_Time then
            Chosen_Task :=
               Run_Scheduler_Agent
                 (Agent  => Agent,
                  Reason => Select_Next_Task);
         end if;
         Agent := SA_Ops.Get_Next_Agent (Agent);
         exit when (Current_Task /= null and Agent /= null)
                  and then Get_Normal_Priority (Current_Task) >
                           SA_Ops.Get_Highest_Priority (Agent);
      end loop;
      if Chosen_Task = null then
         Chosen_Task := Current_Task;
      end if;
   end Run_The_Bloody_Scheduler_Agent_That_Wanted_To_Be_Woken;

   ---------------------------
   -- Add_Task_To_Scheduler --
   ---------------------------

   procedure Add_Task_To_Scheduler
     (Scheduler_Info : in out Oak_Scheduler_Info;
      T              : in Oak_Task_Handler)
   is
      Task_Priority : constant Any_Priority := Get_Normal_Priority (T => T);
      Agent         : Oak_Task_Handler      :=
        Scheduler_Info.Scheduler_Agent_Table;
   begin
      while Agent /= null
        and then Task_Priority < SA_Ops.Get_Lowest_Priority (Agent)
      loop
         Agent := SA_Ops.Get_Next_Agent (Agent);
      end loop;
      Set_Scheduler_Agent_For_Task (T => T, Agent => Agent);
      SA_Ops.Set_Task_To_Manage (Agent => Agent, MT => T);
      Run_Scheduler_Agent (Agent => Agent, Reason => Add_Task);
   end Add_Task_To_Scheduler;

   procedure Remove_Task_From_Scheduler (T : Oak_Task_Handler) is
      Agent : constant Oak_Task_Handler := Get_Scheduler_Agent_For_Task (T);
   begin
      SA_Ops.Set_Task_To_Manage (Agent => Agent, MT => T);
      Run_Scheduler_Agent (Agent => Agent, Reason => Remove_Task);
      Set_Scheduler_Agent_For_Task (T => T, Agent => null);
   end Remove_Task_From_Scheduler;

   procedure Activate_Task
     (Scheduler_Info : in out Oak_Scheduler_Info;
      T              : in Oak_Task_Handler) is
      Agent : constant Oak_Task_Handler := Get_Scheduler_Agent_For_Task (T);
   begin
      Inactive_Queue.Remove_Task (Queue => Scheduler_Info.Inactive_Task_List,
                                  T     => T);
      Set_State (T => T, State => Runnable);
      SA_Ops.Set_Task_To_Manage (Agent => Agent, MT => T);
      Run_Scheduler_Agent (Agent => Agent, Reason => Add_Task);
   end Activate_Task;

   procedure Deactivate_Task
     (Scheduler_Info : in out Oak_Scheduler_Info;
      T              : in Oak_Task_Handler) is
      Agent : constant Oak_Task_Handler := Get_Scheduler_Agent_For_Task (T);
   begin
      SA_Ops.Set_Task_To_Manage (Agent => Agent, MT => T);
      Run_Scheduler_Agent (Agent => Agent, Reason => Remove_Task);
      Set_State (T => T, State => Inactive);
      Inactive_Queue.Add_Task_To_Head
        (Queue => Scheduler_Info.Inactive_Task_List,
         T     => T);
   end Deactivate_Task;

   procedure Add_New_Task_To_Inactive_List
     (Scheduler_Info : in out Oak_Scheduler_Info;
      T              : in Oak_Task_Handler) is
      Task_Priority : constant Any_Priority := Get_Normal_Priority (T => T);
      Agent         : Oak_Task_Handler := Scheduler_Info.Scheduler_Agent_Table;
   begin
      while Agent /= null
        and then Task_Priority < SA_Ops.Get_Lowest_Priority (Agent)
      loop
         Agent := SA_Ops.Get_Next_Agent (Agent);
      end loop;
      Set_Scheduler_Agent_For_Task (T => T, Agent => Agent);
      Set_State (T => T, State => Inactive);
      Inactive_Queue.Add_Task_To_Head
        (Queue => Scheduler_Info.Inactive_Task_List,
         T     => T);
   end Add_New_Task_To_Inactive_List;

   -------------------------
   -- Run_Scheduler_Agent --
   -------------------------

   function Run_Scheduler_Agent
     (Agent  : in Oak_Task_Handler;
      Reason : in Reason_For_Run)
      return   Oak_Task_Handler
   is
   begin
      Run_Scheduler_Agent (Agent => Agent, Reason => Reason);
      return SA_Ops.Get_Task_To_Run (Agent => Agent);
   end Run_Scheduler_Agent;

   -------------------------
   -- Run_Scheduler_Agent --
   -------------------------

   procedure Run_Scheduler_Agent
     (Agent  : in Oak_Task_Handler;
      Reason : in Reason_For_Run)
   is
   begin
      SA_Ops.Set_Run_Reason (Agent => Agent, Reason => Reason);
      Oak.Core.Set_Current_Task (T => Agent);
      Tasks.Context_Switch_To_Scheduler_Agent;
   end Run_Scheduler_Agent;

   ---------------------------------------
   -- Get_Earliest_Scheduler_Agent_Time --
   ---------------------------------------

   function Get_Earliest_Scheduler_Agent_Time
     (Scheduler_Info : Oak_Scheduler_Info)
      return           Time
   is
      Earliest_Time : Time             := Time_Last;
      Agent         : Oak_Task_Handler :=
         Scheduler_Info.Scheduler_Agent_Table;
   begin
      while Agent /= null loop
         if Earliest_Time > SA_Ops.Get_Desired_Run_Time (Agent) then
            Earliest_Time := SA_Ops.Get_Desired_Run_Time (Agent);
         end if;
         Agent := SA_Ops.Get_Next_Agent (Agent);
      end loop;
      return Earliest_Time;
   end Get_Earliest_Scheduler_Agent_Time;

   ----------------------------
   -- Handle_Missed_Deadline --
   ----------------------------

   procedure Handle_Missed_Deadline
     (Scheduler_Info : in out Oak_Scheduler_Info;
      Chosen_Task    : out Oak_Task_Handler)
   is
      pragma Unreferenced (Scheduler_Info, Chosen_Task);
   begin
      --  Generated stub: replace with real body!
      --  pragma Compile_Time_Warning
      --  (True,
      --   "Handle_Missed_Deadline unimplemented");
      null;
   end Handle_Missed_Deadline;

   ----------------------
   -- Get_Running_Task --
   ----------------------

   function Get_Running_Task
     (Scheduler_Info : in Oak_Scheduler_Info)
      return           Oak_Task_Handler
   is
   begin
      return Scheduler_Info.Running_Task;
   end Get_Running_Task;

   -------------------
   -- Get_Next_Task --
   -------------------

   function Get_Next_Task
     (Scheduler_Info : in Oak_Scheduler_Info)
      return           Oak_Task_Handler
   is
   begin
      return Scheduler_Info.Next_Task;
   end Get_Next_Task;

   function Get_Inital_Info_Record return Oak_Scheduler_Info is
   begin
      return
        (Running_Task          => null,
         Next_Task             => null,
         Scheduler_Agent_Table => null,
         Task_Deadline_List    => null,
         Inactive_Task_List    => null);
   end Get_Inital_Info_Record;

end Oak.Scheduler;
