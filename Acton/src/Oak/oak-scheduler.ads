with Oak.Oak_Task;  use Oak.Oak_Task;
with Ada.Real_Time; use Ada.Real_Time;

package Oak.Scheduler is
   pragma Preelaborate;

   type Oak_Scheduler_Info is private;

   procedure Insert_Task_Into_Dealine_List
     (Scheduler_Info : in out Oak_Scheduler_Info;
      Task_To_Add    : Oak_Task_Handler);
   procedure Remove_Task_From_Deadline_List
     (Scheduler_Info : in out Oak_Scheduler_Info;
      Task_To_Remove : Oak_Task_Handler);
   procedure Task_Deadline_Updated
     (Scheduler_Info : in out Oak_Scheduler_Info;
      Updated_Task   : Oak_Task_Handler);
   function Get_Earliest_Deadline
     (Scheduler_Info : Oak_Scheduler_Info)
      return           Time;

   procedure Check_With_Scheduler_Agents_On_Which_Task_To_Run_Next
     (Scheduler_Info : in out Oak_Scheduler_Info;
      Chosen_Task    : out Oak_Task_Handler);
   procedure Check_With_Scheduler_Agents_On_Which_Task_To_Run_Next
     (From_Scheduler_Agent : Oak_Task_Handler;
      Chosen_Task          : out Oak_Task_Handler);
   procedure Run_Current_Task_Scheduler_Agent
     (Scheduler_Info : in out Oak_Scheduler_Info;
      Chosen_Task    : in out Oak_Task_Handler);
   procedure Run_The_Bloody_Scheduler_Agent_That_Wanted_To_Be_Woken
     (Scheduler_Info : in out Oak_Scheduler_Info;
      Chosen_Task    : in out Oak_Task_Handler);
   procedure Handle_Missed_Deadline
     (Scheduler_Info : in out Oak_Scheduler_Info;
      Chosen_Task    : out Oak_Task_Handler);
   procedure Add_Task_To_Scheduler
     (Scheduler_Info : in out Oak_Scheduler_Info;
      T              : Oak_Task_Handler);
   procedure Remove_Task_From_Scheduler
     (T              : Oak_Task_Handler);
   procedure Activate_Task
     (Scheduler_Info : in out Oak_Scheduler_Info;
      T              : in Oak_Task_Handler);
   procedure Deactivate_Task
     (Scheduler_Info : in out Oak_Scheduler_Info;
      T              : in Oak_Task_Handler);

   function Run_Scheduler_Agent
     (Agent  : Oak_Task_Handler;
      Reason : Reason_For_Run)
      return   Oak_Task_Handler;
   procedure Run_Scheduler_Agent
     (Agent  : Oak_Task_Handler;
      Reason : Reason_For_Run);

   function Get_Earliest_Scheduler_Agent_Time
     (Scheduler_Info : Oak_Scheduler_Info)
      return           Time;
   function Get_Running_Task
     (Scheduler_Info : Oak_Scheduler_Info)
      return           Oak_Task_Handler;
   function Get_Next_Task
     (Scheduler_Info : Oak_Scheduler_Info)
      return           Oak_Task_Handler;

   function Get_Inital_Info_Record return Oak_Scheduler_Info;

private

   type Oak_Scheduler_Info is record
      Running_Task          : Oak_Task_Handler := null;
      Next_Task             : Oak_Task_Handler := null;
      Scheduler_Agent_Table : Oak_Task_Handler := null;     --  array of
                                                            --  Scheduler_Agent
                                                            --  s.
      --  Populated by the preprocessor.
      Task_Deadline_List : Oak_Task_Handler := null; --  Should be a (ordered)
      --  linked list (allows us to
      --  insert and remove tasks in
      --  arbitrary posistions. Is
      --  there a better structure I
      --  could use? Could use
      --  Collection Package?)
      Inactive_Task_List   : Oak_Task_Handler := null;
   end record;
end Oak.Scheduler;
