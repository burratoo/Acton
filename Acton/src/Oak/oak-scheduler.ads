with Oak.Agent.Schedulers; use Oak.Agent.Schedulers;
with Oak.Agent.Tasks;      use Oak.Agent.Tasks;
with Oak.Oak_Time;        use Oak.Oak_Time;

package Oak.Scheduler with Preelaborate is

   type Oak_Scheduler_Info is private;

--     function Earliest_Deadline
--       (Scheduler_Info : Oak_Scheduler_Info)
--        return Time;

   function Next_Task
     (Scheduler_Info : Oak_Scheduler_Info)
      return access Task_Agent'Class;

   function Running_Task
     (Scheduler_Info : Oak_Scheduler_Info)
      return access Task_Agent'Class;

   procedure Activate_Task
     (Scheduler_Info : in out Oak_Scheduler_Info;
      T              : access Task_Agent'Class);

   procedure Add_New_Task_To_Inactive_List
     (Scheduler_Info : in out Oak_Scheduler_Info;
      T              : access Task_Agent'Class);

   procedure Add_Task_To_Scheduler
     (Scheduler_Info : in out Oak_Scheduler_Info;
      T              : access Task_Agent'Class);

   procedure Deactivate_Task
     (Scheduler_Info : in out Oak_Scheduler_Info;
      T              : access Task_Agent'Class);

   function Earliest_Scheduler_Agent_Time
     (Scheduler_Info : Oak_Scheduler_Info)
      return Time;

   procedure Check_With_Scheduler_Agents_On_Which_Task_To_Run_Next
     (Scheduler_Info : in out Oak_Scheduler_Info;
      Chosen_Task    : out Task_Handler);

   procedure Check_With_Scheduler_Agents_On_Which_Task_To_Run_Next
     (From_Scheduler_Agent : access Scheduler_Agent'Class;
      Chosen_Task          : out Task_Handler);

   procedure Handle_Missed_Deadline
     (Scheduler_Info : in out Oak_Scheduler_Info;
      Chosen_Task    : out Task_Handler);

   procedure Inform_Scheduler_Agent_Task_Has_Yielded
     (Chosen_Task : in out Task_Handler);

   procedure Insert_Task_Into_Dealine_List
     (Scheduler_Info : in out Oak_Scheduler_Info;
      Task_To_Add    : access Task_Agent'Class);

   procedure Remove_Task_From_Deadline_List
     (Scheduler_Info : in out Oak_Scheduler_Info;
      Task_To_Remove : access Task_Agent'Class);

   procedure Remove_Task_From_Scheduler
     (T              : access Task_Agent'Class);

   function Run_Scheduler_Agent
     (Agent  : access Scheduler_Agent'Class;
      Reason : in Reason_For_Run)
      return access Task_Agent'Class;

   procedure Run_Scheduler_Agent
     (Agent  : access Scheduler_Agent'Class;
      Reason : in Reason_For_Run);

   procedure Run_The_Bloody_Scheduler_Agent_That_Wanted_To_Be_Woken
     (Scheduler_Info : in out Oak_Scheduler_Info;
      Chosen_Task    : in out Task_Handler);

   procedure Task_Deadline_Updated
     (Scheduler_Info : in out Oak_Scheduler_Info;
      Updated_Task   : access Task_Agent'Class);

private

   type Oak_Scheduler_Info is record
      Running_Task          : access Task_Agent'Class := null;
      Next_Task             : access Task_Agent'Class := null;
      Scheduler_Agent_Table : access Scheduler_Agent'Class := null;
      --  array of Scheduler_Agents.
      --  Populated by the preprocessor.

      Task_Deadline_List    : access Task_Agent'Class := null;
      --  Should be a (ordered)
      --  linked list (allows us to
      --  insert and remove tasks in
      --  arbitrary posistions. Is
      --  there a better structure I
      --  could use? Could use
      --  Collection Package?)
      Inactive_Task_List   : access Task_Agent'Class := null;
   end record;

--     function Earliest_Deadline
--       (Scheduler_Info : Oak_Scheduler_Info)
--        return Time is (Deadline_List.Get_Earliest_Deadline
--                         (List_Head => Scheduler_Info.Task_Deadline_List));

   function Next_Task
     (Scheduler_Info : Oak_Scheduler_Info)
      return access Task_Agent'Class is (Scheduler_Info.Next_Task);

   function Running_Task
     (Scheduler_Info : Oak_Scheduler_Info)
      return access Task_Agent'Class is (Scheduler_Info.Running_Task);

end Oak.Scheduler;
