with Oak.Agent.Schedulers; use Oak.Agent.Schedulers;
with Oak.Agent.Tasks;      use Oak.Agent.Tasks;
with Oak.Message;           use Oak.Message;

package Oak.Scheduler with Preelaborate is

   type Oak_Scheduler_Info is limited private
     with Preelaborable_Initialization;

   function Next_Task
     (Scheduler_Info : Oak_Scheduler_Info)
      return access Task_Agent'Class;

   function Running_Task
     (Scheduler_Info : Oak_Scheduler_Info)
      return access Task_Agent'Class;

   procedure Add_Task_To_Scheduler
     (Scheduler_Info : in out Oak_Scheduler_Info;
      T              : access Task_Agent'Class);
   --  Adds task to one of the main scheduler agents based on the task's
   --  priority.

   procedure Add_Scheduler_To_Scheduler_Table
     (Scheduler_Info : in out Oak_Scheduler_Info;
      Scheduler      : access Scheduler_Agent'Class);

   procedure Check_With_Scheduler_Agents_On_Which_Task_To_Run_Next
     (Scheduler_Info : in out Oak_Scheduler_Info;
      Chosen_Task    : out Task_Handler);

   procedure Check_With_Scheduler_Agents_On_Which_Task_To_Run_Next
     (From_Scheduler_Agent : access Scheduler_Agent'Class;
      Chosen_Task          : out Task_Handler);

   procedure Inform_Scheduler_Agent_Task_Has_Changed_State
     (Chosen_Task : in out Task_Handler);

   procedure Remove_Task_From_Scheduler
     (T              : access Task_Agent'Class);

   function Run_Scheduler_Agent
     (Agent  : access Scheduler_Agent'Class;
      Reason : in Oak_Message)
      return access Task_Agent'Class;

   procedure Run_Scheduler_Agent
     (Agent  : access Scheduler_Agent'Class;
      Reason : in Oak_Message);

   procedure Run_The_Bloody_Scheduler_Agent_That_Wanted_To_Be_Woken
     (Scheduler_Info : in out Oak_Scheduler_Info;
      Chosen_Task    : in out Task_Handler);

private

   type Oak_Scheduler_Info is limited record
      Running_Task          : access Task_Agent'Class := null;
      Next_Task             : access Task_Agent'Class := null;
      Scheduler_Agent_Table : access Scheduler_Agent'Class := null;
      --  array of Scheduler_Agents.
      --  Populated by the preprocessor.
   end record;

   function Next_Task
     (Scheduler_Info : Oak_Scheduler_Info)
      return access Task_Agent'Class is (Scheduler_Info.Next_Task);

   function Running_Task
     (Scheduler_Info : Oak_Scheduler_Info)
      return access Task_Agent'Class is (Scheduler_Info.Running_Task);

end Oak.Scheduler;
