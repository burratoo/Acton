with Oak.Agent;            use Oak.Agent;
with Oak.Agent.Schedulers; use Oak.Agent.Schedulers;
with Oak.Agent.Tasks;      use Oak.Agent.Tasks;
with Oak.Message;          use Oak.Message;
with System;               use System;
with System.Multiprocessors;

package Oak.Scheduler with Preelaborate is

   type Oak_Scheduler_Info is limited private
     with Preelaborable_Initialization;

   function Find_Scheduler_For_System_Priority
     (Priority : Any_Priority;
      CPU      : System.Multiprocessors.CPU_Range)
     return access Scheduler_Agent'Class;

   procedure Add_Agent_To_Scheduler (Agent : not null access Oak_Agent'Class);

   procedure Add_Scheduler_To_Scheduler_Table
     (Scheduler_Info : in out Oak_Scheduler_Info;
      Scheduler      : access Scheduler_Agent'Class);

   procedure Check_Sechduler_Agents_For_Next_Task_To_Run
     (Scheduler_Info   : in out Oak_Scheduler_Info;
      Next_Task_To_Run : out Agent_Handler);
   --  Queries the system scheduler agents for the next task to run. Does not
   --  run the scheduler agents themselves, instead it relies on the cached
   --  results of the last run.

   procedure Check_Sechduler_Agents_For_Next_Task_To_Run
     (From_Scheduler_Agent : access Scheduler_Agent'Class;
      Next_Task_To_Run     : out Agent_Handler);

   procedure Inform_Scheduler_Agent_Task_Has_Changed_State
     (Changed_Task     : access Task_Agent'Class;
      Next_Task_To_Run : out Agent_Handler);

   procedure Remove_Agent_From_Scheduler
     (Agent : not null access Oak_Agent'Class);

   procedure Run_The_Bloody_Scheduler_Agent_That_Wanted_To_Be_Woken
     (Agent            : access Scheduler_Agent'Class;
      Current_Agent    : in Agent_Handler;
      Next_Task_To_Run : out Agent_Handler);

private

   type Oak_Scheduler_Info is limited record
      Next_Task             : access Task_Agent'Class := null;
      Scheduler_Agent_Table : access Scheduler_Agent'Class := null;
      --  array of Scheduler_Agents.
      --  Populated by the preprocessor.
   end record;

   function Run_Scheduler_Agent
     (Agent  : not null access Scheduler_Agent'Class;
      Reason : in Oak_Message)
      return access Task_Agent'Class;

   procedure Run_Scheduler_Agent
     (Agent  : not null access Scheduler_Agent'Class;
      Reason : in Oak_Message);

end Oak.Scheduler;
