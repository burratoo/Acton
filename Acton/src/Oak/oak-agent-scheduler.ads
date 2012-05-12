with Oak.Real_Time;
with System;

limited with Oak.Agent.Tasks;

package Oak.Agent.Scheduler with Preelaborate is

   type Scheduler_Agent is new Oak_Agent with private;

   type Reason_For_Run is (Task_Yield,
                           Select_Next_Task,
                           Add_Task,
                           Remove_Task);

   procedure Initialise_Agent
     (Agent        : in out Scheduler_Agent'Class;
      Name         : in String;
      Call_Stack   : in Call_Stack_Handler;
      Max_Priority : in System.Any_Priority;
      Min_Prioirty : in System.Any_Priority;
      Run_Loop     : in System.Address);

   function Desired_Run_Time
     (Agent : in Scheduler_Agent'Class)
      return Oak.Real_Time.Time;

   function Lowest_Priority
     (Agent : in Scheduler_Agent'Class)
      return  System.Any_Priority;

   function Highest_Priority
     (Agent : in Scheduler_Agent'Class)
      return System.Any_Priority;

   function Next_Agent
     (Agent : access Scheduler_Agent'Class)
      return access Scheduler_Agent'Class;

   function Run_Reason
     (Agent : in Scheduler_Agent'Class)
      return  Reason_For_Run;

   function Task_To_Run
     (Agent : in Scheduler_Agent'Class)
      return access Tasks.Task_Agent'Class;

   function Task_To_Manage
     (Agent : in Scheduler_Agent'Class)
      return access Tasks.Task_Agent'Class;

   procedure Set_Chosen_Task (Agent : in out Scheduler_Agent'Class;
                              T     : access Tasks.Task_Agent'Class);

   procedure Set_Desired_Run_Time
     (Agent    : in out Scheduler_Agent'Class;
      Run_Time : in Oak.Real_Time.Time);

   procedure Set_Next_Agent
     (Agent      : not null access Scheduler_Agent'Class;
      Next_Agent : access Scheduler_Agent'Class);

   procedure Set_Run_Reason
     (Agent  : in out Scheduler_Agent'Class;
      Reason : in Reason_For_Run);

   procedure Set_Task_To_Manage
     (Agent : in out Scheduler_Agent'Class;
      MT    : access Tasks.Task_Agent'Class);

private

   type Scheduler_Agent is new Oak_Agent with record
      Lowest_Prioirty        : System.Any_Priority;
      Highest_Prioirty       : System.Any_Priority;

      Task_To_Run            : access Tasks.Task_Agent'Class := null;
      Desired_Agent_Run_Time : Oak.Real_Time.Time   := Oak.Real_Time.Time_Last;

      Manage_Task            : access Tasks.Task_Agent'Class := null;
      Run_Reason             : Reason_For_Run := Select_Next_Task;

      Next_Agent             : access Scheduler_Agent := null;
   end record;

   function Desired_Run_Time
     (Agent : in Scheduler_Agent'Class)
      return Oak.Real_Time.Time is (Agent.Desired_Agent_Run_Time);

   function Lowest_Priority
     (Agent : in Scheduler_Agent'Class)
      return System.Any_Priority is (Agent.Lowest_Prioirty);

   function Highest_Priority
     (Agent : in Scheduler_Agent'Class)
      return System.Any_Priority is (Agent.Highest_Prioirty);

   function Next_Agent
     (Agent : access Scheduler_Agent'Class)
      return access Scheduler_Agent'Class is (Agent.Next_Agent);

   function Run_Reason
     (Agent : in Scheduler_Agent'Class)
      return Reason_For_Run is (Agent.Run_Reason);

   function Task_To_Run
     (Agent : in Scheduler_Agent'Class)
      return access Tasks.Task_Agent'Class is (Agent.Task_To_Run);

   function Task_To_Manage
     (Agent : in Scheduler_Agent'Class)
      return access Tasks.Task_Agent'Class is (Agent.Manage_Task);
end Oak.Agent.Scheduler;
