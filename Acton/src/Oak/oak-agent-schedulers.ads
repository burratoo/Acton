with Oak.Oak_Time;
with Oak.Timers;
with System; use System;

limited with Oak.Agent.Tasks;

package Oak.Agent.Schedulers with Preelaborate is

   type Scheduler_Agent (Min_Priority, Max_Priority : Any_Priority)
     is abstract new Oak_Agent with private with Preelaborable_Initialization;

   type Scheduler_Handler is access all Scheduler_Agent;

   type Reason_For_Run is (Task_State_Change,
                           Select_Next_Task,
                           Add_Task,
                           Remove_Task);

   procedure Initialise_Scheduler_Agent
     (Agent : in out Scheduler_Agent) is abstract;

   procedure Initialise_Scheduler_Agent
     (Agent           : access Scheduler_Agent'Class;
      Name            : in String;
      Call_Stack_Size : in System.Storage_Elements.Storage_Count;
      Run_Loop        : in System.Address);

   function Desired_Run_Time
     (Agent : in Scheduler_Agent'Class)
      return Oak.Oak_Time.Time;

   function Lowest_Priority
     (Agent : in Scheduler_Agent'Class)
      return  System.Any_Priority;

   function Highest_Priority
     (Agent : in Scheduler_Agent'Class)
      return System.Any_Priority;

   function Next_Agent
     (Agent : in Scheduler_Agent'Class)
      return access Scheduler_Agent'Class;

   function Run_Reason
     (Agent : in Scheduler_Agent'Class)
      return Reason_For_Run;

   function Scheduler_Timer
     (Agent : access Scheduler_Agent'Class)
     return access Timers.Scheduler_Timer;

   function Task_To_Run
     (Agent : in Scheduler_Agent'Class)
      return access Tasks.Task_Agent'Class;

   function Task_To_Manage
     (Agent : in Scheduler_Agent'Class)
      return access Tasks.Task_Agent'Class;

   procedure Set_Chosen_Task
     (Agent : in out Scheduler_Agent'Class;
      T     : access Tasks.Task_Agent'Class);

   procedure Set_Desired_Run_Time
     (Agent    : in out Scheduler_Agent'Class;
      Run_Time : in Oak.Oak_Time.Time);

   procedure Set_Next_Agent
     (Agent      : in out Scheduler_Agent'Class;
      Next_Agent : access Scheduler_Agent'Class);

   procedure Set_Run_Reason
     (Agent  : in out Scheduler_Agent'Class;
      Reason : in Reason_For_Run);

   procedure Set_Task_To_Manage
     (Agent : in out Scheduler_Agent'Class;
      MT    : access Tasks.Task_Agent'Class);

private

   type Scheduler_Agent (Min_Priority, Max_Priority : Any_Priority)
     is abstract new Oak_Agent with record
      Lowest_Prioirty        : System.Any_Priority := Min_Priority;
      Highest_Prioirty       : System.Any_Priority := Max_Priority;
      Run_Timer              : aliased Timers.Scheduler_Timer;

      Task_To_Run            : access Tasks.Task_Agent'Class;
      Desired_Agent_Run_Time : Oak.Oak_Time.Time;

      Manage_Task            : access Tasks.Task_Agent'Class;
      Run_Reason             : Reason_For_Run;

      Next_Agent             : access Scheduler_Agent;
   end record;

   function Desired_Run_Time
     (Agent : in Scheduler_Agent'Class)
      return Oak.Oak_Time.Time is (Agent.Desired_Agent_Run_Time);

   function Lowest_Priority
     (Agent : in Scheduler_Agent'Class)
      return System.Any_Priority is (Agent.Lowest_Prioirty);

   function Highest_Priority
     (Agent : in Scheduler_Agent'Class)
      return System.Any_Priority is (Agent.Highest_Prioirty);

   function Next_Agent
     (Agent : in Scheduler_Agent'Class)
      return access Scheduler_Agent'Class is (Agent.Next_Agent);

   function Run_Reason
     (Agent : in Scheduler_Agent'Class)
      return Reason_For_Run is (Agent.Run_Reason);

   function Scheduler_Timer
     (Agent : access Scheduler_Agent'Class)
      return access Timers.Scheduler_Timer is (Agent.Run_Timer'Access);

   function Task_To_Run
     (Agent : in Scheduler_Agent'Class)
      return access Tasks.Task_Agent'Class is (Agent.Task_To_Run);

   function Task_To_Manage
     (Agent : in Scheduler_Agent'Class)
      return access Tasks.Task_Agent'Class is (Agent.Manage_Task);
end Oak.Agent.Schedulers;
