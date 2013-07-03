with Oak.Oak_Time;
with Oak.Timers;
with System;

package Oak.Agent.Schedulers with Preelaborate is

   type Scheduler_Agent (Min_Priority, Max_Priority : Any_Priority)
     is abstract new Oak_Agent with private with Preelaborable_Initialization;

   type Scheduler_Handler is access all Scheduler_Agent;

   procedure Initialise_Scheduler_Agent
     (Agent : in out Scheduler_Agent) is abstract;

   procedure Initialise_Scheduler_Agent
     (Agent           : access Scheduler_Agent'Class;
      Name            : in String;
      Call_Stack_Size : in System.Storage_Elements.Storage_Count;
      Run_Loop        : in System.Address);

   function Agent_To_Run
     (Agent : in Scheduler_Agent'Class)
      return access Oak_Agent'Class;

   function Desired_Run_Time
     (Agent : in Scheduler_Agent'Class)
      return Oak.Oak_Time.Time;

   function Lowest_Priority
     (Agent : in Scheduler_Agent'Class)
      return  System.Any_Priority;

   function Highest_Priority
     (Agent : in Scheduler_Agent'Class)
      return System.Any_Priority;

   function Scheduler_Timer
     (Agent : access Scheduler_Agent'Class)
     return access Timers.Scheduler_Timer;

private

   type Scheduler_Agent (Min_Priority, Max_Priority : Any_Priority)
     is abstract new Oak_Agent with record
      Lowest_Prioirty        : System.Any_Priority := Min_Priority;
      Highest_Prioirty       : System.Any_Priority := Max_Priority;
      Run_Timer              : aliased Timers.Scheduler_Timer;
   end record;

   function Agent_To_Run
     (Agent : in Scheduler_Agent'Class)
      return access Oak_Agent'Class
      is (Agent.Message_Store.Message.Next_Agent);

   function Desired_Run_Time
     (Agent : in Scheduler_Agent'Class)
      return Oak.Oak_Time.Time
      is (Agent.Message_Store.Message.Wake_Scheduler_At);

   function Lowest_Priority
     (Agent : in Scheduler_Agent'Class)
      return System.Any_Priority is (Agent.Lowest_Prioirty);

   function Highest_Priority
     (Agent : in Scheduler_Agent'Class)
      return System.Any_Priority is (Agent.Highest_Prioirty);

   function Scheduler_Timer
     (Agent : access Scheduler_Agent'Class)
      return access Timers.Scheduler_Timer is (Agent.Run_Timer'Access);
end Oak.Agent.Schedulers;
