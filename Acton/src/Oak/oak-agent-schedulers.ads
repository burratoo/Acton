with Oak.Timers;

package Oak.Agent.Schedulers with Preelaborate is

   type Scheduler_Agent (Min_Priority, Max_Priority : Any_Priority)
     is abstract new Oak_Agent with private with Preelaborable_Initialization;

   type Scheduler_Handler is access all Scheduler_Agent;

   procedure Initialise_Scheduler_Agent
     (Agent : in out Scheduler_Agent) is abstract;

   procedure Initialise_Scheduler_Agent
     (Agent                : access Scheduler_Agent'Class;
      Name                 : in String;
      Call_Stack_Size      : in Storage_Elements.Storage_Count;
      Run_Loop             : in Address;
      When_To_Charge_Agent : in Charge_Occurrence := All_Priorities);

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

   procedure Set_Agent_To_Run
     (Agent        : in out Scheduler_Agent'Class;
      Agent_To_Run : access Oak_Agent'Class);

   procedure Set_Priority_Range
     (Agent : in out Scheduler_Agent'Class;
      From  : in Any_Priority;
      To    : in Any_Priority);

   function Scheduler_Timer
     (Agent : not null access Scheduler_Agent'Class)
     return not null access Timers.Scheduler_Timer;

private

   type Scheduler_Agent (Min_Priority, Max_Priority : Any_Priority)
     is abstract new Oak_Agent with record
      Lowest_Priority        : System.Any_Priority := Min_Priority;
      Highest_Priority       : System.Any_Priority := Max_Priority;
      Agent_To_Run           : access Oak_Agent'Class;
      Run_Timer              : aliased Timers.Scheduler_Timer;
   end record;

   function Agent_To_Run
     (Agent : in Scheduler_Agent'Class)
      return access Oak_Agent'Class
      is (Agent.Agent_To_Run);

   function Desired_Run_Time
     (Agent : in Scheduler_Agent'Class)
      return Oak.Oak_Time.Time
      is (Agent.Message_Store.Message.Wake_Scheduler_At);

   function Lowest_Priority
     (Agent : in Scheduler_Agent'Class)
      return System.Any_Priority is (Agent.Lowest_Priority);

   function Highest_Priority
     (Agent : in Scheduler_Agent'Class)
      return System.Any_Priority is (Agent.Highest_Priority);

   function Scheduler_Timer
     (Agent : not null access Scheduler_Agent'Class)
      return not null access Timers.Scheduler_Timer
      is (Agent.Run_Timer'Unchecked_Access);
end Oak.Agent.Schedulers;
