with Ada.Cyclic_Tasks;
with Oak.Message;  use Oak.Message;
with Oak.Oak_Time; use Oak.Oak_Time;
with System; use System;

limited with Oak.Agent.Schedulers;
limited with Oak.Agent.Tasks;

package Oak.Timers with Preelaborate is

   type Oak_Timer       is tagged private
     with Preelaborable_Initialization;
   type Action_Timer    is new Oak_Timer with private
     with Preelaborable_Initialization;
   type Scheduler_Timer is new Oak_Timer with private
     with Preelaborable_Initialization;

   type Oak_Timer_Info is tagged limited private
     with Preelaborable_Initialization;

   procedure Add_Timer
     (Timer      : not null access Oak_Timer'Class;
      Timer_Info : not null access Oak_Timer_Info);

   procedure Add_Timer_To_Current_Processor
     (Timer : not null access Oak_Timer'Class);

   function Is_Armed (Timer : in out Oak_Timer'Class) return Boolean;

   function Earliest_Timer_To_Fire
     (Timer_Info     : Oak_Timer_Info;
      Above_Priority : Any_Priority := Interrupt_Priority'First - 1)
      return access Oak_Timer'Class;

   function Priority (Timer : in out Oak_Timer'Class) return Oak_Priority;

   procedure Remove_Timer (Timer : not null access Oak_Timer'Class);

   procedure Set_Timer
     (Timer     : in out Oak_Timer;
      Fire_Time : in Oak_Time.Time;
      Priority  : in Oak_Priority);

   procedure Update_Timer
     (Timer    : in out Oak_Timer'Class;
      New_Time : in Oak_Time.Time);

   procedure Delay_Timer
     (Timer    : in out Oak_Timer'Class;
      Delay_By : in Oak_Time.Time_Span);

   procedure Delay_Delayable_Timers
     (Timer_Info     : in Oak_Timer_Info;
      Delay_By       : in Oak_Time.Time_Span;
      Below_Priority : in Oak_Priority);

   procedure Set_Timer
     (Timer           : in out Action_Timer;
      Fire_Time       : in Oak_Time.Time := Oak_Time.Time_Last;
      Priority        : in Oak_Priority;
      Timer_Action    : in Ada.Cyclic_Tasks.Event_Action;
      Handler         : in Ada.Cyclic_Tasks.Action_Handler;
      Agent_To_Handle : access Oak.Agent.Tasks.Task_Agent'Class);

   procedure Set_Timer
     (Timer     : in out Scheduler_Timer;
      Fire_Time : in Oak_Time.Time := Oak_Time.Time_Last;
      Priority  : in Oak_Priority;
      Scheduler : not null access Oak.Agent.Schedulers.Scheduler_Agent'Class);

   procedure Set_Timer_Deferrable_Behaviour
     (Timer      : not null access Scheduler_Timer'Class;
      Timer_Info : not null access Oak_Timer_Info;
      Defer_Kind : in Deferrable_Type);

   procedure Set_Timer_To_Be_Deferrable
     (Timer      : not null access Scheduler_Timer'Class;
      Timer_Info : not null access Oak_Timer_Info;
      Defer_Kind : in Deferrable_Type);

   procedure Set_Timer_To_Not_Be_Deferrable
     (Timer : not null access Scheduler_Timer'Class);

   function Agent_To_Handle (Timer : in out Action_Timer'Class)
     return Oak.Agent.Agent_Handler;

   function Firing_Time
     (Timer : in out Oak_Timer'Class) return Oak_Time.Time;

   function Handler (Timer : in out Action_Timer'Class) return
     Ada.Cyclic_Tasks.Action_Handler;

   function Timer_Action (Timer : in out Action_Timer'Class) return
     Ada.Cyclic_Tasks.Event_Action;

   function Timer_Has_Fired (Timer : in out Oak_Timer'Class) return Boolean;

   function Timer_Scheduler_Agent (Timer : in out Scheduler_Timer'Class)
     return access Oak.Agent.Schedulers.Scheduler_Agent'Class;

private
   type Interrupt_Timers is array (Oak_Priority) of
     access Oak_Timer'Class;

   type Oak_Timer_Info is tagged limited record
      Timers                      : Interrupt_Timers;
      Timers_Delayed_By_Execution : access Scheduler_Timer'Class;
   end record;

   type Oak_Timer is tagged record
      Timer_Manager  : access Oak_Timer_Info;
      Fire_Time      : Time;
      Priority       : Oak_Priority;
      Next_Timer     : access Oak_Timer'Class;
      Previous_Timer : access Oak_Timer'Class;
   end record;

   type Action_Timer is new Oak_Timer with record
      Timer_Action    : Ada.Cyclic_Tasks.Event_Action;
      Handler         : Ada.Cyclic_Tasks.Action_Handler;
      Agent_To_Handle : access Oak.Agent.Oak_Agent'Class;
   end record;

   type Scheduler_Timer is new Oak_Timer with record
      Scheduler                : access Agent.Schedulers.Scheduler_Agent'Class;
      Next_Scheduler_Timer     : access Scheduler_Timer'Class;
      Previous_Scheduler_Timer : access Scheduler_Timer'Class;
      Deferrable_Timer         : Deferrable_Type;
   end record;

   function Firing_Time
     (Timer : in out Oak_Timer'Class) return Oak_Time.Time is
      (Timer.Fire_Time);

   function Is_Armed (Timer : in out Oak_Timer'Class) return Boolean is
     (Timer.Timer_Manager /= null);

   function Handler (Timer : in out Action_Timer'Class) return
     Ada.Cyclic_Tasks.Action_Handler is (Timer.Handler);

   function Priority (Timer : in out Oak_Timer'Class) return Oak_Priority
     is (Timer.Priority);

   function Timer_Action (Timer : in out Action_Timer'Class) return
     Ada.Cyclic_Tasks.Event_Action is (Timer.Timer_Action);

   function Timer_Has_Fired (Timer : in out Oak_Timer'Class) return Boolean
     is (Timer.Is_Armed and then Clock >= Timer.Fire_Time);

   function Timer_Scheduler_Agent (Timer : in out Scheduler_Timer'Class)
     return access Oak.Agent.Schedulers.Scheduler_Agent'Class
   is (Timer.Scheduler);

end Oak.Timers;
