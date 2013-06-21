with Ada.Cyclic_Tasks;
with Oak.Oak_Time;
with System; use System;

limited with Oak.Agent.Schedulers;
limited with Oak.Agent.Tasks;

package Oak.Timers with Preelaborate is

   type Oak_Timer       is tagged private;
   type Action_Timer    is new Oak_Timer with private;
   type Scheduler_Timer is new Oak_Timer with private;

   type Oak_Timer_Info is tagged private;

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
      Priority  : in Oak_Interrupt_Priority);

   procedure Update_Timer
     (Timer    : in out Oak_Timer'Class;
      New_Time : in Oak_Time.Time);

   procedure Delay_Timer
     (Timer    : in out Oak_Timer'Class;
      Delay_To : in Oak_Time.Time_Span);

   procedure Set_Timer
     (Timer           : in out Action_Timer;
      Fire_Time       : in Oak_Time.Time := Oak_Time.Time_Last;
      Priority        : in Oak_Interrupt_Priority;
      Timer_Action    : in Ada.Cyclic_Tasks.Event_Action;
      Handler         : in Ada.Cyclic_Tasks.Action_Handler;
      Agent_To_Handle : access Oak.Agent.Tasks.Task_Agent'Class);

   procedure Set_Timer
     (Timer     : in out Scheduler_Timer;
      Fire_Time : in Oak_Time.Time := Oak_Time.Time_Last;
      Priority  : in Oak_Interrupt_Priority;
      Scheduler : not null access Oak.Agent.Schedulers.Scheduler_Agent'Class);

   function Firing_Time
     (Timer : in out Oak_Timer'Class) return Oak_Time.Time;

   function Agent_To_Handle (Timer : in out Action_Timer'Class)
     return Oak.Agent.Tasks.Task_Handler;

   function Handler (Timer : in out Action_Timer'Class) return
     Ada.Cyclic_Tasks.Action_Handler;

   function Timer_Action (Timer : in out Action_Timer'Class) return
     Ada.Cyclic_Tasks.Event_Action;

   function Scheduler_Agent (Timer : in out Scheduler_Timer'Class) return
     access Oak.Agent.Schedulers.Scheduler_Agent;

private
   type Interrupt_Timers is array (Oak_Interrupt_Priority) of
     access Oak_Timer'Class;

   type Oak_Timer_Info is tagged record
      Timers : Interrupt_Timers;
   end record;

   type Oak_Timer is tagged record
      Timer_Manager  : access Oak_Timer_Info  := null;
      Fire_Time      : Oak_Time.Time          := Oak_Time.Time_Last;
      Priority       : Oak_Interrupt_Priority := Oak_Interrupt_Priority'First;
      Next_Timer     : access Oak_Timer'Class := null;
      Previous_Timer : access Oak_Timer'Class := null;
   end record;

   type Action_Timer is new Oak_Timer with record
      Timer_Action    : Ada.Cyclic_Tasks.Event_Action;
      Handler         : Ada.Cyclic_Tasks.Action_Handler;
      Agent_To_Handle : access Oak.Agent.Tasks.Task_Agent'Class;
   end record;

   type Scheduler_Timer is new Oak_Timer with record
      Scheduler : access Oak.Agent.Schedulers.Scheduler_Agent'Class;
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

end Oak.Timers;
