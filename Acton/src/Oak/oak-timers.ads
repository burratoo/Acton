------------------------------------------------------------------------------
--                                                                          --
--                              OAK COMPONENTS                              --
--                                                                          --
--                                OAK.TIMERS                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                 Copyright (C) 2011-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

--  This package provides the Oak Timer data structure and its storage.

with Ada.Cyclic_Tasks; use Ada.Cyclic_Tasks;

with Oak.Agent;                   use Oak.Agent;
with Oak.Oak_Time;                use Oak.Oak_Time;
with Oak.Project_Support_Package; use Oak.Project_Support_Package;

with System; use System;

package Oak.Timers with Preelaborate is

   ---------------------
   -- Oak Timer Types --
   ---------------------

   type Oak_Timer_Id is private;
   --  Type use to identify an Oak Timer in the system.

   type Oak_Timer_Kind is (Scheduler_Timer, Action_Timer);
   --  Oak Timers come in three forms.

   -----------------
   -- Subprograms --
   -----------------

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

   procedure Set_Timer
     (Timer           : in out Action_Timer;
      Fire_Time       : in Oak_Time.Time := Oak_Time.Time_Last;
      Priority        : in Oak_Priority;
      Timer_Action    : in Ada.Cyclic_Tasks.Event_Response;
      Handler         : in Ada.Cyclic_Tasks.Response_Handler;
      Agent_To_Handle : access Oak.Agent.Tasks.Task_Agent'Class);

   procedure Set_Timer
     (Timer     : in out Scheduler_Timer;
      Fire_Time : in Oak_Time.Time := Oak_Time.Time_Last;
      Priority  : in Oak_Priority;
      Scheduler : not null access Oak.Agent.Schedulers.Scheduler_Agent'Class);

   function Agent_To_Handle (Timer : in out Action_Timer'Class)
     return Oak.Agent.Agent_Handler;

   function Firing_Time
     (Timer : in out Oak_Timer'Class) return Oak_Time.Time;

   function Handler (Timer : in out Action_Timer'Class) return
     Ada.Cyclic_Tasks.Response_Handler;

   function Timer_Action (Timer : in out Action_Timer'Class) return
     Ada.Cyclic_Tasks.Event_Response;

   function Timer_Has_Fired (Timer : in out Oak_Timer'Class) return Boolean;

   function Timer_Scheduler_Agent (Timer : in out Scheduler_Timer'Class)
     return access Oak.Agent.Schedulers.Scheduler_Agent'Class;

private

   ---------------------
   -- Oak Timer Types --
   ---------------------

   type Oak_Timer_Id is mod Max_Timers;
   --  The type that represents each Oak_Timer in the system.

   type Action_Timer_Data is record
   --  This record holds the data specific to the Action Timer. It is held in
   --  a seperate record so that we do not have to fill part of the Oak Timer
   --  storage pool with execution budget related timers for each task agent.
   --  This is a problem since the storage pool used here is an ordered set
   --  and we do not want to waste time balancing the internal tree with timers
   --  that do not need to be ordered (since these timers are only ever active
   --  when their task run). Instead, each Oak Instance holds a single timer
   --  for the active execution timer. Each Task Agent then contains a copy of
   --  this record, which the Oak Instance will access as need to fill the
   --  relavent details for its timer. By using this record it simplifies
   --  storing and moving the data as needed, as oppose to copying its
   --  components individually.



   type Oak_Timer (Kind : Oak_Timer_Kind := Timer_Action) is record
   --  Oak Timer is a variant record to support the three different types of
   --  Oak Timers. The sizes are fairly similar, with the smallest – Scheduler
   --  Timer – used less frequently that the other two so not much space is
   --  wasted.

      Fire_Time : Time;
      --  The time the timer will fire.

      case Oak_Timer_Kind is
         when Scheduler_Timer =>
            Priority  : Oak_Priority;
            --  The priority of the scheduler timer.

            Scheduler : Scheduler_Id;
            --  The scheduler that will run when the timer fires.


   end record;

   type Action_Timer is new Oak_Timer with record
      Timer_Action    : Ada.Cyclic_Tasks.Event_Response;
      Handler         : Ada.Cyclic_Tasks.Response_Handler;
      Agent_To_Handle : access Oak.Agent.Oak_Agent'Class;
   end record;

   type Scheduler_Timer is new Oak_Timer with record
      Scheduler                : access Agent.Schedulers.Scheduler_Agent'Class;
   end record;

   function Firing_Time
     (Timer : in out Oak_Timer'Class) return Oak_Time.Time is
      (Timer.Fire_Time);

   function Is_Armed (Timer : in out Oak_Timer'Class) return Boolean is
     (Timer.Timer_Manager /= null);

   function Handler (Timer : in out Action_Timer'Class) return
     Ada.Cyclic_Tasks.Response_Handler is (Timer.Handler);

   function Priority (Timer : in out Oak_Timer'Class) return Oak_Priority
     is (Timer.Priority);

   function Timer_Action (Timer : in out Action_Timer'Class) return
     Ada.Cyclic_Tasks.Event_Response is (Timer.Timer_Action);

   function Timer_Has_Fired (Timer : in out Oak_Timer'Class) return Boolean
     is (Timer.Is_Armed and then Clock >= Timer.Fire_Time);

   function Timer_Scheduler_Agent (Timer : in out Scheduler_Timer'Class)
     return access Oak.Agent.Schedulers.Scheduler_Agent'Class
   is (Timer.Scheduler);

end Oak.Timers;
