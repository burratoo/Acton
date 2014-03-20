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
with Oak.Storage.Time_Priority_Pool;

with System; use System;

package Oak.Timers with Preelaborate is

   ---------------------
   -- Oak Timer Types --
   ---------------------

   type Oak_Timer_Id is private with Preelaborable_Initialization;
   --  Type use to identify an Oak Timer in the system.

   No_Timer : constant Oak_Timer_Id;

   type Oak_Timer_Kind is (Empty_Timer, Scheduler_Timer, Event_Timer);
   --  Oak Timers come in three forms.

   -----------------
   -- Subprograms --
   -----------------

   procedure Activate_Timer (Timer : in Oak_Timer_Id)
     with Pre  => Is_Valid (Timer),
          Post => Is_Valid (Timer);
   --  Activate the specified timer. Adds the timer to storage pool's timer
   --  list. Can be safely called even if the timer is already activated.

   function Agent_To_Handle (Timer : in Oak_Timer_Id)
                             return Oak_Agent_Id
     with Pre => Timer_Kind (Timer) = Event_Timer;
   --  Returns the agent attached to the Action Timer, that is the Agent which
   --  the action to will apply to.
   --  TIMER KIND: Event_Timer.

   procedure Deactivate_Timer (Timer : in Oak_Timer_Id)
     with Pre  => Is_Valid (Timer),
          Post => Is_Valid (Timer);
   --  Deactivate the specified timer. Can be safely called even if the timer
   --  is not already activated (in this case the request has no effect).

   procedure Delete_Timer (Timer : in Oak_Timer_Id);
   --  Delete the Timer from the storage pool.

   function Earliest_Timer_To_Fire
     (Above_Priority : in Any_Priority := Oak_Priority'First)
      return Oak_Timer_Id;
   --  Returns the timer that will fire first the is above the given priority.

   function Firing_Time (Timer : in Oak_Timer_Id) return Oak_Time.Time
     with Inline;
   --  Returns the time that the given timer will fire.

   function Has_Timer_Fired (Timer : in Oak_Timer_Id) return Boolean;
   --  Has the timer fired.

   function Handler (Timer : in Oak_Timer_Id)
                     return Ada.Cyclic_Tasks.Response_Handler
       with Pre => Timer_Kind (Timer) = Event_Timer;
   --  Returns the handler attach to the Action Timer.
   --  TIMER KIND: Event_Timer.

   function Is_Active (Timer : in Oak_Timer_Id) return Boolean;
   --  Can the timer fire.

   function Is_Valid (Timer : in Oak_Timer_Id) return Boolean;
   --  Is the Id refering to a valid timer.

   procedure New_Timer
     (Timer     : out Oak_Timer_Id;
      Priority  : in  Oak_Priority;
      Fire_Time : in  Oak_Time.Time := Time_Last;
      Activate  : in  Boolean := False);
   --  Create a new generic timer.

   procedure New_Event_Timer
     (Timer        : out Oak_Timer_Id;
      Priority     : in  Oak_Priority;
      Timer_Action : in  Ada.Cyclic_Tasks.Event_Response;
      Agent        : in  Oak_Agent_Id;
      Handler      : in  Ada.Cyclic_Tasks.Response_Handler := null;
      Fire_Time    : in  Oak_Time.Time := Time_Last;
      Activate     : in  Boolean := False);
   --  Creates a new Action Timer from its componenets.

   procedure New_Scheduler_Timer
     (Timer     : out Oak_Timer_Id;
      Priority  : in  Oak_Priority;
      Scheduler : in  Scheduler_Id;
      Fire_Time : in  Oak_Time.Time := Time_Last;
      Activate  : in  Boolean := False);
   --  Create a new Scheduler Timer.

   function Scheduler_Agent (Timer : in Oak_Timer_Id)
                                return Scheduler_Id
     with Pre => Timer_Kind (Timer) = Scheduler_Timer;
   --  Returns the scheduler attached to a scheduler timer.
   --  TIMER KIND: SCHEDULER_TIMER.

   procedure Setup_Timers;
   --  Sets up the timers pool.

   function Timer_Action (Timer : in Oak_Timer_Id)
     return Ada.Cyclic_Tasks.Event_Response
     with Inline, Pre => Timer_Kind (Timer) = Event_Timer;
   --  Returns the action that occurs when an Timer fires.
   --  TIMER KIND: Event_Timer.

   function Timer_Kind (Timer : in Oak_Timer_Id) return Oak_Timer_Kind
     with Inline;
   --  Returns what kind of Oak Timer the specified timer is.

   function Timer_Priority (Timer : in Oak_Timer_Id) return Oak_Priority;
   --  Priority attached to the timer.

   procedure Update_Timer
     (Timer    : in Oak_Timer_Id;
      New_Time : in Oak_Time.Time);
   --  Update the timer's fire time.

private

   ---------------------
   -- Oak Timer Types --
   ---------------------

   type Oak_Timer_Id is mod Max_Timers;
   --  The type that represents each Oak_Timer in the system.

   type Oak_Timer (Kind : Oak_Timer_Kind := Event_Timer) is record
   --  Oak Timer is a variant record to support the three different types of
   --  Oak Timers. The sizes are fairly similar, with the smallest - Scheduler
   --  Timer – used less frequently that the other two so not much space is
   --  wasted.

      Fire_Time : Time;
      --  The time the timer will fire.

      Priority  : Oak_Priority;
      --  The priority of the scheduler timer.

      case Kind is
         when Empty_Timer =>
            null;

         when Scheduler_Timer =>
            Scheduler : Scheduler_Id;
            --  The scheduler that will run when the timer fires.

         when Event_Timer =>
            Timer_Action     : Ada.Cyclic_Tasks.Event_Response;
            --  The action the timer will perform when it fires.

            Agent_To_Handle  : Oak_Agent_Id;
            --  The Agent associated with this timer.

            Event_Handler    : Ada.Cyclic_Tasks.Response_Handler;
            --  If the action of the timer is to call a handler it will call
            --  this one.
      end case;
   end record;

   -------------------
   -- Timer Storage --
   -------------------

   function "<"            (Left, Right : in Oak_Timer) return Boolean
     with Inline;
   function Firing_Time    (Timer : in Oak_Timer) return Oak_Time.Time
     with Inline;
   function Timer_Priority (Timer : in Oak_Timer) return Oak_Priority
     with Inline;

   package Storage is new Oak.Storage.Time_Priority_Pool
     (Element_Type  => Oak_Timer,
      Key_Type      => Oak_Time.Time,
      Node_Location => Oak_Timer_Id,
      Priority_Type => Oak_Priority,
      "<"           => "<",
      Key           => Firing_Time,
      Priority      => Timer_Priority);

   use Storage;

   Pool : Pool_Type;

   No_Timer : constant Oak_Timer_Id := No_Node;

   --------------------------
   -- Function Expressions --
   --------------------------

   function "<" (Left, Right : in Oak_Timer) return Boolean is
     (Left.Fire_Time < Right.Fire_Time);

   function Agent_To_Handle (Timer : in Oak_Timer_Id)
                             return Oak_Agent_Id is
      (Element (Pool, Timer).Agent_To_Handle);

   function Has_Timer_Fired (Timer : in Oak_Timer_Id) return Boolean is
      (Element (Pool, Timer).Fire_Time <= Clock);

   function Is_Active (Timer : in Oak_Timer_Id) return Boolean is
     (In_Tree (Pool, Timer));

   function Is_Valid (Timer : in Oak_Timer_Id) return Boolean is
      (Has_Element (Pool, Timer));

   function Firing_Time (Timer : in Oak_Timer) return Oak_Time.Time is
     (Timer.Fire_Time);

   function Firing_Time (Timer : in Oak_Timer_Id) return Oak_Time.Time is
     (Element (Pool, Timer).Fire_Time);

   function Handler (Timer : in Oak_Timer_Id)
                     return Ada.Cyclic_Tasks.Response_Handler is
     (Element (Pool, Timer).Event_Handler);

   function Scheduler_Agent (Timer : in Oak_Timer_Id)
                             return Scheduler_Id is
      (Element (Pool, Timer).Scheduler);

   function Timer_Action (Timer : in Oak_Timer_Id)
                          return Ada.Cyclic_Tasks.Event_Response is
      (Element (Pool, Timer).Timer_Action);

   function Timer_Kind (Timer : in Oak_Timer_Id) return Oak_Timer_Kind is
     (Element (Pool, Timer).Kind);

   function Timer_Priority (Timer : in Oak_Timer) return Oak_Priority is
     (Timer.Priority);

   function Timer_Priority (Timer : in Oak_Timer_Id) return Oak_Priority is
     (Element (Pool, Timer).Priority);

end Oak.Timers;
