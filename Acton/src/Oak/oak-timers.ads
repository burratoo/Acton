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

   type Event_Timer_Data is private;
   --  Type used to house data for an Action Timer.

   type Scheduler_Timer_Action is (Service, End_Cycle);
   --  The type to use to signal what action Oak should do in response to the
   --  scheduler timer. Service runs the scheduler agent, while End_Cycle
   --  ends the agent's current cycle and puts the agent to sleep.

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

   function Fill_Event_Timer_Data
     (Timer_Action     : in Ada.Cyclic_Tasks.Event_Response;
      Handler_Priority : in Oak_Priority;
      Agent_To_Handle  : in Oak_Agent_Id;
      Handler          : in Ada.Cyclic_Tasks.Response_Handler := null)
      return Event_Timer_Data;
   --  Fills in an Event_Timer_Data record.

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
     (Timer       : out Oak_Timer_Id;
      Priority    : in  Oak_Priority;
      Action_Data : in  Event_Timer_Data;
      Fire_Time   : in  Oak_Time.Time := Time_Last;
      Activate    : in  Boolean := False);
   --  Creates a new Action Timer from an Action Timer record

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

   function Scheduler_Action
     (Timer : in Oak_Timer_Id)
      return Scheduler_Timer_Action
     with Pre => Timer_Kind (Timer) = Scheduler_Timer;
   --  Returns the action that the timer will perform when it fires..
   --  TIMER KIND: SCHEDULER_TIMER.

   procedure Set_Timer_Event_Data
     (Timer : in Oak_Timer_Id;
      Data  : in Event_Timer_Data);
   --  Sets the timing event data provided by the corresponding record;
   --  TIMER KIND: EVENT_TIMER.

   procedure Set_Timer_Scheduler_Action
     (Timer : in Oak_Timer_Id;
      Scheduler_Action : in Scheduler_Timer_Action)
     with Pre => Timer_Kind (Timer) = Scheduler_Timer;
   --  Sets the scheduler action of the timer.

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

   type Event_Timer_Data is record
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

      Timer_Action     : Ada.Cyclic_Tasks.Event_Response;
      --  The action the timer will perform when it fires.

      Handler_Priority : Oak_Priority;
      --  The priority that the handler executes at. Restricted to the
      --  interrupt priority range because these are they only priorities that
      --  have an interrupt agent avaliable to run the handler.

      Agent_To_Handle  : Oak_Agent_Id;
      --  The Agent associated with this timer.

      Handler          : Ada.Cyclic_Tasks.Response_Handler;
      --  If the action of the timer is to call a handler it will call this
      --  one.

   end record;

   type Oak_Timer (Kind : Oak_Timer_Kind := Event_Timer) is record
   --  Oak Timer is a variant record to support the three different types of
   --  Oak Timers. The sizes are fairly similar, with the smallest - Scheduler
   --  Timer – used less frequently that the other two so not much space is
   --  wasted.

      Fire_Time : Time;
      --  The time the timer will fire.

      Priority  : Oak_Priority;
      --  The priority of the scheduler timer.

      --   While all timers have a priority attach to them
      case Kind is
         when Empty_Timer =>
            null;

         when Scheduler_Timer =>
            Scheduler : Scheduler_Id;
            --  The scheduler that will run when the timer fires.

            Scheduler_Action : Scheduler_Timer_Action;
            --  The action Oak will take when the scheduler timer fires.

         when Event_Timer =>
            Data      : Event_Timer_Data;
            --  The data associated with the Action Timer. See the type
            --  definition above for more details.
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
      (Element (Pool, Timer).Data.Agent_To_Handle);

   function Has_Timer_Fired (Timer : in Oak_Timer_Id) return Boolean is
      (Is_Active (Timer) and Element (Pool, Timer).Fire_Time <= Clock);

   function Is_Active (Timer : in Oak_Timer_Id) return Boolean is
     (In_Tree (Pool, Timer));

   function Is_Valid (Timer : in Oak_Timer_Id) return Boolean is
      (Has_Element (Pool, Timer));

   function Fill_Event_Timer_Data
     (Timer_Action     : in Ada.Cyclic_Tasks.Event_Response;
      Handler_Priority : in Oak_Priority;
      Agent_To_Handle  : in Oak_Agent_Id;
      Handler          : in Ada.Cyclic_Tasks.Response_Handler := null)
      return Event_Timer_Data is
     ((Timer_Action     => Timer_Action,
       Handler_Priority => Handler_Priority,
       Agent_To_Handle  => Agent_To_Handle,
       Handler          => Handler));

   function Firing_Time (Timer : in Oak_Timer) return Oak_Time.Time is
     (Timer.Fire_Time);

   function Firing_Time (Timer : in Oak_Timer_Id) return Oak_Time.Time is
     (Element (Pool, Timer).Fire_Time);

   function Handler (Timer : in Oak_Timer_Id)
                     return Ada.Cyclic_Tasks.Response_Handler is
     (Element (Pool, Timer).Data.Handler);

   function Scheduler_Agent (Timer : in Oak_Timer_Id)
                             return Scheduler_Id is
      (Element (Pool, Timer).Scheduler);

   function Scheduler_Action
     (Timer : in Oak_Timer_Id)
      return Scheduler_Timer_Action is
     (Element (Pool, Timer).Scheduler_Action);

   function Timer_Action (Timer : in Oak_Timer_Id)
                          return Ada.Cyclic_Tasks.Event_Response is
      (Element (Pool, Timer).Data.Timer_Action);

   function Timer_Kind (Timer : in Oak_Timer_Id) return Oak_Timer_Kind is
     (Element (Pool, Timer).Kind);

   function Timer_Priority (Timer : in Oak_Timer) return Oak_Priority is
     (Timer.Priority);

   function Timer_Priority (Timer : in Oak_Timer_Id) return Oak_Priority is
     (Element (Pool, Timer).Priority);

end Oak.Timers;
