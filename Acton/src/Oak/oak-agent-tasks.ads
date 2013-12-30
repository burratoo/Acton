------------------------------------------------------------------------------
--                                                                          --
--                              OAK COMPONENTS                              --
--                                                                          --
--                           OAK.AGENT.TASK_AGENT                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                 Copyright (C) 2010-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

--  This package defines Oak's Task Agents, the agent that represents tasks in
--  Oak. It functions a a task's control block. The Agent extends the Oak Agent
--  data structure to include task specific components mainly focusing on
--  cyclic related components and task activation.

with Ada.Cyclic_Tasks;
with Oak.Timers;
with Oak.Oak_Time; use Oak.Oak_Time;

with Oakland.Tasks;

with System;                  use System;
with System.Storage_Elements; use System.Storage_Elements;

package Oak.Agent.Tasks with Preelaborate is

   type Deadline_Base is (Wake_Up_Time, Clock_Time);
   --  Specifies how Oak is to set a cyclic task's next deadline: either from
   --  the task's wake up time or from now.

   Unspecified_Priority : constant Integer := -1;
   --  Allows the task creator not to have to specify the task's priority.
   --  Instead a default priority will be applied.

   procedure New_Task_Agent
     (Agent             : out Task_Id;
      Stack_Address     : in Address;
      Stack_Size        : in Storage_Count;
      Name              : in String;
      Run_Loop          : in Address;
      Task_Value_Record : in Address;
      Normal_Priority   : in Integer;
      Cycle_Behaviour   : in Ada.Cyclic_Tasks.Behaviour;
      Cycle_Period      : in Oak_Time.Time_Span;
      Phase             : in Oak_Time.Time_Span;
      Execution_Budget  : in Oak_Time.Time_Span;
      Budget_Action     : in Ada.Cyclic_Tasks.Event_Response;
      Budget_Handler    : in Ada.Cyclic_Tasks.Response_Handler;
      Relative_Deadline : in Oak_Time.Time_Span;
      Deadline_Action   : in Ada.Cyclic_Tasks.Event_Response;
      Deadline_Handler  : in Ada.Cyclic_Tasks.Response_Handler;
      Scheduler_Agent   : in Scheduler_Id;
      Chain             : in out Task_List;
      Elaborated        : in Address);
   --  Creates a new Task Agent with the given prameters. Allocates the storage
   --  for the Task Agent data structure and any dependents.

   function Budget_Timer (T : not null access Task_Agent'Class)
                          return access Timers.Action_Timer'Class;

   function Cycle_Period
     (T : in Task_Agent'Class)
      return Oak_Time.Time_Span;

   overriding function Destination_On_Wake_Up (Agent : in out Task_Agent)
                                    return Wake_Destination;
   --  Returns whether the tasks that has woken up is sent to its run queue
   --  or is removed from the scheduler. Function updates the current
   --  state of the state.

   function Execution_Budget
     (T : in Task_Agent'Class) return Oak_Time.Time_Span;

   function Is_Elaborated (T : in Task_Agent'Class) return Boolean;

   function Next_Run_Time (T : in Task_Agent'Class) return Oak_Time.Time;

   function Phase (T : in Task_Agent'Class) return Oak_Time.Time_Span;

   function Remaining_Budget
     (T : in Task_Agent'Class) return Oak_Time.Time_Span;

   procedure Set_Activation_List
     (T   : in out Task_Agent'Class;
      Add : access Task_Agent'Class);

   procedure Set_Activation_List
     (T     : in out Task_Agent'Class;
      Chain : in Activation_Chain_Access);

   procedure Set_Cycle_Period
     (T  : in out Task_Agent'Class;
      CP : in Oak_Time.Time_Span);

   procedure Set_Next_Deadline_For_Task
     (T     : in out Task_Agent'Class;
      Using : in Deadline_Base);

   procedure Set_Relative_Deadline
     (T  : in out Task_Agent'Class;
      RD : in Oak_Time.Time_Span);

   procedure Update_Task_Property
     (T                  : in out Task_Agent'Class;
      Property_To_Update : in Task_Property;
      Next_Task_To_Run   : out Agent_Handler);

private
   type Task_Agent_Record is record

   --  Cylic Task Properties

      Cycle_Behaviour   : Ada.Cyclic_Tasks.Behaviour;
      --  The type of cyclic behaviour the task possesses. Takes the value of
      --  Normal, Periodic, Sporadic and Aperiodic.

      Cycle_Period      : Oak_Time.Time_Span;
      --  The cycle period of a cyclic task or the minimum inter-release period
      --  for a sporadic task.

      Phase             : Oak_Time.Time_Span;
      --  The phase of the cyclic task relative to the global start time.

      Execution_Budget  : Oak_Time.Time_Span;
      --  The amount of time alloted to the task for which it is allowed to
      --  execute on the processor per cycle.

      Relative_Deadline : Oak_Time.Time_Span;
      --  The wall time that the task has to complete each cycle.

      Deadline_Timer    : Oak_Timer_Id;
      --  The id of the timer used to enforce the task's deadline.

      Execution_Timer   : ?;
      --  The id of the timer used to enforce the task's execution budget.

      Next_Run_Cycle    : Oak_Time.Time;
      --  The time of the next run cycle is meant to commence for periodic
      --  tasks or the earliest time a sporadic task can commence its next
      --  cycle.

      Event_Raised      : Boolean;
      --  A flag to indicate if an event has occured and thus allowing a
      --  sporadic or aperiodic task to commence its next cycle.

      --  Activation Properties

      Elaborated        : Oakland.Tasks.Elaboration_Boolean;
      --  The elaborated boolean used to indicate if the associated task body
      --  has been elaborated. We do not do anything with the boolean in Oak,
      --  only hang on to it for the activation subprogam in Oakland which
      --  needs it stored somewhere.
   end record;

   function Activation_List
     (T    : in Task_Agent'Class)
      return access Task_Agent'Class is (T.Activation_List);

   function Cycle_Period
     (T : in Task_Agent'Class)
      return Oak_Time.Time_Span is (T.Cycle_Period);

   function Execution_Budget
     (T : in Task_Agent'Class)
      return Oak_Time.Time_Span is (T.Execution_Budget);

   function Is_Elaborated
     (T : in Task_Agent'Class)
      return Boolean is (T.Elaborated.all);

   function Budget_Timer (T : not null access Task_Agent'Class)
                          return access Timers.Action_Timer'Class
     is (T.Execution_Timer'Access);

   function Next_Run_Time
     (T : in Task_Agent'Class)
      return Oak_Time.Time is (T.Next_Run_Cycle);

   function Phase
     (T : in Task_Agent'Class)
      return Oak_Time.Time_Span is (T.Phase);

   function Remaining_Budget (T : in Task_Agent'Class)
     return Oak_Time.Time_Span is (T.Remaining_Budget);
end Oak.Agent.Tasks;
