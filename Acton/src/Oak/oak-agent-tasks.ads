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
--  Oak. It functions with an Oak Agent as the task's control block. The Agent
--  extends the Oak Agent data structure to include task specific components
--  mainly focusing on cyclic related components and task activation.

with Ada.Cyclic_Tasks;

with Oak.Agent.Storage;
with Oak.Indices;  use Oak.Indices;
with Oak.Message;  use Oak.Message;
with Oak.Oak_Time; use Oak.Oak_Time;
with Oak.Timers;   use Oak.Timers;

with System;                  use System;
with System.Storage_Elements; use System.Storage_Elements;

package Oak.Agent.Tasks with Preelaborate is

   -----------
   -- Types --
   -----------

   type Deadline_Base is (Wake_Up_Time, Clock_Time);
   --  Specifies how Oak is to set a cyclic task's next deadline: either from
   --  the task's wake up time or from now.

   Unspecified_Priority : constant Integer := -1;
   --  Allows the task creator not to have to specify the task's priority.
   --  Instead a default priority will be applied.

   -----------------
   -- Subprograms --
   -----------------

   function Budget_Action (T : in Task_Id) return Event_Timer_Data
     with Pre => Has_Task (T);
   --  Return the action details for an execution budget exhaustion event.

   function Cycle_Period (T : in Task_Id) return Oak_Time.Time_Span
     with Pre => Has_Task (T);
   --  Return the cycle period or minimum inter-release time for the task.

   function Execution_Budget (T : in Task_Id) return Oak_Time.Time_Span
     with Pre => Has_Task (T);
   --  Return the execution budget assigned to the task.

   function Elaborated_Boolean (T : in Task_Id) return Address
     with Pre => Has_Task (T), Inline;
   --  Return the elaboration boolean assigned to the task.

   function Id_Of_Entry (For_Task : Task_Id) return Entry_Index;
   --  Returns the entry id of a task that is either inside an entry or queued
   --  on one.

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
      Budget_Response   : in Ada.Cyclic_Tasks.Event_Response;
      Budget_Handler    : in Ada.Cyclic_Tasks.Response_Handler;
      Relative_Deadline : in Oak_Time.Time_Span;
      Deadline_Response : in Ada.Cyclic_Tasks.Event_Response;
      Deadline_Handler  : in Ada.Cyclic_Tasks.Response_Handler;
      Scheduler_Agent   : in Scheduler_Id_With_No := No_Agent;
      Chain             : in out Task_List;
      Elaborated        : in Address := Null_Address);
   --  Creates a new Task Agent with the given prameters. Allocates the storage
   --  for the Task Agent data structure and any dependents.

   function Next_Run_Cycle (T : in Task_Id) return Oak_Time.Time
     with Pre => Has_Task (T), Inline;
   --  The time that the next cycle of the task is due or allowed to commence.

   function Next_Queue (For_Task : in Task_Id) return Task_Id_With_No;
   --  Returns the next entry queue denoted by the Next_Queue link.

   function Phase (T : in Task_Id) return Oak_Time.Time_Span
     with Pre => Has_Task (T), Inline;
   --  Return the phase of the task.

   function Protected_Subprogram_Kind
     (For_Task : in Task_Id)
      return Protected_Subprogram_Type;
   --  Return the protected subprogram kind that the task wishes to acccess.

   function Protected_Agent_To_Access
     (For_Task : in Task_Id)
      return Protected_Id;
   --  Return the protected agent the the agent wishes to enter or is inside.

   procedure Set_Cycle_Period
     (For_Task     : in Task_Id;
      Cycle_Period : in Oak_Time.Time_Span)
     with Pre => Has_Task (For_Task);
   --  Set the cycle period or minimum release time of the task.

   procedure Set_Id_Of_Entry
     (For_Task : Task_Id;
      Entry_Id : Entry_Index);
   --  Set the id of the entry the task is either queued on or is in.

   procedure Set_Next_Deadline_For_Task
     (For_Task : in Task_Id;
      Using    : in Deadline_Base)
     with Pre => Has_Task (For_Task);
   --  Set the next deadline for the Task. Activates the timer if the new
   --  deadline is less than Time_Last.

   procedure Set_Next_Queue
     (For_Task   : Task_Id;
      Next_Queue : Task_Id_With_No);
   --  Set the task that represents the next queue.

   procedure Set_Protected_Entry_Request
     (For_Task         : Task_Id;
      Protected_Object : Protected_Id;
      Subprogram_Kind  : Protected_Subprogram_Type;
      Entry_Id         : Entry_Index);
   --  Set the corresponding components of the task agent to store the entry
   --  request.

   procedure Set_Relative_Deadline
     (For_Task          : in Task_Id;
      Relative_Deadline : in Oak_Time.Time_Span)
     with Pre => Has_Task (For_Task);
   --  Set the relative deadline of the task.

   procedure Update_Task_Property
     (For_Task           : in Task_Id;
      Property_To_Update : in Task_Property)
     with Pre => Has_Task (For_Task);
   --  Update a properity of the task.

   ---------------------
   -- Ghost Functions --
   ---------------------

   function Has_Task (T : Task_Id) return Boolean with Convention => Ghost;
   --  Does the storage have a valid task associated with the id.

private
   type Task_Agent_Record is record
   --  Note the ordering of this record is to try an achieve optimal record
   --  packing (i.e. placing the flags together so they can occupy a single
   --  byte).

   --  Cylic Task Properties

      Cycle_Period      : Oak_Time.Time_Span;
      --  The cycle period of a cyclic task or the minimum inter-release period
      --  for a sporadic task.

      Phase             : Oak_Time.Time_Span;
      --  The phase of the cyclic task relative to the global start time.

      Next_Run_Cycle    : Oak_Time.Time;
      --  The time of the next run cycle is meant to commence for periodic
      --  tasks or the earliest time a sporadic task can commence its next
      --  cycle.

      Execution_Budget  : Oak_Time.Time_Span;
      --  The amount of time alloted to the task for which it is allowed to
      --  execute on the processor per cycle.

      Relative_Deadline : Oak_Time.Time_Span;
      --  The wall time that the task has to complete each cycle.

      Budget_Action     : Event_Timer_Data;
      --  The data associated with the task's execution budget exhaustion
      --  action.

      Deadline_Timer    : Oak_Timer_Id;
      --  The id of the timer used to enforce the task's deadline.

      Cycle_Behaviour   : Ada.Cyclic_Tasks.Behaviour;
      --  The type of cyclic behaviour the task possesses. Takes the value of
      --  Normal, Periodic, Sporadic and Aperiodic.

      Event_Raised      : Boolean;
      --  A flag to indicate if an event has occured and thus allowing a
      --  sporadic or aperiodic task to commence its next cycle.

      --  Protected Object Properties

      Subprogram_Kind   : Protected_Subprogram_Type;
      --  The type of protected subprogram that the task will execute inside
      --  a protected object.

      Protected_Object  : Protected_Id;
      --  The protected object that the task wishes to enter.

      Id_Of_Entry       : Entry_Index;
      --  The entry that the task is either in, is queued to enter or wishes to
      --  enter.

      Next_Queue        : Oak_Agent_Id;
      --  Points to the next entry queue.

      --  Activation Properties

      Elaborated        : Address;
      --  The elaborated boolean used to indicate if the associated task body
      --  has been elaborated. We do not do anything with the boolean in Oak,
      --  only hang on to it for the activation subprogam in Oakland which
      --  needs it stored somewhere.

   end record;

   ------------------------
   -- Task Agent Storage --
   ------------------------

   package Task_Pool is new Oak.Agent.Storage
     (Agent_Record_Type => Task_Agent_Record,
      Agent_Id_Type     => Task_Id);

   use Task_Pool;

   --------------------------
   -- Function Expressions --
   --------------------------

   function Budget_Action (T : in Task_Id) return Event_Timer_Data is
      (Agent_Pool (T).Budget_Action);

   function Cycle_Period (T : in Task_Id) return Oak_Time.Time_Span is
      (Agent_Pool (T).Cycle_Period);

   function Execution_Budget (T : in Task_Id) return Oak_Time.Time_Span is
     (Agent_Pool (T).Execution_Budget);

   function Elaborated_Boolean (T : in Task_Id) return Address is
     (Agent_Pool (T).Elaborated);

   function Has_Task (T : Task_Id) return Boolean is
     (Has_Agent (T));

   function Id_Of_Entry (For_Task : Task_Id) return Entry_Index is
      (Agent_Pool (For_Task).Id_Of_Entry);

   function Next_Run_Cycle (T : in Task_Id) return Oak_Time.Time is
     (Agent_Pool (T).Next_Run_Cycle);

   function Next_Queue (For_Task : in Task_Id) return Task_Id_With_No is
     (Agent_Pool (For_Task).Next_Queue);

   function Phase (T : in Task_Id) return Oak_Time.Time_Span is
     (Agent_Pool (T).Phase);

   function Protected_Subprogram_Kind
     (For_Task : in Task_Id)
      return Protected_Subprogram_Type is
     (Agent_Pool (For_Task).Subprogram_Kind);

   function Protected_Agent_To_Access
     (For_Task : in Task_Id)
      return Protected_Id is
     (Agent_Pool (For_Task).Protected_Object);

end Oak.Agent.Tasks;
