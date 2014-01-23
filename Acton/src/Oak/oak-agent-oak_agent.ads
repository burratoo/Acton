------------------------------------------------------------------------------
--                                                                          --
--                              OAK COMPONENTS                              --
--                                                                          --
--                           OAK.AGENT.OAK_AGENT                            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                 Copyright (C) 2013-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

--  This package defines Oak's primative agent – Oak Agent – from which all
--  agents are derived. An Oak Agent is one of Oak's three main data
--  structures: the other two being Oak Timers and Oak Messages. Every agent in
--  Oak possess an Oak Agent record, in fact except for the sleep agent, no
--  agent can be just an Oak Agent: it must use one of the derivative agents.
--  The record itself provides the neccessary components to schedule and
--  execute Agent in Oak.

with Oak.Agent.Storage;
with Oak.Oak_Time;

with Oak.Memory.Call_Stack;   use Oak.Memory.Call_Stack;
with Oak.States;              use Oak.States;
with System;                  use System;
with System.Storage_Elements; use System.Storage_Elements;

package Oak.Agent.Oak_Agent with Preelaborate is

   subtype Agent_Name_Length is Integer range 1 ..
     Project_Support_Package.Max_Task_Name_Length;
   --  A type used to represent the length of the the Agent Name string. A new
   --  type is used for this based on the maximum length of the name to allow
   --  the compiler to pick an appropriately size variable.

   subtype Agent_Name is String (Agent_Name_Length);
   --  The name of the Agent. Uses a fixed string to make storage easier.

   type Charge_Occurrence is
     (Only_While_Running, Same_Priority, All_Priorities, Below_Priority);
   --  When to charge an Agent which is on a charge list. Priority is
   --  referenced to the Agent's priority when compared to the current priority
   --  of the Oak Instance. So for Below_Priority, an Agent will be charged
   --  when the Oak Instance priority is below the Agent's own priority.

   type Wake_Destination is (Run_Queue, Remove);
   --  A type to identify the destination of a woken task. Normally an agent is
   --  placed back on its Scheduler Agent's run queue when it wakes. The
   --  exception lies when the agent is waiting on a kernel release (used by
   --  sporadic tasks), in which case the agent is removed from the Scheduler
   --  Agent.

--     type Memory_Region;
--     type Memory_Region_Link is access all Memory_Region;
--     type Memory_Permission is (Read_Only, Read_Write);
--
--     type Memory_Region is record
--        Location : Memory_Slice;
--        Next     : Memory_Region_Link;
--        Previous : Memory_Region_Link;
--     end record;

   ------------------------
   --  Setup Subprograms --
   ------------------------

   procedure Setup_Storage
     with Pre  => not Is_Storage_Ready,
          Post => Is_Storage_Ready;
   --  Setup the storage associated with the Oak_Agents. Must be called before
   --  accessing any of the functions here, and only be called once. Sets up
   --  the sleep agent as part of the setup routine.

   function Is_Storage_Ready return Boolean
     with Convention => Ghost;

   ------------------------
   -- Access Subprograms --
   ------------------------

   function Absolute_Deadline
     (For_Agent : in Oak_Agent_Id) return Oak_Time.Time;
   --  The absolute deadline of the agent.

   procedure Charge_Execution_Time
     (To_Agent  : in Oak_Agent_Id;
      Exec_Time : in Oak_Time.Time_Span)
     with Pre => Is_Storage_Ready;
   --  Charges the specified execution time to the agent.

   procedure Charge_Execution_Time_To_List
     (List             : in Charge_List_Head;
      Exec_Time        : in Oak_Time.Time_Span;
      Current_Agent    : in Oak_Agent_Id;
      Current_Priority : in Oak_Priority);
   --  Charges the provided execution time to agents on the the specified
   --  execution time charge list. Whether an agent is charged is dependent on
   --  the priority provided by the procedure and the When_To_Charge attribute
   --  of the agent.

   function Current_Execution_Time
     (For_Agent : in Oak_Agent_Id)
      return Oak_Time.Time_Span
     with Inline;
   --  Return the execution time for the current cycle.

   procedure Delete_Agent (Agent : Oak_Agent_Id);
   --  Deletes the agent specified and deallocates its storage.

   function Earliest_Expiring_Budget
     (Charge_List : in Charge_List_Head) return Oak_Agent_Id;
   --  Returns the id of the agent that has the eariliest expiring budget.
   --  If the list is empty it will return No_Agent.

   procedure Increment_Execution_Cycle_Count
     (For_Agent : in Oak_Agent_Id;
      By        : in Natural)
     with Inline;
   --  Increments the agent's execution cycle count by the specified amount.

   function Is_Agent_Interrupted (Agent : in Oak_Agent_Id) return Boolean;
   --  Returns whether or not the agent had been interrupted.

   function Max_Execution_Time
     (For_Agent : in Oak_Agent_Id)
      return Oak_Time.Time_Span
     with Inline;
   --  Return the maximum execution time for any cycle.

   function Name (Agent : in Oak_Agent_Id) return Agent_Name
     with Pre => Has_Agent (Agent);
   --  Fetches the name of the Agent.

   procedure New_Agent
     (Agent                : in Oak_Agent_Id;
      Name                 : in String;
      Call_Stack_Address   : in Address;
      Call_Stack_Size      : in Storage_Count;
      Run_Loop             : in Address;
      Run_Loop_Parameter   : in Address;
      Normal_Priority      : in Any_Priority;
      Initial_State        : in Agent_State;
      Scheduler_Agent      : in Scheduler_Id_With_No := No_Agent;
      Wake_Time            : in Oak_Time.Time        := Oak_Time.Time_Last;
      When_To_Charge_Agent : in Charge_Occurrence    := All_Priorities)
     with Pre => Is_Storage_Ready;
   --  Allocates a new Oak Agent with the given parameters and allocates a call
   --  stack for the Agent. This procedure is only called by the New_Agent
   --  procedure of derivative Agents since this procedure makes no check to
   --  see if the underlying storage slot assigned to the Agent Id is free.

   function Next_Agent (Agent : in Oak_Agent_Id) return Oak_Agent_Id
     with Inline;
   --  Returns the agent pointed to by the Agent's Next_Agent link.

   function Normal_Priority
     (Agent : in Oak_Agent_Id) return System.Any_Priority
     with Pre => Has_Agent (Agent), Inline;
   --  Retrieves the priority assigned to the agent.

   function Remaining_Budget
     (Agent : in Oak_Agent_Id) return Oak_Time.Time_Span
     with Pre => Has_Agent (Agent), Inline;
   --  Retrieves the amount of execution budget remaining for the agent

   procedure Replenish_Execution_Budget
     (For_Agent : in Oak_Agent_Id;
      By_Amount : in Oak_Time.Time_Span)
     with Pre => Has_Agent (For_Agent);
   --  Add the time span given to the agent's execution budget.

   function Scheduler_Agent_For_Agent
     (Agent : in Oak_Agent_Id) return Scheduler_Id_With_No
     with Pre => Has_Agent (Agent), Inline;
   --  Fetches the Scheduler Agent responsible for the Agent.

   procedure Set_Agent_Interrupted
     (For_Agent : in Oak_Agent_Id;
      Value     : Boolean := True);
   --  Sets whether the agent was interrupted.

   procedure Set_Absolute_Deadline
     (For_Agent : in Oak_Agent_Id;
      Deadline  : in Oak_Time.Time)
     with Pre => Has_Agent (For_Agent);
   --  Set the absolute deadline for the task.

   procedure Set_Current_Execution_Time
     (For_Agent : in Oak_Agent_Id;
      To        : in Oak_Time.Time_Span);
   --  Sets the agent's current execution time.

   procedure Set_Max_Execution_Time
     (For_Agent : in Oak_Agent_Id;
      To        : in Oak_Time.Time_Span);
   --  Sets the agent's maximum execution time.

   procedure Set_Next_Agent
     (For_Agent  : in Oak_Agent_Id;
      Next_Agent : in Oak_Agent_Id);
   --  Set the Next_Agent link for the Agent.

   procedure Set_Remaining_Budget
     (For_Agent : in Oak_Agent_Id;
      To_Amount : in Oak_Time.Time_Span)
     with Pre => Has_Agent (For_Agent);
   --  Set the amount of execution budget remaining for the agent.

   procedure Set_Scheduler_Agent
     (For_Agent : in Oak_Agent_Id;
      Scheduler : in Scheduler_Id_With_No)
     with Pre => Has_Agent (For_Agent);
   --  Sets the Scheduler Agent responsible for scheduling the agent.

   procedure Set_Stack_Pointer
     (For_Agent     : in Oak_Agent_Id;
      Stack_Pointer : in System.Address)
     with Pre => Has_Agent (For_Agent), Inline_Always;
   --  Sets the call stack pointer for the agent. Note that this procedure
   --  should always be inlined since it is called from within the core
   --  interrupt routines.

   procedure Set_State
     (For_Agent : in Oak_Agent_Id;
      State     : in Agent_State)
     with Pre => Has_Agent (For_Agent);
   --  Set the state of the agent.

   procedure Set_Wake_Time
     (For_Agent : in Oak_Agent_Id;
      Wake_Time : in Oak_Time.Time)
     with Pre => Has_Agent (For_Agent);
   --  Set the wake time of the agent.

   function Stack_Pointer
     (Agent : in Oak_Agent_Id)
      return System.Address
     with Pre => Has_Agent (Agent), Inline_Always;
   --  Fetches the stack pointer associated with the Agent. Note that this
   --  procedure should always be inlined should always be inlined since it is
   --  called from within the core interrupt routines.

   function State (Agent : in Oak_Agent_Id) return Agent_State
     with Pre => Has_Agent (Agent), Inline;
   --  Fetches the state of the Agent.

   function Wake_Time (Agent : in Oak_Agent_Id) return Oak_Time.Time
     with Pre => Has_Agent (Agent), Inline;
   --  Fetches the time that the Agent will wake.

   function When_To_Charge (Agent : in Oak_Agent_Id) return Charge_Occurrence;
   --  Returns when the agent is to be charged when it is on a charge list.

   ---------------------
   -- Ghost Functions --
   ---------------------

   function Has_Agent (Agent : Oak_Agent_Id) return Boolean
     with Convention => Ghost;
   --  Check whether the Agent existing in the store.

private

   --------------------
   --  Private Types --
   --------------------

   type Oak_Agent_Record is record
      Name                   : Agent_Name;
      --  The name of the Agent. Allows users and debugger to query the name of
      --  the task to figure out who it is.

      Name_Length            : Agent_Name_Length;
      --  Specifies the actual length of the name. Required since Task_Name is
      --  fixed string which may be (much) longer than the name actually is.
      --  Allows for a smaller string to be returned without the blank space at
      --  the end or dealing with the hell that end of string tokens are.

      Call_Stack             : Call_Stack_Handler;
      --  The agent's call stack.

      Next_Agent             : Oak_Agent_Id;
      --  Used to allow agents to be placed on arbitary linked-lists. In Oak
      --  these are used for entry queues and charge and activation lists
      --  (noting that an agent can only be on one list at a time).

      State                  : Agent_State;
      --  The state of the agent.

      Normal_Priority        : Any_Priority;
      --  The priority of the agent under normal conditions.

      Scheduler_Agent        : Scheduler_Id_With_No;
      --  The Scheduler Agent responsible for scheduling the agent.

      Wake_Time              : Oak_Time.Time;
      --  The time that the agent is eligable to move from a sleeping state
      --  to a runnable state.

      Absolute_Deadline      : Oak_Time.Time;
      --  The time when the agent must finish or is expected to finish running.

      Total_Execution_Time   : Oak_Time.Time_Span;
      --  The total time that the agent has spent executing on the processor.

      Max_Execution_Time     : Oak_Time.Time_Span;
      --  The maximum exectuion time that the agent has been executing a cycle.
      --  This cycle is defined by the agent.

      Current_Execution_Time : Oak_Time.Time_Span;
      --  The current execution time of current cycle of the agent.

      Remaining_Budget       : Oak_Time.Time_Span;
      --  The allowed execution time remaining for the agent's current budget.

      Execution_Cycles       : Natural;
      --  The number of cycle that the agent has been through. What constitutes
      --  a cycle is agent dependent.
      --  ??? Change to a mod type?

      When_To_Charge         : Charge_Occurrence;
      --  Defines the conditions that an agent who is part of a charge list is
      --  charged for a member of the charge list executing on the processor.

      Agent_Interrupted      : Boolean;
      --  Flags whether the agent has been interrupted or not. Indicates
      --  whether a full register restor is needed when the context is switched
      --  back to the task.

   end record;

   package Oak_Agent_Pool is new Oak.Agent.Storage
     (Agent_Record_Type => Oak_Agent_Record,
      Agent_Id_Type     => Oak_Agent_Id);

   ----------------------
   -- Access functions --
   ----------------------

   use Oak_Agent_Pool;

   function Absolute_Deadline
     (For_Agent : in Oak_Agent_Id) return Oak_Time.Time is
      (Agent_Pool (For_Agent).Absolute_Deadline);

   function Current_Execution_Time
     (For_Agent : in Oak_Agent_Id)
      return Oak_Time.Time_Span is
     (Agent_Pool (For_Agent).Current_Execution_Time);

   function Has_Agent (Agent : Oak_Agent_Id) return Boolean is
     (Oak_Agent_Pool.Has_Agent (Agent));

   function Is_Agent_Interrupted (Agent : in Oak_Agent_Id) return Boolean is
     (Agent_Pool (Agent).Agent_Interrupted);

   function Is_Storage_Ready return Boolean is
     (Oak_Agent_Pool.Is_Storage_Ready);

   function Max_Execution_Time
     (For_Agent : in Oak_Agent_Id)
      return Oak_Time.Time_Span is
      (Agent_Pool (For_Agent).Max_Execution_Time);

   function Name (Agent : in Oak_Agent_Id) return Agent_Name is
     (Agent_Pool (Agent).Name
      (Agent_Name_Length'First .. Agent_Pool (Agent).Name_Length));

   function Next_Agent (Agent : in Oak_Agent_Id) return Oak_Agent_Id is
      (Agent_Pool (Agent).Next_Agent);

   function Normal_Priority
     (Agent : in Oak_Agent_Id) return System.Any_Priority is
     (Agent_Pool (Agent).Normal_Priority);

   function Remaining_Budget
     (Agent : in Oak_Agent_Id) return Oak_Time.Time_Span is
     (Agent_Pool (Agent).Remaining_Budget);

   function Scheduler_Agent_For_Agent
     (Agent : in Oak_Agent_Id) return Scheduler_Id_With_No is
     (Agent_Pool (Agent).Scheduler_Agent);

   function Stack_Pointer
     (Agent : in Oak_Agent_Id) return System.Address is
     (Agent_Pool (Agent).Call_Stack.Pointer);

   function State (Agent : in Oak_Agent_Id) return Agent_State is
     (Agent_Pool (Agent).State);

   function Wake_Time (Agent : in Oak_Agent_Id) return Oak_Time.Time is
     (Agent_Pool (Agent).Wake_Time);

   function When_To_Charge
     (Agent : in Oak_Agent_Id)
      return Charge_Occurrence is
      (Agent_Pool (Agent).When_To_Charge);

end Oak.Agent.Oak_Agent;
