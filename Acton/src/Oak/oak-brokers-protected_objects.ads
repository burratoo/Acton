------------------------------------------------------------------------------
--                                                                          --
--                              OAK COMPONENTS                              --
--                                                                          --
--                       OAK.BROKERS.PROTECTED_OBJECTS                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                 Copyright (C) 2011-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

--  This package provides Oak's Protected Agents, the agent that represents
--  protected objects in Oak. The Agent extends the Oak Agent data structure to
--  include protected object specific components including entries.

with Ada.Cyclic_Tasks;

with Oak.Agent.Storage;
with Oak.Indices; use Oak.Indices;
with Oak.Message; use Oak.Message;
with Oak.States;  use Oak.States;

with Oak.Agent;
with Oak.Agent.Oak_Agent; use Oak.Agent.Oak_Agent;

with System; use System;

package Oak.Brokers.Protected_Objects with Preelaborate is

   -----------------
   -- Subprograms --
   -----------------

   function Active_Subprogram_Kind
     (PO : in Protected_Id)
      return Protected_Subprogram_Type;
   --  Identifies the kind of subprogram currently executing inside the
   --  protected object.

   procedure Add_Contending_Task
     (PO : in Protected_Id;
      T  : in Oak.Agent.Task_Id) with Inline;
   --  Add a task to the contending task list.

   procedure Add_Task_To_Entry_Queue
     (PO       : in Protected_Id;
      T        : in Oak.Agent.Task_Id;
      Entry_Id : Entry_Index) with Inline;
   --  Add a task to the entry queue identified by the entry id.

   procedure Add_Task_To_Protected_Object
     (PO : in Protected_Id;
      T  : in Oak.Agent.Task_Id);
   --  Add a task to the task within list.

   function Ceiling_Priority
     (PO : in Protected_Id) return System.Any_Priority;
   --  Retrieves the priority assigned to the agent.

   function Entry_Queue_Length
     (PO       : in Protected_Id;
      Entry_Id : in Entry_Index)
      return Natural;
   --  Returns the number of agents currently queued on an entry queue.

   procedure Find_Open_Entry
     (Protected_Object : in  Protected_Id;
      Open_Entry       : out Entry_Index;
      Exception_Raised : out Boolean;
      Preference       : in  Entry_Index := No_Entry);
   --  Finds an open entry for the protected object and returns the id of the
   --  first open entry with task's it finds, otherwise it returns No_Entry is
   --  no open entry with tasks are found. If a prefered entry is provided, it
   --  will it will be checked first and the function will return with that
   --  index as long as the entry is open.

   procedure Get_And_Remove_Next_Contending_Task
     (PO        : in Protected_Id;
      Next_Task : out Oak.Agent.Task_Id_With_No) with Inline;
   --  Remove and return the first task on the contending task list.

   procedure Get_And_Remove_Next_Task_From_Entry_Queue
     (PO        : in Protected_Id;
      Entry_Id  : in Entry_Index;
      Next_Task : out Oak.Agent.Task_Id)
     with Pre => Entry_Queue_Length (PO, Entry_Id) > 0;
   --  Remove and return a task from the specified entry queue.

   function Has_Entries
     (PO : in Protected_Id)
      return Boolean;
   --  Does the protected object have entries.

   function Is_Entry_Id_Valid
     (PO       : in Protected_Id;
      Entry_Id : in Entry_Index)
      return Boolean;
   --  Returns true if the entry id is valid.

   function Is_Task_Inside_Protect_Object
     (PO : in Protected_Id;
      T  : in Oak.Agent.Task_Id)
      return Boolean with Inline;
   --  Returns whether or not the task is inside the protected object.

   procedure New_Protected_Broker
     (Broker                : out Protected_Id;
      Name                  : in String;
      Ceiling_Priority      : in Integer;
      Barriers_Function     : in Address;
      Object_Record_Address : in Address);
   --  Creates a new Protected Broker with the given prameters. Allocates the
   --  storage for the Protected Broker data structure and any dependents.

   procedure Set_Next_Broker (PO : Protected_Id; Next : Protected_Id_With_No)
     with Inline;
   function Next_Broker (PO : Protected_Id) return Protected_Id_With_No;

   function Name (PO : in Protected_Id) return Agent_Name;
   --  Fetches the name of the Agent.

   function State (PO : in Protected_Id) return Agent_State
     with Inline;
   --  Fetches the state of the Agent.

   procedure Purge_Entry_Queues
     (PO             : in Protected_Id;
      New_Task_State : in Agent_State);
   --  Removes all tasks from the protected object's entry queues.

   procedure Remove_Task_From_Entry_Queue
     (PO       : in Protected_Id;
      T        : in Oak.Agent.Task_Id);
   --  Remove the specified task from the entry queues.

   procedure Remove_Task_From_Within_Protected_Object
     (PO : in Protected_Id;
      T  : in Oak.Agent.Task_Id)
     with Pre => Is_Task_Inside_Protect_Object (PO, T);
   --  Remove the specified task from the protected object.

   procedure Set_State
     (PO    : in Protected_Id;
      State : in Agent_State) with Inline;
   --  Set the state of the agent.

   function Task_Within
     (PO : in Protected_Id)
      return Oak.Agent.Task_Id_With_No;
   --  Returns the first task inside the protected object.

   -----------------------------------------------
   --  Non Spark compliant types and subprogams --
   -----------------------------------------------

   --  TODO: these may have to be moved somewhere better?

   type Parameterless_Access is access protected procedure;
   --  A general parameterless access type for use in the below functions

   function Protected_Object_From_Access
     (Handler : Parameterless_Access)
      return Protected_Id;

   function Protected_Object_From_Access
     (Handler : Ada.Cyclic_Tasks.Response_Handler)
      return Protected_Id;
   --  Retrieves the protected object associated with a protected access
   --  pointer.

private

   -------------------
   -- Private Types --
   -------------------

   type Protected_Broker_Record is record

      Name                   : Agent_Name;
      --  The name of the Agent. Allows users and debugger to query the name of
      --  the task to figure out who it is.

      Name_Length            : Agent_Name_Length;
      --  Specifies the actual length of the name. Required since Task_Name is
      --  fixed string which may be (much) longer than the name actually is.
      --  Allows for a smaller string to be returned without the blank space at
      --  the end or dealing with the hell that end of string tokens are.

      Ceiling_Priority       : Any_Priority;
      --  The priority of the agent under normal conditions.

      Next_Object            : Protected_Id_With_No;

      State                  : Agent_State;
      --  The state of the agent.

      --  Protected Object Properties

      Object_Record          : Address;
      --  Address of the record that holds the protected object's contents.

      Entry_Barriers         : Address;
      --  The address of the function that determines the barrier state of a
      --  given entry.

      Entry_Queues           : Oak.Agent.Task_Id_With_No;
      --  Denotes the head of the two-demensional entry queue for the protected
      --  object. Tasks are sorted by first by entry then FIFO.

      Active_Subprogram_Kind : Protected_Subprogram_Type;
      --  The type of subprogram currently operating inside the object.

      Contending_Tasks       : Oak.Agent.Agent_List;
      --  List of tasks that wanting to enter the protected object.

      Tasks_Within           : Oak.Agent.Agent_List;
      --  List of tasks currently executing inside the the object.

   end record;

   ------------------------------
   -- Protected Broker Storage --
   ------------------------------

   --  The agent generic pool is also suitable for brokers

   package Protected_Pool is new Oak.Agent.Storage
     (Agent_Record_Type => Protected_Broker_Record,
      Agent_Id_Type     => Protected_Id);

   use Protected_Pool;

   --------------------------
   -- Function Expressions --
   --------------------------

   function Active_Subprogram_Kind
     (PO : in Protected_Id)
      return Protected_Subprogram_Type is
     (Agent_Pool (PO).Active_Subprogram_Kind);

   function Ceiling_Priority
     (PO : in Protected_Id) return System.Any_Priority is
     (Agent_Pool (PO).Ceiling_Priority);

   function Has_Entries
     (PO : in Protected_Id)
      return Boolean is (Agent_Pool (PO).Entry_Barriers /= Null_Address);

   function Is_Entry_Id_Valid
     (PO       : in Protected_Id;
      Entry_Id : in Entry_Index)
      return Boolean is
     (Entry_Id > No_Entry);

   function Next_Broker (PO : Protected_Id) return Protected_Id_With_No is
      (Agent_Pool (PO).Next_Object);

   function Name (PO : in Protected_Id) return Agent_Name is
     (Agent_Pool (PO).Name
      (Agent_Name_Length'First .. Agent_Pool (PO).Name_Length));

   function State (PO : in Protected_Id) return Agent_State is
     (Agent_Pool (PO).State);

   function Task_Within
     (PO : in Protected_Id)
      return Oak.Agent.Task_Id_With_No is (Agent_Pool (PO).Tasks_Within.Head);

end Oak.Brokers.Protected_Objects;
