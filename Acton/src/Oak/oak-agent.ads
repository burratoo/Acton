with Oak.Message; use Oak.Message;
with Oak.States;  use Oak.States;

with Oak.Core_Support_Package;
with Oak.Oak_Time;
with System;
with Oak.Memory.Call_Stack;         use Oak.Memory.Call_Stack;
with System.Storage_Elements;
--  with Ada.Finalization;

package Oak.Agent with Preelaborate is

   type Task_Id is range 0 .. Oak.Core_Support_Package.Max_Tasks;
   subtype Task_Name is String
     (1 .. Core_Support_Package.Max_Task_Name_Length);

--     type Memory_Region;
--     type Memory_Region_Link is access all Memory_Region;
--     type Memory_Permission is (Read_Only, Read_Write);
--
--     type Memory_Region is record
--        Location : Memory_Slice;
--        Next     : Memory_Region_Link;
--        Previous : Memory_Region_Link;
--     end record;

   type Oak_Agent is tagged limited private with Preelaborable_Initialization;

   function Agent_Id (Agent : in Oak_Agent'Class) return Task_Id;
   function Name (Agent : in Oak_Agent'Class) return Task_Name;
   function Stack_Pointer
     (Agent : in Oak_Agent'Class)
      return System.Address with Inline_Always;

   procedure Initialise_Agent
     (Agent      : access Oak_Agent'Class;
      Name       : in String);

   procedure Initialise_Agent
     (Agent      : access Oak_Agent'Class;
      Name       : in String;
      Call_Stack : in Call_Stack_Handler);

   procedure Initialise_Agent
     (Agent           : access Oak_Agent'Class;
      Name            : in String;
      Call_Stack_Size : in System.Storage_Elements.Storage_Count);

   function Agent_Message
     (For_Agent : in Oak_Agent'Class)
      return Oak_Message;

   function Agent_Yield_Status
     (For_Agent : in Oak_Agent'Class)
      return Yielded_State;

   procedure Set_Agent_Message
     (For_Agent : in out Oak_Agent'Class;
      Message   : in     Oak_Message) with Inline_Always;

   procedure Set_Agent_Yield_Status
     (For_Agent : in out Oak_Agent'Class;
      Yielded   : in     Yielded_State) with Inline_Always;

   procedure Set_Stack_Pointer
     (Agent         : in out Oak_Agent'Class;
      Stack_Pointer : in System.Address)
     with Inline_Always;

   procedure Set_State
     (A     : in out Oak_Agent'Class;
      State : in     Agent_State);

   function State (A : in Oak_Agent'Class) return Agent_State;

   procedure New_Execution_Cycle (Agent : in out Oak_Agent'Class);
   procedure Charge_Execution_Time
     (To_Agent  : in out Oak_Agent;
      Exec_Time : in Oak_Time.Time_Span);

private

   type Oak_Agent is tagged limited record
      Id          : Task_Id;
      Name        : Task_Name;
      Name_Length : Natural;

      ----
      --  This gives us a pointer to the starting location of the Stack (is
      --  this useful?) and the size of the stack.
      --  We also need to store the stack pointer. Its is probably useful to
      --  store the stack pointer in its own variable in the OTCR. But why?
      --  usually it is stored in a register anyway.
      -----
      Call_Stack : Call_Stack_Handler;

      State             : Agent_State;
      Message_Location  : Oak_Message_Location;

      Total_Execution_Time   : Oak_Time.Time_Span;
      Max_Execution_Time     : Oak_Time.Time_Span;
      Current_Execution_Time : Oak_Time.Time_Span;
      Execution_Cycles       : Natural;

      --  Memory_List : Memory_Region_Link := null;
   end record;

   function Agent_Message
     (For_Agent : in Oak_Agent'Class)
      return Oak_Message is (For_Agent.Message_Location.Message);

   function Agent_Id (Agent : in Oak_Agent'Class) return Task_Id is
     (Agent.Id);

   function Agent_Yield_Status
     (For_Agent : in Oak_Agent'Class)
      return Yielded_State is (For_Agent.Message_Location.Yield_Status);

   function Name (Agent : in Oak_Agent'Class) return Task_Name is
     (Agent.Name);

   function Stack_Pointer
     (Agent : in Oak_Agent'Class)
      return System.Address is (Agent.Call_Stack.Pointer);

   function State
     (A : in Oak_Agent'Class)
      return Agent_State is (A.State);
end Oak.Agent;
