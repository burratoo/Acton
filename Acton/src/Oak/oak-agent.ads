with Oak.Message; use Oak.Message;
with Oak.States;  use Oak.States;
with System;      use System;

with Oak.Core_Support_Package;
with Oak.Oak_Time;
with Oak.Memory.Call_Stack;   use Oak.Memory.Call_Stack;
with System.Storage_Elements; use System.Storage_Elements;

limited with Oak.Agent.Schedulers;

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

   type Agent_Handler is access all Oak_Agent'Class;

   type Wake_Destination is (Run_Queue, Remove);

   function Agent_Id (Agent : in Oak_Agent'Class) return Task_Id;
   function Name (Agent : in Oak_Agent'Class) return Task_Name;
   function Stack_Pointer
     (Agent : in Oak_Agent'Class)
      return System.Address with Inline_Always;

   procedure Initialise_Agent
     (Agent      : not null access Oak_Agent'Class;
      Name       : in String);

   procedure Initialise_Agent
     (Agent      : not null access Oak_Agent'Class;
      Name       : in String;
      Call_Stack : in Call_Stack_Handler);

   procedure Initialise_Agent
     (Agent           : not null access Oak_Agent'Class;
      Name            : in String;
      Call_Stack_Size : in System.Storage_Elements.Storage_Count);

   procedure Initialise_Agent
     (Agent              : not null access Oak_Agent'Class;
      Name               : in String;
      Call_Stack_Address : in Address;
      Call_Stack_Size    : in Storage_Count;
      Run_Loop           : in Address;
      Run_Loop_Parameter : in Address;
      Normal_Priority    : in Integer;
      Initial_State      : in Agent_State);

   function Agent_Message
     (For_Agent : in Oak_Agent'Class)
      return Oak_Message;

   function Agent_Yield_Status
     (For_Agent : in Oak_Agent'Class)
      return Yielded_State;

   function Destination_On_Wake_Up (Agent : in out Oak_Agent)
                                    return Wake_Destination;
   --  Returns whether the tasks that has woken up is sent to its run queue
   --  or is removed from the scheduler. Function updates the current
   --  state of the state.

   function Normal_Priority
     (Agent : in Oak_Agent'Class)
      return System.Any_Priority;

   function Scheduler_Agent_For_Agent
     (Agent : in Oak_Agent'Class)
      return access Schedulers.Scheduler_Agent'Class;

   procedure Set_Agent_Message
     (For_Agent : in out Oak_Agent'Class;
      Message   : in     Oak_Message) with Inline_Always;

   procedure Set_Agent_Yield_Status
     (For_Agent : in out Oak_Agent'Class;
      Yielded   : in     Yielded_State) with Inline_Always;

   procedure Set_Scheduler_Agent
     (Agent     : in out Oak_Agent'Class;
      Scheduler : access Schedulers.Scheduler_Agent'Class);

   procedure Set_Stack_Pointer
     (Agent         : in out Oak_Agent'Class;
      Stack_Pointer : in System.Address)
     with Inline_Always;

   procedure Set_State
     (A     : in out Oak_Agent'Class;
      State : in     Agent_State);

   function State (Agent : in Oak_Agent'Class) return Agent_State;

   procedure Set_Wake_Time
     (Agent : in out Oak_Agent'Class;
      WT    : in Oak_Time.Time);

   function Wake_Time (Agent : in Oak_Agent'Class) return Oak_Time.Time;

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

      State                  : Agent_State;
      Normal_Priority        : Any_Priority;
      Wake_Time              : Oak_Time.Time;

      Absolute_Deadline      : Oak_Time.Time;
      Message_Store          : Oak_Message_Location;

      Scheduler_Agent        : access Schedulers.Scheduler_Agent'Class := null;

      Total_Execution_Time   : Oak_Time.Time_Span;
      Max_Execution_Time     : Oak_Time.Time_Span;
      Current_Execution_Time : Oak_Time.Time_Span;
      Execution_Cycles       : Natural;

      Next_Agent             : access Oak_Agent'Class := null;
      Previous_Agent         : access Oak_Agent'Class := null;

      --  Memory_List : Memory_Region_Link := null;
   end record;

   function Agent_Message
     (For_Agent : in Oak_Agent'Class)
      return Oak_Message is (For_Agent.Message_Store.Message);

   function Agent_Id (Agent : in Oak_Agent'Class) return Task_Id is
     (Agent.Id);

   function Agent_Yield_Status
     (For_Agent : in Oak_Agent'Class)
      return Yielded_State is (For_Agent.Message_Store.Yield_Status);

   function Name (Agent : in Oak_Agent'Class) return Task_Name is
     (Agent.Name);

   function Normal_Priority
     (Agent : in Oak_Agent'Class)
      return System.Any_Priority is (Agent.Normal_Priority);

   function Scheduler_Agent_For_Agent
     (Agent : in Oak_Agent'Class)
      return access Schedulers.Scheduler_Agent'Class
      is (Agent.Scheduler_Agent);

   function Stack_Pointer
     (Agent : in Oak_Agent'Class)
      return System.Address is (Agent.Call_Stack.Pointer);

   function State
     (Agent : in Oak_Agent'Class)
      return Agent_State is (Agent.State);

   function Wake_Time
     (Agent : in Oak_Agent'Class)
      return Oak_Time.Time is (Agent.Wake_Time);

end Oak.Agent;
