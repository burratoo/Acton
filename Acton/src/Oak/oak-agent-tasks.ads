with Oak.Entries;
with Oak.Interrupts;
with Oak.Protected_Object;
with System;
with System.Storage_Elements;

with Ada.Real_Time; use Ada.Real_Time;

limited with Oak.Agent.Scheduler;
limited with Oak.Agent.Tasks.Protected_Object;

package Oak.Agent.Tasks with Preelaborate is

   type Task_Agent is tagged private;

   type Task_Handler is access all Task_Agent'Class;

   type Task_State is (
                       Bad_State,                   -- 0
                       Activation_Pending,          -- 1
                       Activation_Failed,           -- 2
                       Activation_Successful,       -- 3
                       Activation_Complete,         -- 4
                       Running,                     -- 5
                       Runnable,                    -- 6
                       Sleeping,                    -- 7
                       Waiting,                     -- 8
                       Inactive,                    -- 9
                       Shared_State,                -- 10
                       Cycle_Completed,             -- 11
                       Change_Cycle_Period,         -- 12
                       Change_Relative_Deadline,    -- 13
                       Terminated,                  -- 14
                       Entering_PO,                 -- 15
                       Enter_PO_Refused,            -- 16
                       Exiting_PO,                  -- 17
                       Exit_PO_Error,               -- 18
                       Waiting_On_Protected_Object, -- 19
                       Attach_Interrupt_Handlers,   -- 20
                       No_State);                   -- 21

   type Oak_Task_Message (Message_Type : Task_State := No_State) is record
      case Message_Type is
         when Sleeping =>
            Wake_Up_At : Time := Time_Last;
         when Change_Cycle_Period =>
            New_Cycle_Period : Time_Span := Time_Span_Zero;
         when Change_Relative_Deadline =>
            New_Deadline_Span : Time_Span := Time_Span_Zero;
         when Entering_PO =>
            PO_Enter          : not null access
              Protected_Object.Protected_Agent'Class;
            Subprogram_Kind  : Oak.Protected_Object.Protected_Subprogram_Type;
            Entry_Id_Enter   : Entries.Entry_Index;
         when Exiting_PO =>
            PO_Exit           : not null access
              Protected_Object.Protected_Agent'Class;
         when Attach_Interrupt_Handlers =>
            Attach_Handlers   : Oak.Interrupts.Interrupt_Handlers_Access;
            Attach_Handler_PO : not null access
              Protected_Object.Protected_Agent'Class;
         when others =>
            null;
      end case;
   end record;

   type Shared_Task_State is access all Task_State;
   No_Shared_State : constant Shared_Task_State := null;

   type Yielded_State is (Forced, Voluntary);

   type Oak_Task_Message_Store is record
      Yield_Status : Yielded_State;
      Message      : Oak_Task_Message;
   end record;

   type Oak_Task_Message_Location is access all Oak_Task_Message_Store;

   type Boolean_Access is access all Boolean;

   type Activation_Chain is limited private;

   type Activation_Chain_Access is access all Activation_Chain;

   Unspecified_Priority : constant Integer := -1;

   procedure Initialise_Agent
     (Agent             : in out Task_Agent'Class;
      Stack_Address     : in System.Address;
      Stack_Size        : in System.Storage_Elements.Storage_Count;
      Name              : in String;
      Normal_Priority   : in Integer;
      Relative_Deadline : in Time_Span;
      Cycle_Period      : in Time_Span;
      Phase             : in Time_Span;
      Run_Loop          : in System.Address;
      Task_Value_Record : in System.Address;
      Chain             : in out Activation_Chain;
      Elaborated        : in Boolean_Access);

   procedure Initialise_Main_Task
     (Stack_Size      : in System.Storage_Elements.Storage_Count;
      Name            : in String;
      Normal_Priority : in Integer;
      Run_Loop        : in System.Address);

   function Activation_List
     (T    : in Task_Agent'Class)
      return access Task_Agent'Class;

   function Cycle_Period (T : in Task_Agent'Class) return Time_Span;

   function Deadline (T : in Task_Agent'Class) return Time_Span;

   function Is_Elaborated (T : in Task_Agent'Class) return Boolean;

   function Next_Run_Time (T : in Task_Agent'Class) return Time;

   function Normal_Priority
     (T : in Task_Agent'Class)
      return System.Any_Priority;

   function Task_Message
     (For_Task : in Task_Agent'Class)
      return Oak_Task_Message;

   function Phase (T : in Task_Agent'Class) return Time_Span;

   function Shared_State (For_Task : in Task_Agent'Class) return Task_State;

   function State (T : in Task_Agent'Class) return Task_State;

   function Wake_Time (T : in Task_Agent'Class) return Time;

   procedure Set_Activation_List
     (T     : in out Task_Agent'Class;
      Chain : in Activation_Chain_Access);

   procedure Store_Oak_Task_Message
     (For_Task : in out Task_Agent'Class;
      Message  : in Oak_Task_Message);

   procedure Set_Scheduler_Agent
     (T               : in out Task_Agent'Class;
      Scheduler_Agent : access Scheduler.Scheduler_Agent'Class);

   procedure Set_Shared_State
     (For_Task : in out Task_Agent'Class;
      With_State_Pointer : in Shared_Task_State);

   procedure Set_State (T : in out Task_Agent'Class; State : in Task_State);

   procedure Set_Wake_Time (T : in out Task_Agent'Class; WT : in Time);

private
   type Task_Agent_Link_Element is record
      Next     : access Task_Agent'Class := null;
      Previous : access Task_Agent'Class := null;
   end record;

   type Task_Agent is new Oak_Agent with record
      State            : Task_State                := Sleeping;
      Shared_State     : Shared_Task_State         := No_Shared_State;
      Message_Location : Oak_Task_Message_Location := null;

      Normal_Priority : System.Any_Priority := System.Default_Priority;
      Deadline        : Time_Span           := Time_Span_Zero;
      Cycle_Period    : Time_Span           := Time_Span_Zero;
      Phase           : Time_Span           := Time_Span_Zero;

      Next_Deadline  : Time := Time_Last;
      Next_Run_Cycle : Time := Time_Last;
      Wake_Time      : Time := Time_Last;

      Scheduler_Agent : access Scheduler.Scheduler_Agent'Class := null;
      Queue_Link      : Task_Agent_Link_Element;
      Deadline_List   : Task_Agent_Link_Element;

      Activation_List : access Task_Agent'Class := null;
      Elaborated      : Boolean_Access   := null;
   end record;

   type Activation_Chain is limited record
      Head : access Task_Agent'Class := null;
   end record;

   function Activation_List
     (T    : in Task_Agent'Class)
      return access Task_Agent'Class is (T.Activation_List);

   function Cycle_Period
     (T : in Task_Agent'Class)
      return Time_Span is (T.Cycle_Period);

   function Deadline
     (T : in Task_Agent'Class)
      return Time_Span is (T.Deadline);

   function Is_Elaborated
     (T : in Task_Agent'Class)
      return Boolean is (T.Elaborated.all);

   function Next_Run_Time
     (T : in Task_Agent'Class)
      return Time is (T.Next_Run_Cycle);

   function Normal_Priority
     (T : in Task_Agent'Class)
      return System.Any_Priority is (T.Normal_Priority);

   function Task_Message
     (For_Task : in Task_Agent'Class)
      return Oak_Task_Message is (For_Task.Message_Location.Message);

   function Phase
     (T : in Task_Agent'Class)
      return Time_Span is (T.Phase);

   function Shared_State
     (For_Task : in Task_Agent'Class)
      return Task_State is (For_Task.Shared_State.all);

   function State
     (T : in Task_Agent'Class)
      return Task_State is (T.State);

   function Wake_Time
     (T : in Task_Agent'Class)
      return Time is (T.Wake_Time);

end Oak.Agent.Tasks;