with Oak.Indices;
with Oak.Protected_Objects;
with System;
with System.Storage_Elements;

with Oak.Oak_Time; use Oak.Oak_Time;

limited with Oak.Agent.Schedulers;
limited with Oak.Agent.Tasks.Protected_Objects;
limited with Oak.Atomic_Actions;
limited with Oak.Interrupts;

package Oak.Agent.Tasks with Preelaborate is

   type Task_Agent is new Oak_Agent with private;

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
                       Entering_Atomic_Action,      -- 21
                       Enter_Atomic_Action_Refused, -- 22
                       Exiting_Atomic_Action,       -- 23
                       Exit_Atomic_Action_Error,    -- 24
                       Entering_Exit_Barrier,       -- 25
                       Atomic_Action_Error,         -- 26
                       No_State);                   -- 27

   type Oak_Task_Message (Message_Type : Task_State := No_State) is record
      case Message_Type is
         when Sleeping =>
            Wake_Up_At : Oak_Time.Time := Oak_Time.Time_Last;
         when Change_Cycle_Period =>
            New_Cycle_Period : Oak_Time.Time_Span := Oak_Time.Time_Span_Zero;
         when Change_Relative_Deadline =>
            New_Deadline_Span : Oak_Time.Time_Span :=
                                  Oak_Time.Time_Span_Zero;
         when Entering_PO =>
            PO_Enter          : not null access
              Protected_Objects.Protected_Agent'Class;
            Subprogram_Kind  : Oak.Protected_Objects.Protected_Subprogram_Type;
            Entry_Id_Enter   : Indices.Entry_Index;
         when Exiting_PO =>
            PO_Exit           : not null access
              Protected_Objects.Protected_Agent'Class;
         when Attach_Interrupt_Handlers =>
            Attach_Handlers   : access Oak.Interrupts.Interrupt_Handler_Array;
            Attach_Handler_PO : not null access
              Protected_Objects.Protected_Agent'Class;
         when Entering_Atomic_Action =>
            AA_Enter          : not null access
              Atomic_Actions.Atomic_Object;
            Action_Id_Enter   : Indices.Action_Index;
         when Entering_Exit_Barrier =>
            AA_EB             : not null access
              Atomic_Actions.Atomic_Object;
            Action_Id_EB      : Indices.Action_Index;
            Exception_Raised  : Boolean;
         when Exiting_Atomic_Action =>
            AA_Exit           : not null access
              Atomic_Actions.Atomic_Object;
            Action_Id_Exit    : Indices.Action_Index;
            Atomic_Exception  : Boolean;
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

   procedure Initialise_Task_Agent
     (Agent             : access Task_Agent'Class;
      Stack_Address     : in System.Address;
      Stack_Size        : in System.Storage_Elements.Storage_Count;
      Name              : in String;
      Normal_Priority   : in Integer;
      Relative_Deadline : in Oak_Time.Time_Span;
      Cycle_Period      : in Oak_Time.Time_Span;
      Phase             : in Oak_Time.Time_Span;
      Run_Loop          : in System.Address;
      Task_Value_Record : in System.Address;
      Chain             : in out Activation_Chain;
      Elaborated        : in Boolean_Access);

   procedure Initialise_Sleep_Agent
     (Agent    : access Task_Agent'Class;
      Run_Loop : in System.Address);
   function Activation_List
     (T    : in Task_Agent'Class)
      return access Task_Agent'Class;

   function Current_Atomic_Action
     (T : in Task_Agent'Class)
      return access Atomic_Actions.Atomic_Object;

   function Cycle_Period
     (T : in Task_Agent'Class)
      return Oak_Time.Time_Span;

   function Deadline (T : in Task_Agent'Class) return Oak_Time.Time_Span;

   function Is_Elaborated (T : in Task_Agent'Class) return Boolean;

   function Next_Run_Time (T : in Task_Agent'Class) return Oak_Time.Time;

   function Normal_Priority
     (T : in Task_Agent'Class)
      return System.Any_Priority;

   function Phase (T : in Task_Agent'Class) return Oak_Time.Time_Span;

   function Scheduler_Agent_For_Task
     (T    : in Task_Agent'Class)
      return access Schedulers.Scheduler_Agent'Class;

   function Shared_State
     (For_Task : in Task_Agent'Class)
      return Task_State;

   function Task_Message
     (For_Task : in Task_Agent'Class)
      return Oak_Task_Message;

   function Task_Yield_Status
     (For_Task : in Task_Agent'Class)
      return Yielded_State;

   function State (T : in Task_Agent'Class) return Task_State;

   function Wake_Time (T : in Task_Agent'Class) return Oak_Time.Time;

   procedure Next_Run_Cycle (T : in out Task_Agent'Class);

   procedure Set_Activation_List
     (T   : in out Task_Agent'Class;
      Add : access Task_Agent'Class);

   procedure Set_Activation_List
     (T     : in out Task_Agent'Class;
      Chain : in Activation_Chain_Access);

   procedure Set_Current_Atomic_Action
     (T  : in out Task_Agent'Class;
      AA : access Atomic_Actions.Atomic_Object);

   procedure Set_Cycle_Period
     (T  : in out Task_Agent'Class;
      CP : in Oak_Time.Time_Span);

   procedure Store_Oak_Task_Message
     (For_Task : in out Task_Agent'Class;
      Message  : in Oak_Task_Message) with Inline_Always;

   procedure Set_Relative_Deadline
     (T  : in out Task_Agent'Class;
      RD : in Oak_Time.Time_Span);

   procedure Set_Scheduler_Agent
     (T     : in out Task_Agent'Class;
      Agent : access Schedulers.Scheduler_Agent'Class);

   procedure Set_Scheduler_Agent_For_Task
     (T     : in out Task_Agent'Class;
      Agent : access Schedulers.Scheduler_Agent'Class);

   procedure Set_Shared_State
     (For_Task : in out Task_Agent'Class;
      With_State_Pointer : in Shared_Task_State);

   procedure Set_State
     (T     : in out Task_Agent'Class;
      State : in Task_State);

   procedure Store_Task_Yield_Status
     (For_Task : in out Task_Agent'Class;
      Yielded  : in Yielded_State) with Inline_Always;

   procedure Set_Wake_Time
     (T  : in out Task_Agent'Class;
      WT : in Oak_Time.Time);

private
   type Task_Agent_Link_Element is record
      Next     : access Task_Agent'Class := null;
      Previous : access Task_Agent'Class := null;
   end record;

   type Task_Agent is new Oak_Agent with record
      State            : Task_State                := Sleeping;
      Shared_State     : Shared_Task_State         := No_Shared_State;
      Message_Location : Oak_Task_Message_Location := null;

      Normal_Priority  : System.Any_Priority := System.Default_Priority;
      Deadline         : Oak_Time.Time_Span := Oak_Time.Time_Span_Zero;
      Cycle_Period     : Oak_Time.Time_Span := Oak_Time.Time_Span_Zero;
      Phase            : Oak_Time.Time_Span := Oak_Time.Time_Span_Zero;

      Next_Deadline    : Oak_Time.Time := Oak_Time.Time_Last;
      Next_Run_Cycle   : Oak_Time.Time := Oak_Time.Time_Last;
      Wake_Time        : Oak_Time.Time := Oak_Time.Time_Last;

      Scheduler_Agent  : access Schedulers.Scheduler_Agent'Class := null;
      Queue_Link       : Task_Agent_Link_Element;
      Deadline_List    : Task_Agent_Link_Element;

      Activation_List  : access Task_Agent'Class := null;
      Elaborated       : Boolean_Access   := null;

      In_Atomic_Action : access Atomic_Actions.Atomic_Object := null;
   end record;

   type Activation_Chain is limited record
      Head : access Task_Agent'Class := null;
   end record;

   function Activation_List
     (T    : in Task_Agent'Class)
      return access Task_Agent'Class is (T.Activation_List);

   function Current_Atomic_Action
     (T : in Task_Agent'Class)
      return access Atomic_Actions.Atomic_Object is (T.In_Atomic_Action);

   function Cycle_Period
     (T : in Task_Agent'Class)
      return Oak_Time.Time_Span is (T.Cycle_Period);

   function Deadline
     (T : in Task_Agent'Class)
      return Oak_Time.Time_Span is (T.Deadline);

   function Is_Elaborated
     (T : in Task_Agent'Class)
      return Boolean is (T.Elaborated.all);

   function Next_Run_Time
     (T : in Task_Agent'Class)
      return Oak_Time.Time is (T.Next_Run_Cycle);

   function Normal_Priority
     (T : in Task_Agent'Class)
      return System.Any_Priority is (T.Normal_Priority);

   function Task_Message
     (For_Task : in Task_Agent'Class)
      return Oak_Task_Message is (For_Task.Message_Location.Message);

   function Phase
     (T : in Task_Agent'Class)
      return Oak_Time.Time_Span is (T.Phase);

   function Scheduler_Agent_For_Task
     (T : in Task_Agent'Class)
      return access Schedulers.Scheduler_Agent'Class is (T.Scheduler_Agent);

   function Shared_State
     (For_Task : in Task_Agent'Class)
      return Task_State is (For_Task.Shared_State.all);

   function State
     (T : in Task_Agent'Class)
      return Task_State is (T.State);

   function Task_Yield_Status
     (For_Task : in Task_Agent'Class)
      return Yielded_State is (For_Task.Message_Location.Yield_Status);

   function Wake_Time
     (T : in Task_Agent'Class)
      return Oak_Time.Time is (T.Wake_Time);

end Oak.Agent.Tasks;
