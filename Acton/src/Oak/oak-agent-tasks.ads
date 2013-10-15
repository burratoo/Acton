with Ada.Cyclic_Tasks;
with Oak.Timers;
with System;
with System.Storage_Elements;

with Oak.Oak_Time; use Oak.Oak_Time;

limited with Oak.Atomic_Actions;

package Oak.Agent.Tasks with Preelaborate is

   type Task_Agent is new Oak_Agent with private
     with Preelaborable_Initialization;

   type Task_Handler is access all Task_Agent'Class;

   type Boolean_Access is access all Boolean;

   type Activation_Chain is limited private;

   type Activation_Chain_Access is access all Activation_Chain;

   type Deadline_Base is (Wake_Up_Time, Clock_Time);

   Unspecified_Priority : constant Integer := -1;

   procedure Initialise_Task_Agent
     (Agent             : not null access Task_Agent'Class;
      Stack_Address     : in System.Address;
      Stack_Size        : in System.Storage_Elements.Storage_Count;
      Name              : in String;
      Run_Loop          : in System.Address;
      Task_Value_Record : in System.Address;
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
      Scheduler_Agent   : access Schedulers.Scheduler_Agent'Class;
      Chain             : in out Activation_Chain;
      Elaborated        : in Boolean_Access);

   function Activation_List
     (T    : in Task_Agent'Class)
      return access Task_Agent'Class;

   function Budget_Timer (T : not null access Task_Agent'Class)
                          return access Timers.Action_Timer'Class;

   function Current_Atomic_Action
     (T : in Task_Agent'Class)
      return access Atomic_Actions.Atomic_Object;

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

   procedure Set_Current_Atomic_Action
     (T  : in out Task_Agent'Class;
      AA : access Atomic_Actions.Atomic_Object);

   procedure Set_Cycle_Period
     (T  : in out Task_Agent'Class;
      CP : in Oak_Time.Time_Span);

   procedure Set_Next_Deadline_For_Task
     (T     : in out Task_Agent'Class;
      Using : in Deadline_Base);

   procedure Set_Relative_Deadline
     (T  : in out Task_Agent'Class;
      RD : in Oak_Time.Time_Span);

private
   type Task_Agent is new Oak_Agent with record
      Cycle_Behaviour   : Ada.Cyclic_Tasks.Behaviour;
      Cycle_Period      : Oak_Time.Time_Span;
      Phase             : Oak_Time.Time_Span;

      Execution_Budget  : Oak_Time.Time_Span;
      Relative_Deadline : Oak_Time.Time_Span;

      Deadline_Timer    : aliased Oak.Timers.Action_Timer;
      Execution_Timer   : aliased Oak.Timers.Action_Timer;

      Next_Run_Cycle    : Oak_Time.Time;
      Event_Raised      : Boolean;

      Activation_List   : access Task_Agent'Class;
      Elaborated        : Boolean_Access;

      In_Atomic_Action  : access Atomic_Actions.Atomic_Object;
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
