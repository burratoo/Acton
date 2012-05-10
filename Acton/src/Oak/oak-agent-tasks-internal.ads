package Oak.Agent.Tasks.Internal with Preelaborate is

   function Scheduler_Agent_For_Task
     (T    : in Task_Agent'Class)
     return access Scheduler_Agent'Class with Inline_Always;

   function Task_Yield_Status
     (For_Task : in Task_Agent'Class)
      return Yielded_State;

   procedure Next_Run_Cycle (T : in out Task_Agent'Class);

   procedure Set_Cycle_Period (T : in out Task_Agent'Class; CP : in Time_Span);

   procedure Set_Relative_Deadline
     (T  : in out Task_Agent'Class;
      RD : in Time_Span);

   procedure Set_Scheduler_Agent_For_Task
     (T     : in out Task_Agent'Class;
      Agent : access Scheduler_Agent'Class);

   procedure Set_State
     (T         : in out Task_Agent'Class;
      New_State : in Task_State);

   procedure Store_Task_Yield_Status
     (For_Task : in out Task_Agent'Class;
      Yielded  : in Yielded_State)
   with Inline_Always;

private
   function Scheduler_Agent_For_Task
     (T    : in Task_Agent'Class)
      return Oak_Task_Handler is (T.Scheduler_Agent);

   function Task_Yield_Status
     (For_Task : in Task_Agent'Class)
      return Yielded_State is (For_Task.Message_Location.Yield_Status);

end Oak.Agent.Tasks.Internal;
