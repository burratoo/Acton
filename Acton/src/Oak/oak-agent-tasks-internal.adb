package body Oak.Agent.Tasks.Internal is

   procedure Next_Run_Cycle (T : in out Task_Agent'Class) is
   begin
      T.Wake_Time      := T.Next_Run_Cycle;
      T.Next_Run_Cycle := T.Next_Run_Cycle + T.Cycle_Period;
   end Next_Run_Cycle;

   procedure Set_Cycle_Period
     (T  : in out Task_Agent'Class;
      CP : in Time_Span) is
   begin
      T.Cycle_Period := CP;
   end Set_Cycle_Period;

   procedure Set_Relative_Deadline
     (T  : in out Task_Agent'Class;
      RD : in Time_Span) is
   begin
      T.Deadline := RD;
   end Set_Relative_Deadline;

   procedure Set_Scheduler_Agent_For_Task
     (T     : in out Task_Agent'Class;
      Agent : access Scheduler.Scheduler_Agent'Class) is
   begin
      T.Scheduler_Agent := Agent;
   end Set_Scheduler_Agent_For_Task;

   procedure Set_State
     (T         : in out Task_Agent'Class;
      New_State : in Task_State) is
   begin
      T.State := New_State;
   end Set_State;

   procedure Store_Task_Yield_Status
     (For_Task : in out Task_Agent'Class;
      Yielded  : in Yielded_State)
   is
   begin
      For_Task.Message_Location.Yield_Status := Yielded;
   end Store_Task_Yield_Status;

end Oak.Agent.Tasks.Internal;
