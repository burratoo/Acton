package body Oak.Oak_Task.Internal is

   -----------------------
   -- Get_Stack_Pointer --
   -----------------------

   function Get_Stack_Pointer (T : in Oak_Task_Handler) return Address is
   begin
      return T.Call_Stack.Pointer;
   end Get_Stack_Pointer;

   procedure Set_Stack_Pointer
     (T             : in Oak_Task_Handler;
      Stack_Pointer : Address)
   is
   begin
      T.Call_Stack.Pointer := Stack_Pointer;
   end Set_Stack_Pointer;

   -----------------------
   -- Get_Stack_Pointer --
   -----------------------

   function Get_Call_Stack
     (T    : in Oak_Task_Handler)
      return Call_Stack_Handler
   is
   begin
      return T.Call_Stack;
   end Get_Call_Stack;

   procedure Set_Call_Stack
     (T     : in Oak_Task_Handler;
      Stack : Call_Stack_Handler)
   is
   begin
      T.Call_Stack := Stack;
   end Set_Call_Stack;
   ----------------------------------
   -- Get_Scheduler_Agent_For_Task --
   ----------------------------------

   function Get_Scheduler_Agent_For_Task
     (T    : in Oak_Task_Handler)
      return Oak_Task_Handler
   is
   begin
      return T.Scheduler_Agent;
   end Get_Scheduler_Agent_For_Task;

   procedure Set_Scheduler_Agent_For_Task
     (T     : in Oak_Task_Handler;
      Agent : in Oak_Task_Handler)
   is
   begin
      T.Scheduler_Agent := Agent;
   end Set_Scheduler_Agent_For_Task;

   ---------------
   -- Set_State --
   ---------------

   procedure Set_State (T : in Oak_Task_Handler; New_State : in Task_State) is
   begin
      T.State := New_State;
   end Set_State;

   procedure Next_Run_Cycle (T : in Oak_Task_Handler) is
   begin
      T.Wake_Time      := T.Next_Run_Cycle;
      T.Next_Run_Cycle := T.Next_Run_Cycle + T.Cycle_Period;
   end Next_Run_Cycle;

   function New_Task_Id return Task_Id is
      Chosen_Id : constant Task_Id := Global_Task_Id;
   begin
      Global_Task_Id := Global_Task_Id + 1;
      return Chosen_Id;
   end New_Task_Id;

   procedure Set_Cycle_Period (T : in Oak_Task_Handler; CP : in Time_Span) is
   begin
      T.Cycle_Period := CP;
   end Set_Cycle_Period;

   procedure Set_Relative_Deadline
     (T  : in Oak_Task_Handler;
      RD : in Time_Span)
   is
   begin
      T.Deadline := RD;
   end Set_Relative_Deadline;

   function Is_Scheduler_Agent (T : in Oak_Task_Handler) return Boolean is
   begin
      return T.Kind = Scheduler;
   end Is_Scheduler_Agent;

   function Is_Regular_Task (T : in Oak_Task_Handler) return Boolean is
   begin
      return T.Kind = Regular;
   end Is_Regular_Task;
end Oak.Oak_Task.Internal;
