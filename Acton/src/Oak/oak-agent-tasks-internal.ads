package Oak.Oak_Task.Internal with Preelaborate is

   function Get_Stack_Pointer (T : in Oak_Task_Handler) return Address;
   pragma Inline_Always (Get_Stack_Pointer);

   procedure Set_Stack_Pointer
     (T             : in Oak_Task_Handler;
      Stack_Pointer : Address)
     with Inline_Always;

   function Get_Call_Stack
     (T    : in Oak_Task_Handler)
      return Call_Stack_Handler
     with Inline_Always;

   procedure Set_Call_Stack
     (T     : in Oak_Task_Handler;
      Stack : Call_Stack_Handler)
     with Inline_Always;

   function Get_Scheduler_Agent_For_Task
     (T    : in Oak_Task_Handler)
     return Oak_Task_Handler
     with Inline_Always;

   procedure Set_Scheduler_Agent_For_Task
     (T     : in Oak_Task_Handler;
      Agent : in Oak_Task_Handler);

   procedure Set_State (T : in Oak_Task_Handler; New_State : in Task_State);

   procedure Next_Run_Cycle (T : in Oak_Task_Handler);

   procedure Set_Cycle_Period (T : in Oak_Task_Handler; CP : in Time_Span);

   procedure Set_Relative_Deadline
     (T  : in Oak_Task_Handler;
      RD : in Time_Span);

   function Is_Scheduler_Agent (T : in Oak_Task_Handler) return Boolean
     with Inline_Always;
   function Is_Regular_Task (T : in Oak_Task_Handler) return Boolean
     with Inline_Always;

   function Get_Task_Yield_Status (For_Task : in Oak_Task_Handler) return
     Yielded_State;
   procedure Store_Task_Yield_Status
     (For_Task : in Oak_Task_Handler;
      Yielded  : in Yielded_State)
     with Inline_Always;

end Oak.Oak_Task.Internal;
