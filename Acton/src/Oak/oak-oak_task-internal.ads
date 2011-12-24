package Oak.Oak_Task.Internal is
   pragma Preelaborate;

   function Get_Stack_Pointer (T : in Oak_Task_Handler) return Address;
   pragma Inline_Always (Get_Stack_Pointer);

   procedure Set_Stack_Pointer
     (T             : in Oak_Task_Handler;
      Stack_Pointer : Address);
   pragma Inline_Always (Set_Stack_Pointer);

   function Get_Call_Stack
     (T    : in Oak_Task_Handler)
      return Call_Stack_Handler;
   --  pragma Inline_Always (Get_Stack_Pointer);

   procedure Set_Call_Stack
     (T     : in Oak_Task_Handler;
      Stack : Call_Stack_Handler);
   --  pragma Inline_Always (Set_Stack_Pointer);

   function Get_Scheduler_Agent_For_Task
     (T    : in Oak_Task_Handler)
      return Oak_Task_Handler;

   procedure Set_Scheduler_Agent_For_Task
     (T     : in Oak_Task_Handler;
      Agent : in Oak_Task_Handler);

   procedure Set_State (T : in Oak_Task_Handler; New_State : in Task_State);

   function Get_Task_Request (For_Task : in Oak_Task_Handler)
                              return Task_Requested_State;
   procedure Store_Task_Request (For_Task : in Oak_Task_Handler;
                               With_Request : in Task_Requested_State);

   procedure Next_Run_Cycle (T : in Oak_Task_Handler);

   function New_Task_Id return Task_Id;

   procedure Set_Cycle_Period (T : in Oak_Task_Handler; CP : in Time_Span);

   procedure Set_Relative_Deadline
     (T  : in Oak_Task_Handler;
      RD : in Time_Span);

   function Is_Scheduler_Agent (T : in Oak_Task_Handler) return Boolean;
   function Is_Regular_Task (T : in Oak_Task_Handler) return Boolean;

   pragma Inline_Always (Is_Scheduler_Agent);
   pragma Inline_Always (Is_Regular_Task);
end Oak.Oak_Task.Internal;
