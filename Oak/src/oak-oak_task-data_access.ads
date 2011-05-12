package Oak.Oak_Task.Data_Access is

   pragma Preelaborate;

   procedure Initialise_Task
     (T               : access Oak_Task;
      Name            : in Task_Name;
      Normal_Priority : Priority;
      Deadline        : Time_Span;
      Cycle_Period    : Time_Span;
      Phase           : Time_Span;
      Run_Loop        : Address;
      Stack_Access    : Call_Stack_Handler);

   procedure Set_Scheduler_Agent
     (T               : access Oak_Task;
      Scheduler_Agent : in Oak_Task_Handler);

   function Get_Id (T : access Oak_Task) return Task_Id;
   function Get_Name (T : access Oak_Task) return Task_Name;

   function Get_State (T : access Oak_Task) return Task_State;
   procedure Set_State (T : access Oak_Task; State : Task_State);

   function Get_Normal_Priority (T : access Oak_Task) return Priority;
   function Get_Deadline (T : access Oak_Task) return Time_Span;
   function Get_Cycle_Period (T : access Oak_Task) return Time_Span;
   function Get_Phase (T : access Oak_Task) return Time_Span;
   function Get_Next_Run_Time (T : access Oak_Task) return Time;

   function Get_Wake_Time (T : access Oak_Task) return Time;
   procedure Set_Wake_Time (T : access Oak_Task; WT : Time);

end Oak.Oak_Task.Data_Access;
