package Oak.Oak_Task.Scheduler_Agent is

   pragma Preelaborate;

   procedure Initialise_Agent
     (Agent                          : access Oak_Task;
      Name                           : in Task_Name;
      Call_Stack                     : in Call_Stack_Handler;
      Max_Priority                   : in Priority;
      Min_Prioirty                   : in Priority;
      Run_Loop                       : in Address);

   function Get_Task_To_Run
     (Agent : in Oak_Task_Handler)
      return  Oak_Task_Handler;
   procedure Set_Chosen_Task (Agent, T : in Oak_Task_Handler);

   function Get_Task_To_Manage (Agent : in Oak_Task_Handler) return Oak_Task_Handler;

   procedure Set_Run_Reason
     (Agent  : in Oak_Task_Handler;
      Reason : in Reason_For_Run);
   function Get_Run_Reason (Agent : in Oak_Task_Handler) return Reason_For_Run;

   function Get_Lowest_Priority (Agent : in Oak_Task_Handler) return Priority;
   function Get_Highest_Priority (Agent : in Oak_Task_Handler) return Priority;
   procedure Set_Priority_Range
     (Agent    : in Oak_Task_Handler;
      Min, Max : in Priority);
   procedure Set_Task_To_Manage
     (Agent : in Oak_Task_Handler;
      MT    : in Oak_Task_Handler);

   function Get_Next_In_Queue (T : Oak_Task_Handler) return Oak_Task_Handler;
   function Get_Prev_In_Queue (T : Oak_Task_Handler) return Oak_Task_Handler;
   procedure Set_Next_In_Queue (T, Next : Oak_Task_Handler);
   procedure Set_Prev_In_Queue (T, Prev : Oak_Task_Handler);

   function Get_Next_Agent (T : Oak_Task_Handler) return Oak_Task_Handler;
   procedure Set_Next_Agent (T : in Oak_Task_Handler; Next_Agent : in Oak_Task_Handler);

   function Get_Desired_Run_Time (Agent : Oak_Task_Handler) return Time;
   procedure Set_Desired_Run_Time (Agent : Oak_Task_Handler; Run_Time : Time);

end Oak.Oak_Task.Scheduler_Agent;
