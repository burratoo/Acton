with Oak.Memory.Call_Stack.Ops; use Oak.Memory.Call_Stack.Ops;
with System; use System;

package body Oak.Agent.Schedulers is

   procedure Initialise_Scheduler_Agent
     (Agent        : access Scheduler_Agent'Class;
      Name         : in String;
      Call_Stack   : in Call_Stack_Handler;
      Max_Priority : in System.Any_Priority;
      Min_Prioirty : in System.Any_Priority;
      Run_Loop     : in System.Address)
   is
   begin
      Oak.Agent.Initialise_Agent
        (Agent      => Agent,
         Name       => Name,
         Call_Stack => Call_Stack);

      Agent.Lowest_Prioirty        := Min_Prioirty;
      Agent.Highest_Prioirty       := Max_Priority;
      Agent.Desired_Agent_Run_Time := Oak_Time.Time_Zero;
      Agent.Run_Timer.Set_Timer
        (Priority  => Oak_Interrupt_Priority'Last,
         Scheduler => Agent);
      Agent.Run_Timer.Add_Timer_To_Current_Processor;

      Initialise_Call_Stack
        (Stack             => Agent.Call_Stack,
         Start_Instruction => Run_Loop);
   end Initialise_Scheduler_Agent;

   procedure Set_Chosen_Task
     (Agent : in out Scheduler_Agent'Class;
      T     : access Tasks.Task_Agent'Class) is
   begin
      Agent.Task_To_Run := T;
   end Set_Chosen_Task;

   procedure Set_Desired_Run_Time
     (Agent    : in out Scheduler_Agent'Class;
      Run_Time : in Oak.Oak_Time.Time) is
   begin
      Agent.Desired_Agent_Run_Time := Run_Time;
   end Set_Desired_Run_Time;

   procedure Set_Next_Agent
     (Agent      : in out Scheduler_Agent'Class;
      Next_Agent : access Scheduler_Agent'Class)
   is
   begin
      Agent.Next_Agent := Next_Agent;
   end Set_Next_Agent;

   procedure Set_Run_Reason
     (Agent  : in out Scheduler_Agent'Class;
      Reason : in Reason_For_Run) is
   begin
      Agent.Run_Reason := Reason;
   end Set_Run_Reason;

   procedure Set_Task_To_Manage
     (Agent : in out Scheduler_Agent'Class;
      MT    : access Tasks.Task_Agent'Class) is
   begin
      Agent.Manage_Task := MT;
   end Set_Task_To_Manage;

end Oak.Agent.Schedulers;
