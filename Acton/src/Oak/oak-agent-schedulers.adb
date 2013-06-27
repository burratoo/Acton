with Oak.Core;

with Oak.Memory.Call_Stack.Ops; use Oak.Memory.Call_Stack.Ops;
with Oak.Scheduler.Agent_List; use Oak.Scheduler.Agent_List;

package body Oak.Agent.Schedulers is

   procedure Initialise_Scheduler_Agent
     (Agent           : access Scheduler_Agent'Class;
      Name            : in String;
      Call_Stack_Size : in System.Storage_Elements.Storage_Count;
      Run_Loop        : in System.Address)
   is
      Call_Stack : Oak.Memory.Call_Stack.Call_Stack_Handler;

      OI : constant access Oak.Core.Oak_Data := Oak.Core.Oak_Instance;

      Scheduler : constant access Oak.Scheduler.Oak_Scheduler_Info :=
         Oak.Core.Scheduler_Info (OI);
   begin
      Oak.Memory.Call_Stack.Ops.Allocate_Call_Stack
        (Stack            => Call_Stack,
         Size_In_Elements => Call_Stack_Size);

      Oak.Agent.Initialise_Agent
        (Agent      => Agent,
         Name       => Name,
         Call_Stack => Call_Stack);

      Agent.Task_To_Run            := null;
      Agent.Desired_Agent_Run_Time := Oak_Time.Time_Zero;
      Agent.Manage_Task            := null;
      Agent.Run_Reason             := Select_Next_Task;
      Agent.Next_Agent             := null;

      Agent.Run_Timer.Set_Timer
        (Priority  => Oak_Interrupt_Priority'Last,
         Scheduler => Agent);
      Agent.Run_Timer.Add_Timer_To_Current_Processor;

      Initialise_Call_Stack
        (Stack             => Agent.Call_Stack,
         Start_Instruction => Run_Loop,
         Task_Value_Record => Agent.all'Address,
         Message_Location  => Agent.Message_Location);

      Add_Scheduler_Agent
        (Scheduler_Info => Scheduler.all,
         New_Agent      => Agent);

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
