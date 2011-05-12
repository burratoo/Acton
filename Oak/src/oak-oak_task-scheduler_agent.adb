with Oak.Processor_Support_Package.Call_Stack.Ops;
use  Oak.Processor_Support_Package.Call_Stack.Ops;

with Oak.Oak_Task.Internal;

package body Oak.Oak_Task.Scheduler_Agent is

   procedure Initialise_Agent
     (Agent                          : access Oak_Task;
      Name                           : in Task_Name;
      Call_Stack                     : in Call_Stack_Handler;
      Max_Priority                   : in Priority;
      Min_Prioirty                   : in Priority;
      Run_Loop                       : in Address)
   is
   begin
      Agent.all :=
        (Kind                           => Scheduler,
         Id                             => Internal.New_Task_Id,
         Name                           => Name,
         Call_Stack                     => Call_Stack,
         Lowest_Prioirty                => Min_Prioirty,
         Highest_Prioirty               => Max_Priority,
         Run_Loop                       => Run_Loop,
         Memory_List                    => null,
         Task_To_Run                    => null,
         Manage_Task                    => null,
         Desired_Agent_Run_Time         => Time_Zero,
         Run_Reason                     => Select_Next_Task,
         Next_Agent                     => null);

      Initialise_Call_Stack
        (Stack             => Agent.Call_Stack,
         Start_Instruction => Agent.Run_Loop);
   end Initialise_Agent;

   ---------------------
   -- Get_Task_To_Run --
   ---------------------

   function Get_Task_To_Run
     (Agent : in Oak_Task_Handler)
      return  Oak_Task_Handler
   is
   begin
      if Agent.Kind = Scheduler then
         return Agent.Task_To_Run;
      else
         return null;
      end if;
   end Get_Task_To_Run;
   ---------------------
   -- Set_Chosen_Task --
   ---------------------

   procedure Set_Chosen_Task (Agent, T : in Oak_Task_Handler) is
   begin
      if Agent.Kind = Scheduler then
         Agent.Task_To_Run := T;
      end if;
   end Set_Chosen_Task;

   ------------------------
   -- Get_Task_To_Manage --
   ------------------------

   function Get_Task_To_Manage
     (Agent : in Oak_Task_Handler)
      return  Oak_Task_Handler
   is
   begin
      return Agent.Manage_Task;
   end Get_Task_To_Manage;

   --------------------
   -- Set_Run_Reason --
   --------------------

   procedure Set_Run_Reason
     (Agent  : in Oak_Task_Handler;
      Reason : in Reason_For_Run)
   is
   begin
      Agent.Run_Reason := Reason;
   end Set_Run_Reason;

   function Get_Run_Reason
     (Agent : in Oak_Task_Handler)
      return  Reason_For_Run
   is
   begin
      return Agent.Run_Reason;
   end Get_Run_Reason;

   function Get_Lowest_Priority
     (Agent : in Oak_Task_Handler)
      return  Priority
   is
   begin
      return Agent.Lowest_Prioirty;
   end Get_Lowest_Priority;

   function Get_Highest_Priority
     (Agent : in Oak_Task_Handler)
      return  Priority
   is
   begin
      return Agent.Highest_Prioirty;
   end Get_Highest_Priority;

   ------------------------
   -- Set_Priority_Range --
   ------------------------
   procedure Set_Priority_Range
     (Agent    : in Oak_Task_Handler;
      Min, Max : in Priority)
   is
   begin
      Agent.Lowest_Prioirty  := Min;
      Agent.Highest_Prioirty := Max;
   end Set_Priority_Range;

   ----------------------
   -- Set_Managed_Task --
   ----------------------

   procedure Set_Task_To_Manage
     (Agent : in Oak_Task_Handler;
      MT    : in Oak_Task_Handler)
   is
   begin
      Agent.Manage_Task := MT;
   end Set_Task_To_Manage;

   function Get_Next_In_Queue
     (T    : Oak_Task_Handler)
      return Oak_Task_Handler
   is
   begin
      return T.Scheduler_Queue.Next;
   end Get_Next_In_Queue;

   function Get_Prev_In_Queue
     (T    : Oak_Task_Handler)
      return Oak_Task_Handler
   is
   begin
      return T.Scheduler_Queue.Previous;
   end Get_Prev_In_Queue;

   procedure Set_Next_In_Queue (T, Next : Oak_Task_Handler) is
   begin
      T.Scheduler_Queue.Next := Next;
   end Set_Next_In_Queue;

   procedure Set_Prev_In_Queue (T, Prev : Oak_Task_Handler) is
   begin
      T.Scheduler_Queue.Previous := Prev;
   end Set_Prev_In_Queue;

   function Get_Next_Agent (T : Oak_Task_Handler) return Oak_Task_Handler is
   begin
      return T.Next_Agent;
   end Get_Next_Agent;

   procedure Set_Next_Agent
     (T          : in Oak_Task_Handler;
      Next_Agent : in Oak_Task_Handler)
   is
   begin
      T.Next_Agent := Next_Agent;
   end Set_Next_Agent;

   function Get_Desired_Run_Time (Agent : Oak_Task_Handler) return Time is
   begin
      return Agent.Desired_Agent_Run_Time;
   end Get_Desired_Run_Time;

   procedure Set_Desired_Run_Time
     (Agent    : Oak_Task_Handler;
      Run_Time : Time)
   is
   begin
      Agent.Desired_Agent_Run_Time := Run_Time;
   end Set_Desired_Run_Time;

end Oak.Oak_Task.Scheduler_Agent;
