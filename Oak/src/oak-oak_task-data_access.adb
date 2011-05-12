with Oak.Processor_Support_Package.Call_Stack.Ops;
use  Oak.Processor_Support_Package.Call_Stack.Ops;

with Oak.Oak_Task.Internal;
with Ada.Real_Time;

package body Oak.Oak_Task.Data_Access is

   -----------------
   -- Set_Up_Task --
   ------------------
   procedure Initialise_Task
     (T               : access Oak_Task;
      Name            : in Task_Name;
      Normal_Priority : Priority;
      Deadline        : Time_Span;
      Cycle_Period    : Time_Span;
      Phase           : Time_Span;
      Run_Loop        : Address;
      Stack_Access    : Call_Stack_Handler)
   is
   begin
      T.all :=
        (Kind            => Regular,
         Id              => Internal.New_Task_Id,
         Name            => Name,
         State           => Sleeping,
         Normal_Priority => Normal_Priority,
         Deadline        => Deadline,
         Cycle_Period    => Cycle_Period,
         Phase           => Phase,
         Next_Deadline   => Time_Last,
         Next_Run_Cycle  => Time_Zero + Ada.Real_Time.Seconds (1) + Cycle_Period,
         Wake_Time       => Time_Zero + Ada.Real_Time.Seconds (1),
         Run_Loop        => Run_Loop,
         Call_Stack      => Stack_Access,
         Scheduler_Agent => null,
         Scheduler_Queue => (null, null),
         Deadline_List   => (null, null),
         Memory_List     => null);
      Initialise_Call_Stack
        (Stack             => T.Call_Stack,
         Start_Instruction => Run_Loop);
   end Initialise_Task;

   -------------------------
   -- Get_Normal_Priority --
   -------------------------

   procedure Set_Scheduler_Agent
     (T               : access Oak_Task;
      Scheduler_Agent : in Oak_Task_Handler)
   is
   begin
      T.Scheduler_Agent := Scheduler_Agent;
   end Set_Scheduler_Agent;

   ------------
   -- Get_Id --
   ------------

   function Get_Id (T : access Oak_Task) return Task_Id is
   begin
      return T.Id;
   end Get_Id;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (T : access Oak_Task) return Task_Name is
   begin
      return T.Name;
   end Get_Name;

   ---------------
   -- Get_State --
   ---------------

   function Get_State (T : access Oak_Task) return Task_State is
   begin
      return T.State;
   end Get_State;

   procedure Set_State (T : access Oak_Task; State : Task_State) is
   begin
      T.State := State;
   end Set_State;

   -------------------------
   -- Get_Normal_Priority --
   -------------------------

   function Get_Normal_Priority (T : access Oak_Task) return Priority is
   begin
      return T.Normal_Priority;
   end Get_Normal_Priority;

   ------------------
   -- Get_Deadline --
   ------------------

   function Get_Deadline (T : access Oak_Task) return Time_Span is
   begin
      return T.Deadline;
   end Get_Deadline;

   ----------------------
   -- Get_Cycle_Period --
   ----------------------

   function Get_Cycle_Period (T : access Oak_Task) return Time_Span is
   begin
      return T.Cycle_Period;
   end Get_Cycle_Period;

   ---------------------
   -- Get_Start_Delay --
   ---------------------

   function Get_Phase (T : access Oak_Task) return Time_Span is
   begin
      return T.Phase;
   end Get_Phase;

   function Get_Next_Run_Time (T : access Oak_Task) return Time is
   begin
      return T.Next_Run_Cycle;
   end Get_Next_Run_Time;

   function Get_Wake_Time (T : access Oak_Task) return Time is
   begin
      return T.Wake_Time;
   end Get_Wake_Time;

   procedure Set_Wake_Time (T : access Oak_Task; WT : Time) is
   begin
      T.Wake_Time := WT;
   end Set_Wake_Time;

end Oak.Oak_Task.Data_Access;
