------------------------------------------------------------------------------
--                                                                          --
--                              OAK COMPONENTS                              --
--                                                                          --
--                           OAK.AGENT.SCHEDULERS                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                 Copyright (C) 2010-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

with Oak.States;   use Oak.States;
with Oak.Core;

package body Oak.Agent.Schedulers is

   -------------------------
   -- New_Scheduler_Agent --
   -------------------------

   procedure New_Scheduler_Agent
     (Agent                 : out Scheduler_Id;
      Name                  : in  String;
      Call_Stack_Size       : in  Storage_Count;
      Run_Loop              : in  Address;
      Lowest_Priority       : in  Any_Priority;
      Highest_Priority      : in  Any_Priority;
      Scheduler_Agent       : in  Scheduler_Id_With_No := No_Agent;
      When_To_Charge_Agent  : in  Charge_Occurrence := Only_While_Running;
      Interpret_No_Agent_As : in  No_Agent_Interpretation := As_Is;
      Charge_While_No_Agent : in  Boolean := False;
      Agent_Active_Till     : in  Active_Till := Always_Active;
      Cycle_Period          : in  Oak_Time.Time_Span := Time_Span_Last;
      Cycle_Phase           : in  Oak_Time.Time_Span := Time_Span_Zero;
      Relative_Deadline     : in  Oak_Time.Time_Span := Time_Span_Last;
      Execution_Budget      : in  Oak_Time.Time_Span := Time_Span_Last)
   is
      WT : Time;
   begin
      Allocate_An_Agent (Agent);

      if Scheduler_Agent = No_Agent then
         WT := Time_Zero;
      else
         WT := Core.Global_Start_Time + Cycle_Phase;
      end if;

      New_Agent
        (Agent                => Agent,
         Name                 => Name,
         Call_Stack_Address   => Null_Address,
         Call_Stack_Size      => Call_Stack_Size,
         Run_Loop             => Run_Loop,
         Run_Loop_Parameter   => Null_Address,
         Normal_Priority      => Highest_Priority,
         Initial_State        => Not_Initialised,
         Scheduler_Agent      => Scheduler_Agent,
         Wake_Time            => WT,
         When_To_Charge_Agent => When_To_Charge_Agent);

      Setup_Scheduler_Agent : declare
         S : Scheduler_Agent_Record renames Agent_Pool (Agent);
      begin
         S.Lowest_Priority       := Lowest_Priority;
         S.Highest_Priority      := Highest_Priority;
         S.Agent_To_Run          := No_Agent;
         S.Interpret_No_Agent_As := Interpret_No_Agent_As;
         S.Charge_While_No_Agent := Charge_While_No_Agent;
         S.Agent_Active_Till     := Agent_Active_Till;
         S.Period                := Cycle_Period;
         S.Phase                 := Cycle_Phase;
         S.Relative_Deadline     := Relative_Deadline;
         S.Execution_Budget      := Execution_Budget;

         New_Scheduler_Timer
           (Timer     => S.Run_Timer,
            Priority  => Highest_Priority,
            Scheduler => Agent,
            Fire_Time => Oak_Time.Time_Zero,
            Activate  => False);

      end Setup_Scheduler_Agent;
   end New_Scheduler_Agent;

   ----------------------
   -- Set_Agent_To_Run --
   ----------------------

   procedure Set_Agent_To_Run
     (For_Agent    : in Scheduler_Id;
      Agent_To_Run : in Oak_Agent_Id)
   is
   begin
      Agent_Pool (For_Agent).Agent_To_Run := Agent_To_Run;
   end Set_Agent_To_Run;

   -------------------------------
   -- Set_Next_Cycle_Start_Time --
   -------------------------------

   procedure Set_Next_Cycle_Start_Time
     (Scheduler  : in Scheduler_Id;
      Start_Time : in Oak_Time.Time) is
   begin
      Agent_Pool (Scheduler).Next_Run_Cycle := Start_Time;
   end Set_Next_Cycle_Start_Time;

   ------------------------
   -- Set_Priority_Range --
   ------------------------

   procedure Set_Priority_Range
     (Agent : in Scheduler_Id;
      From  : in Any_Priority;
      To    : in Any_Priority)
   is
   begin
      Agent_Pool (Agent).Lowest_Priority := From;
      Agent_Pool (Agent).Highest_Priority := To;
   end Set_Priority_Range;

end Oak.Agent.Schedulers;
