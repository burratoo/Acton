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

with Oak.Oak_Time; use Oak.Oak_Time;
with Oak.States;   use Oak.States;

package body Oak.Agent.Schedulers is

   -------------------------
   -- New_Scheduler_Agent --
   -------------------------

   procedure New_Scheduler_Agent
     (Agent                : out Scheduler_Id;
      Name                 : in  String;
      Call_Stack_Size      : in  Storage_Count;
      Run_Loop             : in  Address;
      Lowest_Priority      : in  Any_Priority;
      Highest_Priority     : in  Any_Priority;
      When_To_Charge_Agent : in  Charge_Occurrence := All_Priorities)
   is
   begin
      Allocate_An_Agent (Agent);

      New_Agent
        (Agent                => Agent,
         Name                 => Name,
         Call_Stack_Address   => Null_Address,
         Call_Stack_Size      => Call_Stack_Size,
         Run_Loop             => Run_Loop,
         Run_Loop_Parameter   => Null_Address,
         Normal_Priority      => Highest_Priority,
         Initial_State        => Selecting_Next_Agent,
         Wake_Time            => Oak_Time.Time_Zero,
         When_To_Charge_Agent => When_To_Charge_Agent);

      Setup_Scheduler_Agent : declare
         S : Scheduler_Agent_Record renames Agent_Pool (Agent);
      begin
         S.Lowest_Priority := Lowest_Priority;
         S.Highest_Priority := Highest_Priority;
         S.Agent_To_Run := No_Agent;

         New_Scheduler_Timer
           (Timer     => S.Run_Timer,
            Priority  => Highest_Priority,
            Scheduler => Agent,
            Fire_Time => Oak_Time.Time_Zero,
            Activate  => True);
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
