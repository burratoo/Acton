------------------------------------------------------------------------------
--                                                                          --
--                              OAK COMPONENTS                              --
--                                                                          --
--                         OAK.AGENT.TASKS.MAIN_TASK                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                 Copyright (C) 2012-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

with Oak.Agent.Oak_Agent; use Oak.Agent.Oak_Agent;
with Oak.Scheduler;       use Oak.Scheduler;
with Oak.States;          use Oak.States;

package body Oak.Agent.Tasks.Main_Task is

   ---------------------
   -- Setup_Main_Task --
   ---------------------

   procedure Setup_Main_Task
     (Agent           : out Task_Id;
      Stack_Size      : in  Storage_Count;
      Name            : in  String;
      Normal_Priority : in  Integer;
      Run_Loop        : in  Address)
   is
      No_Chain : Task_List := No_Agent;
   begin
      New_Task_Agent
        (Agent             => Agent,
         Stack_Address     => Null_Address,
         Stack_Size        => Stack_Size,
         Name              => Name,
         Run_Loop          => Run_Loop,
         Task_Value_Record => Null_Address,
         Normal_Priority   => Normal_Priority,
         Cycle_Behaviour   => Ada.Cyclic_Tasks.Normal,
         Cycle_Period      => Oak_Time.Time_Span_Last,
         Phase             => Oak_Time.Time_Span_Zero,
         Execution_Budget  => Oak_Time.Time_Span_Last,
         Budget_Response   => Ada.Cyclic_Tasks.No_Response,
         Budget_Handler    => null,
         Relative_Deadline => Oak_Time.Time_Span_Last,
         Deadline_Response => Ada.Cyclic_Tasks.No_Response,
         Deadline_Handler  => null,
         Chain             => No_Chain);

      Set_State     (For_Agent => Agent, State => Sleeping);
      Set_Wake_Time (For_Agent => Agent, Wake_Time => Clock);

      Add_Agent_To_Scheduler (Agent);
   end Setup_Main_Task;
end Oak.Agent.Tasks.Main_Task;
