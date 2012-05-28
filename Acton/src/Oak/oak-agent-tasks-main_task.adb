with Oak.Core;
with Oak.Scheduler; use Oak.Scheduler;

package body Oak.Agent.Tasks.Main_Task is
   procedure Initialise_Main_Task
     (Stack_Size      : in System.Storage_Elements.Storage_Count;
      Name            : in String;
      Normal_Priority : in Integer;
      Run_Loop        : in Address)
   is
      Agent : constant access Task_Agent  := Core.Main_Task;
      OI : constant access Core.Oak_Data := Core.Oak_Instance;

      Scheduler    : constant access Oak_Scheduler_Info :=
                       Oak.Core.Scheduler_Info (OI);
      Current_Time : constant Time                      := Clock;
      No_Chain : Activation_Chain := (Head => null);
   begin
      Initialise_Task_Agent
        (Agent             => Agent,
         Stack_Address     => Null_Address,
         Stack_Size        => Stack_Size,
         Name              => Name,
         Normal_Priority   => Normal_Priority,
         Relative_Deadline => Time_Span_Zero,
         Cycle_Period      => Time_Span_Zero,
         Phase             => Time_Span_Zero,
         Run_Loop          => Run_Loop,
         Task_Value_Record => Null_Address,
         Chain             => No_Chain,
         Elaborated        => null);

      Agent.State           := Sleeping;
      Agent.Next_Deadline   := Time_Last;
      Agent.Next_Run_Cycle  := Current_Time;
      Agent.Wake_Time       := Current_Time;

      Add_Task_To_Scheduler (Scheduler_Info => Scheduler.all, T => Agent);
   end Initialise_Main_Task;
end Oak.Agent.Tasks.Main_Task;
