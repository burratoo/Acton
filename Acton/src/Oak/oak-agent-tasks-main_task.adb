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
      Current_Time : constant Time                      := Clock;
      No_Chain : Activation_Chain := (Head => null);
   begin
      Initialise_Task_Agent
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
         Budget_Action     => Ada.Cyclic_Tasks.No_Response,
         Budget_Handler    => null,
         Relative_Deadline => Oak_Time.Time_Span_Last,
         Deadline_Action   => Ada.Cyclic_Tasks.No_Response,
         Deadline_Handler  => null,
         Scheduler_Agent   => null,
         Chain             => No_Chain,
         Elaborated        => null);

      Agent.State           := Sleeping;
      Agent.Next_Run_Cycle  := Current_Time;
      Agent.Wake_Time       := Current_Time;

      Add_Agent_To_Scheduler (Agent);
   end Initialise_Main_Task;
end Oak.Agent.Tasks.Main_Task;
