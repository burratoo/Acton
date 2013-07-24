package body Oak.Agent.Schedulers is

   procedure Initialise_Scheduler_Agent
     (Agent                : access Scheduler_Agent'Class;
      Name                 : in String;
      Call_Stack_Size      : in Storage_Elements.Storage_Count;
      Run_Loop             : in Address;
      When_To_Charge_Agent : in Charge_Occurrence := All_Priorities) is
   begin

      Oak.Agent.Initialise_Agent
        (Agent               => Agent,
         Name                => Name,
         Call_Stack_Address  => Null_Address,
         Call_Stack_Size     => Call_Stack_Size,
         Run_Loop            => Run_Loop,
         Run_Loop_Parameter => Agent.all'Address,
         Normal_Priority     => Agent.Highest_Priority,
         Initial_State       => Selecting_Next_Agent,
         Wake_Time           => Oak_Time.Time_Zero,
        When_To_Charge_Agent => When_To_Charge_Agent);

      Agent.Run_Timer.Set_Timer
         (Priority  => Agent.Normal_Priority,
          Fire_Time => Agent.Wake_Time,
          Scheduler => Agent);
      Agent.Run_Timer.Add_Timer_To_Current_Processor;

   end Initialise_Scheduler_Agent;

   procedure Set_Agent_To_Run
     (Agent        : in out Scheduler_Agent'Class;
      Agent_To_Run : access Oak_Agent'Class) is
   begin
      Agent.Agent_To_Run := Agent_To_Run;
   end Set_Agent_To_Run;

   procedure Set_Priority_Range
     (Agent : in out Scheduler_Agent'Class;
      From  : in Any_Priority;
      To    : in Any_Priority) is
   begin
      Agent.Lowest_Priority := From;
      Agent.Highest_Priority := To;
   end Set_Priority_Range;

end Oak.Agent.Schedulers;
