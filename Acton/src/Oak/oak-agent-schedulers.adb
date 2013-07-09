with Oak.Memory.Call_Stack.Ops; use Oak.Memory.Call_Stack.Ops;

package body Oak.Agent.Schedulers is

   procedure Initialise_Scheduler_Agent
     (Agent           : access Scheduler_Agent'Class;
      Name            : in String;
      Call_Stack_Size : in System.Storage_Elements.Storage_Count;
      Run_Loop        : in System.Address) is
   begin

      Oak.Agent.Initialise_Agent
        (Agent      => Agent,
         Name       => Name,
         Call_Stack_Size => Call_Stack_Size);

      Agent.Set_State (Selecting_Next_Agent);

      Agent.Wake_Time       := Oak_Time.Time_Zero;
      Agent.Normal_Priority := Agent.Highest_Prioirty;

      Agent.Run_Timer.Set_Timer
        (Priority  => Agent.Normal_Priority,
         Fire_Time => Agent.Wake_Time,
         Scheduler => Agent);
      Agent.Run_Timer.Add_Timer_To_Current_Processor;

      Initialise_Call_Stack
        (Stack             => Agent.Call_Stack,
         Start_Instruction => Run_Loop,
         Task_Value_Record => Agent.all'Address,
         Message_Location  => Agent.Message_Store);
   end Initialise_Scheduler_Agent;

end Oak.Agent.Schedulers;
