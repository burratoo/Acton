with Oak.Memory.Call_Stack.Ops; use Oak.Memory.Call_Stack.Ops;

package body Oak.Agent.Schedulers is

   procedure Initialise_Scheduler_Agent
     (Agent           : access Scheduler_Agent'Class;
      Name            : in String;
      Call_Stack_Size : in System.Storage_Elements.Storage_Count;
      Run_Loop        : in System.Address)
   is
      Call_Stack : Oak.Memory.Call_Stack.Call_Stack_Handler;
   begin
      Oak.Memory.Call_Stack.Ops.Allocate_Call_Stack
        (Stack            => Call_Stack,
         Size_In_Elements => Call_Stack_Size);

      Oak.Agent.Initialise_Agent
        (Agent      => Agent,
         Name       => Name,
         Call_Stack => Call_Stack);

      Agent.Set_State (Selecting_Next_Agent);

      Agent.Run_Timer.Set_Timer
        (Priority  => Oak_Interrupt_Priority'Last,
         Scheduler => Agent);
      Agent.Run_Timer.Add_Timer_To_Current_Processor;

      Initialise_Call_Stack
        (Stack             => Agent.Call_Stack,
         Start_Instruction => Run_Loop,
         Task_Value_Record => Agent.all'Address,
         Message_Location  => Agent.Message_Store);

      Agent.Wake_Time := Oak_Time.Time_Zero;
   end Initialise_Scheduler_Agent;

end Oak.Agent.Schedulers;
