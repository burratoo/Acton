with Oak.Core_Support_Package.Call_Stack;
with Oak.Memory.Call_Stack.Ops;
with Oak.Core_Support_Package.Task_Support;
use Oak.Core_Support_Package.Task_Support;

package body Oak.Agent.Sleep is
   procedure Initialise_Sleep_Agent (Agent : access Oak_Agent'Class) is
   begin
      Initialise_Agent (Agent, "Sleep Task",  No_Call_Stack);
      Oak.Memory.Call_Stack.Ops.Allocate_Call_Stack
        (Stack => Agent.Call_Stack,
         Size_In_Elements => Core_Support_Package.Call_Stack.Sleep_Stack_Size);
      Oak.Memory.Call_Stack.Ops.Initialise_Call_Stack
        (Stack             => Agent.Call_Stack,
         Start_Instruction => Sleep_Agent'Address);
   end Initialise_Sleep_Agent;

end Oak.Agent.Sleep;
