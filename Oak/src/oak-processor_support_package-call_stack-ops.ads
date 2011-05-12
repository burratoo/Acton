with Oak.Memory.Call_Stack;
with System;

package Oak.Processor_Support_Package.Call_Stack.Ops is

   pragma Preelaborate;

   procedure Set_Task_Instruction_Pointer
     (Stack  : in Oak.Memory.Call_Stack.Call_Stack_Handler;
      Instruction_Address : in System.Address);
   pragma Inline (Set_Task_Instruction_Pointer);

   procedure Initialise_Call_Stack
     (Stack : in out Oak.Memory.Call_Stack.Call_Stack_Handler;
      Start_Instruction  : in System.Address);

end Oak.Processor_Support_Package.Call_Stack.Ops;
