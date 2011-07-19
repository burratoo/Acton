with Oak.Memory.Call_Stack;
with System;
with System.Storage_Elements;

package Oak.Processor_Support_Package.Call_Stack.Ops is

   pragma Preelaborate;

   procedure Set_Task_Instruction_Pointer
     (Stack               : in Oak.Memory.Call_Stack.Call_Stack_Handler;
      Instruction_Address : in System.Address);
   pragma Inline (Set_Task_Instruction_Pointer);

   procedure Initialise_Call_Stack
     (Stack             : in out Oak.Memory.Call_Stack.Call_Stack_Handler;
      Start_Instruction : in System.Address);

   procedure Initialise_Call_Stack
     (Stack             : in out Oak.Memory.Call_Stack.Call_Stack_Handler;
      Start_Instruction : in System.Address;
      Stack_Address     : in System.Address;
      Stack_Size        : in System.Storage_Elements.Storage_Count);

end Oak.Processor_Support_Package.Call_Stack.Ops;
