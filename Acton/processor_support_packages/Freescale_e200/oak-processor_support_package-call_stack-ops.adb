with System.Machine_Code; use System.Machine_Code;

package body Oak.Processor_Support_Package.Call_Stack.Ops is
   use System.Storage_Elements;
   ----------------------------------
   -- Set_Task_Instruction_Pointer --
   ----------------------------------

   procedure Set_Task_Instruction_Pointer
     (Stack               : in Oak.Memory.Call_Stack.Call_Stack_Handler;
      Instruction_Address : in System.Address)
   is
      use System;
   begin
      Asm
        ("mr  r14, %0" & ASCII.LF & ASCII.HT & "stw %1, 0(r14)",
         Inputs   => (Address'Asm_Input ("r", Stack.Pointer),
                      Address'Asm_Input ("r", Instruction_Address)),
         Clobber  => "r14",
         Volatile => True);
   end Set_Task_Instruction_Pointer;

   procedure Initialise_Call_Stack
     (Stack             : in out Oak.Memory.Call_Stack.Call_Stack_Handler;
      Start_Instruction : in System.Address)
   is
   begin
      Stack.Pointer := Stack.Pointer - Task_Registers_Save_Size;
      Set_Task_Instruction_Pointer
        (Stack               => Stack,
         Instruction_Address => Start_Instruction);
   end Initialise_Call_Stack;

   procedure Initialise_Call_Stack
     (Stack             : in out Oak.Memory.Call_Stack.Call_Stack_Handler;
      Start_Instruction : in System.Address;
      Stack_Address     : in System.Address;
      Stack_Size        : in System.Storage_Elements.Storage_Count)
   is
   begin
      Stack.Top     := Stack_Address +  Stack_Size / Bits_In_A_Byte;
      Stack.Pointer := Stack.Top;
      Stack.Bottom  := Stack_Address;

      Initialise_Call_Stack
        (Stack             => Stack,
         Start_Instruction => Start_Instruction);
   end Initialise_Call_Stack;

end Oak.Processor_Support_Package.Call_Stack.Ops;
