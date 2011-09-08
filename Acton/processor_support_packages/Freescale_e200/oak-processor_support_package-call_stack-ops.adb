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
        ("mr  r14, %0" & ASCII.LF & ASCII.HT &
         "stw %1, 0(r14)",
         Inputs   => (Address'Asm_Input ("r", Stack.Pointer),
                      Address'Asm_Input ("r", Instruction_Address)),
         Clobber  => "r14",
         Volatile => True);
   end Set_Task_Instruction_Pointer;

   procedure Set_Task_Body_Procedure
     (Stack             : in Oak.Memory.Call_Stack.Call_Stack_Handler;
      Procedure_Address : in System.Address;
      Task_Value_Record : in System.Address)
   is
      use System;
   begin
      Asm
        ("mr  r14, %0"       & ASCII.LF & ASCII.HT &
         "stw %1, 0(r14)"    & ASCII.LF & ASCII.HT &
         "addi r14, r14, 40" & ASCII.LF & ASCII.HT &
         "stw %2, 200(r14)",
         Inputs   => (Address'Asm_Input ("r", Stack.Pointer),
                      Address'Asm_Input ("r", Procedure_Address),
                      Address'Asm_Input ("r", Task_Value_Record)),
         Clobber  => "r14",
         Volatile => True);
   end Set_Task_Body_Procedure;

   procedure Initialise_Call_Stack
     (Stack             : in out Oak.Memory.Call_Stack.Call_Stack_Handler;
      Start_Instruction : in System.Address;
      Task_Value_Record : in System.Address)
   is
   begin
      Stack.Pointer := Stack.Pointer - Task_Registers_Save_Size;
      Set_Task_Body_Procedure
        (Stack               => Stack,
         Procedure_Address => Start_Instruction,
         Task_Value_Record    => Task_Value_Record);
   end Initialise_Call_Stack;

   procedure Initialise_Call_Stack
     (Stack             : in out Oak.Memory.Call_Stack.Call_Stack_Handler;
      Start_Instruction : in System.Address;
      Task_Value_Record : in System.Address;
      Stack_Address     : in System.Address;
      Stack_Size        : in System.Storage_Elements.Storage_Count)
   is
   begin
      Stack.Top     := Stack_Address +  Stack_Size;
      Stack.Pointer := Stack.Top;
      Stack.Bottom  := Stack_Address;

      Initialise_Call_Stack
        (Stack             => Stack,
         Start_Instruction => Start_Instruction,
         Task_Value_Record    => Task_Value_Record);
   end Initialise_Call_Stack;

end Oak.Processor_Support_Package.Call_Stack.Ops;
