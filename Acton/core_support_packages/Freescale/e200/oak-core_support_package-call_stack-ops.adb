------------------------------------------------------------------------------
--                                                                          --
--                         OAK CORE SUPPORT PACKAGE                         --
--                              FREESCALE e200                              --
--                                                                          --
--                 OAK.CORE_SUPPORT_PACKAGE.CALL_STACK.OPS                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                 Copyright (C) 2010-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

with System.Machine_Code; use System.Machine_Code;

package body Oak.Core_Support_Package.Call_Stack.Ops is
   use System.Storage_Elements;

   ----------------------------------
   -- Set_Task_Instruction_Pointer --
   ----------------------------------

   procedure Set_Task_Instruction_Pointer
     (Stack               : in Call_Stack_Handler;
      Instruction_Address : in System.Address) is
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
     (Stack             : in Call_Stack_Handler;
      Procedure_Address : in System.Address;
      Task_Value_Record : in System.Address) is
   begin
      Asm
        ("mr  r14, %0"       & ASCII.LF & ASCII.HT &
         "stw %1, 0(r14)"    & ASCII.LF & ASCII.HT &
         "addi r14, r14, 40" & ASCII.LF & ASCII.HT &
         "evstdd %2, 224(r14)", -- sets r3 to the memory address of the TSR
         Inputs   => (Address'Asm_Input ("r", Stack.Pointer),
                      Address'Asm_Input ("r", Procedure_Address),
                      Address'Asm_Input ("r", Task_Value_Record)),
         Clobber  => "r14",
         Volatile => True);
   end Set_Task_Body_Procedure;

end Oak.Core_Support_Package.Call_Stack.Ops;
