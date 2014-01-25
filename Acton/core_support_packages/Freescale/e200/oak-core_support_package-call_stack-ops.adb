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
     (Stack               : in out Call_Stack_Handler;
      Instruction_Address : in System.Address) is
   begin
      Asm
        ("stwu %0, -4(%0)" & ASCII.LF & ASCII.HT & -- Allocate stack space
         "stw  %1,  0(%0)",                        -- Store instr. address
         Outputs  => Address'Asm_Output ("+r", Stack.Pointer),
         Inputs   => Address'Asm_Input ("r", Instruction_Address),
         Volatile => True);
   end Set_Task_Instruction_Pointer;

   procedure Set_Task_Body_Procedure
     (Stack             : in out Call_Stack_Handler;
      Procedure_Address : in System.Address;
      Task_Value_Record : in System.Address) is
   begin
      Asm
        --  Allocate room for GPR3 which is at the top of the register file.
        ("stwu %0,   -8(%0)" & ASCII.LF & ASCII.HT &
         "stw  %2,    0(%0)" & ASCII.LF & ASCII.HT &
         --  Allocate room for the rest of the register file
         "stwu %0, -200(%0)" & ASCII.LF & ASCII.HT &
         "stwu %0,  -80(%0)" & ASCII.LF & ASCII.HT &
         --  With the instruction pointer going right at the bottom.
         "stw  %1,    0(%0)",
         Outputs  => Address'Asm_Output ("+r", Stack.Pointer),
         Inputs   => (Address'Asm_Input ("r", Procedure_Address),
                      Address'Asm_Input ("r", Task_Value_Record)),
         Volatile => True);
   end Set_Task_Body_Procedure;

end Oak.Core_Support_Package.Call_Stack.Ops;
