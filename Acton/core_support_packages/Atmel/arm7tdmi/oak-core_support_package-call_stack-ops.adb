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
        --  Calculate the start of the agent's stack
        ("mov %0, r5" & ASCII.LF & ASCII.HT &
         "sub r5, r5, #72" & ASCII.LF & ASCII.HT & -- 8-byte alignment
         "mov r6, %1"      & ASCII.LF & ASCII.HT &
         --  Default CPSR value - User mode, all interrupts enabled
         "mov r7, #0x10" & ASCII.LF & ASCII.HT &
         --  Store the new stack pointer, link register and agent CPSR onto
         --  the agents register store (represented by Stack.Pointer)
         "stm  %0, {r5 - r6}",
         Outputs  => Address'Asm_Output ("+r", Stack.Pointer),
         Inputs   => Address'Asm_Input ("r", Instruction_Address),
         Volatile => True,
         Clobber  => "r5, r6, r7");
   end Set_Task_Instruction_Pointer;

   procedure Set_Task_Body_Procedure
     (Stack             : in out Call_Stack_Handler;
      Procedure_Address : in System.Address;
      Task_Value_Record : in System.Address) is
   begin
      Asm
      --  Calculate the start of the agent's stack
        ("mov %0, r5" & ASCII.LF & ASCII.HT &
         "sub r5, r5, #72" & ASCII.LF & ASCII.HT & -- 8-byte alignment
         --  Default CPSR value - User mode, all interrupts enabled
         "mov r6, #0x10" & ASCII.LF & ASCII.HT &
         --  Store the new stack pointer, procedure argument, link register
         --  and agent CPSR onto the agents register store (represented by
         --  Stack.Pointer)
         "stm  %0, {%2}" & ASCII.LF & ASCII.HT & -- r0 = Task_Value_Record
         "sub %0, %0, #52" & ASCII.LF & ASCII.HT &
         "stm %0, {r5}"    & ASCII.LF & ASCII.HT & -- r13 = agent sp
         "sub %0, %0, #4"  & ASCII.LF & ASCII.HT & -- skip r14
         "stm %0, {%1, r6}", -- Procedure start address and CPSR
         Outputs  => Address'Asm_Output ("+r", Stack.Pointer),
         Inputs   => (Address'Asm_Input ("r", Procedure_Address),
                      Address'Asm_Input ("r", Task_Value_Record)),
         Volatile => True,
         Clobber  => "r5, r6");
   end Set_Task_Body_Procedure;

end Oak.Core_Support_Package.Call_Stack.Ops;
