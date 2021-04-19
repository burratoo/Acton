------------------------------------------------------------------------------------------
--                                                                                      --
--                               OAK CORE SUPPORT PACKAGE                               --
--                                     ARM ARM7TDMI                                     --
--                                                                                      --
--                        OAK.CORE_SUPPORT_PACKAGE.CALL_STACK.OPS                       --
--                                                                                      --
--                       Copyright (C) 2014-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

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
        ("mov r6, %0" & ASCII.LF & ASCII.HT &
         --  8-byte alignment. Holds full frame
         "sub r6, #72"     & ASCII.LF & ASCII.HT &
         "mov r4, #0"      & ASCII.LF & ASCII.HT &   --  Agent fp
         "mov r5, #0"      & ASCII.LF & ASCII.HT &   --  Agent ip
         "mov r7, #0"      & ASCII.LF & ASCII.HT &   --  Agent lr
         --  Default CPSR value - User mode, all interrupts enabled
         "mov r8, #0x10"   & ASCII.LF & ASCII.HT &
         "mov r9, %1"      & ASCII.LF & ASCII.HT &   --  Agent first instr.
         --  Stack.Pointer lives here
         "sub %0, #60"     & ASCII.LF & ASCII.HT &
         --  Store the agent's fp, sp, lr, initial address and agent CPSR onto
         --  the agents register store (represented by Stack.Pointer)
         "stm   %0, {r4 - r7}" & ASCII.LF & ASCII.HT &
         "stmdb %0,  {r8 - r9}",
         Outputs  => Address'Asm_Output ("+r", Stack.Pointer),
         Inputs   => Address'Asm_Input ("r", Instruction_Address),
         Volatile => True,
         Clobber  => "r4, r5, r6, r7, r8, r9");
   end Set_Task_Instruction_Pointer;

   procedure Set_Task_Body_Procedure
     (Stack             : in out Call_Stack_Handler;
      Procedure_Address : in System.Address;
      Task_Value_Record : in System.Address) is
   begin
      Asm
      --  Calculate the start of the agent's stack
        ("mov r5, %0"       & ASCII.LF & ASCII.HT &
         "sub r5, #72"      & ASCII.LF & ASCII.HT & -- 8-byte alignment
         --  Default CPSR value - User mode, all interrupts enabled
         "mov r6, #0x10"    & ASCII.LF & ASCII.HT &
         "mov r4, #0"       & ASCII.LF & ASCII.HT & -- fp, lr
         "sub %0, #60"      & ASCII.LF & ASCII.HT &
         --  Store the stack pointer, procedure argument, link register
         --  and agent CPSR onto the agents register store (represented by
         --  Stack.Pointer).
         "str %2, [%0]"      & ASCII.LF & ASCII.HT & -- r0 = Task_Value_Record
         "str r4, [%0, #44]" & ASCII.LF & ASCII.HT & -- r11 = agent fp
         "str r5, [%0, #52]" & ASCII.LF & ASCII.HT & -- r13 = agent sp
         "str r4, [%0, #56]" & ASCII.LF & ASCII.HT & -- r14 = agent lr
         --  Agent start addr. 4 bytes need to be added on the address since
         --  the full agent switch will remove these 4 bytes (Needed since
         --  the full switch is normally used when the agent is interrupts
         --  through an IRQ or FIQ interrupt. The return instructions from
         --  these interrupts need to remove the 4 bytes that where added to
         --  the pc in the course of taking these interrupts.
         "add r7, %1, #4"    & ASCII.LF & ASCII.HT &
         "stmdb %0, {r6, r7}", -- Procedure start address and CPSR
         Outputs  => Address'Asm_Output ("+r", Stack.Pointer),
         Inputs   => (Address'Asm_Input ("r", Procedure_Address),
                      Address'Asm_Input ("r", Task_Value_Record)),
         Volatile => True,
         Clobber  => "r4, r5, r6, r7");
   end Set_Task_Body_Procedure;

end Oak.Core_Support_Package.Call_Stack.Ops;
