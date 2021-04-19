------------------------------------------------------------------------------------------
--                                                                                      --
--                               OAK CORE SUPPORT PACKAGE                               --
--                                    ARM CORTEX M4F                                    --
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
        --  Load 0 into r2 which is unused
        ("mov r2, #0" & ASCII.LF & ASCII.HT &
         --  ARMv7-M automatic stack frame is 32 bytes large
         "sub %0, #32"     & ASCII.LF & ASCII.HT &
         --  Load PC and clear LR and xPSR
         --  Clearing LR prevents GDB from panicking.
         "str %1, [%0, #24]"  & ASCII.LF & ASCII.HT & -- PC
         "str r2, [%0, #20]"  & ASCII.LF & ASCII.HT & -- LR
         "mov r2, #0x01000000" & ASCII.LF & ASCII.HT & -- xPSR default value
         "str r2, [%0, #28]"  & ASCII.LF & ASCII.HT & -- xPSR
         --  Store exe_return value (goes at the bottom of the stack)
         --  Using 0xFFFFFFFD as there is no floating point registers and
         --  agents work in tasking mode using PSP.
           "mvn r2, #0x2"    & ASCII.LF & ASCII.HT &
           "str r2, [%0, #-4]!",
         Outputs  => Address'Asm_Output ("+r", Stack.Pointer),
         Inputs   => Address'Asm_Input ("r", Instruction_Address),
         Clobber  => "r2",
         Volatile => True);
   end Set_Task_Instruction_Pointer;

   procedure Set_Task_Body_Procedure
     (Stack             : in out Call_Stack_Handler;
      Procedure_Address : in System.Address;
      Task_Value_Record : in System.Address) is
   begin
      Asm
      --  Load 0 into r2 which is unused
        ("mov r2, #0"           & ASCII.LF & ASCII.HT &
         --  ARMv7-M automatic stack frame is 32 bytes large
         "sub %0, #32"        & ASCII.LF & ASCII.HT &
         --  Load PC, R0 and clear LR and xPSR
         --  Clearing LR prevents GDB from panicking.
         "str %1, [%0, #24]"  & ASCII.LF & ASCII.HT & -- PC
         "str r2, [%0, #20]"  & ASCII.LF & ASCII.HT & -- LR
         "mov r2, #0x01000000" & ASCII.LF & ASCII.HT & -- xPSR default value
         "str r2, [%0, #28]"  & ASCII.LF & ASCII.HT & -- xPSR
         "str %2, [%0, #0]"   & ASCII.LF & ASCII.HT & -- r0
         --  Rest of stack (36 bytes)
         "sub %0, #36"        & ASCII.LF & ASCII.HT &
         --  Store exe_return value (goes at the bottom of the stack)
         --  Using 0xFFFFFFFD as there is no floating point registers and
         --  task agents work in tasking mode using PSP.
         "mvn r2, #0x2"    & ASCII.LF & ASCII.HT &
         "str r2, [%0, #0]",
         Outputs  => Address'Asm_Output ("+r", Stack.Pointer),
         Inputs   => (Address'Asm_Input ("r", Procedure_Address),
                      Address'Asm_Input ("r", Task_Value_Record)),
         Clobber => "r2",
         Volatile => True);
   end Set_Task_Body_Procedure;

end Oak.Core_Support_Package.Call_Stack.Ops;
