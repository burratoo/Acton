------------------------------------------------------------------------------------------
--                                                                                      --
--                               OAK CORE SUPPORT PACKAGE                               --
--                                    FREESCALE e200                                    --
--                                                                                      --
--                        OAK.CORE_SUPPORT_PACKAGE.CALL_STACK.OPS                       --
--                                                                                      --
--                       Copyright (C) 2010-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

with Oak.Memory.Call_Stack; use Oak.Memory.Call_Stack;

with System; use System;

package Oak.Core_Support_Package.Call_Stack.Ops with Pure is

   procedure Set_Task_Instruction_Pointer
     (Stack               : in out Call_Stack_Handler;
      Instruction_Address : in Address)
     with Inline;

   procedure Set_Task_Body_Procedure
     (Stack             : in out Call_Stack_Handler;
      Procedure_Address : in Address;
      Task_Value_Record : in Address);

end Oak.Core_Support_Package.Call_Stack.Ops;
