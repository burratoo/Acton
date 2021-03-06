------------------------------------------------------------------------------------------
--                                                                                      --
--                               OAK CORE SUPPORT PACKAGE                               --
--                                      ATMEL AVR                                       --
--                                                                                      --
--                        OAK.CORE_SUPPORT_PACKAGE.CALL_STACK.OPS                       --
--                                                                                      --
--                       Copyright (C) 2012-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

with Oak.Memory.Call_Stack;
with System;
with System.Storage_Elements;

limited with Oak.Agent.Tasks;

package Oak.Core_Support_Package.Call_Stack.Ops with Preelaborate is

   procedure Set_Task_Instruction_Pointer
     (Stack               : in Oak.Memory.Call_Stack.Call_Stack_Handler;
      Instruction_Address : in System.Address)
     with Inline;

   procedure Set_Task_Body_Procedure
     (Stack             : in Oak.Memory.Call_Stack.Call_Stack_Handler;
      Procedure_Address : in System.Address;
      Task_Value_Record : in System.Address);

   procedure Initialise_Call_Stack
     (Stack             : in out Oak.Memory.Call_Stack.Call_Stack_Handler;
      Start_Instruction : in System.Address);

   procedure Initialise_Call_Stack
     (Stack             : in out Oak.Memory.Call_Stack.Call_Stack_Handler;
      Start_Instruction : in System.Address;
      Task_Value_Record : in System.Address;
      Message_Location  : out Oak.Agent.Tasks.Oak_Task_Message_Location);

   procedure Initialise_Call_Stack
     (Stack             : in out Oak.Memory.Call_Stack.Call_Stack_Handler;
      Start_Instruction : in System.Address;
      Task_Value_Record : in System.Address;
      Stack_Address     : in System.Address;
      Stack_Size        : in System.Storage_Elements.Storage_Count;
      Message_Location  : out Oak.Agent.Tasks.Oak_Task_Message_Location);

end Oak.Core_Support_Package.Call_Stack.Ops;
