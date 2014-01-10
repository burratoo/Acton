------------------------------------------------------------------------------
--                                                                          --
--                              OAK COMPONENTS                              --
--                                                                          --
--                        OAK.MEMORY.CALL_STACK.OPS                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                 Copyright (C) 2010-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

with Oak.Core_Support_Package.Call_Stack.Ops;
use Oak.Core_Support_Package.Call_Stack.Ops;

package body Oak.Memory.Call_Stack.Ops is

   --------------------
   -- New_Call_Stack --
   --------------------

   procedure Allocate_Call_Stack
     (Stack            : out Call_Stack_Handler;
      Size_In_Elements : in Storage_Elements.Storage_Count :=
        CSP_Stack.Call_Stack_Size)
   is
      Size : Storage_Elements.Storage_Count := Size_In_Elements;
   begin
      if (Size mod CSP_Stack.Call_Stack_Allignment) /= 0 then
         Size := (Size / CSP_Stack.Call_Stack_Allignment + 1) *
                 CSP_Stack.Call_Stack_Allignment;
      end if;
      Stack.Top         := Stack_Pool_Bottom;
      Stack.Pointer     := Stack_Pool_Bottom;
      Stack_Pool_Bottom := Stack_Pool_Bottom - Size;
      Stack.Bottom      := Stack_Pool_Bottom;
   end Allocate_Call_Stack;

   ---------------------------
   -- Initialise_Call_Stack --
   ---------------------------

   procedure Initialise_Call_Stack
     (Stack             : in out Call_Stack_Handler;
      Start_Instruction : in     Address)
   is
   begin
      Stack.Pointer := Stack.Pointer - Task_Registers_Save_Size;
      Set_Task_Body_Procedure
        (Stack             => Stack,
         Procedure_Address => Start_Instruction,
         Task_Value_Record => System.Null_Address);
   end Initialise_Call_Stack;

   procedure Initialise_Call_Stack
     (Stack             : in out Call_Stack_Handler;
      Start_Instruction : in     Address;
      Task_Value_Record : in     Address := Null_Address)
   is
   begin
      Stack.Pointer := Stack.Pointer - Task_Registers_Save_Size;
      Set_Task_Body_Procedure
        (Stack                => Stack,
         Procedure_Address    => Start_Instruction,
         Task_Value_Record    => Task_Value_Record);
   end Initialise_Call_Stack;

   procedure Initialise_Call_Stack
     (Stack             : in out Call_Stack_Handler;
      Start_Instruction : in     Address;
      Task_Value_Record : in     Address := Null_Address;
      Stack_Address     : in     Address;
      Stack_Size        : in     Storage_Elements.Storage_Count)
   is
   begin
      Stack.Top     := Stack_Address +  Stack_Size;
      Stack.Pointer := Stack.Top;
      Stack.Bottom  := Stack_Address;

      Initialise_Call_Stack
        (Stack             => Stack,
         Start_Instruction => Start_Instruction,
         Task_Value_Record => Task_Value_Record);
   end Initialise_Call_Stack;
end Oak.Memory.Call_Stack.Ops;
