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

with Oak.Agent;           use Oak.Agent;
with Oak.Agent.Kernel;    use Oak.Agent.Kernel;
with Oak.Agent.Oak_Agent; use Oak.Agent.Oak_Agent;

package body Oak.Memory.Call_Stack.Ops is

   --------------------
   -- New_Call_Stack --
   --------------------

   procedure Allocate_Call_Stack
     (Stack            : out Call_Stack_Handler;
      Size_In_Elements : in  Storage_Count :=
        Project_Support_Package.Call_Stack_Size)
   is
      Size : Storage_Elements.Storage_Count := Size_In_Elements;
   begin
      --  Check to ensure that the stack end doesn't go pass the end of the
      --  stack space

      if Stack_Pool_Bottom - Size < Stack_Pointer_End'Address then
         raise Storage_Error;
      end if;

      if (Size mod CSP_Stack.Call_Stack_Allignment) /= 0 then
         Size := (Size / CSP_Stack.Call_Stack_Allignment + 1) *
                 CSP_Stack.Call_Stack_Allignment;
      end if;
      Stack.Top         := Stack_Pool_Bottom;
      Stack.Pointer     := Stack_Pool_Bottom;
      Stack_Pool_Bottom := Stack_Pool_Bottom - Size;
      Stack.Bottom      := Stack_Pool_Bottom;

      Stack.Secondary_Stack_Pointer := Stack_Pool_Bottom;
      Stack.Secondary_Stack_Limit   := Stack_Pool_Bottom +
        Oak.Project_Support_Package.Secondary_Stack_Percentage * Size;
   end Allocate_Call_Stack;

   ---------------------------
   -- Initialise_Call_Stack --
   ---------------------------

   procedure Initialise_Call_Stack
     (Stack             : in out Call_Stack_Handler;
      Start_Instruction : in     Address)
   is
   begin
      Set_Task_Instruction_Pointer
        (Stack               => Stack,
         Instruction_Address => Start_Instruction);
   end Initialise_Call_Stack;

   procedure Initialise_Call_Stack
     (Stack             : in out Call_Stack_Handler;
      Start_Instruction : in     Address;
      Task_Value_Record : in     Address)
   is
   begin
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

   procedure Stack_Check (Stack_Address : in Address) is
      This_Agent : constant Oak_Agent_Id := Current_Agent (This_Oak_Kernel);
   begin
      if Stack_Address < Stack (This_Agent).Bottom
        or else Stack_Address > Stack (This_Agent).Top
      then
         raise Storage_Error;
      end if;
   end Stack_Check;

end Oak.Memory.Call_Stack.Ops;
