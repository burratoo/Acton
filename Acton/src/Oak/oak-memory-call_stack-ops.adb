with Oak.Core_Support_Package.Call_Stack.Ops;

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

   procedure Initialise_Call_Stack
     (Stack             : in out Oak.Memory.Call_Stack.Call_Stack_Handler;
      Start_Instruction : in System.Address)
      renames
     Oak.Core_Support_Package.Call_Stack.Ops.Initialise_Call_Stack;

   procedure Initialise_Call_Stack
     (Stack             : in out Oak.Memory.Call_Stack.Call_Stack_Handler;
      Start_Instruction : in System.Address;
      Task_Value_Record : in System.Address := Null_Address;
      Message_Location  : out Oak.Agent.Tasks.Oak_Task_Message_Location)
      renames
     Oak.Core_Support_Package.Call_Stack.Ops.Initialise_Call_Stack;

   procedure Initialise_Call_Stack
     (Stack             : in out Oak.Memory.Call_Stack.Call_Stack_Handler;
      Start_Instruction : in System.Address;
      Task_Value_Record : in System.Address := Null_Address;
      Stack_Address     : in System.Address;
      Stack_Size        : in System.Storage_Elements.Storage_Count;
      Message_Location  : out Oak.Agent.Tasks.Oak_Task_Message_Location)
      renames
     Oak.Core_Support_Package.Call_Stack.Ops.Initialise_Call_Stack;
end Oak.Memory.Call_Stack.Ops;
