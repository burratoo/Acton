with Oak.Core_Support_Package.Call_Stack.Ops;
with Oak.Oak_Task;

package Oak.Memory.Call_Stack.Ops with Preelaborate is
   package OT renames Oak.Oak_Task;

   procedure Allocate_Call_Stack
     (Stack            : out Call_Stack_Handler;
      Size_In_Elements : in Storage_Elements.Storage_Count :=
     CSP_Stack.Call_Stack_Size);

   procedure Initialise_Call_Stack
     (Stack             : in out Oak.Memory.Call_Stack.Call_Stack_Handler;
      Start_Instruction : in System.Address;
      Task_Value_Record : in System.Address := Null_Address;
      Message_Location  : out OT.Oak_Task_Message_Location)
      renames
     Oak.Core_Support_Package.Call_Stack.Ops.Initialise_Call_Stack;

   procedure Initialise_Call_Stack
     (Stack             : in out Oak.Memory.Call_Stack.Call_Stack_Handler;
      Start_Instruction : in System.Address;
      Task_Value_Record : in System.Address := Null_Address;
      Stack_Address     : in System.Address;
      Stack_Size        : in System.Storage_Elements.Storage_Count;
      Message_Location  : out OT.Oak_Task_Message_Location)
      renames
     Oak.Core_Support_Package.Call_Stack.Ops.Initialise_Call_Stack;

end Oak.Memory.Call_Stack.Ops;
