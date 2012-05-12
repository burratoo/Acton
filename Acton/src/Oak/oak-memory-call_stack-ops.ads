with Oak.Core_Support_Package.Call_Stack.Ops;

limited with Oak.Agent.Tasks;

package Oak.Memory.Call_Stack.Ops with Preelaborate is

   procedure Allocate_Call_Stack
     (Stack            : out Call_Stack_Handler;
      Size_In_Elements : in Storage_Elements.Storage_Count :=
     CSP_Stack.Call_Stack_Size);

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

private
   Stack_Pool_Bottom : System.Address := Stack_Pointer_Init'Address;

end Oak.Memory.Call_Stack.Ops;
