with Oak.Processor_Support_Package.Call_Stack.Ops;

package Oak.Memory.Call_Stack.Ops is
   pragma Preelaborate;

   procedure Allocate_Call_Stack
     (Stack            : out Call_Stack_Handler;
      Size_In_Elements : in Storage_Elements.Storage_Count :=
     PSP_Stack.Call_Stack_Size);

   procedure Initialise_Call_Stack
     (Stack             : in out Call_Stack_Handler;
      Start_Instruction : in System.Address) renames
     Oak.Processor_Support_Package.Call_Stack.Ops.Initialise_Call_Stack;

   procedure Initialise_Call_Stack
     (Stack             : in out Call_Stack_Handler;
      Start_Instruction : in System.Address;
      Stack_Address     : in System.Address;
      Stack_Size        : in System.Storage_Elements.Storage_Count) renames
     Oak.Processor_Support_Package.Call_Stack.Ops.Initialise_Call_Stack;

end Oak.Memory.Call_Stack.Ops;
