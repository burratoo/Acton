with System;
with System.Storage_Elements;
with Oak.Processor_Support_Package;
with Oak.Processor_Support_Package.Call_Stack;

package Oak.Memory.Call_Stack is
   pragma Preelaborate;

   use System;
   package PSP_Stack renames Oak.Processor_Support_Package.Call_Stack;

   --   type Storage_Pointer is access all Storage_Element;
   type Call_Stack is
     array (Storage_Offset range <>)
            of aliased Storage_Elements.Storage_Element;
   for Call_Stack'Component_Size use Storage_Unit;

   type Call_Stack_Handler is record
      Top, Bottom, Pointer : System.Address := Null_Address;
   end record;

   function Default_Stack_Size return  System.Storage_Elements.Storage_Count
      renames Oak.Processor_Support_Package.Call_Stack.Default_Call_Stack_Size;

   --  Value used to indicate tha no size has been set

   Unspecified_Call_Stack_Size : constant Storage_Count := 0;

   subtype Call_Stack_Size is Storage_Elements.Storage_Count range
     PSP_Stack.Minimum_Call_Stack_Size ..
      Storage_Elements.Storage_Count'Last;
private
   --  Warning! This is not a real variable. It is defined in the linker script
   --  and as such does not have any data storage allocated for it. Instead
   --  only a memory address is attached.
   Stack_Pointer_Init : constant Storage_Elements.Storage_Element;
   pragma Import (Assembler, Stack_Pointer_Init, "__SP_INIT");

   Stack_Pool_Bottom : System.Address := Stack_Pointer_Init'Address;
end Oak.Memory.Call_Stack;
