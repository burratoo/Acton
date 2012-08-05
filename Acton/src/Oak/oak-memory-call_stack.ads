with System;
with Oak.Core_Support_Package;
with Oak.Core_Support_Package.Call_Stack;

with System.Storage_Elements; use System.Storage_Elements;

package Oak.Memory.Call_Stack with Pure is

   use System;
   package CSP_Stack renames Oak.Core_Support_Package.Call_Stack;

   --   type Storage_Pointer is access all Storage_Element;
   type Call_Stack is
     array (Storage_Offset range <>)
            of aliased Storage_Elements.Storage_Element;
   for Call_Stack'Component_Size use Storage_Unit;

   type Call_Stack_Handler is record
      Top, Bottom, Pointer : System.Address := Null_Address;
   end record;

   function No_Call_Stack return Call_Stack_Handler;

   Default_Stack_Size : constant :=
     Oak.Core_Support_Package.Call_Stack.Default_Call_Stack_Size;

   --  Value used to indicate tha no size has been set

   Unspecified_Call_Stack_Size : constant Storage_Count := 0;

   subtype Call_Stack_Size is Storage_Elements.Storage_Count range
     CSP_Stack.Minimum_Call_Stack_Size ..
      Storage_Elements.Storage_Count'Last;
private
   --  Warning! This is not a real variable. It is defined in the linker script
   --  and as such does not have any data storage allocated for it. Instead
   --  only a memory address is attached.
   Stack_Pointer_Init : constant Storage_Elements.Storage_Element;
   pragma Import (Assembler, Stack_Pointer_Init, "__SP_INIT");

   function No_Call_Stack return Call_Stack_Handler is
     ((Null_Address, Null_Address, Null_Address));
end Oak.Memory.Call_Stack;
