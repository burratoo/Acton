--                                                                          --
--                              OAK COMPONENTS                              --
--                                                                          --
--                          OAK.MEMORY.CALL_STACK                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                 Copyright (C) 2010-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

--  This package defines the types and platform independent constants
--  associated with Oak's call stacks.

with Oak.Core_Support_Package;
with Oak.Core_Support_Package.Call_Stack;

with Oak.Project_Support_Package;

with System;                  use System;
with System.Storage_Elements; use System.Storage_Elements;

package Oak.Memory.Call_Stack with Pure is

   package CSP_Stack renames Oak.Core_Support_Package.Call_Stack;

--     type Call_Stack is
--       array (Storage_Offset range <>)
--              of aliased Storage_Elements.Storage_Element;
--     for Call_Stack'Component_Size use Storage_Unit;

   type Call_Stack_Handler is record
      Top, Bottom, Pointer : Address;
   end record;

   function No_Call_Stack return Call_Stack_Handler
     with Inline_Always;

   Default_Stack_Size : constant :=
     Oak.Project_Support_Package.Default_Call_Stack_Size;

   --  Value used to indicate tha no size has been set

   Unspecified_Call_Stack_Size : constant Storage_Count := Default_Stack_Size;

   subtype Call_Stack_Size is Storage_Elements.Storage_Count range
     CSP_Stack.Minimum_Call_Stack_Size ..
      Storage_Elements.Storage_Count'Last;
private
   function No_Call_Stack return Call_Stack_Handler is
     ((Null_Address, Null_Address, Null_Address));
end Oak.Memory.Call_Stack;
