------------------------------------------------------------------------------
--                                                                          --
--                              OAK COMPONENTS                              --
--                                                                          --
--                        OAK.MEMORY.CALL_STACK.OPS                         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                 Copyright (C) 2010-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

--  This package provides the operations on the call stack. These operations
--  are kept in a seperate package to prevent circular dependences. Also
--  includes the global package varibale that keeps track of the bottom of the
--  stack pool. It is placed here so the parent package can be kept Pure by
--  simply defining the call stack types and constants.

with Oak.Core_Support_Package.Call_Stack;
use Oak.Core_Support_Package.Call_Stack;

package Oak.Memory.Call_Stack.Ops with Preelaborate is

   procedure Allocate_Call_Stack
     (Stack            : out Call_Stack_Handler;
      Size_In_Elements : in  Storage_Count := CSP_Stack.Call_Stack_Size);
   --  Allocates a call stack from the call stack pool.

   procedure Initialise_Call_Stack
     (Stack             : in out Call_Stack_Handler;
      Start_Instruction : in     Address);
   --  Initialise a call stack with the specified instruction address (which
   --  will execute when the context is switched to the agent).

   procedure Initialise_Call_Stack
     (Stack             : in out Call_Stack_Handler;
      Start_Instruction : in     Address;
      Task_Value_Record : in     Address := Null_Address);
   --  Initialise a call stack with the specified instrucution address and a
   --  pointer to the record that contains the task's private data.

   procedure Initialise_Call_Stack
     (Stack             : in out Call_Stack_Handler;
      Start_Instruction : in     Address;
      Task_Value_Record : in     Address := Null_Address;
      Stack_Address     : in     Address;
      Stack_Size        : in     Storage_Elements.Storage_Count);
   --  Initialise a call stack like above, but to a paricular size???

private

   Stack_Pool_Bottom : System.Address := Stack_Pointer_Init'Address;
   --  Keeps track of the bottom of the stack pool.

end Oak.Memory.Call_Stack.Ops;
