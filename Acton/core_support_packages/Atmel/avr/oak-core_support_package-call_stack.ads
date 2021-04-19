------------------------------------------------------------------------------------------
--                                                                                      --
--                               OAK CORE SUPPORT PACKAGE                               --
--                                      ATMEL AVR                                       --
--                                                                                      --
--                          OAK.CORE_SUPPORT_PACKAGE.CALL_STACK                         --
--                                                                                      --
--                       Copyright (C) 2012-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

with System.Storage_Elements;

package Oak.Core_Support_Package.Call_Stack with Pure is

   --  Call_Stack_Size could be defined in the linker script (Enviroment
   --  defined variables don't make sense in this system) Call_Stack_Size in
   --  Storage_Elements
   Call_Stack_Size            : constant := 1 * 1024;
   Default_Call_Stack_Size    : constant := Call_Stack_Size;
   Main_Task_Call_Stack_Size  : constant := 1 * 1024;
   Oak_Call_Stack_Size        : constant := 4 * 1024;

   --  Call_Stack_Alignment in bytes
   Call_Stack_Allignment      : constant := 8;

   Minimum_Call_Stack_Size    : constant := 1 * 512;

   Task_Registers_Save_Size   : constant := 36;
   Stack_Pointer_Size         : constant := 2;
   Timer_Overhead             : constant := 8;

   Sleep_Stack_Size           : constant := Task_Registers_Save_Size +
     Stack_Pointer_Size + Timer_Overhead;

   --  Warning! This is not a real variable. It is defined in the linker script
   --  and as such does not have any data storage allocated for it. Instead
   --  only a memory address is attached.
   Stack_Pointer_Init : constant System.Storage_Elements.Storage_Element;
   pragma Import (Assembler, Stack_Pointer_Init, "__stack");

   --
   --  SPRG0 -> Kernel stack pointer
   --  SPRG4 -> Task stack pointer.
   --
   --  Storing Registers on the Stack during a context switch:
   --  -----------------------------
   --  |                           | <---
   --  |                           | --
   --  |    R0 -> R31              |   |- 8 bit registers
   --  |    SREG                   | --
   --  |                           | <--- Bottom of stack
   --  -----------------------------

   --  33 x 8 bit registers --  33 bytes!
   --  Plus need space for the the intruction register at the start.

end Oak.Core_Support_Package.Call_Stack;
