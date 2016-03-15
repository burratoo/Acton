------------------------------------------------------------------------------
--                                                                          --
--                         OAK CORE SUPPORT PACKAGE                         --
--                              FREESCALE e200                              --
--                                                                          --
--                   OAK.CORE_SUPPORT_PACKAGE.CALL_STACK                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                 Copyright (C) 2010-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

with System.Storage_Elements;

package Oak.Core_Support_Package.Call_Stack with Pure is

   --  Call_Stack_Alignment in bytes
   Call_Stack_Allignment      : constant := 8;
   Oak_Call_Stack_Size        : constant := 512;
   Minimum_Call_Stack_Size    : constant := 1 * 512;
   Sleep_Stack_Size           : constant := 72;

   --  Warning! These are not a real variable. It is defined in the linker
   --  script and as such does not have any data storage allocated for it.
   --  Instead only a memory address is attached.
   Stack_Pointer_Init : constant System.Storage_Elements.Storage_Element
     with Import, Convention => Assembler,
          External_Name => "__stack_space_start";

   Stack_Pointer_End  : constant System.Storage_Elements.Storage_Element
     with Import, Convention => Assembler,
          External_Name => "__stack_space_end";

   --
   --  SPRG0 -> Kernel stack pointer
   --  SPRG4 -> Task stack pointer.
   --
   --  Storing Registers on the Stack during a context switch for tasks:
   --  -----------------------------
   --  |    GPR0                   | --
   --  |    GPR13 - 2              |   |- 64 bit registers --
   --  |    GPR31 -> GPR14         |   |                     |
   --  |    Accumulator            | --                      |
   --  |    SPEFSCR                | --                      | -- 284 bytes!
   --  |    XER                    |   |                     |
   --  |    LR                     |   |                     |
   --  |    CTR                    |   |- 32 bit registers --
   --  |    CR                     |   |
   --  |    USPRG0                 |   |
   --  |    Instruction Pointer    | <----        Bottom of stack
   --  -----------------------------
   --  Storing Registers on the Stack during a context switch for the kernel:
   --  -----------------------------
   --  |    GPR0                   | --
   --  |    GPR13 - 2              |   |
   --  |    GPR31 -> GPR14         |   |
   --  |    XER                    |   | 37 x 32 bit registers -- 144 bytes!
   --  |    LR                     |   |
   --  |    CTR                    |   |
   --  |    CR                     |   |
   --  |    USPRG0                 |   |
   --  |    Instruction Pointer    | <----        Bottom of stack
   --  -----------------------------

end Oak.Core_Support_Package.Call_Stack;
