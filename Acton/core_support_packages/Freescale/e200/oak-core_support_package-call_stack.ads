with System.Storage_Elements;

package Oak.Core_Support_Package.Call_Stack with Pure is

   --  Call_Stack_Size could be defined in the linker script (Enviroment
   --  defined variables don't make sense in this system) Call_Stack_Size in
   --  Storage_Elements
   Call_Stack_Size            : constant := 4 * 1024;
   Default_Call_Stack_Size    : constant := Call_Stack_Size;
   Main_Task_Call_Stack_Size  : constant := 4 * 1024;
   Oak_Call_Stack_Size        : constant := 4 * 1024;

   --  Call_Stack_Alignment in bytes
   Call_Stack_Allignment      : constant := 8;

   Minimum_Call_Stack_Size    : constant := 1 * 1024;

   Task_Registers_Save_Size   : constant := 296;
   Kernel_Registers_Save_Size : constant := 152;

   Sleep_Stack_Size           : constant := 2 * Task_Registers_Save_Size;

   --  Warning! This is not a real variable. It is defined in the linker script
   --  and as such does not have any data storage allocated for it. Instead
   --  only a memory address is attached.
   Stack_Pointer_Init : constant System.Storage_Elements.Storage_Element;
   pragma Import (Assembler, Stack_Pointer_Init, "__SP_INIT");

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
