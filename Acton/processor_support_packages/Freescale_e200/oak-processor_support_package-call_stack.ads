with System.Storage_Elements;
package Oak.Processor_Support_Package.Call_Stack is

   pragma Pure;

   --  Call_Stack_Size could be defined in the linker script (Enviroment
   --  defined variables don't make sense in this system) Call_Stack_Size in
   --  Storage_Elements
   Call_Stack_Size : constant := 4 * 1024;
   Main_Task_Call_Stack_Size : constant := 48 * 1024;
   --  Call_Stack_Alignment in bytes
   Call_Stack_Allignment : constant := 8;

   function Default_Call_Stack_Size return
     System.Storage_Elements.Storage_Count;

   Bits_In_A_Byte : constant := 4;

   Task_Registers_Save_Size   : constant := 296;
   Kernel_Registers_Save_Size : constant := 152;

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

end Oak.Processor_Support_Package.Call_Stack;
