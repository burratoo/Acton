------------------------------------------------------------------------------------------
--                                                                                      --
--                               OAK CORE SUPPORT PACKAGE                               --
--                                    ARM CORTEX M4F                                    --
--                                                                                      --
--                                  ISA.ARM.CORTEX_M4                                   --
--                                                                                      --
--                       Copyright (C) 2014-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

package ISA.ARM.Cortex_M4 with Pure is

   procedure Disable_Interrupts with Inline_Always;
   procedure Enable_Interrupts with Inline_Always;

   procedure Data_Memory_Barrier with Inline_Always;
   procedure Data_Synchronization_Barrier with Inline_Always;
   procedure Instruction_Synchronization_Barrier with Inline_Always;

   procedure Clear_Exclusive_Lock with Inline_Always;
end ISA.ARM.Cortex_M4;
