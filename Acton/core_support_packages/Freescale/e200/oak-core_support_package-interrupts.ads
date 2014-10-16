------------------------------------------------------------------------------
--                                                                          --
--                         OAK CORE SUPPORT PACKAGE                         --
--                              FREESCALE e200                              --
--                                                                          --
--                    OAK.CORE_SUPPORT_PACKAGE.INTERRUPTS                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                 Copyright (C) 2010-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

--  This package provides the core-level interrupt support, including
--  interrupts that occur at the core level.

--  This is the Freescal e200 version.

package Oak.Core_Support_Package.Interrupts with Preelaborate is

   procedure Set_Up_Interrupts;
   procedure Enable_External_Interrupts  with Inline_Always;
   procedure Disable_External_Interrupts with Inline_Always;
   procedure Disable_Oak_Wake_Up_Interrupt with Inline_Always;
   procedure Enable_Oak_Wake_Up_Interrupt with Inline_Always;

   procedure Decrementer_Interrupt
   with Linker_Section => ".acton_intr_branch_table";

   procedure External_Interrupt_Handler
   with Linker_Section => ".acton_intr_branch_table";

   procedure Full_Context_Switch_To_Agent_Interrupt
     with Linker_Section => ".acton_intr_branch_table";

   procedure In_Place_Context_Switch_To_Agent_Interrupt
     with Linker_Section => ".acton_intr_branch_table";

   procedure In_Place_Context_Switch_To_Oak_Interrupt
     with Linker_Section => ".acton_intr_branch_table";

   procedure Request_Context_Switch_To_Agent_Interrupt
     with Linker_Section => ".acton_intr_branch_table";

   procedure Request_Context_Switch_To_Oak_Interrupt
     with Linker_Section => ".acton_intr_branch_table";

   ---------------------------------
   -- Internal Package Components --
   ---------------------------------

   procedure External_Interrupt_Present
     with Linker_Section => ".acton_intr_branch_table";
   --  Sets r3 to 1 to indicate that an external interrupt is present

private

   procedure Enable_SPE_Instructions with Inline_Always;

   procedure Full_Context_Switch_To_Oak;
   --  Not callable as an interrupt. Called by Decrementer_Interrupt and
   --  External_Interrupt. Does not save r3.

   --  Notes on register uses:
   --
   --    SPRG0: Stores the kernel's stack pointer
   --    SPRG1: Stores the kernel's instruction pointer
   --    SPRG2: Return interrupt handler address

--     pragma Machine_Attribute
--       (Context_Switch_To_Kernel_Interrupt, "aligned", 16);
--     pragma Machine_Attribute
--       (Context_Switch_To_Task_Interrupt, "aligned", 16);
--     pragma Machine_Attribute
--       (Decrementer_Interrupt, "aligned", 16);
--     pragma Machine_Attribute
--       (External_Interrupt_Handler, "aligned", 16);
end Oak.Core_Support_Package.Interrupts;
