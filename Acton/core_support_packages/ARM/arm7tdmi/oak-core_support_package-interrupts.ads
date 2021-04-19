------------------------------------------------------------------------------------------
--                                                                                      --
--                               OAK CORE SUPPORT PACKAGE                               --
--                                     ARM ARM7TDMI                                     --
--                                                                                      --
--                          OAK.CORE_SUPPORT_PACKAGE.INTERRUPTS                         --
--                                                                                      --
--                       Copyright (C) 2014-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

--  This package provides the core-level interrupt support, including
--  interrupts that occur at the core level.

--  This is the ARM Arm7TDMI version

with System; use System;

package Oak.Core_Support_Package.Interrupts with Preelaborate is

   --  Only modify these across all variants

   procedure Set_Up_Interrupts;

   --  May customise this section with the appropriate linker section
   procedure Decrementer_Interrupt;
   pragma Machine_Attribute
     (Decrementer_Interrupt, "naked");

   procedure External_Interrupt_Handler
     with Export, Convention => Ada, External_Name => "irq_handler";
   pragma Machine_Attribute
     (External_Interrupt_Handler, "naked");

   procedure Full_Context_Switch_To_Agent_Interrupt;
   pragma Machine_Attribute
     (Full_Context_Switch_To_Agent_Interrupt, "naked");

   procedure In_Place_Context_Switch_To_Agent_Interrupt;
   pragma Machine_Attribute
     (In_Place_Context_Switch_To_Agent_Interrupt, "naked");

   procedure In_Place_Context_Switch_To_Oak_Interrupt;
   pragma Machine_Attribute
     (In_Place_Context_Switch_To_Oak_Interrupt, "naked");

   procedure Request_Context_Switch_To_Agent_Interrupt;
   pragma Machine_Attribute
     (Request_Context_Switch_To_Agent_Interrupt, "naked");

   procedure Request_Context_Switch_To_Oak_Interrupt;
   pragma Machine_Attribute
     (Request_Context_Switch_To_Oak_Interrupt, "naked");

   procedure Full_Context_Switch_To_Oak;
   pragma Machine_Attribute
     (Full_Context_Switch_To_Oak, "naked");
   --  Called by Decrementer_Interrupt and External_Interrupt.

private

   SWI_Vector   : Address
     with Import, Convention => Asm, External_Name => "swi_vector";

   SWI_Return_Vector : Address
     with Import, Convention => Ada, External_Name => "swi_return_vector";

end Oak.Core_Support_Package.Interrupts;
