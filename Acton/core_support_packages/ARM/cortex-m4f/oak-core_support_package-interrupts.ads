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

with Oak.Agent;

with System; use System;

with ISA.ARM.Cortex_M4.Exceptions; use ISA.ARM.Cortex_M4.Exceptions;

package Oak.Core_Support_Package.Interrupts with Preelaborate is

   ---------------------------------
   -- INTERNAL PACKAGE COMPONENTS --
   ---------------------------------

   procedure Set_Up_Interrupts;

   pragma Warnings (Off, "*attribute directive ignored*");

   --  May customise this section with the appropriate linker section
   procedure Decrementer_Interrupt;
   pragma Machine_Attribute
     (Decrementer_Interrupt, "naked");

   procedure IRQ_Interrupt_Handler;
   pragma Machine_Attribute
     (IRQ_Interrupt_Handler, "naked");

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

   procedure SVCall_Handler;
   pragma Machine_Attribute
     (SVCall_Handler, "naked");

   pragma Warnings (On, "*attribute directive ignored*");

   Current_IRQ : Exception_Id;
   --  Current exception as picked up in the external interrupt handler

   Agent_With_FPR_To_Save : Oak.Agent.Oak_Agent_Id;

private

   SVC_Vector : Address
     with Import, Convention => Ada, External_Name => "svc_vector";

   SVC_Return_Vector : Address
     with Import, Convention => Ada, External_Name => "svc_return_vector";

end Oak.Core_Support_Package.Interrupts;
