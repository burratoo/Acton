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

with System;                   use System;
with System.Storage_Elements;  use System.Storage_Elements;

package Oak.Core_Support_Package.Interrupts with Preelaborate is

   procedure Set_Up_Interrupts;
   procedure Enable_SPE_Instructions with Inline_Always;
   procedure Enable_External_Interrupts  with Inline_Always;
   procedure Disable_External_Interrupts with Inline_Always;
   procedure Disable_Oak_Wake_Up_Interrupt with Inline_Always;
   procedure Enable_Oak_Wake_Up_Interrupt with Inline_Always;

private

   procedure Context_Switch_To_Kernel_Interrupt
     with Export, Convention => Assembler,
          External_Name => "__OTS_Context_Switch_To_Kernel";

   procedure Context_Switch_To_Task_Interrupt
     with Export, Convention => Assembler,
          External_Name => "__OTS_Context_Switch_To_Task";

   procedure Decrementer_Interrupt
     with Export, Convention => Assembler,
          External_Name => "__OTS_Decrementer_Interrupt";

   procedure External_Interrupt_Handler
     with Export, Convention => Assembler,
          External_Name => "__OTS_General_Interrupt_Handler";

   CSTT : constant Storage_Element
     with Import, Convention => Assembler, External_Name =>  "__OTS_CSTT";
   CSTK : constant Storage_Element
     with Import, Convention => Assembler, External_Name =>  "__OTS_CSTK";
   DI   : constant Storage_Element
   with Import, Convention => Assembler, External_Name =>  "__OTS_DI";
   EI   : constant Storage_Element
     with Import, Convention => Assembler, External_Name =>  "__OTS_EI";

   IVOR4_Ex_Interrupt      : constant Address := EI'Address;
   IVOR8_CS_To_Kernel      : constant Address := CSTK'Address;
   IVOR8_CS_To_Task        : constant Address := CSTT'Address;
   IVOR10_Decrementer_Intr : constant Address := DI'Address;
end Oak.Core_Support_Package.Interrupts;
