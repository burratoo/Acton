with System;
with System.Storage_Elements;

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

   CSTT : constant System.Storage_Elements.Storage_Element
     with Import, Convention => Assembler, External_Name =>  "__OTS_CSTT";
   CSTK : constant System.Storage_Elements.Storage_Element
     with Import, Convention => Assembler, External_Name =>  "__OTS_CSTK";
   DI   : constant System.Storage_Elements.Storage_Element
   with Import, Convention => Assembler, External_Name =>  "__OTS_DI";
   EI   : constant System.Storage_Elements.Storage_Element
     with Import, Convention => Assembler, External_Name =>  "__OTS_EI";

   IVOR4_Ex_Interrupt      : constant System.Address := EI'Address;
   IVOR8_CS_To_Kernel      : constant System.Address := CSTK'Address;
   IVOR8_CS_To_Task        : constant System.Address := CSTT'Address;
   IVOR10_Decrementer_Intr : constant System.Address := DI'Address;
end Oak.Core_Support_Package.Interrupts;
