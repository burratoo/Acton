with System;
with System.Storage_Elements;

package Oak.Core_Support_Package.Task_Interrupts with Preelaborate is

   procedure Initialise_Task_Enviroment;

   procedure Enable_SPE_Instructions;
   pragma Inline_Always (Enable_SPE_Instructions);

   procedure Enable_External_Interrupts  with Inline_Always;
   procedure Disable_External_Interrupts with Inline_Always;

   procedure Context_Switch_To_Task;
   procedure Context_Switch_To_Kernel;
   procedure Context_Switch_To_Sleep;
   procedure Decrementer_Interrupt;
   procedure Sleep_Interrupt;

   pragma Export
     (Asm,
      Context_Switch_To_Task,
      "__OTS_Context_Switch_To_Task");
   pragma Export
     (Asm,
      Context_Switch_To_Kernel,
      "__OTS_Context_Switch_To_Kernel");
   pragma Export
     (Asm,
      Context_Switch_To_Sleep,
      "__OTS_Context_Switch_To_Sleep");

   pragma Export (Asm, Decrementer_Interrupt, "__OTS_Decrementer_Interrupt");
   pragma Export (Asm, Sleep_Interrupt, "__OTS_Sleep_Interrupt");

   CSTT, CSTK,  CSTS, EI : constant System.Storage_Elements.Storage_Element;
   pragma Import (Assembler, CSTT, "__OTS_CSTT");
   pragma Import (Assembler, CSTK, "__OTS_CSTK");
   pragma Import (Assembler, CSTS, "__OTS_CSTS");
   pragma Import (Assembler, EI, "__OI_EI");

   IVOR8_CS_To_Task   : constant System.Address := CSTT'Address;
   IVOR8_CS_To_Kernel : constant System.Address := CSTK'Address;
   IVOR8_CS_To_Sleep  : constant System.Address := CSTS'Address;
   IVOR4_Ex_Interrupt : constant System.Address := EI'Address;
end Oak.Core_Support_Package.Task_Interrupts;
