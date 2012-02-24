with System.Storage_Elements;
with System;

package Oak.Processor_Support_Package.Task_Interrupts is
   pragma Preelaborate;

   procedure Initialise_Task_Enviroment;

   procedure E200_Context_Switch_To_Task;
   procedure E200_Context_Switch_To_Kernel;
   procedure Decrementer_Interrupt;
   procedure Sleep_Interrupt;

   pragma Export
     (Asm,
      E200_Context_Switch_To_Task,
      "__OTS_E200_Context_Switch_To_Task");
   pragma Export
     (Asm,
      E200_Context_Switch_To_Kernel,
      "__OTS_E200_Context_Switch_To_Kernel");
   pragma Export (Asm, Decrementer_Interrupt, "__OTS_Decrementer_Interrupt");
   pragma Export (Asm, Sleep_Interrupt, "___OTS_Sleep_Interrupt");

   IVPR : constant := 16#2_0000#;

   CSTT, CSTK : System.Storage_Elements.Storage_Element;
   pragma Import (Assembler, CSTT, "__OTS_CSTT");
   pragma Import (Assembler, CSTK, "__OTS_CSTK");

   IVOR8_CS_To_Task   : constant System.Address := CSTT'Address;
   IVOR8_CS_To_Kernel : constant System.Address := CSTK'Address;
end Oak.Processor_Support_Package.Task_Interrupts;
