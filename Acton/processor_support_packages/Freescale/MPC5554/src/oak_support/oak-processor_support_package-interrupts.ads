with System; use System;
with MPC5554.INTC; use MPC5554.INTC;

limited with Oak.Oak_Task;

package Oak.Processor_Support_Package.Interrupts is

   pragma Preelaborate;

   subtype Oak_Interrupt_Id is MPC5554.INTC.INTC_ID_Type;
   Default_Interrupt_Priority : constant Interrupt_Priority :=
                                  Interrupt_Priority'Last;

   type Parameterless_Handler is access protected procedure;

   procedure External_Interrupt_Handler;
   procedure Default_Handler;
   procedure Initialise_Interrupts;

   procedure Attach_Handler (Interrupt : Oak_Interrupt_Id;
                             Handler   : Parameterless_Handler;
                             Priority  : Interrupt_Priority);
   procedure Get_Resource (PO : Oak.Oak_Task.Oak_Task_Handler);
   procedure Release_Resource;

   pragma Export (Asm,
                  External_Interrupt_Handler,
                  "__OI_External_Interrupt");

private
   type FIFO_Array is array
     (Oak_Interrupt_Id'First + 1 .. Oak_Interrupt_Id'Last)
       of MPC5554_Interrupt_Priority;
   type Interrupt_FIFO is record
      Stack : FIFO_Array;
      Top  : Oak_Interrupt_Id := 0;
   end record;

   Interrupt_Priority_FIFO : Interrupt_FIFO;

   --  Explicitly initialise the INTC vector table so that it does not have to
   --  be generated during initialisation. Note however this arrangment only
   --  works when all used interrupt handlers are set during initialisation as
   --  changes to the table is permanent.
   INTC_Vector_Table : array (Oak_Interrupt_Id) of Parameterless_Handler :=
    (null, null, null, null, null, null, null, null, null, null, null, null,
     null, null, null, null, null, null, null, null, null, null, null, null,
     null, null, null, null, null, null, null, null, null, null, null, null,
     null, null, null, null, null, null, null, null, null, null, null, null,
     null, null, null, null, null, null, null, null, null, null, null, null,
     null, null, null, null, null, null, null, null, null, null, null, null,
     null, null, null, null, null, null, null, null, null, null, null, null,
     null, null, null, null, null, null, null, null, null, null, null, null,
     null, null, null, null, null, null, null, null, null, null, null, null,
     null, null, null, null, null, null, null, null, null, null, null, null,
     null, null, null, null, null, null, null, null, null, null, null, null,
     null, null, null, null, null, null, null, null, null, null, null, null,
     null, null, null, null, null, null, null, null, null, null, null, null,
     null, null, null, null, null, null, null, null, null, null, null, null,
     null, null, null, null, null, null, null, null, null, null, null, null,
     null, null, null, null, null, null, null, null, null, null, null, null,
     null, null, null, null, null, null, null, null, null, null, null, null,
     null, null, null, null, null, null, null, null, null, null, null, null,
     null, null, null, null, null, null, null, null, null, null, null, null,
     null, null, null, null, null, null, null, null, null, null, null, null,
     null, null, null, null, null, null, null, null, null, null, null, null,
     null, null, null, null, null, null, null, null, null, null, null, null,
     null, null, null, null, null, null, null, null, null, null, null, null,
     null, null, null, null, null, null, null, null, null, null, null, null,
     null, null, null, null, null, null, null, null, null, null, null, null,
     null, null, null, null, null, null, null, null);
   pragma Linker_Section (INTC_Vector_Table, ".intc_vector_table");
end Oak.Processor_Support_Package.Interrupts;
