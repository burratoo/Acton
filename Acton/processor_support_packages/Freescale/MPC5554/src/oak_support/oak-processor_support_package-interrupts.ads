with Interfaces; use Interfaces;
with System; use System;
with MPC5554; use MPC5554;
with MPC5554.INTC; use MPC5554.INTC;

limited with Oak.Agent.Tasks.Protected_Object;

package Oak.Processor_Support_Package.Interrupts with Preelaborate is

   subtype Oak_Interrupt_Id is MPC5554.INTC.INTC_ID_Type;
   Default_Interrupt_Priority : constant Interrupt_Priority :=
                                  Interrupt_Priority'Last;

   type Parameterless_Handler is access protected procedure;

   procedure External_Interrupt_Handler;
   procedure Initialise_Interrupts;
   procedure Complete_Interrupt_Initialisation;

   procedure Attach_Handler (Interrupt : Oak_Interrupt_Id;
                             Handler   : Parameterless_Handler;
                             Priority  : Interrupt_Priority);
   procedure Get_Resource
     (PO : in Agent.Tasks.Protected_Object.Protected_Agent'Class);
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

   Interrupt_Flash_Block : constant := 2;

   ------------------------
   --  INTC Vector Table --
   ------------------------

   --  The INTC vector table is an array of Parameterless_Handler, which is an
   --  access-to-protected-subprogram pointer. These pointers consist of the
   --  address of the protected object and the address of the protected
   --  procedure and occupy two words in memory.
   --
   --  The INTC vector table is set up in two stages. The enties of the initial
   --  table programmed into Flash when the program is downloaded are set to
   --  the value (16#FFFF_FFFF#, 16#FFFF_FFFF#). This pair allows the table
   --  to be easily written to when handlers are later attached in the second
   --  stage.

   type Vector_Pair is record
      Protected_Object : Unsigned_32;
      Handler_Address  : Unsigned_32;
   end record;

   Default_Handler : constant Vector_Pair := (16#FFFF_FFFF#, 16#FFFF_FFFF#);

   Programmed_Vector_Table   : array (Oak_Interrupt_Id) of Vector_Pair :=
    ((16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#),
     (16#FFFF_FFFF#, 16#FFFF_FFFF#), (16#FFFF_FFFF#, 16#FFFF_FFFF#));

   pragma Linker_Section (Programmed_Vector_Table, ".intc_vector_table");
   pragma Export (Assembly, Programmed_Vector_Table, "__OI_Vector_Table");

   INTC_Vector_Table : array (Oak_Interrupt_Id) of Parameterless_Handler;
   pragma Import (Assembly, INTC_Vector_Table, "__OI_Vector_Table");
end Oak.Processor_Support_Package.Interrupts;
