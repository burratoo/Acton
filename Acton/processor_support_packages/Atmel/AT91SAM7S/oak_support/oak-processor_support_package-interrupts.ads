with System; use System;

with Oak.Agent; use Oak.Agent;

with Atmel.AT91SAM7S;

package Oak.Processor_Support_Package.Interrupts with Preelaborate is

   subtype External_Interrupt_Id is Atmel.AT91SAM7S.Peripheral_Id;
   Default_Interrupt_Priority : constant Interrupt_Priority :=
                                  Interrupt_Priority'Last;

   type Parameterless_Handler is access protected procedure;

   procedure External_Interrupt_Handler (Interrupt_Id : External_Interrupt_Id);
   procedure Initialise_Interrupts;
   procedure Complete_Interrupt_Initialisation;

   procedure Attach_Handler (Interrupt : External_Interrupt_Id;
                             Handler   : Parameterless_Handler;
                             Priority  : Interrupt_Priority);

   function Handler_Protected_Object
     (Interrupt : External_Interrupt_Id) return Protected_Id_With_No;

   function Current_Interrupt_Priority return Any_Priority;
   function Get_External_Interrupt_Id return External_Interrupt_Id;

   procedure Set_Hardware_Priority (P : Any_Priority);
   procedure Clear_Hardware_Priority;

private
   AIC_Vector_Table : array (External_Interrupt_Id) of Parameterless_Handler :=
                        (others => null);
end Oak.Processor_Support_Package.Interrupts;
