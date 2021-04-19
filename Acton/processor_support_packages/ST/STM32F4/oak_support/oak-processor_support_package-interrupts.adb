------------------------------------------------------------------------------------------
--                                                                                      --
--                            OAK PROCESSOR SUPPORT PACKAGE                             --
--                                      ST STM32F4                                      --
--                                                                                      --
--                       OAK.PROCESSOR_SUPPORT_PACKAGE.INTERRUPTS                       --
--                                                                                      --
--                       Copyright (C) 2014-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

with Oak.Brokers.Protected_Objects; use Oak.Brokers.Protected_Objects;

with ISA.ARM;                use ISA.ARM;
with ISA.ARM.Cortex_M4.NVIC; use ISA.ARM.Cortex_M4.NVIC;

package body Oak.Processor_Support_Package.Interrupts is

   procedure External_Interrupt_Handler (Interrupt_Id : External_Interrupt_Id)
   is
   begin
      Interrupt_Vector_Table (Interrupt_Id).all;

      --  Need to manually clear the interrupt because although it was
      --  cleared once when the system interrupt handler was taken to enter the
      --  kernel to dispatch this handler, since the original source was not
      --  cleared this causes the interrupt to be pending again.

      Interrupt_Clear_Pending_Register (Interrupt_Id) := Clear;
   end External_Interrupt_Handler;

   function Get_External_Interrupt_Id return External_Interrupt_Id is
   begin
      return
        External_Interrupt_Id
          (Oak.Core_Support_Package.Interrupts.Current_IRQ);
   end Get_External_Interrupt_Id;

   procedure Initialise_Interrupts is null;

   procedure Complete_Interrupt_Initialisation is null;

   procedure Attach_Handler (Interrupt : External_Interrupt_Id;
                             Handler   : Parameterless_Handler;
                             Priority  : Interrupt_Priority)
   is
   begin
      Interrupt_Priority_Register (Interrupt) := To_Cortex_Priority (Priority);
      Interrupt_Vector_Table (Interrupt) := Handler;
      Interrupt_Enable_Register (Interrupt) := Enable;
   end Attach_Handler;

   function Current_Interrupt_Priority return Any_Priority is
   begin
      --  Figure this out!!!!!!!
      return
        To_Ada_Priority
          (Interrupt_Priority_Register (Get_External_Interrupt_Id));
   end Current_Interrupt_Priority;

   procedure Set_Hardware_Priority (P : Any_Priority) is null;

   procedure Clear_Hardware_Priority is null;

   function Handler_Protected_Object
     (Interrupt : External_Interrupt_Id) return Protected_Id_With_No is
   begin
      return Protected_Object_From_Access
        (Parameterless_Access (Interrupt_Vector_Table (Interrupt)));
   end Handler_Protected_Object;

   function Has_Outstanding_Interrupts (Above_Priority : Any_Priority)
                                        return Boolean is
      E : constant Exception_Id := Current_Exception;
      Interrupt_Priority : Any_Priority;
   begin
      if E >= IRQ0 then
         Interrupt_Priority := To_Ada_Priority
           (Interrupt_Priority_Register (To_IRQ (E)));

         if Interrupt_Priority > Above_Priority then
            Oak.Core_Support_Package.Interrupts.Current_IRQ := To_IRQ (E);
            return True;
         end if;
      end if;

      return False;
   end Has_Outstanding_Interrupts;

   procedure Trap_Handler is
   begin
      loop
         null;
      end loop;
   end Trap_Handler;

end Oak.Processor_Support_Package.Interrupts;
