with Oak.Brokers.Protected_Objects; use Oak.Brokers.Protected_Objects;

with ISA.ARM.Cortex_M4.NVIC;       use ISA.ARM.Cortex_M4.NVIC;

package body Oak.Processor_Support_Package.Interrupts is

   procedure External_Interrupt_Handler (Interrupt_Id : External_Interrupt_Id)
   is
   begin
      Interrupt_Vector_Table (Interrupt_Id).all;
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
      pragma Unreferenced (Above_Priority);
   begin
      return False;
   end Has_Outstanding_Interrupts;

   procedure Trap_Handler is
   begin
      loop
         null;
      end loop;
   end Trap_Handler;

end Oak.Processor_Support_Package.Interrupts;
