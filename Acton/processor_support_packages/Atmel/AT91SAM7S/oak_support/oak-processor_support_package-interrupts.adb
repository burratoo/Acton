with Oak.Brokers.Protected_Objects; use Oak.Brokers.Protected_Objects;

with Atmel; use Atmel;
with Atmel.AT91SAM7S;     use Atmel.AT91SAM7S;
with Atmel.AT91SAM7S.AIC; use Atmel.AT91SAM7S.AIC;

package body Oak.Processor_Support_Package.Interrupts is

   procedure External_Interrupt_Handler (Interrupt_Id : External_Interrupt_Id)
   is
      ICRV : Peripheral_Bit_Field := (others => False);
   begin
      AIC_Vector_Table (Interrupt_Id).all;
      ICRV (Interrupt_Id) := True;
      Interrupt_Clear_Command_Register.Clear_Interrupt := ICRV;
      End_Of_Interrupt_Command_Register := 1;
   end External_Interrupt_Handler;

   function Get_External_Interrupt_Id return External_Interrupt_Id is
      A : Address with Unreferenced;
   begin
      A := Interrupt_Vector_Register;
      return Interrupt_Status_Register.Current_Interrupt;
   end Get_External_Interrupt_Id;

   procedure Initialise_Interrupts is
   begin
      --  Need to turn on interrupts here?
      null;

   end Initialise_Interrupts;

   procedure Complete_Interrupt_Initialisation is null;

   procedure Attach_Handler (Interrupt : External_Interrupt_Id;
                             Handler   : Parameterless_Handler;
                             Priority  : Interrupt_Priority)
   is
      pragma Unreferenced (Priority);
      IECV : Interrupt_Enable_No_Change_Field := (others => No_Change);
   begin
      --  All interrupts use the same priority since we cannot mask priorities
      --  from software

      AIC.Source_Mode_Register (Interrupt).Priority_Level := 1;

      AIC_Vector_Table (Interrupt) := Handler;

      IECV (Interrupt) := Enable;
      Interrupt_Enable_Command_Register.Interrupt := IECV;
   end Attach_Handler;

   function Current_Interrupt_Priority return Any_Priority is
      Interrupt_Id : constant External_Interrupt_Id :=
                       Interrupt_Status_Register.Current_Interrupt;
   begin
      --  On the AT91SAM7S there is only one interrupt priority level
      --  available since we cannot mask the hardware interrupts based on
      --  priority.

      return
        (if Source_Mode_Register (Interrupt_Id).Priority_Level > 0
         then Interrupt_Priority'Last else Priority'Last);
   end Current_Interrupt_Priority;

   procedure Set_Hardware_Priority (P : Any_Priority) is null;

   procedure Clear_Hardware_Priority is null;

   function Handler_Protected_Object
     (Interrupt : External_Interrupt_Id) return Protected_Id_With_No is
   begin
      return Protected_Object_From_Access
        (Parameterless_Access (AIC_Vector_Table (Interrupt)));
   end Handler_Protected_Object;

   function Has_Outstanding_Interrupts (Above_Priority : Any_Priority)
                                        return Boolean is
   begin
      return Core_Interrupt_Status_Register.NIRQ  = Active
        and Above_Priority = 0;
   end Has_Outstanding_Interrupts;

end Oak.Processor_Support_Package.Interrupts;
