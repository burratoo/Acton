with MPC5554.Flash;
with Oak.Core_Support_Package.Interrupts;
use Oak.Core_Support_Package.Interrupts;

with Oak.Brokers.Protected_Objects; use Oak.Brokers.Protected_Objects;

with System.Machine_Code; use System.Machine_Code;

with ISA; use ISA;

with ISA.Power.e200.Processor_Control_Registers;
use ISA.Power.e200.Processor_Control_Registers;

package body Oak.Processor_Support_Package.Interrupts is

   procedure External_Interrupt_Handler (Interrupt_Id : External_Interrupt_Id)
   is
   begin
      INTC_Vector_Table (Interrupt_Id).all;
      End_Of_Interrupt_Register := End_Interrupt;
   end External_Interrupt_Handler;

   function Get_External_Interrupt_Id return External_Interrupt_Id is
   begin
      return Interrupt_Acknowledge_Component.Interrupt_Vector;
   end Get_External_Interrupt_Id;

   procedure Initialise_Interrupts is
      use MPC5554.Flash;
   begin
      Oak.Core_Support_Package.Interrupts.Disable_External_Interrupts;

      Module_Config_Register :=
        (Vector_Table_Entry_Size => Four_Bytes,
         Hardware_Vector_Enable  => Software_Vector_Mode);
      Interrupt_Acknowledge_Register := Null_Address;
      Current_Priority_Register := MPC5554_Interrupt_Priority'First;

      Initialise_For_Flash_Programming;

      Unlock_Space_Block_Locking_Register (Space => Low_Primary);
      Low_Mid_Address_Space_Block_Locking_Register :=
        (Locks => Editable,
         Shadow_Lock => Locked,
         Mid_Address_Locks => (others => Locked),
         Low_Address_Locks => (B3 => Unlocked, others => Locked));

      Unlock_Space_Block_Locking_Register (Space => Low_Secondary);
      Secondary_Low_Mid_Address_Space_Block_Locking_Register :=
        (Locks => Editable,
         Shadow_Lock => Locked,
         Mid_Address_Locks => (others => Locked),
         Low_Address_Locks => (B3 => Unlocked, others => Locked));
   end Initialise_Interrupts;

   procedure Complete_Interrupt_Initialisation is
      use MPC5554.Flash;
   begin
      Low_Mid_Address_Space_Block_Locking_Register :=
        (Locks => Editable,
         Shadow_Lock => Locked,
         Mid_Address_Locks => (others => Locked),
         Low_Address_Locks => (others => Locked));

      Completed_Flash_Programming;
   end Complete_Interrupt_Initialisation;

   procedure Attach_Handler (Interrupt : External_Interrupt_Id;
                             Handler   : Parameterless_Handler;
                             Priority  : Interrupt_Priority)
   is
      use MPC5554.Flash;
   begin
      if INTC_Vector_Table (Interrupt) /= Handler then
         if Programmed_Vector_Table (Interrupt) /= Default_Handler then
            raise Program_Error;
         end if;

         Program_Protected_Access
           (P           => Handler,
            Destination => INTC_Vector_Table (Interrupt)'Address);
      end if;

      Priority_Select_Register_Array (Interrupt)
        := MPC5554_Interrupt_Priority (Priority - System.Priority'Last);
   end Attach_Handler;

   function Current_Interrupt_Priority return Any_Priority is
   begin
      --  The first priority of the interrupt hardware is mapped to
      --  Priority'Last - the priority level that comes before
      --  Interrupt_Priority'First.

      return Any_Priority (Current_Priority_Register) + Priority'Last;
   end Current_Interrupt_Priority;

   procedure Set_Hardware_Priority (P : Any_Priority) is
   begin
      if P in Interrupt_Priority then
         Current_Priority_Register :=
           MPC5554_Interrupt_Priority (P - Priority'Last);
      else
         Current_Priority_Register := MPC5554_Interrupt_Priority'First;
      end if;
   end Set_Hardware_Priority;

   procedure Clear_Hardware_Priority is
   begin
      Current_Priority_Register := MPC5554_Interrupt_Priority'First;
   end Clear_Hardware_Priority;

   function Handler_Protected_Object
     (Interrupt : External_Interrupt_Id) return Protected_Id_With_No is
   begin
      return Protected_Object_From_Access
        (Parameterless_Access (INTC_Vector_Table (Interrupt)));
   end Handler_Protected_Object;

   function Has_Outstanding_Interrupts (Above_Priority : Any_Priority)
                                        return Boolean is
      pragma Unreferenced (Above_Priority);

      Interrupt_Present : Boolean := False;
      Oak_Current_MSR   : Machine_State_Register_Type;
      Oak_New_MSR       : Machine_State_Register_Type;

   begin
      --  Setup external interrupt vector to External_Interrupt_Present to
      --  see if an external interrupt is present and then enable external
      --  interrupts.
      Asm
        ("mtivor4 %1" & ASCII.LF & ASCII.HT &
           "mfmsr   %0",
         Inputs   => System.Address'Asm_Input
           ("r", External_Interrupt_Present'Address),
         Outputs  => Machine_State_Register_Type'Asm_Output
           ("=r", Oak_Current_MSR),
         Volatile => True);

      Oak_New_MSR := Oak_Current_MSR;
      Oak_New_MSR.External_Interrupts := Enable;

      Asm
        ("mtmsr   %1"  & ASCII.LF & ASCII.HT &
         "mtmsr   %2"  & ASCII.LF & ASCII.HT &
         "mtivor4 %3",                               -- register
         Inputs   => (Machine_State_Register_Type'Asm_Input
                      ("r", Oak_New_MSR),
                      Machine_State_Register_Type'Asm_Input
                        ("r", Oak_Current_MSR),
                      System.Address'Asm_Input
                        ("r", Oak.Core_Support_Package.Interrupts.
                           External_Interrupt_Handler'Address)),
         Outputs => Boolean'Asm_Output ("+r", Interrupt_Present),
         Volatile => True);
      return Interrupt_Present;
   end Has_Outstanding_Interrupts;

end Oak.Processor_Support_Package.Interrupts;
