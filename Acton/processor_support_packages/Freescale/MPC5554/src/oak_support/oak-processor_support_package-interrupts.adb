with ISA.Power;
with MPC5554.Flash;
with Oak.Agent.Protected_Objects;
with Oak.Core_Support_Package.Interrupts;

package body Oak.Processor_Support_Package.Interrupts is

   procedure External_Interrupt_Handler (Interrupt_Id : Oak_Interrupt_Id) is
   begin
      INTC_Vector_Table (Interrupt_Id).all;
      End_Of_Interrupt_Register := End_Interrupt;
   end External_Interrupt_Handler;

   function External_Interrupt_Id return Oak_Interrupt_Id is
   begin
      return Interrupt_Acknowledge_Component.Interrupt_Vector;
   end External_Interrupt_Id;

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

   procedure Attach_Handler (Interrupt : Oak_Interrupt_Id;
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

   procedure Get_Resource
     (PO : access Agent.Protected_Objects.Protected_Agent'Class)
   is
      FIFO : Interrupt_FIFO renames Interrupt_Priority_FIFO;
      P : MPC5554_Interrupt_Priority;
   begin
      if PO.Normal_Priority in Interrupt_Priority then
         P := MPC5554_Interrupt_Priority (PO.Normal_Priority - Priority'Last);
         FIFO.Top := FIFO.Top + 1;
         FIFO.Stack (FIFO.Top) := P;
         Current_Priority_Register := P;
         ISA.Power.Memory_Barrier;
         ISA.Power.Instruction_Synchronize;
      end if;
   end Get_Resource;

   procedure Release_Resource
     (PO : access Agent.Protected_Objects.Protected_Agent'Class)
   is
      FIFO : Interrupt_FIFO renames Interrupt_Priority_FIFO;
   begin
      if PO.Normal_Priority in Interrupt_Priority then
         ISA.Power.Memory_Barrier;
         FIFO.Top := FIFO.Top - 1;
         if FIFO.Top = 0 then
            Current_Priority_Register := MPC5554_Interrupt_Priority'First;
         else
            Current_Priority_Register := FIFO.Stack (FIFO.Top + 1);
         end if;
      end if;
   end Release_Resource;

end Oak.Processor_Support_Package.Interrupts;
