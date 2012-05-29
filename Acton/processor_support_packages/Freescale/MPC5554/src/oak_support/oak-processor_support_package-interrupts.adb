with ISA.Power;
with MPC5554.Flash;
with Oak.Agent.Tasks;
with Oak.Agent.Tasks.Protected_Objects;
with Oak.Core_Support_Package.Task_Interrupts;
with Oak.Core_Support_Package.Task_Support;

with System.Machine_Code;     use System.Machine_Code;

package body Oak.Processor_Support_Package.Interrupts is
   --  Stack frame design for interrupt handling on the MPC5554 with support
   --  for SPE unit:
   --               *****************
   --     0x68      *   GPR12       * ---------------
   --     0x60      *   GPR11       *       ^
   --     0x58      *   GPR10       *       |
   --     0x50      *   GPR9        *       |
   --     0x48      *   GPR8        *       |
   --     0x40      *   GPR7        *   GPR (64 bit)
   --     0x38      *   GPR6        *       |
   --     0x30      *   GPR5        *       |
   --     0x28      *   GPR4        *       |
   --     0x20      *   GPR3        *       v
   --     0x18      *   GPR0        * ---------------
   --     0x14      *   CR          *       ^
   --     0x10      *   XER         *       |
   --     0x0C      *   CTR         *  local registers (32 bit)
   --     0x08      *   LR          *       |
   --     0x04      *   SRR1        *       v
   --     0x00      *   SRR0        * ---------------
   --               *****************
   --
   --  Depth: 0x70 or 112 bytes

   procedure External_Interrupt_Handler is
   begin
      Oak.Core_Support_Package.Task_Interrupts.Enable_SPE_Instructions;
      --  Create Stack frame, save working registers and save srr0 and SRR1
      Asm (
           "stwu     r1,  -0x70(r1)"  & ASCII.LF & ASCII.HT &
           "evstdd   r3,  0x20(r1)"   & ASCII.LF & ASCII.HT &
           "mfsrr0   r3"              & ASCII.LF & ASCII.HT &
           "stw      r3,  0x00(r1)"   & ASCII.LF & ASCII.HT &
           "mfsrr1   r3"              & ASCII.LF & ASCII.HT &
           "stw      r3,  0x04(r1)"   & ASCII.LF & ASCII.HT &
           --  Load INTC_IACKR address
           "lis      r3,  %0@ha"      & ASCII.LF & ASCII.HT &
           "lwz      r3,  %0@l(r3)"   & ASCII.LF & ASCII.HT &
           --  Read ISR Address from vector table.
           "lwz      r4,  0x4(r3)"    & ASCII.LF & ASCII.HT &
           --  Read object address from vector table.
           "lwz      r3,  0x0(r3)"    & ASCII.LF & ASCII.HT &
           --  Save r9 and r10
           "evstdd   r9,  0x50(r1)"   & ASCII.LF & ASCII.HT &
           "evstdd   r10, 0x58(r1)",
        Inputs => System.Address'Asm_Input ("i",
          MPC5554.INTC.Interrupt_Acknowledge_Register'Address),
        Volatile => True);

      Oak.Core_Support_Package.Task_Support.Disable_Oak_Wake_Up_Interrupt;

      Asm (
           --  Enable processor recongition of interrupts
           "wrteei   1"               & ASCII.LF & ASCII.HT &
           --  Save rest of the context
           "evstdd   r0,  0x18(r1)"   & ASCII.LF & ASCII.HT &
           "evstdd   r4,  0x28(r1)"   & ASCII.LF & ASCII.HT &
           "evstdd   r5,  0x30(r1)"   & ASCII.LF & ASCII.HT &
           "evstdd   r6,  0x38(r1)"   & ASCII.LF & ASCII.HT &
           "evstdd   r7,  0x40(r1)"   & ASCII.LF & ASCII.HT &
           "evstdd   r8,  0x48(r1)"   & ASCII.LF & ASCII.HT &
           "evstdd   r11, 0x60(r1)"   & ASCII.LF & ASCII.HT &
           "evstdd   r12, 0x68(r1)"   & ASCII.LF & ASCII.HT &
           "mflr     r5"              & ASCII.LF & ASCII.HT &
           "stw      r5,  0x08(r1)"   & ASCII.LF & ASCII.HT &
           "mfctr    r5"              & ASCII.LF & ASCII.HT &
           "stw      r5,  0x0C(r1)"   & ASCII.LF & ASCII.HT &
           "mfxer    r5"              & ASCII.LF & ASCII.HT &
           "stw      r5,  0x10(r1)"   & ASCII.LF & ASCII.HT &
           "mfcr     r5"              & ASCII.LF & ASCII.HT &
           "stw      r5,  0x14(r1)"   & ASCII.LF & ASCII.HT &
           --  Store ISR address to link register
           "mtlr     r4"              & ASCII.LF & ASCII.HT &
           --  Branch to ISR...
           "blrl"                     & ASCII.LF & ASCII.HT &
           --  ... and return here when done. Restore the context except for
           --  working register r3 and r4
           "lwz      r3,  0x08(r1)"   & ASCII.LF & ASCII.HT &
           "mtlr     r3"              & ASCII.LF & ASCII.HT &
           "lwz      r3,  0x0C(r1)"   & ASCII.LF & ASCII.HT &
           "mtctr    r3"              & ASCII.LF & ASCII.HT &
           "lwz      r3,  0x10(r1)"   & ASCII.LF & ASCII.HT &
           "mtxer    r3"              & ASCII.LF & ASCII.HT &
           "lwz      r3,  0x14(r1)"   & ASCII.LF & ASCII.HT &
           "mtcr     r3"              & ASCII.LF & ASCII.HT &
           "evldd    r0,  0x18(r1)"   & ASCII.LF & ASCII.HT &
           "evldd    r5,  0x30(r1)"   & ASCII.LF & ASCII.HT &
           "evldd    r6,  0x38(r1)"   & ASCII.LF & ASCII.HT &
           "evldd    r7,  0x40(r1)"   & ASCII.LF & ASCII.HT &
           "evldd    r8,  0x48(r1)"   & ASCII.LF & ASCII.HT &
           "evldd    r11, 0x60(r1)"   & ASCII.LF & ASCII.HT &
           "evldd    r12, 0x68(r1)"   & ASCII.LF & ASCII.HT &
           --  mbar to ensure the store to clear flag bit has completed
           "mbar"                     & ASCII.LF & ASCII.HT &
           --  Load INTC_EOIR address
           "lis      r3, %0@ha"       & ASCII.LF & ASCII.HT &
           "li       r4, 0"           & ASCII.LF & ASCII.HT &
           --  Disable processor recognition of interrupts
           "wrteei   0"               & ASCII.LF & ASCII.HT &
           --  Store to INTC_EOIR to complete interrupt
           "stw      r4, %0@l(r3)",
        Inputs => System.Address'Asm_Input ("i",
          MPC5554.INTC.End_Of_Interrupt_Register'Address),
        Volatile => True);

      Oak.Core_Support_Package.Task_Support.Enable_Oak_Wake_Up_Interrupt;

      --  Restore SRR0 and SRR1, restore working registers and delete
      --  stack frame
      Asm (
           "lwz      r3, 0x00(r1)"    & ASCII.LF & ASCII.HT &
           "mtsrr0   r3"              & ASCII.LF & ASCII.HT &
           "lwz      r4, 0x04(r1)"    & ASCII.LF & ASCII.HT &
           "mtsrr1   r4"              & ASCII.LF & ASCII.HT &
           "evldd    r3,  0x20(r1)"   & ASCII.LF & ASCII.HT &
           "evldd    r4,  0x28(r1)"   & ASCII.LF & ASCII.HT &
           "evldd    r9,  0x50(r1)"   & ASCII.LF & ASCII.HT &
           "evldd    r10, 0x58(r1)"   & ASCII.LF & ASCII.HT &
           "stwu     r1,  0x70(r1)"   & ASCII.LF & ASCII.HT &
           --  Return
           "rfi",
        Volatile => True);

   end External_Interrupt_Handler;

   procedure Initialise_Interrupts is
      use MPC5554.Flash;
   begin
      Oak.Core_Support_Package.Task_Interrupts.Disable_External_Interrupts;

      Module_Config_Register :=
        (Vector_Table_Entry_Size => Eight_Bytes,
         Hardware_Vector_Enable  => Software_Vector_Mode);
      Interrupt_Acknowledge_Register := INTC_Vector_Table'Address;
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

      Oak.Core_Support_Package.Task_Interrupts.Enable_External_Interrupts;
   end Initialise_Interrupts;

   procedure Complete_Interrupt_Initialisation is
      use MPC5554.Flash;
   begin
      Oak.Core_Support_Package.Task_Interrupts.Disable_External_Interrupts;

      Low_Mid_Address_Space_Block_Locking_Register :=
        (Locks => Editable,
         Shadow_Lock => Locked,
         Mid_Address_Locks => (others => Locked),
         Low_Address_Locks => (others => Locked));

      Completed_Flash_Programming;

      Oak.Core_Support_Package.Task_Interrupts.Enable_External_Interrupts;
   end Complete_Interrupt_Initialisation;

   procedure Attach_Handler (Interrupt : Oak_Interrupt_Id;
                             Handler   : Parameterless_Handler;
                             Priority  : Interrupt_Priority)
   is
      use MPC5554.Flash;
      use Oak.Core_Support_Package.Task_Interrupts;
   begin
      Oak.Core_Support_Package.Task_Interrupts.Disable_External_Interrupts;

      if INTC_Vector_Table (Interrupt) /= Handler then
         if Programmed_Vector_Table (Interrupt) /= Default_Handler then
            raise Program_Error;
         end if;

         Program_Protected_Access
           (P           => Handler,
            Destination => INTC_Vector_Table (Interrupt)'Address);
      end if;

      Priority_Select_Register_Array (Interrupt)
        := MPC5554_Interrupt_Priority (Priority - Interrupt_Priority'First);

      Oak.Core_Support_Package.Task_Interrupts.Enable_External_Interrupts;
   end Attach_Handler;

   procedure Get_Resource
     (PO : access Agent.Tasks.Protected_Objects.Protected_Agent'Class)
   is
      FIFO : Interrupt_FIFO renames Interrupt_Priority_FIFO;
      P : MPC5554_Interrupt_Priority;
   begin
      if PO.Normal_Priority in Interrupt_Priority then
         P := MPC5554_Interrupt_Priority (PO.Normal_Priority -
            Interrupt_Priority'First);
         FIFO.Top := FIFO.Top + 1;
         FIFO.Stack (FIFO.Top) := P;
         Current_Priority_Register := P;
         ISA.Power.Memory_Barrier;
         ISA.Power.Instruction_Synchronize;
      end if;
   end Get_Resource;

   procedure Release_Resource
     (PO : access Agent.Tasks.Protected_Objects.Protected_Agent'Class)
   is
      FIFO : Interrupt_FIFO renames Interrupt_Priority_FIFO;
   begin
      if PO.Normal_Priority in Interrupt_Priority then
         ISA.Power.Memory_Barrier;
         FIFO.Top := FIFO.Top - 1;
         if FIFO.Top = 0 then
            Current_Priority_Register := 0;
         else
            Current_Priority_Register := FIFO.Stack (FIFO.Top);
         end if;
      end if;
   end Release_Resource;

end Oak.Processor_Support_Package.Interrupts;
