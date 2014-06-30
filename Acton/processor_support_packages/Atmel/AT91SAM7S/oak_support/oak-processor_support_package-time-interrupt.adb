with Atmel.AT91SAM7S;     use Atmel.AT91SAM7S;
with Atmel.AT91SAM7S.AIC; use Atmel.AT91SAM7S.AIC;

with System.Machine_Code; use System.Machine_Code;

package body Oak.Processor_Support_Package.Time.Interrupt is
   procedure Update_Clock is
      Status : Interrupt_Status_Type with Unreferenced;

   begin
      Internal_Clock := Internal_Clock + 1;

      Status := Interrupt_Status_Register; -- Clear status register

      Interrupt_Clear_Command_Register.Clear_Interrupt :=
        (P_SYSC => True, others => False);
      End_Of_Interrupt_Command_Register := 1;

      if Alarm_Armed and then Internal_Clock > Alarm_Time then
         --  Branch via instruction rather than procedure call to ensure
         --  bl instruction is not used

         Asm ("b oak__core_support_package__interrupts__decrementer_interrupt",
              Volatile => True);
      else
         --  Return from interrupt
         Asm ("subs pc, r14, #4", Volatile => True);
      end if;
   end Update_Clock;

end Oak.Processor_Support_Package.Time.Interrupt;
