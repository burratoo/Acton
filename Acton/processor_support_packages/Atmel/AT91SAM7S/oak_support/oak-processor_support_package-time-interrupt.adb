with Atmel.AT91SAM7S;     use Atmel.AT91SAM7S;
with Atmel.AT91SAM7S.AIC; use Atmel.AT91SAM7S.AIC;
with Atmel.AT91SAM7S.PIT; use Atmel.AT91SAM7S.PIT;

with ISA.ARM.ARM7TDMI; use ISA.ARM.ARM7TDMI;

with System;              use System;
with System.Machine_Code; use System.Machine_Code;

package body Oak.Processor_Support_Package.Time.Interrupt is
   procedure Update_Clock is
      Ack       : Address with Unreferenced;
      PIT_Value : Timer_Value_Type with Unreferenced;
   begin
      Internal_Clock := Internal_Clock + 1;

      Ack := Interrupt_Vector_Register; -- Acknowledge interrupt
      PIT_Value := Periodic_Interval_Timer_Value_Register;

      Interrupt_Clear_Command_Register.Clear_Interrupt :=
        (P_FIQ => True, P_SYSC => True, others => False);
      End_Of_Interrupt_Command_Register := 1;

      if Alarm_Armed and then Internal_Clock > Alarm_Time
        and then not Saved_Program_Status_Register.IRQ_Disable
      then
         --  Branch via instruction rather than procedure call to ensure
         --  bl instruction is not used
         Alarm_Armed := False;
         Asm ("b oak__core_support_package__interrupts__decrementer_interrupt",
              Volatile => True);
      end if;
      --  Return from interrupt
      Asm ("subs pc, r14, #4", Volatile => True);
   end Update_Clock;

end Oak.Processor_Support_Package.Time.Interrupt;
