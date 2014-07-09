with Atmel;               use Atmel;
with Atmel.AT91SAM7S;     use Atmel.AT91SAM7S;
with Atmel.AT91SAM7S.AIC; use Atmel.AT91SAM7S.AIC;
with Atmel.AT91SAM7S.PIT; use Atmel.AT91SAM7S.PIT;

package body Oak.Processor_Support_Package.Time is

   procedure Initialise_Clock is
   begin
      Interrupt_Disable_Command_Register.Interrupt :=
        (P_SYSC => Disable,
         others => No_Change);

      PIT.Mode_Register :=
        (Periodic_Interval_Value           =>
           Periodic_Interval (Clock_Speed / 16 / Ticks_Per_Second),
         Period_Interval_Timer             => Enable,
         Periodic_Interval_Timer_Interrupt => Enable);

      --  Attach interrupts

      Fast_Forcing_Enable_Register.Interrupt :=
        (P_SYSC => Enable, others => No_Change);

      Interrupt_Enable_Command_Register.Interrupt :=
        (P_FIQ  => Enable,
         P_SYSC => Enable,
         others => No_Change);
   end Initialise_Clock;

   procedure Update_Alarm (To : in Oak.Core_Support_Package.Time.Oak_Time) is
   begin
      Alarm_Time  := To;
      Alarm_Armed := True;
   end Update_Alarm;

end Oak.Processor_Support_Package.Time;
