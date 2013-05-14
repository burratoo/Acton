--  with Oak.Core_Support_Package.Task_Interrupts;
--  use  Oak.Core_Support_Package.Task_Interrupts;
with System; use System;
with System.Machine_Code; use System.Machine_Code;
with Oak.Core_Support_Package.Task_Support;
with AVR; use AVR;
with AVR.Timers; use AVR.Timers;

package body Oak.Core_Support_Package.Time.Clock is

   pragma Suppress (All_Checks);
   --  Suppress all checks since they get in the way and cause unpredictable
   --  problems.

--  Downside of this approach is that we will loose ticks when interrupts
--  are disabled.

   procedure Increment_Clock is
   begin

      --  Save working registers r18 -> r27
      --  Store SREG in the zero register

      Asm ("push r17"          & ASCII.LF & ASCII.HT &
           "push r18"          & ASCII.LF & ASCII.HT &
           "push r19"          & ASCII.LF & ASCII.HT &
           "push r20"          & ASCII.LF & ASCII.HT &
           "push r21"          & ASCII.LF & ASCII.HT &
           "push r22"          & ASCII.LF & ASCII.HT &
           "push r23"          & ASCII.LF & ASCII.HT &
           "push r24"          & ASCII.LF & ASCII.HT &
           "push r25"          & ASCII.LF & ASCII.HT &
           "push r26"          & ASCII.LF & ASCII.HT &
           "push r27"          & ASCII.LF & ASCII.HT &
           "in   r17, __SREG__",
           Volatile => True);

      Avr_Clock := Avr_Clock + 1;
      Timer_Counter2_Interrupt_Flag_Register.Overflow := Raised;

      if Avr_Clock > Interrupt_Time then

         Asm ("ldi r18, 0x80" & ASCII.LF & ASCII.HT &
              "mov r1, r18", Volatile => True);
         Task_Support.Context_Switch_To_Kernel;

      end if;

      --  Restore working registers.

      Asm ("out __SREG__, r17" & ASCII.LF & ASCII.HT &
           "pop r27"           & ASCII.LF & ASCII.HT &
           "pop r26"           & ASCII.LF & ASCII.HT &
           "pop r25"           & ASCII.LF & ASCII.HT &
           "pop r24"           & ASCII.LF & ASCII.HT &
           "pop r23"           & ASCII.LF & ASCII.HT &
           "pop r22"           & ASCII.LF & ASCII.HT &
           "pop r21"           & ASCII.LF & ASCII.HT &
           "pop r20"           & ASCII.LF & ASCII.HT &
           "pop r19"           & ASCII.LF & ASCII.HT &
           "pop r18"           & ASCII.LF & ASCII.HT &
           "pop r17",
           Volatile => True);

      --  Return from interrupt.

      Asm ("reti", Volatile => True);
   end Increment_Clock;

   procedure Set_Interrupt_Time (T : Oak_Time) is
   begin
      Interrupt_Time := T;
   end Set_Interrupt_Time;

end Oak.Core_Support_Package.Time.Clock;
