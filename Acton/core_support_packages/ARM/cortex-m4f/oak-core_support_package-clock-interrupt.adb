------------------------------------------------------------------------------------------
--                                                                                      --
--                               OAK CORE SUPPORT PACKAGE                               --
--                                    ARM CORTEX M4F                                    --
--                                                                                      --
--                       OAK.CORE_SUPPORT_PACKAGE.CLOCK.INTERRUPT                       --
--                                                                                      --
--                       Copyright (C) 2014-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

with ISA.ARM.Cortex_M4;            use ISA.ARM.Cortex_M4;
with ISA.ARM.Cortex_M4.NVIC;       use ISA.ARM.Cortex_M4.NVIC;
with ISA.ARM.Cortex_M4.SysTick;    use ISA.ARM.Cortex_M4.SysTick;
with ISA.ARM.Cortex_M4.Exceptions; use ISA.ARM.Cortex_M4.Exceptions;

with Oak.Processor_Support_Package.Interrupts;
use Oak.Processor_Support_Package.Interrupts;

with System.Machine_Code; use System.Machine_Code;

package body Oak.Core_Support_Package.Clock.Interrupt is

   procedure Update_Clock is
      Base_Priority_Mask : Exception_Priority;
      Clock              : Oak_Time;
      Service_Alarm      : Boolean;

      Decrementer_Value : Sys_Tick := Current_Value_Register
        with Unreferenced;
      --  The read clears the SysTick flag

   begin
      Clear_Exclusive_Lock;

      Clock := Time_Base;

      Asm ("mrs %0, basepri",
           Outputs  => Exception_Priority'Asm_Output
             ("=r", Base_Priority_Mask),
           Volatile => True);

      --  We do not use Get_Clock since it requires a call that modifies lr
      --  and besides Time_Base before it is updated is more or less the time
      --  anyway since time will only have had two or three instructions by
      --  this point.

      if Alarm_Armed
        and then Base_Priority_Mask /= Oak_Mask_Priority
        and then Clock >= Alarm_Time
      then
         --  Branch via instruction rather than procedure call to ensure
         --  bl instruction is not used
         Service_Alarm := True;
      else
         Service_Alarm := False;
      end if;

      --  Adding two 64 bit numbers requires lots of registers we don't have
      --  access to so do it here after we have determined if the alarm needs
      --  servicing (so that the original value of Time_Base is no longer
      --  needed).

      Time_Base := Time_Base + Time_Base_Tick;
      if Service_Alarm then
         Alarm_Armed := False;
         Asm ("b " &
                "oak__core_support_package__" &
                "interrupts__decrementer_interrupt",
              Volatile => True);
      else
         Asm ("bx lr", Volatile => True);
      end if;
   end Update_Clock;

end Oak.Core_Support_Package.Clock.Interrupt;
