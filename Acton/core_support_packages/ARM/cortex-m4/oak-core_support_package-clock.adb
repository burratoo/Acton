with ISA.ARM.Cortex_M4;         use ISA.ARM.Cortex_M4;
with ISA.ARM.Cortex_M4.SysTick; use ISA.ARM.Cortex_M4.SysTick;

with System;              use System;
with System.Machine_Code; use System.Machine_Code;

package body Oak.Core_Support_Package.Clock is

   ---------------
   -- Get_Clock --
   ---------------

   --  The Cortex-M4 only has one 23-bit timer from which timing information
   --  can be generated. Thus it needs to be used as both the Clock source as
   --  well as an alarm. Complicating matters is that the ARM (Ada) requires
   --  a 1 ms tick, a Time_Unit of no greater than 20µs and a 50 year range of
   --  values. This requires a 64-bit number.

   --  The arrangement used by Oak should give a useful range of just over
   --  1700 years while incrementing the clock by at the clock rate of the
   --  processor. This allows for more precise monitioring of the timing of
   --  agents running on Oak.

   --  The clock itself is derived from a 64-bit time base and a decrementer.
   --  The decrementer is used to generate the Time_Base_Tick (currently 1 ms),
   --  which is used to increment the time base by the value of the
   --  Time_Base_Tick every Time_Base_Tick. By having the time base set to the
   --  next occuring Time_Base_Tick, the actual clock can be derived by
   --  subtracting the value remaining in the decrementer from the time base.

   --  A wrinkle in this technique is the decrementer can reach 0 and reload
   --  while the three read operations to calculate the value of the clock are
   --  carried out – which are two to get the time base (since it is made up
   --  of two words) and one to get the current value of the decrementer. The
   --  effect this has on the time return depends on the order of the read
   --  (and noting the handler for the decrementer will have also run since
   --  it runs at the highest priority), but put simply the wrong time will be
   --  return.

   --  To cover this memory exclusive operations are used to ensure that the
   --  timer has not be serviced while the timer has been read.

   function Get_Clock return Oak_Time is
      Decrementer  : Sys_Tick;
      Clock        : Oak_Time;

      Interrupted  : Boolean;

      Ex_Flag         : Integer;
      --  Use a word type to match the word load/store instructions below
      Ex_Flag_Address : Address := Ex_Flag'Address;

   begin
      loop
         --  Get memory reservation

         Asm ("ldrex r0, [%0]",
              Inputs   => Address'Asm_Input ("r", Ex_Flag_Address),
              Clobber  => "r0",
              Volatile => True);

         Data_Memory_Barrier;

         Clock := Time_Base;
         Decrementer := SysTick.Current_Value_Register;

         --  Check reservation still exists. Loop otherwise.
         Asm ("strex %0, r0, [%1]",
              Outputs  => Boolean'Asm_Output ("=r", Interrupted),
              Inputs   => Address'Asm_Input ("r", Ex_Flag_Address),
              Volatile => True);

         --  Silence warnings that Interrupted is never set (hint: it is).
         pragma Warnings (Off);
         exit when not Interrupted;
         pragma Warnings (On);
      end loop;

      --  Subtract decrementer

      return Clock - Oak_Time (Decrementer);
   end Get_Clock;

   procedure Update_Alarm (To : in Oak.Core_Support_Package.Time.Oak_Time) is
   begin
      Alarm_Time  := To;
      Alarm_Armed := True;
   end Update_Alarm;
end Oak.Core_Support_Package.Clock;
