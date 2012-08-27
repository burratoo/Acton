with System;

package Oak.Core_Support_Package.Time.Clock with Preelaborate is

   function Get_Clock return Oak_Time;
   pragma Export (Assembler, Get_Clock, "oak_get_clock");

   procedure Increment_Clock;
   pragma Export (Asm, Increment_Clock, "__Clock_Interrupt");

   procedure Set_Interrupt_Time (T : Oak_Time);

private
   Avr_Clock         : Oak_Time := 0;
   Interrupt_Time    : Oak_Time;
   Interrupt_Enabled : Boolean := False;
   System_Call_Address : constant System.Address := System'To_Address (16#2#);

   function Get_Clock return Oak_Time is (Avr_Clock);

end Oak.Core_Support_Package.Time.Clock;
