------------------------------------------------------------------------------------------
--                                                                                      --
--                               OAK CORE SUPPORT PACKAGE                               --
--                                      ATMEL AVR                                       --
--                                                                                      --
--                          OAK.CORE_SUPPORT_PACKAGE.TIME.CLOCK                         --
--                                                                                      --
--                       Copyright (C) 2012-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

package Oak.Core_Support_Package.Time.Clock with Preelaborate is

   function Get_Clock return Oak_Time;
   pragma Export (Assembler, Get_Clock, "oak_get_clock");

   procedure Increment_Clock;
   pragma Export (Asm, Increment_Clock, "__Clock_Interrupt");

   procedure Set_Interrupt_Time (T : Oak_Time);

private
   Avr_Clock         : Oak_Time := 0;
   Interrupt_Time    : Oak_Time;

   function Get_Clock return Oak_Time is (Avr_Clock);

end Oak.Core_Support_Package.Time.Clock;
