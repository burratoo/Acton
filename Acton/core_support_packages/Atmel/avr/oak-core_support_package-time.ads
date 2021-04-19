------------------------------------------------------------------------------------------
--                                                                                      --
--                               OAK CORE SUPPORT PACKAGE                               --
--                                      ATMEL AVR                                       --
--                                                                                      --
--                             OAK.CORE_SUPPORT_PACKAGE.TIME                            --
--                                                                                      --
--                       Copyright (C) 2012-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

--
--  The e200z6 core in the MPC5554 has a 64 bit Time Base register that we can
--  used to base the clock on. Since all timing events are based off this
--  register, it makes sense for the internal representation of time to be a 64
--  bit unsigned integer. However, since ARM D-8-31 specifies that Time_Span
--  holds negative numbers and 64-bits is the largest integer we have to use,
--  we have to deal with the tricky situation that occurs when we perform an
--  operation on two Time variables that result in a Time_Span that cannot be
--  represented by the Time_Span type. For example
--                        Time'Last - Time'First
--  results in a number that cannot be represented in a signed 64 bit integer.

--  While it could be dealt with, prehaps by returning an error or the max
--  value possible (we do have the downside that the end user now has to check
--  for this), it does make these simple operations much more difficuilt. It
--  becomes much easier when Time and Time_Span are dervived from the same
--  types as this problem doesn't show up. And losing a bit in the process is
--  not a big issue. At 132Mhz, it will take over 2200 years to cycle through
--  all 2^bits.
--
--  Actually we run into the problem where we cannot acurrately represent
--  nanoseconds internally (ie all numbers less than 1000ns), as most clocks
--  are slow enough that their resolution is in nanoseconds. And since we store
--  time in terms of these ticks, we run into rounding problems. Which do not
--  cause problems until you multiple these numbers or repeatedly add some
--  nanoseconds to an existing Time. In this case the rounding errors
--  accumulate, resulting in a large difference between the expected time and
--  the time given by the results. More likely to be an issue on slower
--  microcontrollers as their timer resolution moves into the microseconds
--  region. Also makes the time range useable similar to existing systems.

package Oak.Core_Support_Package.Time with Pure is

   MHz         : constant := 1E6;
   KHz         : constant := 1E3;
   Clock_Speed : constant := 1024;

   type Oak_Time is range -(2 ** 31) .. +(2 ** 31 - 1) with Size => 32;

   --     type Oak_Time_Span is new Oak_Time; for Oak_Time_Span'Size use 64;

   Oak_Time_Unit      : constant := 1.0 / Clock_Speed;
   Oak_Time_Span_Unit : constant := 1;

   Oak_Tick : constant := 1;

   Ticks_Per_Second : constant := Clock_Speed;

   function Get_Clock return Oak_Time;
   pragma Import (Assembler, Get_Clock, "oak_get_clock");

   --
   --  At 132 MHz -> 7.58 ns per tick. 132 x 10^6 ticks per second.
   --  In Hex : 0x7DE2900 Which is less than 2^32 or 0xFFFFFFFF.
   Seconds_Max : constant := 2 ** 31 - 1;

end Oak.Core_Support_Package.Time;
