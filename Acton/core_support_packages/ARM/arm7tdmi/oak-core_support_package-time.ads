package Oak.Core_Support_Package.Time with Pure is

   MHz         : constant := 1E6;
   Clock_Speed : constant := 48 * MHz;

   type Oak_Time is mod 2 ** 31 with Size => 32;
   type Oak_Time_Span is range -(2 ** 31) .. +(2 ** 31 - 1) with Size => 32;

   --     type Oak_Time_Span is new Oak_Time; for Oak_Time_Span'Size use 32;

   Oak_Time_Unit      : constant := 1 / 1E4;
   Oak_Time_Span_Unit : constant := 1;

   Oak_Tick : constant := 1;

   Ticks_Per_Second : constant := 10_000;

   function Get_Clock return Oak_Time
     with Import,
          Convention => Assembler,
          External_Name => "oak_get_clock";

   Seconds_Max : constant := 2 ** 31 - 1;

end Oak.Core_Support_Package.Time;
