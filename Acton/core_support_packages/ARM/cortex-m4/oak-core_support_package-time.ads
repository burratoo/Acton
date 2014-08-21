with Oak.Project_Support_Package; use Oak.Project_Support_Package;

package Oak.Core_Support_Package.Time with Pure is

   type Oak_Time is mod 2 ** 63 with Size => 64;
   type Oak_Time_Span is range -(2 ** 63) .. +(2 ** 63 - 1) with Size => 64;

   --     type Oak_Time_Span is new Oak_Time; for Oak_Time_Span'Size use 32;

   Oak_Time_Unit      : constant := 1.0 / Clock_Speed;
   Oak_Time_Span_Unit : constant := 1;

   Oak_Tick : constant := 1;

   Ticks_Per_Second : constant := Clock_Speed;

   function Get_Clock return Oak_Time
     with Import,
          Convention => Assembler,
          External_Name => "oak_get_clock";

   Seconds_Max : constant := 2 ** 31 - 1;

end Oak.Core_Support_Package.Time;
