with Oak.Core_Support_Package.Time; use Oak.Core_Support_Package.Time;
with Oak.Project_Support_Package;   use Oak.Project_Support_Package;

package Oak.Core_Support_Package.Clock with Preelaborate is

   ---------------------------------
   -- INTERNAL PACKAGE COMPONENTS --
   ---------------------------------

   procedure Update_Alarm (To : in Oak_Time);

   Time_Base_Update : constant := 1_000; -- 1 ms
   Time_Base_Tick   : constant := Clock_Speed / Time_Base_Update;

private

   function Get_Clock return Oak_Time
     with Export,
     Convention => Assembler,
     External_Name => "oak_get_clock";

   Time_Base : Oak_Time := Time_Base_Tick with Volatile;

   --  Time_Base starts at Time_Base_Tick since the Time Base increments only
   --  in Time_Base_Tick steps and always stores the time of the next occuring
   --  Time_Base_Tick. The clock is derived from this by subtracting the value
   --  of the decrementer which is used to increment the Time Base.

   Alarm_Time  : Oak_Time := 0;
   Alarm_Armed : Boolean  := False;

end Oak.Core_Support_Package.Clock;
