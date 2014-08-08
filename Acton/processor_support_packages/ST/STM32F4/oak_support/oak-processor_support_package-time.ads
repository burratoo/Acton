--  Only exists on platforms where a processor specific timing source is used

with Oak.Core_Support_Package.Time; use Oak.Core_Support_Package.Time;

package Oak.Processor_Support_Package.Time with Preelaborate is
   procedure Initialise_Clock;

   procedure Update_Alarm (To : in Oak.Core_Support_Package.Time.Oak_Time);

private

   function Get_Clock return Oak_Time
     with Export,
       Convention => Assembler,
       External_Name => "oak_get_clock";

   Internal_Clock : Oak_Time := 0;
   Alarm_Time     : Oak_Time := 0;
   Alarm_Armed    : Boolean  := False;

   function Get_Clock return Oak_Time is (Internal_Clock);
end Oak.Processor_Support_Package.Time;
