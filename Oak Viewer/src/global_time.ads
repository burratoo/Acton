package Global_Time is
   type Processor_Cycles is mod 2 ** 63;

   Time : Processor_Cycles := 0;

   Time_First : constant Processor_Cycles := Processor_Cycles'First;
   Time_Last  : constant Processor_Cycles := Processor_Cycles'Last;
   Time_Zero  : constant Processor_Cycles := 0;


end Global_Time;
