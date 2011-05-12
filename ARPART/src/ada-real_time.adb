package body Ada.Real_Time is

   Billion : constant := 1E9;

   -----------
   -- Clock --
   -----------

   function Clock return Time is
   begin
      return Time (Oak_PSP_Time.Get_Clock);
   end Clock;

   ---------
   -- "*" --
   ---------

   function "*" (Left : Time_Span; Right : Integer) return Time_Span is
   begin
      return Time_Span (Integer (Left) * Right);
   end "*";

   ---------
   -- "*" --
   ---------

   function "*" (Left : Integer; Right : Time_Span) return Time_Span is
   begin
      return Time_Span (Left * Integer (Right));
   end "*";

   ---------
   -- "/" --
   ---------

   function "/" (Left, Right : Time_Span) return Integer is
   begin
      return Integer (Left) / Integer (Right);
   end "/";

   ---------
   -- "/" --
   ---------

   function "/" (Left : Time_Span; Right : Integer) return Time_Span is
   begin
      return Time_Span (Integer (Left) / Right);
   end "/";

   -----------------
   -- To_Duration --
   -----------------

   function To_Duration (TS : Time_Span) return Duration is
      Seconds : Time_Span;
      DS, DF  : Duration;
   begin
      Seconds := TS / Integer (Oak_PSP_Time.Ticks_Per_Second);
      DS      := Duration (Seconds);
      DF      := Duration (Seconds *
                             Integer (Oak_PSP_Time.Ticks_Per_Second) - TS);
      return DS + DF / Billion;
   end To_Duration;

   ------------------
   -- To_Time_Span --
   ------------------

   function To_Time_Span (D : Duration) return Time_Span is
      TS_Seconds, TS_Nano : Time_Span;
   begin
      TS_Seconds := Time_Span (D) * Integer (Oak_PSP_Time.Ticks_Per_Second);
      TS_Nano    := Time_Span ((D - Duration (TS_Seconds)) * Billion);
      return TS_Seconds + TS_Nano;
   end To_Time_Span;

   -----------------
   -- Nanoseconds --
   -----------------

   function Nanoseconds (NS : Integer) return Time_Span is
   begin
      return Time_Span (NS) / Time_Span (Time_Unit *  Billion);
   end Nanoseconds;

   ------------------
   -- Microseconds --
   ------------------

   function Microseconds (US : Integer) return Time_Span is
   begin
      return Time_Span (US) * Integer (1000) /
       Time_Span (Time_Unit * Billion);
   end Microseconds;

   ------------------
   -- Milliseconds --
   ------------------

   function Milliseconds (MS : Integer) return Time_Span is
   begin
      return Time_Span (MS) * Integer (1_000_000) /
       Time_Span (Time_Unit * Billion);
   end Milliseconds;

   -------------
   -- Seconds --
   -------------

   function Seconds (S : Integer) return Time_Span is
   begin
      return Time_Span (S) * Integer (1_000_000_000) /
        Time_Span (Time_Unit * Billion);
   end Seconds;

   -------------
   -- Minutes --
   -------------

   function Minutes (M : Integer) return Time_Span is
   begin
      return Time_Span (M) * Time_Span (60_000_000_000)  /
             Time_Span (Time_Unit * Billion);
   end Minutes;

   -----------
   -- Split --
   -----------

   procedure Split (T : Time; SC : out Seconds_Count; TS : out Time_Span) is
   begin
      SC := Seconds_Count (T / Oak_PSP_Time.Ticks_Per_Second);
      TS := T - Time (SC * Oak_PSP_Time.Ticks_Per_Second);
   end Split;

   -------------
   -- Time_Of --
   -------------

   function Time_Of (SC : Seconds_Count; TS : Time_Span) return Time is
   begin
      return Time (SC * Oak_PSP_Time.Ticks_Per_Second) + Time (TS);
   end Time_Of;

end Ada.Real_Time;
