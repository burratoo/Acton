------------------------------------------------------------------------------------------
--                                                                                      --
--                                  OAKLAND COMPONENTS                                  --
--                                                                                      --
--                                     ADA.REAL_TIME                                    --
--                                                                                      --
--                       Copyright (C) 2011-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

package body Ada.Real_Time is

   -----------
   -- Clock --
   -----------

   function Clock return Time is
   begin
      return Time (Oak_CSP_Time.Get_Clock);
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
      Seconds := TS / Integer (Oak_CSP_Time.Ticks_Per_Second);
      DS      := Duration (Seconds);
      DF      := Duration (Seconds *
                             Integer (Oak_CSP_Time.Ticks_Per_Second) - TS);
      return DS + DF / 1_000_000_000;
   end To_Duration;

   ------------------
   -- To_Time_Span --
   ------------------

   function To_Time_Span (D : Duration) return Time_Span is
      TS_Seconds, TS_Nano : Time_Span;
   begin
      TS_Seconds := Time_Span (D) * Integer (Oak_CSP_Time.Ticks_Per_Second);
      TS_Nano    := Time_Span ((D - Duration (TS_Seconds)) * 1_000_000_000);
      return TS_Seconds + TS_Nano;
   end To_Time_Span;

   -----------------
   -- Nanoseconds --
   -----------------

   function Nanoseconds (NS : Integer) return Time_Span is
   begin
      return Time_Span (NS * Integer (Ticks_Per_Second / 1_000_000_000));
   end Nanoseconds;

   ------------------
   -- Microseconds --
   ------------------

   function Microseconds (US : Integer) return Time_Span is
   begin
      return Time_Span (US) * Time_Span (Ticks_Per_Second) /
        Time_Span (1_000_000);
   end Microseconds;

   ------------------
   -- Milliseconds --
   ------------------

   function Milliseconds (MS : Integer) return Time_Span is
   begin
      return Time_Span (MS) * Time_Span (Ticks_Per_Second) / Time_Span (1_000);
   end Milliseconds;

   -------------
   -- Seconds --
   -------------

   function Seconds (S : Integer) return Time_Span is
   begin
      return Time_Span (S * Integer (Ticks_Per_Second));
   end Seconds;

   -------------
   -- Minutes --
   -------------

   function Minutes (M : Integer) return Time_Span is
   begin
      return Time_Span (M * Integer (Ticks_Per_Second) * Integer (60));
   end Minutes;

   -----------
   -- Split --
   -----------

   procedure Split (T : Time; SC : out Seconds_Count; TS : out Time_Span) is
   begin
      SC := Seconds_Count (T / Oak_CSP_Time.Ticks_Per_Second);
      TS := T - Time (SC * Oak_CSP_Time.Ticks_Per_Second);
   end Split;

   -------------
   -- Time_Of --
   -------------

   function Time_Of (SC : Seconds_Count; TS : Time_Span) return Time is
   begin
      return Time (SC * Oak_CSP_Time.Ticks_Per_Second) + Time (TS);
   end Time_Of;

end Ada.Real_Time;
