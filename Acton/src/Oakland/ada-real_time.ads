with Oak.Core_Support_Package.Time;

package Ada.Real_Time is

   package Oak_CSP_Time renames Oak.Core_Support_Package.Time;

   type Time is private;
   Time_First : constant Time;
   Time_Last  : constant Time;
   Time_Zero  : constant Time;
   Time_Unit  : constant := Oak_CSP_Time.Oak_Time_Unit;

   type Time_Span is private;
   Time_Span_First : constant Time_Span;
   Time_Span_Last  : constant Time_Span;
   Time_Span_Zero  : constant Time_Span;
   Time_Span_Unit  : constant Time_Span;

   Tick : constant Time_Span;
   function Clock return Time;

   function "+" (Left : Time; Right : Time_Span) return Time;
   function "+" (Left : Time_Span; Right : Time) return Time;
   function "-" (Left : Time; Right : Time_Span) return Time;
   function "-" (Left : Time; Right : Time) return Time_Span;

   function "<" (Left, Right : Time) return Boolean;
   function "<=" (Left, Right : Time) return Boolean;
   function ">" (Left, Right : Time) return Boolean;
   function ">=" (Left, Right : Time) return Boolean;

   function "+" (Left, Right : Time_Span) return Time_Span;
   function "-" (Left, Right : Time_Span) return Time_Span;
   function "-" (Right : Time_Span) return Time_Span;
   function "*" (Left : Time_Span; Right : Integer) return Time_Span;
   function "*" (Left : Integer; Right : Time_Span) return Time_Span;
   function "/" (Left, Right : Time_Span) return Integer;
   function "/" (Left : Time_Span; Right : Integer) return Time_Span;

   function "abs" (Right : Time_Span) return Time_Span;

   function "<" (Left, Right : Time_Span) return Boolean;
   function "<=" (Left, Right : Time_Span) return Boolean;
   function ">" (Left, Right : Time_Span) return Boolean;
   function ">=" (Left, Right : Time_Span) return Boolean;

   function To_Duration (TS : Time_Span) return Duration;
   function To_Time_Span (D : Duration) return Time_Span;

   function Nanoseconds (NS : Integer) return Time_Span;
   function Microseconds (US : Integer) return Time_Span;
   function Milliseconds (MS : Integer) return Time_Span;
   function Seconds (S : Integer) return Time_Span;
   function Minutes (M : Integer) return Time_Span;

   type Seconds_Count is range
     -(Oak_CSP_Time.Seconds_Max) .. +(Oak_CSP_Time.Seconds_Max);

   procedure Split (T : Time; SC : out Seconds_Count; TS : out Time_Span);
   function Time_Of (SC : Seconds_Count; TS : Time_Span) return Time;

private
   type Time is new Oak_CSP_Time.Oak_Time;
   Time_First : constant Time := Time'First;
   Time_Last  : constant Time := Time'Last;
   Time_Zero : constant Time := 0;

   type Time_Span is new Oak_CSP_Time.Oak_Time_Span;
   Time_Span_First : constant Time_Span := Time_Span'First;
   Time_Span_Last  : constant Time_Span := Time_Span'Last;
   Time_Span_Zero  : constant Time_Span := 0;
   Time_Span_Unit  : constant Time_Span := Oak_CSP_Time.Oak_Time_Span_Unit;

   Tick : constant Time_Span := Oak_CSP_Time.Oak_Tick;
   Ticks_Per_Second : constant := Oak_CSP_Time.Ticks_Per_Second;

   pragma Import (Intrinsic, "+");
   pragma Import (Intrinsic, "-");
   pragma Import (Intrinsic, "abs");
   pragma Import (Intrinsic, "<");
   pragma Import (Intrinsic, "<=");
   pragma Import (Intrinsic, ">");
   pragma Import (Intrinsic, ">=");

   pragma Inline (Clock);
   pragma Inline (Nanoseconds);
   pragma Inline (Microseconds);
   pragma Inline (Milliseconds);
   pragma Inline (Seconds);
   pragma Inline (Minutes);
end Ada.Real_Time;
