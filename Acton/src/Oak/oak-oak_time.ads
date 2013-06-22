with Oak.Core_Support_Package.Time;

package Oak.Oak_Time with Pure is

   package Oak_CSP_Time renames Oak.Core_Support_Package.Time;

   type Time is private with Preelaborable_Initialization;
   Time_First : constant Time;
   Time_Last  : constant Time;
   Time_Zero  : constant Time;
   Time_Unit  : constant := Oak_CSP_Time.Oak_Time_Unit;

   type Time_Span is private with Preelaborable_Initialization;
   Time_Span_First : constant Time_Span;
   Time_Span_Last  : constant Time_Span;
   Time_Span_Zero  : constant Time_Span;
   Time_Span_Unit  : constant Time_Span;

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

   function "abs" (Right : Time_Span) return Time_Span;

   function "<" (Left, Right : Time_Span) return Boolean;
   function "<=" (Left, Right : Time_Span) return Boolean;
   function ">" (Left, Right : Time_Span) return Boolean;
   function ">=" (Left, Right : Time_Span) return Boolean;

private
   type Time is new Oak_CSP_Time.Oak_Time;
   Time_First : constant Time := Time'First;
   Time_Last  : constant Time := Time'Last;
   Time_Zero : constant Time := 0;

   type Time_Span is new Oak_CSP_Time.Oak_Time;
   Time_Span_First : constant Time_Span := Time_Span'First;
   Time_Span_Last  : constant Time_Span := Time_Span'Last;
   Time_Span_Zero  : constant Time_Span := 0;
   Time_Span_Unit  : constant Time_Span := Oak_CSP_Time.Oak_Time_Span_Unit;

   pragma Import (Intrinsic, "+");
   pragma Import (Intrinsic, "-");
   pragma Import (Intrinsic, "abs");
   pragma Import (Intrinsic, "<");
   pragma Import (Intrinsic, "<=");
   pragma Import (Intrinsic, ">");
   pragma Import (Intrinsic, ">=");

   pragma Inline (Clock);

end Oak.Oak_Time;
