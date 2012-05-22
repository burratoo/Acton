with Ada.Real_Time;
with Ada.Unchecked_Conversion;

package Oak.Real_Time.Conversion is
   function To_Oak_Time is
     new Ada.Unchecked_Conversion (Ada.Real_Time.Time, Oak.Real_Time.Time);

   function To_Oak_Time_Span is
     new Ada.Unchecked_Conversion (Ada.Real_Time.Time_Span,
                                   Oak.Real_Time.Time_Span);

end Oak.Real_Time.Conversion;
