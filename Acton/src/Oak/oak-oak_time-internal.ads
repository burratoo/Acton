with Ada.Unchecked_Conversion;
with Oak.Core_Support_Package.Time;

package Oak.Oak_Time.Internal with Pure is
   function To_Internal_Time is
     new Ada.Unchecked_Conversion (Oak.Oak_Time.Time,
                                   Oak.Core_Support_Package.Time.Oak_Time);
end Oak.Oak_Time.Internal;
