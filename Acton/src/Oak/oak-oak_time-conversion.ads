------------------------------------------------------------------------------------------
--                                                                                      --
--                                    OAK COMPONENTS                                    --
--                                                                                      --
--                                OAK.OAK_TIME.CONVERSION                               --
--                                                                                      --
--                       Copyright (C) 2012-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

with Ada.Real_Time;
with Ada.Unchecked_Conversion;

package Oak.Oak_Time.Conversion is

   function To_Oak_Time is
     new Ada.Unchecked_Conversion (Ada.Real_Time.Time, Oak.Oak_Time.Time);

   function To_Oak_Time_Span is
     new Ada.Unchecked_Conversion (Ada.Real_Time.Time_Span,
                                   Oak.Oak_Time.Time_Span);

end Oak.Oak_Time.Conversion;
