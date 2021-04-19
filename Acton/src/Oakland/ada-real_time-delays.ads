------------------------------------------------------------------------------------------
--                                                                                      --
--                                  OAKLAND COMPONENTS                                  --
--                                                                                      --
--                                 ADA.REAL_TIME.DELAYS                                 --
--                                                                                      --
--                       Copyright (C) 2011-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

package Ada.Real_Time.Delays is

   procedure Delay_Until (T : Time);
   --  Delay until Clock has reached (at least) time T,
   --  or the task is aborted to at least the current ATC nesting level.
   --  The body of this procedure must perform all the processing
   --  required for an abort point.

end Ada.Real_Time.Delays;
