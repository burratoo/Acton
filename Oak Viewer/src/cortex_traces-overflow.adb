------------------------------------------------------------------------------------------
--                                                                                      --
--                                      OAK VIEWER                                      --
--                                                                                      --
--                                CORTEX_TRACES.OVERFLOW                                --
--                                                                                      --
--                       Copyright (C) 2014-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

package body Cortex_Traces.Overflow is

   function Event_Constructor
     (Params : not null access Constructor_Parameters)
      return Overflow_Event
   is
      Event : Overflow_Event;
   begin
      return Event;
   end Event_Constructor;

   function Image (Event : Overflow_Event) return String is
   begin
      return "Trace Overflow";
   end Image;

end Cortex_Traces.Overflow;
