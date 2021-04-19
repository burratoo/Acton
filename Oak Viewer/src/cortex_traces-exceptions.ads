------------------------------------------------------------------------------------------
--                                                                                      --
--                                      OAK VIEWER                                      --
--                                                                                      --
--                               CORTEX_TRACES.EXCEPTIONS                               --
--                                                                                      --
--                       Copyright (C) 2014-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

with ISA.ARM.Cortex_M4.Exceptions; use ISA.ARM.Cortex_M4.Exceptions;
with DWT_ITM_Packets;              use DWT_ITM_Packets;

package Cortex_Traces.Exceptions is

   type Exception_Event is new Cortex_Event with record
      Exception_Number : Exception_Id;
      Action           : Exception_Action;
   end record;

   function Event_Constructor
     (Params : not null access Constructor_Parameters)
      return Exception_Event;

   overriding
   function Image (Event : Exception_Event) return String;

end Cortex_Traces.Exceptions;
