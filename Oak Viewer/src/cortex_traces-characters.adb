with DWT_ITM_Packets; use DWT_ITM_Packets;

package body Cortex_Traces.Characters is
   function Event_Constructor
     (Params : not null access Constructor_Parameters)
      return Character_Event
   is
      Event : Character_Event;
   begin
      if Params.Header.Kind /= Source_1_Byte then
         raise Program_Error with "Bad character message length!";
      end if;

      Character'Read (Params.Stream, Event.C);
      return Event;
   end Event_Constructor;

   function Image (Event : Character_Event) return String is
   begin
      return Image (Cortex_Event (Event)) & " Character: " & Event.C;
   end Image;

end Cortex_Traces.Characters;
