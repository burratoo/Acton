with Oak_Support; use Oak_Support;

package Cortex_Traces.Oak_Exit is
   type Oak_Exiting_Event is new Cortex_Event with record
      To_Agent : Oak_Agent_Id;
   end record;

   function Event_Constructor (Params : not null access Constructor_Parameters)
                               return Oak_Exiting_Event;

   overriding
   function Image (Event : Oak_Exiting_Event) return String;

end Cortex_Traces.Oak_Exit;
