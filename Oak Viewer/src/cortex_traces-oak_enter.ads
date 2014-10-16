with Oak_Support; use Oak_Support;

package Cortex_Traces.Oak_Enter is
   type Oak_Entering_Event is new Cortex_Event with record
      Reason  : Run_Reason;
      Request : Agent_State;
   end record;

   function Event_Constructor (Params : not null access Constructor_Parameters)
                               return Oak_Entering_Event;
   overriding
   function Image (Event : Oak_Entering_Event) return String;

end Cortex_Traces.Oak_Enter;
