package Cortex_Traces.Overflow is

   type Overflow_Event is new Cortex_Event with null record;

   function Event_Constructor
     (Params : not null access Constructor_Parameters)
      return Overflow_Event;

   overriding
   function Image (Event : Overflow_Event) return String;

end Cortex_Traces.Overflow;
