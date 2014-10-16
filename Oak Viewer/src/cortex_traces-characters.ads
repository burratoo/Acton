package Cortex_Traces.Characters is
   type Character_Event is new Cortex_Event with record
      C : Character;
   end record;

   function Image (Event : Character_Event) return String;

   overriding
   function Event_Constructor
     (Params : not null access Constructor_Parameters)
      return Character_Event;

end Cortex_Traces.Characters;
