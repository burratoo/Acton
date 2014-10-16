with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;

package body Cortex_Traces.Oak_Exit is
   function Event_Constructor (Params : not null access Constructor_Parameters)
                               return Oak_Exiting_Event is
      function To_Kernel_Exiting is new
        Ada.Unchecked_Conversion (Source => Unsigned_8,
                                  Target => Kernel_Exit_Tracing);
      Tracing_Data : Kernel_Exit_Tracing := To_Kernel_Exiting (Unsigned_8'Input (Params.Stream));
      Event        : Oak_Exiting_Event;
   begin
      Event.To_Agent := Tracing_Data.To_Agent;
      return Event;
   end Event_Constructor;

   function Image (Event : Oak_Exiting_Event) return String is
   begin
      return Image (Cortex_Event (Event)) & " " &
                      "← kernel to agent " & Oak_Agent_Id'Image (Event.To_Agent);
   end Image;
end Cortex_Traces.Oak_Exit;
