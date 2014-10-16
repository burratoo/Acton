with Ada.Unchecked_Conversion;

package body Cortex_Traces.Exceptions is

   function Event_Constructor
     (Params : not null access Constructor_Parameters)
         return Exception_Event
   is
      Event           : Exception_Event;
      Exception_Trace : Exception_Trace_Packet;

   begin
      Exception_Trace := Exception_Trace_Packet'Input (Params.Stream);

      Event.Exception_Number := Exception_Trace.Exception_Number;
      Event.Action := Exception_Trace.Action;
      return Event;
   end Event_Constructor;

   function Image (Event : Exception_Event) return String is
   begin
      return Image (Cortex_Event (Event)) & " Exception " &
      (case Event.Exception_Number is
          when Thread_Mode => "Task",
          when SVCall      => "SVCall",
          when SysTick     => "SysTick",
          when others      => "") & " " &
         Exception_Action'Image (Event.Action);
   end Image;

end Cortex_Traces.Exceptions;
