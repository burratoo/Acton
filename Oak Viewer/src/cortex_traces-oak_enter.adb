------------------------------------------------------------------------------------------
--                                                                                      --
--                                      OAK VIEWER                                      --
--                                                                                      --
--                                CORTEX_TRACES.OAK_ENTER                               --
--                                                                                      --
--                       Copyright (C) 2014-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;

package body Cortex_Traces.Oak_Enter is

   function Event_Constructor (Params : not null access Constructor_Parameters)
                                  return Oak_Entering_Event is
      function To_Kernel_Entering is new
        Ada.Unchecked_Conversion (Source => Unsigned_16,
                                  Target => Kernel_Entry_Tracing);
      Tracing_Data : Kernel_Entry_Tracing := To_Kernel_Entering (Unsigned_16'Input (Params.Stream));
      Event        : Oak_Entering_Event;
   begin
      Event.Reason := Tracing_Data.Reason;
      Event.Request := Tracing_Data.Request;
      return Event;
   end Event_Constructor;

   function Image (Event : Oak_Entering_Event) return String is
   begin
      return Image (Cortex_Event (Event)) &  " " &
                      "→ kernel for reason: " & Run_Reason'Image (Event.Reason) &
                      " Request: " & Agent_State'Image (Event.Request);
   end Image;
end Cortex_Traces.Oak_Enter;
