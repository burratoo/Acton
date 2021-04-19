------------------------------------------------------------------------------------------
--                                                                                      --
--                                      OAK VIEWER                                      --
--                                                                                      --
--                                     CORTEX_TRACES                                    --
--                                                                                      --
--                       Copyright (C) 2014-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

with DWT_ITM_Packets; use DWT_ITM_Packets;
with Global_Time;     use Global_Time;

with Ada.Streams;                use Ada.Streams;

package Cortex_Traces is

   type Timestamp_Relationship is (Missing_Timestamp,
                                   Corresponds_To_Event,
                                   Timestamp_Later_Than_Event,
                                   Event_Capture_Delayed,
                                   Event_Capture_Delayed_And_Timestamp_Late);

   type Cortex_Event is tagged record
      Time_Of_Event : Processor_Cycles;
      Time_Status   : Timestamp_Relationship;
      Is_Orphaned_Timestamp : Boolean := False;
   end record;

   function Image (Event : Cortex_Event) return String;

   procedure Read_Cortex_Event
     (Stream : not null access Root_Stream_Type'Class;
      Event  : out Cortex_Event) is null;

   for Cortex_Event'Read use Read_Cortex_Event;

   --  The contents of the Cortex_Event is read after the contents of the
   --  derived tagged types has been read. However, Ada calls this read
   --  procedure first which is not what is wanted. To get around this the
   --  read op here is made null and the Input_Cortex_Event_Class procedure
   --  does the read itself.

   function Input_Cortex_Event_Class
     (Stream : access Ada.Streams.Root_Stream_Type'Class)
      return Cortex_Event'Class;

   for Cortex_Event'Class'Input use Input_Cortex_Event_Class;
   --  Will update global time if a packet is followed by a local timestamp
   --  packet.

   type Constructor_Parameters is record
      Stream : access Root_Stream_Type'Class;
      Header : DWT_ITM_Packet_Header;
   end record;

   function Event_Constructor (Params : not null access Constructor_Parameters)
     return Cortex_Event;

private

end Cortex_Traces;
