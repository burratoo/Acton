with Ada.Streams; use Ada.Streams;
with Interfaces;  use Interfaces;
with System;      use System;
with ISA.ARM.Cortex_M4.Exceptions; use ISA.ARM.Cortex_M4.Exceptions;

with Ada.Unchecked_Conversion;

package DWT_ITM_Packets with Pure is

   -------------------
   -- Packet Header --
   -------------------

   type Packet_Header_Contents is mod 2 ** 6;
   Synchronization_Packet : constant := 0;

   type Packet_Category is (Protocol, Source_1_Byte, Source_2_Bytes,
                            Source_4_Bytes);

   type Protocol_Kind is (Synchronization,
                          Overflow,
                          Local_Timestamp,
                          Extension,
                          Global_Timestamp_1,
                          Global_Timestamp_2);

   type Source_Kind is (Instrumentation, Hardware);


   type Protocol_Static_Headers is (Overflow,
                                    Global_Timestamp_1,
                                    Global_Timestamp_2)
     with Size => Packet_Header_Contents'Size;

   type Raw_Header is mod 2 ** 6;
   type Protocol_Header_Data is mod 2 ** 3;

   type Protocol_Header_Type (Kind : Protocol_Kind := Overflow) is record
      case Kind is
         when Synchronization =>
            Raw_Header_Data : Raw_Header;
         when Overflow | Global_Timestamp_1 | Global_Timestamp_2 =>
            Static_Header   : Protocol_Static_Headers;
         when Local_Timestamp | Extension =>
            Is_Extension    : Boolean;
            Source          : Boolean;
            Data            : Protocol_Header_Data;
            Continues       : Boolean;
      end case;
   end record with Unchecked_Union, Size => Packet_Header_Contents'Size;

   type Source_Id is mod 2 ** 5;
   subtype Hardware_Source_Id is Source_Id;
   subtype Stimulus_Port_Id is Source_Id;

   type Source_Header_Type (Source_Origin : Source_Kind := Instrumentation) is record
      case Source_Origin is
         when Instrumentation =>
            Stimulus_Port   : Stimulus_Port_Id;
         when Hardware =>
            Hardware_Source : Hardware_Source_Id;
      end case;
   end record with Size => Packet_Header_Contents'Size;

   type DWT_ITM_Packet_Header (Kind : Packet_Category := Source_1_Byte) is record
      case Kind is
         when Protocol =>
            Protocol_Header : Packet_Header_Contents;

         when Source_1_Byte | Source_2_Bytes | Source_4_Bytes =>
            Source_Header   : Packet_Header_Contents;
      end case;
   end record with Size => 8;

   for Protocol_Static_Headers use (Overflow           => 2#011100#,
                                    Global_Timestamp_1 => 2#100101#,
                                    Global_Timestamp_2 => 2#101101#);

   for Protocol_Header_Type use record
      Raw_Header_Data at 0 range 0 .. 5;
      Static_Header   at 0 range 0 .. 5;
      Source          at 0 range 0 .. 0;
      Is_Extension    at 0 range 1 .. 1;
      Data            at 0 range 2 .. 4;
      Continues       at 0 range 5 .. 5;
   end record;

   for Source_Header_Type use record
      Source_Origin   at 0 range 0 .. 0;
      Stimulus_Port   at 0 range 1 .. 5;
      Hardware_Source at 0 range 1 .. 5;
   end record;

   for DWT_ITM_Packet_Header use record
      Kind            at 0 range 0 .. 1;
      Protocol_Header at 0 range 2 .. 7;
      Source_Header   at 0 range 2 .. 7;
   end record;

   function Input_DWT_ITM_Packet_Header
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
       return DWT_ITM_Packet_Header;

   for DWT_ITM_Packet_Header'Input use Input_DWT_ITM_Packet_Header;

   function To_Protocol_Header is new
     Ada.Unchecked_Conversion (Source => Packet_Header_Contents,
                               Target => Protocol_Header_Type);

   function To_Source_Header is new
     Ada.Unchecked_Conversion (Source => Packet_Header_Contents,
                               Target => Source_Header_Type);

   function To_Raw_Header is new
     Ada.Unchecked_Conversion (Source => DWT_ITM_Packet_Header,
                               Target => Unsigned_8);
   function To_DWT_ITM_Packet_Header is new
     Ada.Unchecked_Conversion (Source => Unsigned_8,
                               Target => DWT_ITM_Packet_Header);

   function Input_DWT_ITM_Packet_Header
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
   return DWT_ITM_Packet_Header is (To_DWT_ITM_Packet_Header (Unsigned_8'Input (Stream)));

   ----------------------------
   -- Local Timestamp Packet --
   ----------------------------

   Timestamp_Synchronous     : constant Protocol_Header_Data := 2#100#;
   Timestamp_Delayed         : constant Protocol_Header_Data := 2#101#;
   DWT_ITM_Delayed           : constant Protocol_Header_Data := 2#110#;
   Timestamp_DWT_ITM_Delayed : constant Protocol_Header_Data := 2#111#;

   type Timestamp_Fragment is mod 2 ** 7;

   type Local_Timestamp_Payload is record
      Continues : Boolean;
      Value     : Timestamp_Fragment;
   end record with Size => 8;

   for Local_Timestamp_Payload use record
      Continues at 0 range 7 .. 7;
      Value     at 0 range 0 .. 6;
   end record;

   function Input_Local_Timestamp_Payload
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Local_Timestamp_Payload;

   for Local_Timestamp_Payload'Input use Input_Local_Timestamp_Payload;

   function To_Raw_Timestamp_Payload is new
     Ada.Unchecked_Conversion (Source => Local_Timestamp_Payload,
                               Target => Unsigned_8);

   function To_Timestamp_Payload is new
     Ada.Unchecked_Conversion (Source => Unsigned_8,
                               Target => Local_Timestamp_Payload);

   function Packet_Is_Local_Timestap (Packet_Header : in DWT_ITM_Packet_Header)
                                      return Boolean;

   Not_Local_Timestamp_Packet_1 : constant Raw_Header := 2#000000#;
   Not_Local_Timestamp_Packet_2 : constant Raw_Header := 2#011100#;
   Local_Timestamp_Mask         : constant Raw_Header := 2#000011#;

   function Packet_Is_Local_Timestap
     (Packet_Header : in DWT_ITM_Packet_Header)
      return Boolean is
     (Packet_Header.Kind = Protocol and then
      To_Protocol_Header (Packet_Header.Protocol_Header).Raw_Header_Data not in Not_Local_Timestamp_Packet_1 | Not_Local_Timestamp_Packet_2 and then
        (To_Protocol_Header (Packet_Header.Protocol_Header).Raw_Header_Data and Local_Timestamp_Mask) = 0);

   function Input_Local_Timestamp_Payload
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Local_Timestamp_Payload is (To_Timestamp_Payload (Unsigned_8'Input (Stream)));

   --------------------
   -- Source Packets --
   --------------------

   type Source_Payload_8  is new Unsigned_8;
   type Source_Payload_16 is new Unsigned_16;
   type Source_Payload_32 is new Unsigned_32;

   -----------------------------
   -- Hardware Source Packets --
   -----------------------------

   Event_Counter_Wrapping : constant Hardware_Source_Id := 0;
   Exception_Tracing      : constant Hardware_Source_Id := 1;
   PC_Sampling            : constant Hardware_Source_Id := 2;
   Data_Tracing_Low       : constant Hardware_Source_Id := 8;
   Data_Tracing_High      : constant Hardware_Source_Id := 23;

   ----------------------------------------------
   -- Hardware Soruce Packets: Exception Trace --
   ----------------------------------------------

   type Exception_Action is (Entered, Exited, Returned);
   for Exception_Action use (Entered => 1, Exited => 2, Returned => 3);

   type Exception_Trace_Packet is record
      Exception_Number : Exception_Id;
      Action           : Exception_Action;
   end record with Size => 16;

   for Exception_Trace_Packet use record
      Exception_Number at 0 range 0 .. 8;
      Action           at 0 range 12 .. 13;
   end record;

   function Input_Exception_Trace_Packet
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Exception_Trace_Packet;

   for Exception_Trace_Packet'Input use Input_Exception_Trace_Packet;

   function To_Exception_Trace_Packet is new
     Ada.Unchecked_Conversion (Source => Unsigned_16,
                               Target => Exception_Trace_Packet);

   function Input_Exception_Trace_Packet
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Exception_Trace_Packet is (To_Exception_Trace_Packet (Unsigned_16'Input (Stream)));

end DWT_ITM_Packets;
