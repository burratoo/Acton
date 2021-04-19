------------------------------------------------------------------------------------------
--                                                                                      --
--                                      OAK VIEWER                                      --
--                                                                                      --
--                                     CORTEX_TRACES                                    --
--                                                                                      --
--                       Copyright (C) 2014-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

with Cortex_Trace_Tags; use Cortex_Trace_Tags;
with GNAT.Sockets;      use GNAT.Sockets;

with Ada.Tags;   use Ada.Tags;
with Interfaces; use Interfaces;

with Ada.Tags.Generic_Dispatching_Constructor;
with Ada.Unchecked_Conversion;

with Ada.Text_IO;

package body Cortex_Traces is

   Last_Packet_Header : DWT_ITM_Packet_Header;
   Use_Last_Header    : Boolean := False;

   function Image (Event : Cortex_Event) return String is
   begin
      return "Time: " & Processor_Cycles'Image (Event.Time_Of_Event) &
      (if Event.Time_Status /= Corresponds_To_Event then "!" else " ");
   end Image;

   function Input_Cortex_Event_Class
     (Stream : access Ada.Streams.Root_Stream_Type'Class)
      return Cortex_Event'Class
   is
      Packet_Header : DWT_ITM_Packet_Header;
      Packet_Key    : Trace_Keys;

      Object_Tag    : Tag;

      package Unsigned_IO is new Ada.Text_IO.Modular_IO (Unsigned_8);
      use Unsigned_IO;
   begin
      --  Read header packet to figure out what kind of we are dealing with

      if Use_Last_Header then
         Packet_Header := Last_Packet_Header;
      else
         Packet_Header := DWT_ITM_Packet_Header'Input (Stream);
      end if;

      while To_Raw_Header (Packet_Header) = Synchronization_Packet loop
         Packet_Header := DWT_ITM_Packet_Header'Input (Stream);
      end loop;

      case Packet_Header.Kind is
         when Protocol =>
            declare
               Protocol_Header : Protocol_Header_Type :=
                                   To_Protocol_Header (Packet_Header.Protocol_Header);
            begin

               if Protocol_Header.Static_Header = Overflow then
                  Packet_Key := Overflow_Key;

               elsif Protocol_Header.Static_Header = Global_Timestamp_1 then
                  Packet_Key := Global_Timestamp_1_Key;

               elsif Protocol_Header.Static_Header = Global_Timestamp_2 then
                  Packet_Key := Global_Timestamp_1_Key;

               elsif Protocol_Header.Is_Extension then
                  Packet_Key := Exension_Key;

               else
                  --  Have a Local_Timestamp
                  Packet_Key := Local_Timestamp_Key;
               end if;
            end;
         when Source_1_Byte | Source_2_Bytes | Source_4_Bytes =>
            declare
               Source_Header : Source_Header_Type := To_Source_Header (Packet_Header.Source_Header);
            begin
               case Source_Header.Source_Origin is
               when Instrumentation =>

                  Packet_Key := Instrumentation_Key &
                    Source_Header.Stimulus_Port;

               when Hardware =>
                  Packet_Key := Hardware_Key &
                    Source_Header.Hardware_Source;
               end case;
            end;
      end case;

      Object_Tag := Tag_For_Key (Packet_Key);
      if Object_Tag = No_Tag then
         raise Program_Error with "No handler is defined for key " & String (Packet_Key);
      end if;

      declare
         function Create_Event_Object is
           new Generic_Dispatching_Constructor (Cortex_Event,
                                                Constructor_Parameters,
                                                Event_Constructor);

         P : aliased Constructor_Parameters :=
               (Stream => Stream, Header => Packet_Header);

         Event : Cortex_Event'Class :=
                   Create_Event_Object (The_Tag => Object_Tag,
                                        Params => P'Access);

         Next_Header : DWT_ITM_Packet_Header;
      begin

         if Object_Tag = Cortex_Event'Tag then
            Next_Header := Packet_Header;
         else
            Next_Header := DWT_ITM_Packet_Header'Input (Stream);
         end if;

         --  Process Local Timestamp packet, if present

         if Packet_Is_Local_Timestap (Next_Header) then
            declare
               Timestap_Header  : constant Protocol_Header_Type := To_Protocol_Header (Next_Header.Protocol_Header);
               Payload          : Local_Timestamp_Payload;
               Time_Delta       : Processor_Cycles := Time_Zero;
               Timestamp_Status : Timestamp_Relationship;

            begin
               if not Timestap_Header.Continues then
                  Time_Delta := Processor_Cycles (Timestap_Header.Data);
                  Timestamp_Status := Corresponds_To_Event;
               else
                  case Timestap_Header.Data is
                     when Timestamp_Synchronous =>
                        Timestamp_Status := Corresponds_To_Event;
                     when Timestamp_Delayed =>
                        Timestamp_Status := Timestamp_Later_Than_Event;
                     when DWT_ITM_Delayed =>
                        Timestamp_Status := Event_Capture_Delayed;
                     when Timestamp_DWT_ITM_Delayed =>
                        Timestamp_Status := Event_Capture_Delayed_And_Timestamp_Late;
                     when others =>
                        raise Program_Error with "Invalid local timestamp header";
                  end case;

                  for J in 0 .. 3 loop
                     Payload := Local_Timestamp_Payload'Input (Stream);
                     Time_Delta := Time_Delta + Processor_Cycles (Shift_Left (Unsigned_32 (Payload.Value), 7 * J));
                     exit when not Payload.Continues;
                  end loop;

               end if;

               Time := Time + Time_Delta;
               Event.Time_Of_Event := Time;
               Event.Time_Status   := Timestamp_Status;

               Use_Last_Header := False;
            end;

         else
            Event.Time_Status   := Missing_Timestamp;
            Event.Time_Of_Event := Time_First;

            Last_Packet_Header := Next_Header;
            Use_Last_Header := True;
         end if;
         return Event;
      end;
   end Input_Cortex_Event_Class;

   function Event_Constructor (Params : not null access Constructor_Parameters)
                               return Cortex_Event is
      Event : Cortex_Event;
   begin
      Event.Is_Orphaned_Timestamp := True;
      return Event;
   end Event_Constructor;


end Cortex_Traces;
