with DWT_ITM_Packets; use DWT_ITM_Packets;

with Ada.Containers; use Ada.Containers;
with Ada.Tags;       use Ada.Tags;

with Ada.Containers.Hashed_Maps;

package Cortex_Trace_Tags is

   --  Key Names

   type Trace_Keys is new String (1 .. 3);

   Overflow_Key           : constant Trace_Keys := "100";
   Global_Timestamp_1_Key : constant Trace_Keys := "200";
   Global_Timestamp_2_Key : constant Trace_Keys := "201";
   Exension_Key           : constant Trace_Keys := "300";
   Local_Timestamp_Key    : constant Trace_Keys := "400";
   Instrumentation_Key    : constant Trace_Keys := "500";
   Hardware_Key           : constant Trace_Keys := "600";

   function "&" (K : Trace_Keys; S : Source_Id) return Trace_Keys;

   procedure Register_Tag (Key : Trace_Keys;  T : Tag);
   function Tag_For_Key (Key : Trace_Keys) return Tag;

private
   function Key_Hash (K : Trace_Keys) return Hash_Type;

   package Tag_Maps is new
     Ada.Containers.Hashed_Maps (Key_Type        => Trace_Keys,
                                 Element_Type    => Tag,
                                 Hash            => Key_Hash,
                                 Equivalent_Keys => "=",
                                 "="             => "=");

   Tag_Store : Tag_Maps.Map;

end Cortex_Trace_Tags;
