------------------------------------------------------------------------------------------
--                                                                                      --
--                                      OAK VIEWER                                      --
--                                                                                      --
--                                  CORTEX_TRACE_TAGS                                   --
--                                                                                      --
--                       Copyright (C) 2014-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

with Ada.Strings.Hash;

with Cortex_Traces;            use Cortex_Traces;
with Cortex_Traces.Characters; use Cortex_Traces.Characters;
with Cortex_Traces.Exceptions; use Cortex_Traces.Exceptions;
with Cortex_Traces.Oak_Enter;  use Cortex_Traces.Oak_Enter;
with Cortex_Traces.Oak_Exit;   use Cortex_Traces.Oak_Exit;
with Cortex_Traces.Overflow;   use Cortex_Traces.Overflow;

package body Cortex_Trace_Tags is

   function "&" (K : Trace_Keys; S : Source_Id) return Trace_Keys is
      R : Trace_Keys := K;
      I : String := Source_Id'Image (S);
   begin
      if S < 10 then
         R (2) := '0';
         R (3) := I (I'Last);
      else
         R (2) := I (I'Last - 1);
         R (3) := I (I'Last);
      end if;
      return R;
   end "&";

   function Key_Hash (K : Trace_Keys) return Hash_Type is
   begin
      return Ada.Strings.Hash (String (K));
   end Key_Hash;

   procedure Register_Tag (Key : Trace_Keys; T : Tag) is
   begin
      Tag_Maps.Insert (Container => Tag_Store,
                       Key       => Key,
                       New_Item  => T);
   end Register_Tag;

   function Tag_For_Key (Key : Trace_Keys) return Tag is
      C : Tag_Maps.Cursor;
      T : Tag;

      use type Tag_Maps.Cursor;
   begin
      C := Tag_Maps.Find (Container => Tag_Store,
                          Key       => Key);
      if C = Tag_Maps.No_Element then
         T := No_Tag;
      else
         T := Tag_Maps.Element (C);
      end if;

      return T;
   end Tag_For_Key;

begin
   Register_Tag (Key => Local_Timestamp_Key, T => Cortex_Event'Tag);
   Register_Tag (Key => Overflow_Key, T => Overflow_Event'Tag);
   Register_Tag (Key => Hardware_Key & Exception_Tracing,
                 T   => Exception_Event'Tag);
   Register_Tag (Key => Instrumentation_Key & 0, T => Character_Event'Tag);
   Register_Tag (Key => Instrumentation_Key & 10, T => Oak_Entering_Event'Tag);
   Register_Tag (Key => Instrumentation_Key & 11, T => Oak_Exiting_Event'Tag);
end Cortex_Trace_Tags;
