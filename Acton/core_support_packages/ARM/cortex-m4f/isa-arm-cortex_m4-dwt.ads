with System; use System;

package ISA.ARM.Cortex_M4.DWT with Preelaborate is

   --------------------------
   -- DWT Memory Addresses --
   --------------------------

   DWT_Base_Address      : constant := 16#E000_1000#;
   CTRL_Offset_Address   : constant := 16#0#;
   CYCCNT_Offset_Address : constant := 16#4#;

   -----------------------
   -- Hardware Features --
   -----------------------

   type Comparator_Count is mod 2 ** 4;

   ---------------
   -- DWT Types --
   ---------------

   type Support_Type is (Supported, Not_Supported);

   type Control_Type is record
      Number_Of_Comparitors : Comparator_Count;
      Cycle_Count           : Enable_Type;
   end record;

   type Cycle_Count is mod 2 ** 32;

   type Cycle_Count_Type is record
      Count : Cycle_Count;
   end record;

   ------------------------------
   -- Hardware Representations --
   ------------------------------

   for Control_Type use record
      Number_Of_Comparitors at 0 range 28 .. 31;
      Cycle_Count           at 0 range  0 .. 0;
   end record;

   for Cycle_Count_Type use record
      Count at 0 range 0 .. 31;
   end record;

   -------------------
   -- DWT Registers --
   -------------------

   Control_Register : Control_Type
     with Address =>
       System'To_Address (DWT_Base_Address + CTRL_Offset_Address);

   Cycle_Count_Register : Cycle_Count_Type
     with Address =>
       System'To_Address (DWT_Base_Address + CYCCNT_Offset_Address);
end ISA.ARM.Cortex_M4.DWT;
