------------------------------------------------------------------------------------------
--                                                                                      --
--                               OAK CORE SUPPORT PACKAGE                               --
--                                    ARM CORTEX M4F                                    --
--                                                                                      --
--                                ISA.ARM.CORTEX_M4.DWT                                 --
--                                                                                      --
--                       Copyright (C) 2014-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

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
   type Sync_Counter_Tap is (None, Clock_Divide_By_16M, Clock_Divide_By_64M,
                             Clock_Divide_By_256M);
   type Control_Type is record
      Number_Of_Comparitors : Comparator_Count;
      Exception_Trace       : Enable_Type;
      Sync_Counter_Tap_At   : Sync_Counter_Tap;
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
      Exception_Trace       at 0 range 16 .. 16;
      Sync_Counter_Tap_At   at 0 range 10 .. 11;
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
