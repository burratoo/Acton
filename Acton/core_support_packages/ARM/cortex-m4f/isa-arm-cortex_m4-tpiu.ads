------------------------------------------------------------------------------------------
--                                                                                      --
--                               OAK CORE SUPPORT PACKAGE                               --
--                                    ARM CORTEX M4F                                    --
--                                                                                      --
--                                ISA.ARM.CORTEX_M4.TPIU                                --
--                                                                                      --
--                       Copyright (C) 2014-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

with System;     use System;
with Interfaces; use Interfaces;

package ISA.ARM.Cortex_M4.TPIU with Preelaborate is
   --  Not complete

   ---------------------------
   -- TPIU Memory Addresses --
   ---------------------------

   TPIU_Base_Address    : constant := 16#E004_0000#;
   SSPSR_Offset_Address : constant := 16#0#;
   CSPSR_Offset_Address : constant := 16#4#;
   ACPR_Offset_Address  : constant := 16#10#;
   SPPR_Offset_Address  : constant := 16#F0#;
   FFCR_Offset_Address  : constant := 16#304#;

   -----------------------
   -- Hardware Features --
   -----------------------

   ----------------
   -- TPIU Types --
   ----------------

   type TX_Modes is (Parallel_Trace, SWO_Manchester, SWO_NRZ);

   type Selected_Pin_Protocol is record
      Transmit_Mode : TX_Modes;
   end record with Size => 32;

   type Asynchronous_Clock_Prescaler is record
      SWO_Prescaler : Unsigned_16;
   end record with Size => 32;

   type Formatter_And_Flush_Control is record
      Triggers_Inserted     : Boolean;
      Continuous_Formatting : Enable_Type;
   end record;

   ------------------------------
   -- Hardware Representations --
   ------------------------------

   for TX_Modes use (Parallel_Trace => 0, SWO_Manchester => 2#01#,
                     SWO_NRZ => 2#10#);

   for Asynchronous_Clock_Prescaler use record
      SWO_Prescaler at 0 range 0 .. 15;
   end record;

   for Selected_Pin_Protocol use record
      Transmit_Mode at 0 range 0 .. 1;
   end record;

   for Formatter_And_Flush_Control use record
      Triggers_Inserted     at 0 range 8 .. 8;
      Continuous_Formatting at 0 range 1 .. 1;
   end record;

   --------------------
   -- TPIU Registers --
   --------------------

   Asynchronous_Clock_Prescaler_Register : Asynchronous_Clock_Prescaler
     with Address =>
       System'To_Address (TPIU_Base_Address + ACPR_Offset_Address);

   Selected_Pin_Protocol_Register : Selected_Pin_Protocol
     with Address =>
       System'To_Address (TPIU_Base_Address + SPPR_Offset_Address);

   Formatter_And_Flush_Control_Register : Formatter_And_Flush_Control
     with Address =>
       System'To_Address (TPIU_Base_Address + FFCR_Offset_Address);
end ISA.ARM.Cortex_M4.TPIU;
