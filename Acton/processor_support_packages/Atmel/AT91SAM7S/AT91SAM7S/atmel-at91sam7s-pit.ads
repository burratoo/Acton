with System; use System;

package Atmel.AT91SAM7S.PIT with Preelaborate is

   --------------------------
   -- PIT Memory Addresses --
   --------------------------

   PIT_Base_Address    : constant := 16#FFFF_F000#;
   MR_Offset_Address   : constant := 16#0#;
   SR_Offset_Address   : constant := 16#4#;
   PIVR_Offset_Address : constant := 16#8#;
   PIIR_Offset_Address : constant := 16#C#;

   -----------------------
   -- Hardware Features --
   -----------------------

   type Periodic_Interval is mod 2 ** 20;
   type Periodic_Interval_Overflow is mod 2 ** 12;

   ---------------
   -- PIT Types --
   ---------------

   type Mode_Type is record
      Periodic_Interval_Value           : Periodic_Interval;
      Period_Interval_Timer             : Enable_Type;
      Periodic_Interval_Timer_Interrupt : Enable_Type;
   end record with Size => 32;

   type Status_Type is record
      Periodic_Interval_Timer_Event : Occured_Type;
   end record with Size => 32;

   type Timer_Value_Type is record
      Current_Periodic_Interval_Value : Periodic_Interval;
      Periodic_Interval_Counter       : Periodic_Interval_Overflow;
   end record with Size => 32;

   ------------------------------
   -- Hardware Representations --
   ------------------------------

   for Mode_Type use record
      Periodic_Interval_Value           at 0 range 0  .. 19;
      Period_Interval_Timer             at 0 range 24 .. 24;
      Periodic_Interval_Timer_Interrupt at 0 range 25 .. 25;
   end record;

   for Status_Type use record
      Periodic_Interval_Timer_Event at 0 range 0 .. 0;
   end record;

   for Timer_Value_Type use record
      Current_Periodic_Interval_Value at 0 range 0 .. 19;
      Periodic_Interval_Counter       at 0 range 20 .. 31;
   end record;

   -------------------
   -- AIC Registers --
   -------------------

   Mode_Register : Mode_Type
     with Address => System'To_Address (PIT_Base_Address + MR_Offset_Address);

   Status_Register : Status_Type
     with Address => System'To_Address (PIT_Base_Address + SR_Offset_Address);

   Periodic_Interval_Timer_Value_Register : Timer_Value_Type
     with Address => System'To_Address
       (PIT_Base_Address + PIVR_Offset_Address);

   Periodic_Interval_Timer_Image_Register : Timer_Value_Type
     with Address => System'To_Address
       (PIT_Base_Address + PIIR_Offset_Address);
end Atmel.AT91SAM7S.PIT;
