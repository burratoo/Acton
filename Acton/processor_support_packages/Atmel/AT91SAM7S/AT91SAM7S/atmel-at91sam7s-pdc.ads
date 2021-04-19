------------------------------------------------------------------------------------------
--                                                                                      --
--                            ACTON PROCESSOR SUPPORT PACKAGE                           --
--                                                                                      --
--                                  ATMEL.AT91SAM7S.PDC                                 --
--                                                                                      --
--                       Copyright (C) 2014-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

package Atmel.AT91SAM7S.PDC with Pure is

   --------------------------
   -- PDC Memory Addresses --
   --------------------------

   RPR_Offset_Address  : constant := 16#0100#;
   RCR_Offset_Address  : constant := 16#0104#;
   TPR_Offset_Address  : constant := 16#0108#;
   TCR_Offset_Address  : constant := 16#010C#;
   RNPR_Offset_Address : constant := 16#0110#;
   RNCR_Offset_Address : constant := 16#0114#;
   TNPR_Offset_Address : constant := 16#0118#;
   TNCR_Offset_Address : constant := 16#011C#;
   PTCR_Offset_Address : constant := 16#0120#;
   PTSR_Offset_Address : constant := 16#0124#;

   ---------------
   -- PDC Types --
   ---------------

   type Transfer_Control_Type is record
      Receiver_Transfer    : Wide_Enable_Type;
      Transmitter_Transfer : Wide_Enable_Type;
   end record;

   type Transfer_Status_Type is record
      Receiver_Transfer    : Enabled_Type;
      Transmitter_Transfer : Enabled_Type;
   end record;

   ------------------------------
   -- Hardware Representations --
   ------------------------------

   for Transfer_Control_Type use record
      Receiver_Transfer     at 0 range 0 .. 1;
      Transmitter_Transfer  at 0 range 8 .. 9;
   end record;

   for Transfer_Status_Type use record
      Receiver_Transfer    at 0 range 0 .. 0;
      Transmitter_Transfer at 0 range 8 .. 8;
   end record;
end Atmel.AT91SAM7S.PDC;
