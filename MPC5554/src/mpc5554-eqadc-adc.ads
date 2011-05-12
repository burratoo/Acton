with System;
with System.Storage_Elements;  use System.Storage_Elements;
with Ada.Unchecked_Conversion;

package MPC5554.eQADC.ADC is

   ----------------------------------------------------------------------------
   -- Memory Addresses
   ----------------------------------------------------------------------------
   eQADC_Base_Address    : constant Integer_Address := 16#FFF4_4000#;
   MCR_Offset_Address    : constant Integer_Address := 16#0000#;
   NMSFR_Offset_Address  : constant Integer_Address := 16#0008#;
   ETDFR_Offset_Address  : constant Integer_Address := 16#000C#;
   CFPR_Offset_Address   : constant Integer_Address := 16#0010#;
   RFPR_Offset_Address   : constant Integer_Address := 16#0030#;
   CFCR_Offset_Address   : constant Integer_Address := 16#0050#;
   IDCR_Offset_Address   : constant Integer_Address := 16#0060#;
   FISR_Offset_Address   : constant Integer_Address := 16#0070#;
   CFTCR_Offset_Address  : constant Integer_Address := 16#0090#;
   CFSSR_Offset_Address  : constant Integer_Address := 16#00A0#;
   CFSR_Offset_Address   : constant Integer_Address := 16#00AC#;
   SSICR_Offset_Address  : constant Integer_Address := 16#00B4#;
   SSIRDR_Offset_Address : constant Integer_Address := 16#00B8#;
   CF_Offset_Address     : constant Integer_Address := 16#0100#;
   RF_Offset_Address     : constant Integer_Address := 16#0300#;

   ----------------------------------------------------------------------------
   -- Hardware Features
   ----------------------------------------------------------------------------
   type ADC_Register is mod 2 ** 16;
   type ADC_Register_Address is mod 2 ** 8;

   ----------------------------------------------------------------------------
   -- ADC Registers
   ---------------------------------------------------------------------------

   -- ADCn Control Register (ADC0_CR and ADC1_CR)
   type Clock_Prescaler_Type is mod 2 ** 5;

   type Control_Register_Type is record
      ADC                  : Enable_Type;
      External_Multiplexer : Enable_Type;
      Clock_Prescaler      : Clock_Prescaler_Type;
   end record;

   -- ADC Time_Stamp Control Register (ADC_TSCR)
   type TS_Clock_Prescaler_Type is mod 2 ** 4;

   type Time_Stamp_Clock_Register_Type is record
      Clock_Prescaler : TS_Clock_Prescaler_Type;
   end record;

   -- ADC Time Base Counter Register (ADC_TBCR)
   type Time_Base_Counter_Value is mod 2 ** 16;

   -- ADC Gain Calibration Constant Registers (ADCn_GCCR)
   type Gain_Calibration_Constant is mod 2 ** 15;

   -- ADC Offset Calibration Constant Registers (ADCn_OCCR)
   type Offset_Calibration_Constant is mod 2 ** 14;

   ----------------------------------------------------------------------------
   -- Command Messages
   ---------------------------------------------------------------------------
   type ADC_Address is mod 2 ** 8;
   type RW_Type is (Write, Read);
   for RW_Type use (Write => 0, Read => 1);

   type Buffer_Type is (Internal, External);
   type ADC_Type is (ADC0, ADC1);
   type Message_Type is (
      RFIFO0,
      RFIFO1,
      RFIFO2,
      RFIFO3,
      RFIFO4,
      RFIFO5,
      Null_Message);
   type LST_Type is (Cycle_2, Cycle_8, Cycle_64, Cycle_128);
   type Conversion_Format_Type is (Unsigned, Signed);
   type Channel_ID is mod 99;
   type Reserve_Type is mod 2 ** 8;

   type Conversion_Command_Message is record
      End_Of_Queue           : Yes_No_Type;
      Pause                  : Enable_Type;
      Buffer                 : Buffer_Type;
      Buffer_Number          : ADC_Type;
      Calibration            : Enable_Type;
      Message_Tag            : Message_Type;
      Long_Sampling_Time     : LST_Type;
      Time_Stamp_Request     : Enable_Type;
      Conversion_Data_Format : Conversion_Format_Type;
      Channel_Number         : Channel_ID;
      Reserved               : Reserve_Type := 0;
   end record;

   type Read_Write_Command_Message (Read_Write : RW_Type) is record
      End_Of_Queue    : Yes_No_Type;
      Pause           : Enable_Type;
      Buffer          : Buffer_Type;
      Buffer_Number   : ADC_Type;
      ADC_Reg_Address : ADC_Register_Address;
      case Read_Write is
         when Write =>
            ADC_Register_Data : ADC_Register;
         when Read =>
            Message_Tag : Message_Type;
      end case;
   end record;

   for Control_Register_Type use record
      ADC                  at 0 range 0 .. 0;
      External_Multiplexer at 0 range 4 .. 4;
      Clock_Prescaler      at 0 range 11 .. 15;
   end record;

   for Time_Stamp_Clock_Register_Type use record
      Clock_Prescaler at 0 range 12 .. 15;
   end record;

   for Buffer_Type use (Internal => 0, External => 1);
   for ADC_Type use (ADC0 => 0, ADC1 => 1);
   for Message_Type use
     (RFIFO0       => 0,
      RFIFO1       => 1,
      RFIFO2       => 2,
      RFIFO3       => 3,
      RFIFO4       => 4,
      RFIFO5       => 5,
      Null_Message => 2#1000#);
   for LST_Type use
     (Cycle_2   => 0,
      Cycle_8   => 1,
      Cycle_64  => 2,
      Cycle_128 => 3);
   for Conversion_Format_Type use (Unsigned => 0, Signed => 1);

   for Conversion_Command_Message use record
      End_Of_Queue           at 0 range 0 .. 0;
      Pause                  at 0 range 1 .. 1;
      Buffer                 at 0 range 5 .. 5;
      Buffer_Number          at 0 range 6 .. 6;
      Calibration            at 0 range 7 .. 7;
      Message_Tag            at 0 range 8 .. 11;
      Long_Sampling_Time     at 0 range 12 .. 13;
      Time_Stamp_Request     at 0 range 14 .. 14;
      Conversion_Data_Format at 0 range 15 .. 15;
      Channel_Number         at 0 range 16 .. 23;
      Reserved               at 0 range 24 .. 31;
   end record;
   for Conversion_Command_Message'Size use 32;

   for Read_Write_Command_Message use record
      End_Of_Queue      at 0 range 0 .. 0;
      Pause             at 0 range 1 .. 1;
      Buffer            at 0 range 5 .. 5;
      Buffer_Number     at 0 range 6 .. 6;
      Message_Tag       at 0 range 8 .. 11;
      Read_Write        at 0 range 7 .. 7;
      ADC_Register_Data at 0 range 8 .. 23;
      ADC_Reg_Address   at 0 range 24 .. 31;
   end record;

   ----------------------------------------------------------------------------
   -- Helper functions
   ----------------------------------------------------------------------------
   function To_eQADC_Command is new Ada.Unchecked_Conversion (
      Conversion_Command_Message,
      eQADC_Command);
   function To_eQADC_Command is new Ada.Unchecked_Conversion (
      Read_Write_Command_Message,
      eQADC_Command);
   function To_ADC_Register is new Ada.Unchecked_Conversion (
      Control_Register_Type,
      ADC_Register);
end MPC5554.eQADC.ADC;
