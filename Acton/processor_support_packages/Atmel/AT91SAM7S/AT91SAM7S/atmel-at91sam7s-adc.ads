with Interfaces; use Interfaces;
with System;     use System;

package Atmel.AT91SAM7S.ADC with Preelaborate is

   ----------------------------------------
   -- Public Hardware Features and Types --
   ----------------------------------------

   type ADC_Channel_Id is range 0 .. 7;

   type Trigger_Source is (TIOA0, TIOA1, TIOA2, External);
   type Resolution_Type is (Ten_Bit, Eight_Bit);
   type Startup_Time_Range is mod 2 ** 7;
   type SHT is mod 2 ** 4;

   type Mode_Type is record
      Hardware_Trigger     : Enable_Type;
      Trigger_Selection    : Trigger_Source;
      Resolution           : Resolution_Type;
      Sleep_Mode           : Enable_Type;
      Prescaler            : Unsigned_8;
      Startup_Time         : Startup_Time_Range;
      Sample_And_Hold_Time : SHT;
   end record with Size => Standard'Word_Size;

   ----------------------
   -- Public Interface --
   ----------------------

   procedure Initialise_Interface (Settings : Mode_Type);
   function Interface_Is_Ready return Boolean;

   procedure Enable_Channel (Channel : ADC_Channel_Id) with Inline;
   procedure Disable_Channel (Channel : ADC_Channel_Id) with Inline;

   function Read_Channel (Channel : ADC_Channel_Id) return Unsigned_16;

private

   --------------------------
   -- ADC Memory Addresses --
   --------------------------

   ADC_Base_Address    : constant := 16#FFFD_8000#;
   CR_Offset_Address   : constant := 16#00#;
   MR_Offset_Address   : constant := 16#04#;
   CHER_Offset_Address : constant := 16#10#;
   CHDR_Offset_Address : constant := 16#14#;
   CHSR_Offset_Address : constant := 16#18#;
   SR_Offset_Address   : constant := 16#1C#;
   LCDR_Offset_Address : constant := 16#20#;
   IER_Offset_Address  : constant := 16#24#;
   IDR_Offset_Address  : constant := 16#28#;
   IMR_Offset_Address  : constant := 16#2C#;
   CDR_Offset_Address  : constant := 16#30#;

   -----------------------
   -- Hardware Features --
   -----------------------

   ---------------
   -- ADC Types --
   ---------------

   type Control_Register_Type is record
      Software_Reset   : Boolean;
      Start_Conversion : Boolean;
   end record with Size => 32;

   type ADC_Enable_Set is array (ADC_Channel_Id) of Enable_No_Change_Type
     with Pack, Alignment => 4;

   type Channel_Enable_Type is record
      Channel : ADC_Enable_Set;
   end record with Size => Standard'Word_Size;

   type ADC_Disable_Set is array (ADC_Channel_Id) of Disable_No_Change_Type
     with Pack, Alignment => 4;

   type Channel_Disable_Type is record
      Channel : ADC_Disable_Set;
   end record with Size => Standard'Word_Size;

   type ADC_Status_Set is array (ADC_Channel_Id) of Enabled_Type
     with Pack, Alignment => 4;

   type Channel_Status_Type is record
      Channel : ADC_Status_Set;
   end record with Size => Standard'Word_Size;

   type ADC_Boolean_Set is array (ADC_Channel_Id) of Boolean
     with Pack, Alignment => 4;

   type Status_Type is record
      End_Of_Conversion     : ADC_Boolean_Set;
      Overrun_Error         : ADC_Boolean_Set;
      Data_Read             : Boolean;
      General_Overrun_Error : Boolean;
      End_Of_Receive_Buffer : Boolean;
      Receive_Buffer_Full   : Boolean;
   end record with Size => Standard'Word_Size;

   type Interrupt_Enable_Type is record
      End_Of_Conversion     : ADC_Enable_Set;
      Overrun_Error         : ADC_Enable_Set;
      Data_Read             : Enable_No_Change_Type;
      General_Overrun_Error : Enable_No_Change_Type;
      End_Of_Receive_Buffer : Enable_No_Change_Type;
      Receive_Buffer_Full   : Enable_No_Change_Type;
   end record with Size => Standard'Word_Size;

   type Interrupt_Disable_Type is record
      End_Of_Conversion     : ADC_Disable_Set;
      Overrun_Error         : ADC_Disable_Set;
      Data_Read             : Disable_No_Change_Type;
      General_Overrun_Error : Disable_No_Change_Type;
      End_Of_Receive_Buffer : Disable_No_Change_Type;
      Receive_Buffer_Full   : Disable_No_Change_Type;
   end record with Size => Standard'Word_Size;

   type Interrupt_Mask_Type is record
      End_Of_Conversion     : ADC_Status_Set;
      Overrun_Error         : ADC_Status_Set;
      Data_Read             : Enabled_Type;
      General_Overrun_Error : Enabled_Type;
      End_Of_Receive_Buffer : Enabled_Type;
      Receive_Buffer_Full   : Enabled_Type;
   end record with Size => Standard'Word_Size;

   type ADC_Channel_Data is record
      Data : Unsigned_16;
   end record with Size => Standard'Word_Size;

   ------------------------------
   -- Hardware Representations --
   ------------------------------

   for Control_Register_Type use record
      Software_Reset   at 0 range 0 .. 0;
      Start_Conversion at 0 range 1 .. 1;
   end record;

   for Trigger_Source use (TIOA0    => 0,
                           TIOA1    => 1,
                           TIOA2    => 2,
                           External => 6);

   for Resolution_Type use (Ten_Bit => 0, Eight_Bit => 1);

   for Mode_Type use record
      Hardware_Trigger     at 0 range 0 .. 0;
      Trigger_Selection    at 0 range 1 .. 3;
      Resolution           at 0 range 4 .. 4;
      Sleep_Mode           at 0 range 5 .. 5;
      Prescaler            at 0 range 8 .. 15;
      Startup_Time         at 0 range 16 .. 22;
      Sample_And_Hold_Time at 0 range 24 .. 27;
   end record;

   for Channel_Enable_Type use record
      Channel at 0 range 0 .. 7;
   end record;

   for Status_Type use record
      End_Of_Conversion     at 0 range 0 .. 7;
      Overrun_Error         at 0 range 8 .. 15;
      Data_Read             at 0 range 16 .. 16;
      General_Overrun_Error at 0 range 17 .. 17;
      End_Of_Receive_Buffer at 0 range 18 .. 18;
      Receive_Buffer_Full   at 0 range 19 .. 19;
   end record;

   for Interrupt_Enable_Type use record
      End_Of_Conversion     at 0 range 0 .. 7;
      Overrun_Error         at 0 range 8 .. 15;
      Data_Read             at 0 range 16 .. 16;
      General_Overrun_Error at 0 range 17 .. 17;
      End_Of_Receive_Buffer at 0 range 18 .. 18;
      Receive_Buffer_Full   at 0 range 19 .. 19;
   end record;

   for Interrupt_Disable_Type use record
      End_Of_Conversion     at 0 range 0 .. 7;
      Overrun_Error         at 0 range 8 .. 15;
      Data_Read             at 0 range 16 .. 16;
      General_Overrun_Error at 0 range 17 .. 17;
      End_Of_Receive_Buffer at 0 range 18 .. 18;
      Receive_Buffer_Full   at 0 range 19 .. 19;
   end record;

   for Interrupt_Mask_Type use record
      End_Of_Conversion     at 0 range 0 .. 7;
      Overrun_Error         at 0 range 8 .. 15;
      Data_Read             at 0 range 16 .. 16;
      General_Overrun_Error at 0 range 17 .. 17;
      End_Of_Receive_Buffer at 0 range 18 .. 18;
      Receive_Buffer_Full   at 0 range 19 .. 19;
   end record;

   -------------------
   -- ADC Registers --
   -------------------

   Control_Register : Control_Register_Type
     with Address => System'To_Address (ADC_Base_Address + CR_Offset_Address);

   Mode_Register : Mode_Type
     with Address => System'To_Address (ADC_Base_Address + MR_Offset_Address);

   Channel_Enable_Register : Channel_Enable_Type
     with Address =>
       System'To_Address (ADC_Base_Address + CHER_Offset_Address);

   Channel_Disable_Register : Channel_Disable_Type
     with Address =>
       System'To_Address (ADC_Base_Address + CHDR_Offset_Address);

   Channel_Status_Register : Channel_Status_Type
     with Address =>
       System'To_Address (ADC_Base_Address + CHSR_Offset_Address);

   Status_Register : Status_Type
     with Address => System'To_Address (ADC_Base_Address + SR_Offset_Address);

   Last_Converted_Data_Register : ADC_Channel_Data
     with Address =>
       System'To_Address (ADC_Base_Address + LCDR_Offset_Address);

   Interrupt_Enable_Register : Interrupt_Enable_Type
     with Address => System'To_Address (ADC_Base_Address + IER_Offset_Address);

   Interrupt_Disable_Register : Interrupt_Disable_Type
     with Address => System'To_Address (ADC_Base_Address + IDR_Offset_Address);

   Interrupt_Mask_Register : Interrupt_Mask_Type
     with Address => System'To_Address (ADC_Base_Address + IMR_Offset_Address);

   Channel_Data_Register : array (ADC_Channel_Id) of ADC_Channel_Data
     with Alignment => 4,
       Address => System'To_Address (ADC_Base_Address + CDR_Offset_Address);

   -------------------
   -- Internal Data --
   -------------------

   Interface_Ready : Boolean := False;

   --------------------------
   -- Expression Functions --
   --------------------------

   function Interface_Is_Ready return Boolean is
     (Interface_Ready);

   function Read_Channel (Channel : ADC_Channel_Id) return Unsigned_16 is
     (Channel_Data_Register (Channel).Data);

end Atmel.AT91SAM7S.ADC;
