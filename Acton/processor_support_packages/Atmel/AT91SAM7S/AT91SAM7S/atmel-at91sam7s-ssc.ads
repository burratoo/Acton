with Interfaces; use Interfaces;
with System;     use System;

with Atmel.AT91SAM7S.PDC; use Atmel.AT91SAM7S.PDC;

package Atmel.AT91SAM7S.SSC is

   --------------------------
   -- SSC Memory Addresses --
   --------------------------

   SSC_Base_Address    : constant := 16#FFFD_4000#;
   CR_Offset_Address   : constant := 16#00#;
   CMR_Offset_Address  : constant := 16#04#;
   RCMR_Offset_Address : constant := 16#10#;
   RFMR_Offset_Address : constant := 16#14#;
   TCMR_Offset_Address : constant := 16#18#;
   TFMR_Offset_Address : constant := 16#1C#;
   RHR_Offset_Address  : constant := 16#20#;
   THR_Offset_Address  : constant := 16#24#;
   RSHR_Offset_Address : constant := 16#30#;
   TSHR_Offset_Address : constant := 16#34#;
   RC0R_Offset_Address : constant := 16#38#;
   RC1R_Offset_Address : constant := 16#3C#;
   SR_Offset_Address   : constant := 16#40#;
   IER_Offset_Address  : constant := 16#44#;
   IDR_Offset_Address  : constant := 16#48#;
   IMR_Offset_Address  : constant := 16#4C#;

   -----------------------
   -- Hardware Features --
   -----------------------

   ---------------
   -- PIO Types --
   ---------------

   type Control_Type is record
      Receive        : Wide_Enable_Type;
      Transmit       : Wide_Enable_Type;
      Software_Reset : Boolean;
   end record with Size => Standard'Word_Size;

   type Clock_Divider_Type is mod  2 ** 12;

   type Clock_Mode_Type is record
      Divider : Clock_Divider_Type;
   end record with Size => Standard'Word_Size;

   type Clock_Selection is (Divided_Clock, TK_Clock, RK_Pin);
   type Clock_Output_Mode_Selection is (None,
                                        Continuous,
                                        Only_During_Data_Transfer);
   type Gating_Selection is (None, RF_Low, RF_High);
   type Receive_Start_Selection is
     (Continuous,
      Transmit_Start,
      Low_Level_On_RF_Signal,
      High_Level_On_RF_Signal,
      Falling_Edge_On_RF_Signal,
      Rising_Edge_On_RF_Signal,
      Any_Level_Change_On_RF_Signal,
      Any_Edge_Change_On_RF_Signal,
      Compare_0) with Size => 4;
   type Receive_Stop_Selection is (Stop_Till_New_Compare_0, Run_Til_Compare_1);

   type Receive_Clock_Mode_Type is record
      Clock             : Clock_Selection;
      Clock_Output_Mode : Clock_Output_Mode_Selection;
      Clock_Inversion   : Boolean;
      Clock_Gating      : Gating_Selection;
      Start_Reason      : Receive_Start_Selection;
      Stop_Reason       : Receive_Stop_Selection;
      Start_Delay       : Unsigned_8;
      Period_Divider    : Unsigned_8;
   end record with Size => Standard'Word_Size;

   type Frame_Data_Length is mod 2 ** 5;
   type Data_Number is mod 2 ** 4;
   type Frame_Sync_Length_Selection is mod 2 ** 4;
   type Frame_Sync_Output_Selection is
     (None,
      Negative_Pulse,
      Positive_Pulse,
      Driven_Low_During_Transfer,
      Driven_High_During_Transfer,
      Toggle_At_Start_Of_Transfer) with Size => 3;

   type Receive_Frame_Mode_Type is record
      Data_Length                : Frame_Data_Length;
      Loop_Mode                  : Enable_Type;
      Most_Significant_Bit_First : Boolean;
      Data_Number_Per_Frame      : Data_Number;
      Frame_Sync_Length          : Frame_Sync_Length_Selection;
      Frame_Sync_Output          : Frame_Sync_Output_Selection;
      Fram_Sync_Edge_Detection   : Edge_Type;
   end record with Size => Standard'Word_Size;

   type Transmit_Start_Selection is
     (Continuous,
      Transmit_Start,
      Low_Level_On_RF_Signal,
      High_Level_On_RF_Signal,
      Falling_Edge_On_RF_Signal,
      Rising_Edge_On_RF_Signal,
      Any_Level_Change_On_RF_Signal,
      Any_Edge_Change_On_RF_Signal) with Size => 4;

   type Transmit_Clock_Mode_Type is record
      Clock             : Clock_Selection;
      Clock_Output_Mode : Clock_Output_Mode_Selection;
      Clock_Inversion   : Boolean;
      Clock_Gating      : Gating_Selection;
      Start_Reason      : Transmit_Start_Selection;
      Start_Delay       : Unsigned_8;
      Period_Divider    : Unsigned_8;
   end record with Size => Standard'Word_Size;

   type Transmit_Frame_Mode_Type is record
      Data_Length                : Frame_Data_Length;
      Data_Default_Value         : Pin_Status;
      Most_Significant_Bit_First : Boolean;
      Data_Number_Per_Frame      : Data_Number;
      Frame_Sync_Length          : Frame_Sync_Length_Selection;
      Frame_Sync_Output          : Frame_Sync_Output_Selection;
      Frame_Sync_Data_Enable     : Enable_Type;
      Frame_Sync_Edge_Detection  : Edge_Type;
   end record with Size => Standard'Word_Size;

   type Unsigned_32_Register is record
      Data : Unsigned_32;
   end record with Size => Standard'Word_Size;

   type Unsigned_16_Register is record
      Data : Unsigned_16;
   end record with Size => Standard'Word_Size;

   type Status_Type is record
      Transmit_Ready        : Boolean;
      Transmit_Empty        : Boolean;
      End_Of_Transmission   : Boolean;
      Transmit_Buffer_Empty : Boolean;
      Receive_Ready         : Boolean;
      Receive_Overrun       : Boolean;
      End_Of_Reception      : Boolean;
      Receive_Buffer_Full   : Boolean;
      Compare_0             : Occured_Type;
      Compare_1             : Occured_Type;
      Transmit_Sync         : Occured_Type;
      Receive_Sync          : Occured_Type;
      Transmit_Enable       : Enabled_Type;
      Receive_Enable        : Enabled_Type;
   end record;

   type Interrupt_Enable_Type is record
      Transmit_Ready        : Enable_No_Change_Type;
      Transmit_Empty        : Enable_No_Change_Type;
      End_Of_Transmission   : Enable_No_Change_Type;
      Transmit_Buffer_Empty : Enable_No_Change_Type;
      Receive_Ready         : Enable_No_Change_Type;
      Receive_Overrun       : Enable_No_Change_Type;
      End_Of_Reception      : Enable_No_Change_Type;
      Receive_Buffer_Full   : Enable_No_Change_Type;
      Compare_0             : Enable_No_Change_Type;
      Compare_1             : Enable_No_Change_Type;
      Transmit_Sync         : Enable_No_Change_Type;
      Receive_Sync          : Enable_No_Change_Type;
   end record with Size => Standard'Word_Size;

   type Interrupt_Disable_Type is record
      Transmit_Ready        : Disable_No_Change_Type;
      Transmit_Empty        : Disable_No_Change_Type;
      End_Of_Transmission   : Disable_No_Change_Type;
      Transmit_Buffer_Empty : Disable_No_Change_Type;
      Receive_Ready         : Disable_No_Change_Type;
      Receive_Overrun       : Disable_No_Change_Type;
      End_Of_Reception      : Disable_No_Change_Type;
      Receive_Buffer_Full   : Disable_No_Change_Type;
      Compare_0             : Disable_No_Change_Type;
      Compare_1             : Disable_No_Change_Type;
      Transmit_Sync         : Disable_No_Change_Type;
      Receive_Sync          : Disable_No_Change_Type;
   end record with Size => Standard'Word_Size;

   type Interrupt_Mask_Type is record
      Transmit_Ready        : Enabled_Type;
      Transmit_Empty        : Enabled_Type;
      End_Of_Transmission   : Enabled_Type;
      Transmit_Buffer_Empty : Enabled_Type;
      Receive_Ready         : Enabled_Type;
      Receive_Overrun       : Enabled_Type;
      End_Of_Reception      : Enabled_Type;
      Receive_Buffer_Full   : Enabled_Type;
      Compare_0             : Enabled_Type;
      Compare_1             : Enabled_Type;
      Transmit_Sync         : Enabled_Type;
      Receive_Sync          : Enabled_Type;
   end record with Size => Standard'Word_Size;

   ------------------------------
   -- Hardware Representations --
   ------------------------------

   for Control_Type use record
      Receive        at 0 range 0 .. 1;
      Transmit       at 0 range 8 .. 9;
      Software_Reset at 0 range 15 .. 15;
   end record;

   for Clock_Mode_Type use record
      Divider at 0 range 0 .. 11;
   end record;

   for Clock_Selection use (Divided_Clock => 0, TK_Clock => 1, RK_Pin => 2);
   for Clock_Output_Mode_Selection use (None                      => 0,
                                        Continuous                => 1,
                                        Only_During_Data_Transfer => 2);
   for Gating_Selection use (None => 0, RF_Low => 1, RF_High => 2);
   for Receive_Start_Selection use
     (Continuous                    => 0,
      Transmit_Start                => 1,
      Low_Level_On_RF_Signal        => 2,
      High_Level_On_RF_Signal       => 3,
      Falling_Edge_On_RF_Signal     => 4,
      Rising_Edge_On_RF_Signal      => 5,
      Any_Level_Change_On_RF_Signal => 6,
      Any_Edge_Change_On_RF_Signal  => 7,
      Compare_0                     => 8);

   for Receive_Clock_Mode_Type use record
      Clock             at 0 range 0 .. 1;
      Clock_Output_Mode at 0 range 2 .. 4;
      Clock_Inversion   at 0 range 5 .. 5;
      Clock_Gating      at 0 range 6 .. 7;
      Start_Reason      at 0 range 8 .. 11;
      Stop_Reason       at 0 range 12 .. 12;
      Start_Delay       at 0 range 16 .. 23;
      Period_Divider    at 0 range 24 .. 31;
   end record;

   for Frame_Sync_Output_Selection use
     (None => 0,
      Negative_Pulse => 1,
      Positive_Pulse => 2,
      Driven_Low_During_Transfer => 3,
      Driven_High_During_Transfer => 4,
      Toggle_At_Start_Of_Transfer => 5);

   for Receive_Frame_Mode_Type use record
      Data_Length                at 0 range 0 .. 4;
      Loop_Mode                  at 0 range 5 .. 5;
      Most_Significant_Bit_First at 0 range 7 .. 7;
      Data_Number_Per_Frame      at 0 range 8 .. 11;
      Frame_Sync_Length          at 0 range 16 .. 19;
      Frame_Sync_Output          at 0 range 20 .. 22;
      Fram_Sync_Edge_Detection   at 0 range 24 .. 24;
   end record;

   for Transmit_Start_Selection use
     (Continuous                    => 0,
      Transmit_Start                => 1,
      Low_Level_On_RF_Signal        => 2,
      High_Level_On_RF_Signal       => 3,
      Falling_Edge_On_RF_Signal     => 4,
      Rising_Edge_On_RF_Signal      => 5,
      Any_Level_Change_On_RF_Signal => 6,
      Any_Edge_Change_On_RF_Signal  => 7);

   for Transmit_Clock_Mode_Type use record
      Clock             at 0 range 0 .. 1;
      Clock_Output_Mode at 0 range 2 .. 4;
      Clock_Inversion   at 0 range 5 .. 5;
      Clock_Gating      at 0 range 6 .. 7;
      Start_Reason      at 0 range 8 .. 11;
      Start_Delay       at 0 range 16 .. 23;
      Period_Divider    at 0 range 24 .. 31;
   end record;

   for Transmit_Frame_Mode_Type use record
      Data_Length                at 0 range 0 .. 4;
      Data_Default_Value         at 0 range 5 .. 5;
      Most_Significant_Bit_First at 0 range 7 .. 7;
      Data_Number_Per_Frame      at 0 range 8 .. 11;
      Frame_Sync_Length          at 0 range 16 .. 19;
      Frame_Sync_Output          at 0 range 20 .. 22;
      Frame_Sync_Data_Enable     at 0 range 23 .. 23;
      Frame_Sync_Edge_Detection  at 0 range 24 .. 24;
   end record;

   for Unsigned_32_Register use record
      Data at 0 range 0 .. 31;
   end record;

   for Unsigned_16_Register use record
      Data at 0 range 0 .. 15;
   end record;

   for Status_Type use record
      Transmit_Ready        at 0 range 0 .. 0;
      Transmit_Empty        at 0 range 1 .. 1;
      End_Of_Transmission   at 0 range 2 .. 2;
      Transmit_Buffer_Empty at 0 range 3 .. 3;
      Receive_Ready         at 0 range 4 .. 4;
      Receive_Overrun       at 0 range 5 .. 5;
      End_Of_Reception      at 0 range 6 .. 6;
      Receive_Buffer_Full   at 0 range 7 .. 7;
      Compare_0             at 0 range 8 .. 8;
      Compare_1             at 0 range 9 .. 9;
      Transmit_Sync         at 0 range 10 .. 10;
      Receive_Sync          at 0 range 11 .. 11;
      Transmit_Enable       at 0 range 16 .. 16;
      Receive_Enable        at 0 range 17 .. 17;
   end record;

   for Interrupt_Enable_Type use record
      Transmit_Ready        at 0 range 0 .. 0;
      Transmit_Empty        at 0 range 1 .. 1;
      End_Of_Transmission   at 0 range 2 .. 2;
      Transmit_Buffer_Empty at 0 range 3 .. 3;
      Receive_Ready         at 0 range 4 .. 4;
      Receive_Overrun       at 0 range 5 .. 5;
      End_Of_Reception      at 0 range 6 .. 6;
      Receive_Buffer_Full   at 0 range 7 .. 7;
      Compare_0             at 0 range 8 .. 8;
      Compare_1             at 0 range 9 .. 9;
      Transmit_Sync         at 0 range 10 .. 10;
      Receive_Sync          at 0 range 11 .. 11;
   end record;

   for Interrupt_Disable_Type use record
      Transmit_Ready        at 0 range 0 .. 0;
      Transmit_Empty        at 0 range 1 .. 1;
      End_Of_Transmission   at 0 range 2 .. 2;
      Transmit_Buffer_Empty at 0 range 3 .. 3;
      Receive_Ready         at 0 range 4 .. 4;
      Receive_Overrun       at 0 range 5 .. 5;
      End_Of_Reception      at 0 range 6 .. 6;
      Receive_Buffer_Full   at 0 range 7 .. 7;
      Compare_0             at 0 range 8 .. 8;
      Compare_1             at 0 range 9 .. 9;
      Transmit_Sync         at 0 range 10 .. 10;
      Receive_Sync          at 0 range 11 .. 11;
   end record;

   for Interrupt_Mask_Type use record
      Transmit_Ready        at 0 range 0 .. 0;
      Transmit_Empty        at 0 range 1 .. 1;
      End_Of_Transmission   at 0 range 2 .. 2;
      Transmit_Buffer_Empty at 0 range 3 .. 3;
      Receive_Ready         at 0 range 4 .. 4;
      Receive_Overrun       at 0 range 5 .. 5;
      End_Of_Reception      at 0 range 6 .. 6;
      Receive_Buffer_Full   at 0 range 7 .. 7;
      Compare_0             at 0 range 8 .. 8;
      Compare_1             at 0 range 9 .. 9;
      Transmit_Sync         at 0 range 10 .. 10;
      Receive_Sync          at 0 range 11 .. 11;
   end record;

   -------------------
   -- PIO Registers --
   -------------------

   Control_Register : Control_Type
     with Address => System'To_Address (SSC_Base_Address + CR_Offset_Address);

   Clock_Mode_Register : Clock_Mode_Type
     with Address => System'To_Address (SSC_Base_Address + CMR_Offset_Address);

   Receive_Clock_Mode_Register : Receive_Clock_Mode_Type
     with Address =>
       System'To_Address (SSC_Base_Address + RCMR_Offset_Address);

   Receive_Frame_Mode_Register : Receive_Frame_Mode_Type
     with Address =>
       System'To_Address (SSC_Base_Address + RFMR_Offset_Address);

   Transmit_Clock_Mode_Register : Transmit_Clock_Mode_Type
     with Address =>
       System'To_Address (SSC_Base_Address + TCMR_Offset_Address);

   Transmit_Frame_Mode_Register : Transmit_Frame_Mode_Type
     with Address =>
       System'To_Address (SSC_Base_Address + TFMR_Offset_Address);

   Receive_Holding_Register    : Unsigned_32_Register
     with Address => System'To_Address (SSC_Base_Address + RHR_Offset_Address);

   Transmit_Holding_Register   : Unsigned_32_Register
     with Address => System'To_Address (SSC_Base_Address + THR_Offset_Address);

   Receive_Synchronization_Holding_Register : Unsigned_16_Register
     with Address =>
       System'To_Address (SSC_Base_Address + RSHR_Offset_Address);

   Transmit_Synchronization_Holding_Register : Unsigned_16_Register
     with Address =>
       System'To_Address (SSC_Base_Address + TSHR_Offset_Address);

   Receive_Compare_0_Register  : Unsigned_16_Register
     with Address =>
       System'To_Address (SSC_Base_Address + RC0R_Offset_Address);

   Receive_Compare_1_Register  : Unsigned_16_Register
     with Address =>
       System'To_Address (SSC_Base_Address + RC1R_Offset_Address);

   Status_Register             : Status_Type
     with Address => System'To_Address (SSC_Base_Address + SR_Offset_Address);

   Interrupt_Enable_Register   : Interrupt_Enable_Type
     with Address => System'To_Address (SSC_Base_Address + IER_Offset_Address);

   Interrupt_Disable_Register : Interrupt_Disable_Type
     with Address => System'To_Address (SSC_Base_Address + IDR_Offset_Address);

   Interrupt_Mask_Register : Interrupt_Mask_Type
     with Address => System'To_Address (SSC_Base_Address + IMR_Offset_Address);

   Receive_Pointer_Register : Address
     with Address => System'To_Address (SSC_Base_Address + RPR_Offset_Address);

   Receive_Counter_Register : Unsigned_16
     with Address => System'To_Address (SSC_Base_Address + RCR_Offset_Address);

   Transmit_Pointer_Register : Address
     with Address => System'To_Address (SSC_Base_Address + TPR_Offset_Address);

   Transmit_Counter_Register : Unsigned_16
     with Address => System'To_Address (SSC_Base_Address + TCR_Offset_Address);

   Receive_Next_Pointer_Register : Address
     with Address =>
       System'To_Address (SSC_Base_Address + RNPR_Offset_Address);

   Receive_Next_Counter_Register : Unsigned_16
     with Address =>
       System'To_Address (SSC_Base_Address + RNCR_Offset_Address);

   Transmit_Next_Pointer_Register : Address
     with Address =>
       System'To_Address (SSC_Base_Address + TNPR_Offset_Address);

   Transmit_Next_Counter_Register : Unsigned_16
     with Address =>
       System'To_Address (SSC_Base_Address + TNCR_Offset_Address);

   Transfer_Control_Register : Transfer_Control_Type
     with Address =>
       System'To_Address (SSC_Base_Address + PTCR_Offset_Address);

   pragma Warnings (Off, "*alignment*");

   Transfer_Status_Register : Transfer_Status_Type
     with Address =>
       System'To_Address (SSC_Base_Address + PTSR_Offset_Address);

end Atmel.AT91SAM7S.SSC;
