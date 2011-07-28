with System.Storage_Elements; use System.Storage_Elements;

package MPC5554.eMIOS is

   ----------------------------------------------------------------------------
   --  Memory Addresses
   ----------------------------------------------------------------------------
   EMIOS_Base_Address             : constant Integer_Address := 16#C3FA_0000#;
   GFR_Offset_Address             : constant Integer_Address := 16#0004#;
   Unified_Channel_Offset_Address : constant Integer_Address := 16#0020#;
   ----------------------------------------------------------------------------
   --  Hardware Features
   ----------------------------------------------------------------------------
   type EMIOS_ID_Type is range 0 .. 23;
   Unified_Channel_Type_Size : constant Integer := 256;
   Channel_Register_Size     : constant Integer := 24;

   ----------------------------------------------------------------------------
   --  eMIOS Types
   ---------------------------------------------------------------------------

   --  Common Types

   --  eMIOS Module Configuration Register
   type MDIS_Type is (Enable, Disable);
   type ETB_Type is (Unified_Channel_23, STAC);
   type SRV_Type is (eTPUA_TCR1, eTPUB_TCR1, eTPUA_TCR2, eTPUB_TCR2);
   type GPRE_Type is range 1 .. 256;

   type Module_Configuration_Type is record
      Module_Disable          : MDIS_Type;
      Freeze                  : Enable_Type;
      Global_Time_Base_Enable : Enable_Type;
      External_Time_Base      : ETB_Type;
      Global_Prescale_Enable  : Enable_Type;
      Server_Time_Slot        : SRV_Type;
      Global_Prescaler        : GPRE_Type;
   end record;

   --  eMIOS Channel Data Register
   type Channel_Register_Type is range 0 .. 16#FF_FFFF#;
   for Channel_Register_Type'Size use Channel_Register_Size;

   type ODISSL_Type is range 0 .. 3;
   type Divide_Ratio is range 1 .. 4;
   type DMA_Select_Type is (Interrupt, DMA);
   type IF_Type is (Bypass, Two, Four, Eight, Sixteen);
   type FCL_Type is (Prescaled, Main);
   type BSL_Type is (A, BCD, Internal);
   type Mode_Type is (
      GPIO_Input,
      GPIO_Output,
      Single_Action_Input_Capture,
      Single_Action_Output_Compare,
      Input_Pulse_Width_Measurement,
      Input_Period_Measurement,
      Doblue_Action_Output_Compare_Second_Match,
      Double_Action_Output_Compare_Both_Match,
      Pulse_Edge_Accumulation_Continuous,
      Pulse_Edge_Accumlation_Single,
      Pulse_Edge_Counting_Continuous,
      Pulse_Edge_Counting_Single,
      Quadrature_Decode_1,
      Quadrature_Decode_2,
      Windowed_Programmable_Time_Accumulation,
      Modulus_Counter_Up_Internal,
      Modulus_Counter_Up_External,
      Modulus_Counter_Up_Down_No_Change_Internal,
      Modulus_Counter_Up_Down_No_Change_External,
      Modulus_Counter_Up_Down_Change_Internal,
      Modulus_Counter_Up_Down_Change_External,
      Output_Pulse_Width_and_Frequency_Modulation_1,
      Output_Pulse_Width_and_Frequency_Modulation_2,
      Output_Pulse_Width_and_Frequency_Modulation_3,
      Output_Pulse_Width_and_Frequency_Modulation_4,
      Centre_Aligned_PWM_1,
      Centre_Aligned_PWM_2,
      Centre_Aligned_PWM_3,
      Centre_Aligned_PWM_4,
      Output_PWM_1,
      Output_PWM_2,
      Output_PWM_3,
      Output_PWM_4,
      Modulus_Up_Counter_Buffered_Internal,
      Modulus_Up_Counter_Buffered_External,
      Modulus_Up_Down_Counter_Buffered_1,
      Modulus_Up_Down_Counter_Buffered_2,
      Modulus_Up_Down_Counter_Buffered_3,
      Modulus_Up_Down_Counter_Buffered_4,
      Output_Pulse_Width_and_Frequency_Modulation_Buffered_1,
      Output_Pulse_Width_and_Frequency_Modulation_Buffered_2,
      Centre_Aligned_PWM_Buffered_1,
      Centre_Aligned_PWM_Buffered_2,
      Centre_Aligned_PWM_Buffered_3,
      Centre_Aligned_PWM_Buffered_4,
      Output_PWM_Buffered_1,
      Output_PWM_Buffered_2);

   type CCR_Type is record
      Freeze                : Enable_Type;
      Output_Disable        : Enable_Type;
      Output_Disable_Select : ODISSL_Type;
      Prescaler             : Divide_Ratio;
      Prescaler_Enable      : Enable_Type;
      DMA_Select            : DMA_Select_Type;
      Input_Filter          : IF_Type;
      Filter_Clock_Select   : FCL_Type;
      FLAG_Enable           : Enable_Type;
      Force_Match_A         : Enable_Type;
      Force_Match_B         : Enable_Type;
      Bus_Select            : BSL_Type;
      Edge_Select           : Boolean; --  See page 17-17 of the MPC5554
                                       --  reference manual for more info
      Edge_Polarity         : Boolean; --  See page 17-18
      Mode                  : Mode_Type;
   end record;

   type CSR_Type is record
      Overrun                  : Occurred_Type;
      Overlow                  : Occurred_Type;
      Unified_State_Input_Pin  : Pin_State_Type;
      Unified_State_Output_Pin : Pin_State_Type;
      Flag                     : Occurred_Type;
   end record;

   type Unified_Channel_Type is record
      Channel_A_Data_Register  : Channel_Register_Type;
      Channel_B_Data_Register  : Channel_Register_Type;
      Channel_Counter_Register : Channel_Register_Type;
      Channel_Control_Register : CCR_Type;
      Channel_Status_Register  : CSR_Type;
   end record;

   ----------------------------------------------------------------------------
   --  Hardware Respresentations
   ----------------------------------------------------------------------------

   for MDIS_Type use (Enable => 0, Disable => 1);
   for ETB_Type use (Unified_Channel_23 => 0, STAC => 1);
   for SRV_Type use
     (eTPUA_TCR1 => 2#0000#,
      eTPUB_TCR1 => 2#0001#,
      eTPUA_TCR2 => 2#0010#,
      eTPUB_TCR2 => 2#0011#);

   for Module_Configuration_Type use record
      Module_Disable          at 0 range 1 .. 1;
      Freeze                  at 0 range 2 .. 2;
      Global_Time_Base_Enable at 0 range 3 .. 3;
      External_Time_Base      at 0 range 4 .. 4;
      Global_Prescale_Enable  at 0 range 5 .. 5;
      Server_Time_Slot        at 0 range 12 .. 15;
      Global_Prescaler        at 0 range 16 .. 23;
   end record;

   for DMA_Select_Type use (Interrupt => 0, DMA => 1);
   for IF_Type use
     (Bypass  => 2#0000#,
      Two     => 2#0001#,
      Four    => 2#0010#,
      Eight   => 2#0100#,
      Sixteen => 2#1000#);
   for FCL_Type use (Prescaled => 0, Main => 1);
   for BSL_Type use (A => 2#00#, BCD => 2#01#, Internal => 2#11#);
   for Mode_Type use
     (GPIO_Input                                             => 2#0000000#,
      GPIO_Output                                            => 2#0000001#,
      Single_Action_Input_Capture                            => 2#0000010#,
      Single_Action_Output_Compare                           => 2#0000011#,
      Input_Pulse_Width_Measurement                          => 2#0000100#,
      Input_Period_Measurement                               => 2#0000101#,
      Doblue_Action_Output_Compare_Second_Match              => 2#0000110#,
      Double_Action_Output_Compare_Both_Match                => 2#0000111#,
      Pulse_Edge_Accumulation_Continuous                     => 2#0001000#,
      Pulse_Edge_Accumlation_Single                          => 2#0001001#,
      Pulse_Edge_Counting_Continuous                         => 2#0001010#,
      Pulse_Edge_Counting_Single                             => 2#0001011#,
      Quadrature_Decode_1                                    => 2#0001100#,
      Quadrature_Decode_2                                    => 2#0001101#,
      Windowed_Programmable_Time_Accumulation                => 2#0001110#,
      Modulus_Counter_Up_Internal                            => 2#0010000#,
      Modulus_Counter_Up_External                            => 2#0010001#,
      Modulus_Counter_Up_Down_No_Change_Internal             => 2#0010100#,
      Modulus_Counter_Up_Down_No_Change_External             => 2#0010101#,
      Modulus_Counter_Up_Down_Change_Internal                => 2#0010110#,
      Modulus_Counter_Up_Down_Change_External                => 2#0010111#,
      Output_Pulse_Width_and_Frequency_Modulation_1          => 2#0011000#,
      Output_Pulse_Width_and_Frequency_Modulation_2          => 2#0011001#,
      Output_Pulse_Width_and_Frequency_Modulation_3          => 2#0011010#,
      Output_Pulse_Width_and_Frequency_Modulation_4          => 2#0011011#,
      Centre_Aligned_PWM_1                                   => 2#0011100#,
      Centre_Aligned_PWM_2                                   => 2#0011101#,
      Centre_Aligned_PWM_3                                   => 2#0011110#,
      Centre_Aligned_PWM_4                                   => 2#0011111#,
      Output_PWM_1                                           => 2#0100000#,
      Output_PWM_2                                           => 2#0100001#,
      Output_PWM_3                                           => 2#0100010#,
      Output_PWM_4                                           => 2#0100011#,
      Modulus_Up_Counter_Buffered_Internal                   => 2#1010000#,
      Modulus_Up_Counter_Buffered_External                   => 2#1010001#,
      Modulus_Up_Down_Counter_Buffered_1                     => 2#1010100#,
      Modulus_Up_Down_Counter_Buffered_2                     => 2#1010101#,
      Modulus_Up_Down_Counter_Buffered_3                     => 2#1010110#,
      Modulus_Up_Down_Counter_Buffered_4                     => 2#1010111#,
      Output_Pulse_Width_and_Frequency_Modulation_Buffered_1 => 2#1011000#,
      Output_Pulse_Width_and_Frequency_Modulation_Buffered_2 => 2#1011010#,
      Centre_Aligned_PWM_Buffered_1                          => 2#1011100#,
      Centre_Aligned_PWM_Buffered_2                          => 2#1011101#,
      Centre_Aligned_PWM_Buffered_3                          => 2#1011110#,
      Centre_Aligned_PWM_Buffered_4                          => 2#1011111#,
      Output_PWM_Buffered_1                                  => 2#1100000#,
      Output_PWM_Buffered_2                                  => 2#1100010#);

   for CCR_Type use record
      Freeze                at 0 range 0 .. 0;
      Output_Disable        at 0 range 1 .. 1;
      Output_Disable_Select at 0 range 2 .. 3;
      Prescaler             at 0 range 4 .. 5;
      Prescaler_Enable      at 0 range 6 .. 6;
      DMA_Select            at 0 range 7 .. 7;
      Input_Filter          at 0 range 9 .. 12;
      Filter_Clock_Select   at 0 range 13 .. 13;
      FLAG_Enable           at 0 range 14 .. 14;
      Force_Match_A         at 0 range 18 .. 18;
      Force_Match_B         at 0 range 19 .. 19;
      Bus_Select            at 0 range 21 .. 22;
      Edge_Select           at 0 range 23 .. 23;
      Edge_Polarity         at 0 range 24 .. 24;
      Mode                  at 0 range 25 .. 31;
   end record;

   for CSR_Type use record
      Overrun                  at 0 range 0 .. 0;
      Overlow                  at 0 range 16 .. 16;
      Unified_State_Input_Pin  at 0 range 29 .. 29;
      Unified_State_Output_Pin at 0 range 30 .. 30;
      Flag                     at 0 range 31 .. 31;
   end record;

   for Unified_Channel_Type use record
      Channel_A_Data_Register  at 0 range 0 .. 31;
      Channel_B_Data_Register  at 4 range 0 .. 31;
      Channel_Counter_Register at 8 range 0 .. 31;
      Channel_Control_Register at 12 range 0 .. 31;
      Channel_Status_Register  at 16 range 0 .. 31;
   end record;

   for Unified_Channel_Type'Size use Unified_Channel_Type_Size;

   ----------------------------------------------------------------------------
   --  eMIOS Registers
   ----------------------------------------------------------------------------

   Module_Configuration_Register : Module_Configuration_Type;
   for Module_Configuration_Register'Address use
     To_Address (EMIOS_Base_Address);

   Unified_Channel_Register_Array :
     array (EMIOS_ID_Type) of aliased Unified_Channel_Type;
   for Unified_Channel_Register_Array'Address use
     To_Address (EMIOS_Base_Address + Unified_Channel_Offset_Address);
   type Unified_Channel_Pointer is access all Unified_Channel_Type;

end MPC5554.eMIOS;
