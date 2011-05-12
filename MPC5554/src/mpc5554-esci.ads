with System.Storage_Elements; use System.Storage_Elements;
with Interfaces;              use Interfaces;

package MPC5554.eSCI is

   ----------------------------------------------------------------------------
   -- Memory Addresses
   ----------------------------------------------------------------------------
   eSCIA_Base_Address : constant Integer_Address := 16#FFFB_0000#;
   eSCIB_Base_Address : constant Integer_Address := 16#FFFB_0000#;

   CR1_Offset_Address : constant Integer_Address := 16#0000#;
   CR2_Offset_Address : constant Integer_Address := 16#0004#;
   DR_Offset_Address  : constant Integer_Address := 16#0006#;
   SR_Offset_Address  : constant Integer_Address := 16#0008#;

   LCR_Offset_Address : constant Integer_Address := 16#000C#;
   LTR_Offset_Address : constant Integer_Address := 16#0010#;
   LRR_Offset_Address : constant Integer_Address := 16#0014#;
   LPR_Offset_Address : constant Integer_Address := 16#0018#;

   ----------------------------------------------------------------------------
   -- Hardware Features
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   -- eTPU Types
   ---------------------------------------------------------------------------

   -- Common Types

   -- eSCI Control Register 1 (ESCIx_CR1)
   type Baud_Rate_Type is range 1 .. 8191;
   type RSRC_Type is (Internal, External);
   type M_Type is (Eigth_Data_Bits, Nine_Data_Bits);
   type Wake_Type is (Idle_Line, Address_Mark);
   type ILT_Type is (After_Start_Bit, After_Stop_Bit);
   type Parity_T is (Even, Odd);
   type RWU_Type is (Normal, Wake_Up);
   type SBK_Type is (No_Break, Send_Break);

   type Control_1_Type is record
      Baud_Rate                       : Baud_Rate_Type;
      Loop_Select                     : Enable_Type;
      Receiver_Source                 : RSRC_Type;
      Data_Format_Mode                : M_Type;
      Wake_Up_Condition               : Wake_Type;
      Idle_Line                       : ILT_Type;
      Parity                          : Enable_Type;
      Parity_Type                     : Parity_T;
      Transmitter_Interrupt           : Enable_Type;
      Transmission_Complete_Interrupt : Enable_Type;
      Receiver_Full_Interrupt         : Enable_Type;
      Idle_Line_Interrupt             : Enable_Type;
      Transmitter                     : Enable_Type;
      Receiver                        : Enable_Type;
      Receiver_Wake_Up                : RWU_Type;
      Send_Break                      : SBK_Type;
   end record;

   -- eSCI Control Register 2 (ESCIx_CR2)
   type BRK13_Type is (Break_10_Bits, Break_13_Bits);
   type BESM13_Type is (At_Clock_9, At_Clock_13);
   type SBSTP_Type is (
      Byte_Completely_Transmitted,
      Byte_Partially_Transmitted);

   type Control_2_Type is record
      Module_Disable                  : Disable_Type;
      Fast_Bit_Error_Detection        : Enable_Type;
      Bit_Bus_Error_Stop              : Enable_Type;
      Bit_Error_Interrupt             : Enable_Type;
      RX_DMA_Channel                  : Enable_Type;
      TX_DMA_Channel                  : Enable_Type;
      Break_Transmit_Character_Length : BRK13_Type;
      Bit_Error_Sample_Mode           : BESM13_Type;
      Bit_Error_Stop                  : SBSTP_Type;
      Overrunn_Error_Interrupt        : Enable_Type;
      Noise_Flag_Interrupt            : Enable_Type;
      Frmae_Error_Interrupt           : Enable_Type;
      Parity_Flag_Interrupt           : Enable_Type;
   end record;

   -- eSCI Data Register (ESCIx_DR)
   type Data_Type is record
      Upper_RX : Boolean;
      Upper_TX : Boolean;
      Lower    : Character;
   end record;

   -- eSCI Status Register (ESCIx_SR)
   type Flag is (Low, High);

   type Status_Type is record
      Transmit_Data_Register_Empty_Flag   : Flag;
      Transmit_Complete_Flag              : Flag;
      Receive_Data_Register_Full_Flag     : Flag;
      Idle_Line_Flag                      : Flag;
      Overrun_Flag                        : Flag;
      Noise_Flag                          : Flag;
      Framing_Error_Flag                  : Flag;
      Parity_Error_Flag                   : Flag;
      Bit_Error_Flag                      : Flag;
      Receiver_Active_Flag                : Flag;
      Recieved_LIN_Data_Flag              : Flag;
      LIN_FSM_Ready_Flag                  : Flag;
      LIN_Wake_UP_Flag                    : Flag;
      Slave_Time_Out_Flag                 : Flag;
      Physcial_Bus_Error_Flag             : Flag;
      CRC_Error_Flag                      : Flag;
      Checksum_Error_Flag                 : Flag;
      Frame_Complete_Flag                 : Flag;
      LIN_Received_Register_Overflow_Flag : Flag;
   end record;

   ----------------------------------------------------------------------------
   -- Hardware Respresentations
   ----------------------------------------------------------------------------
   for RSRC_Type use (Internal => 0, External => 1);
   for M_Type use (Eigth_Data_Bits => 0, Nine_Data_Bits => 1);
   for Wake_Type use (Idle_Line => 0, Address_Mark => 1);
   for ILT_Type use (After_Start_Bit => 0, After_Stop_Bit => 1);
   for Parity_T use (Even => 0, Odd => 1);
   for RWU_Type use (Normal => 0, Wake_Up => 1);
   for SBK_Type use (No_Break => 0, Send_Break => 1);

   for Control_1_Type use record
      Baud_Rate                       at 0 range 3 .. 15;
      Loop_Select                     at 0 range 16 .. 16;
      Receiver_Source                 at 0 range 18 .. 18;
      Data_Format_Mode                at 0 range 19 .. 19;
      Wake_Up_Condition               at 0 range 20 .. 20;
      Idle_Line                       at 0 range 21 .. 21;
      Parity                          at 0 range 22 .. 22;
      Parity_Type                     at 0 range 23 .. 23;
      Transmitter_Interrupt           at 0 range 24 .. 24;
      Transmission_Complete_Interrupt at 0 range 25 .. 25;
      Receiver_Full_Interrupt         at 0 range 26 .. 26;
      Idle_Line_Interrupt             at 0 range 27 .. 27;
      Transmitter                     at 0 range 28 .. 28;
      Receiver                        at 0 range 29 .. 29;
      Receiver_Wake_Up                at 0 range 30 .. 30;
      Send_Break                      at 0 range 31 .. 31;
   end record;

   for BRK13_Type use (Break_10_Bits => 0, Break_13_Bits => 1);
   for BESM13_Type use (At_Clock_9 => 0, At_Clock_13 => 1);
   for SBSTP_Type use
     (Byte_Completely_Transmitted => 0,
      Byte_Partially_Transmitted  => 1);

   for Control_2_Type use record
      Module_Disable                  at 0 range 0 .. 0;
      Fast_Bit_Error_Detection        at 0 range 1 .. 1;
      Bit_Bus_Error_Stop              at 0 range 2 .. 2;
      Bit_Error_Interrupt             at 0 range 3 .. 3;
      RX_DMA_Channel                  at 0 range 4 .. 4;
      TX_DMA_Channel                  at 0 range 5 .. 5;
      Break_Transmit_Character_Length at 0 range 6 .. 6;
      Bit_Error_Sample_Mode           at 0 range 8 .. 8;
      Bit_Error_Stop                  at 0 range 9 .. 9;
      Overrunn_Error_Interrupt        at 0 range 12 .. 12;
      Noise_Flag_Interrupt            at 0 range 13 .. 13;
      Frmae_Error_Interrupt           at 0 range 14 .. 14;
      Parity_Flag_Interrupt           at 0 range 15 .. 15;
   end record;

   for Data_Type use record
      Upper_RX at 0 range 0 .. 0;
      Upper_TX at 0 range 1 .. 1;
      Lower    at 0 range 8 .. 15;
   end record;

   for Flag use (Low => 0, High => 1);

   for Status_Type use record
      Transmit_Data_Register_Empty_Flag   at 0 range 0 .. 0;
      Transmit_Complete_Flag              at 0 range 1 .. 1;
      Receive_Data_Register_Full_Flag     at 0 range 2 .. 2;
      Idle_Line_Flag                      at 0 range 3 .. 3;
      Overrun_Flag                        at 0 range 4 .. 4;
      Noise_Flag                          at 0 range 5 .. 5;
      Framing_Error_Flag                  at 0 range 6 .. 6;
      Parity_Error_Flag                   at 0 range 7 .. 7;
      Bit_Error_Flag                      at 0 range 11 .. 11;
      Receiver_Active_Flag                at 0 range 15 .. 15;
      Recieved_LIN_Data_Flag              at 0 range 16 .. 16;
      LIN_FSM_Ready_Flag                  at 0 range 17 .. 17;
      LIN_Wake_UP_Flag                    at 0 range 18 .. 18;
      Slave_Time_Out_Flag                 at 0 range 19 .. 19;
      Physcial_Bus_Error_Flag             at 0 range 20 .. 20;
      CRC_Error_Flag                      at 0 range 21 .. 21;
      Checksum_Error_Flag                 at 0 range 22 .. 22;
      Frame_Complete_Flag                 at 0 range 23 .. 23;
      LIN_Received_Register_Overflow_Flag at 0 range 31 .. 31;
   end record;
   ----------------------------------------------------------------------------
   -- eTPU Registers
   ----------------------------------------------------------------------------

   A_Control_Register_1 : Control_1_Type;
   for A_Control_Register_1'Address use
     To_Address (eSCIA_Base_Address + CR1_Offset_Address);

   A_Control_Register_2 : Control_2_Type;
   for A_Control_Register_2'Address use
     To_Address (eSCIA_Base_Address + CR2_Offset_Address);

   A_Data_Register : Data_Type;
   for A_Data_Register'Address use
     To_Address (eSCIA_Base_Address + DR_Offset_Address);

   A_Status_Register : Status_Type;
   for A_Status_Register'Address use
     To_Address (eSCIA_Base_Address + SR_Offset_Address);

   B_Control_Register_1 : Control_1_Type;
   for B_Control_Register_1'Address use
     To_Address (eSCIB_Base_Address + CR1_Offset_Address);

   B_Control_Register_2 : Control_2_Type;
   for B_Control_Register_2'Address use
     To_Address (eSCIB_Base_Address + CR2_Offset_Address);

   B_Data_Register : Data_Type;
   for B_Data_Register'Address use
     To_Address (eSCIB_Base_Address + DR_Offset_Address);

   B_Status_Register : Status_Type;
   for B_Status_Register'Address use
     To_Address (eSCIB_Base_Address + SR_Offset_Address);
end MPC5554.eSCI;
