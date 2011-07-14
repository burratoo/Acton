with System;
with System.Storage_Elements; use System.Storage_Elements;

package MPC5554.DSPI is

   ----------------------------------------------------------------------------
   --  Memory Addresses
   ----------------------------------------------------------------------------
   DSPI_Base_Address    : constant Integer_Address := 16#FFF9_0000#;
   MCR_Offset_Address   : constant Integer_Address := 16#0000#;
   TCR_Offset_Address   : constant Integer_Address := 16#0004#;
   CTAR_Offset_Address  : constant Integer_Address := 16#0008#;
   SR_Offset_Address    : constant Integer_Address := 16#002C#;
   RSER_Offset_Address  : constant Integer_Address := 16#0030#;
   PUSHR_Offset_Address : constant Integer_Address := 16#0034#;
   POPR_Offset_Address  : constant Integer_Address := 16#0038#;

   ----------------------------------------------------------------------------
   --  Hardware Features
   ----------------------------------------------------------------------------
   type DSPI_Module_ID is (A, B, C, D);
   type Slave_ID is mod 5;
   type CTAR_ID is mod 8;
   type SPI_Data is mod 2 ** 16;

   ----------------------------------------------------------------------------
   --  DSPI Types
   ----------------------------------------------------------------------------

   --  Common Types
   type Select_Type is (Interrupt, eDMA);
   for Select_Type use (Interrupt => 0, eDMA => 1);

   --  DSPI Module Configuration Register (DSPIx_MCR)
   type MS_Type is (Slave, Master);
   type Configuration_Type is (SPI, DSI, CSI);
   type Sample_Type is mod 2;
   type PCSIS_Type is array (Slave_ID) of Pin_State_Type;

   type Module_Configuration_Type is record
      Master_Slave_Select                   : MS_Type;
      Continuous_Serial_Clock               : Enable_Type;
      DSP_Configuration                     : Configuration_Type;
      Freeze                                : Enable_Type;
      Modified_Timing_Format                : Enable_Type;
      Peripheral_Chip_Select_Strobe         : Enable_Type;
      Recieve_FIFO_Overflow_Overwrite       : Enable_Type;
      Peripheral_Chip_Select_Inactive_State : PCSIS_Type;
      Module                                : Disable_Type;
      Transmit_FIFO                         : Disable_Type;
      Reciever_FIFO                         : Disable_Type;
      Clear_Tx_FIFO                         : Yes_No_Type;
      Clear_Rx_FIFO                         : Yes_No_Type;
      Sample_Point                          : Sample_Type;
      Halt                                  : Yes_No_Type;
   end record;

   --  DSPI Transfer Count Register (DSPIx_TCR)
   type Transfer_Counter_Type is mod 2 ** 16;

   type Transfer_Count_Type is record
      SPI_Transfer_Counter : Transfer_Counter_Type;
   end record;

   --  DSPI Clock and Transfer Attributes Registers 0-7 (DSPIx_CTAREn)
   type Frame_Size_Type is mod 2 ** 4;
   type Clock_Phase_Type is (Positive, Negative);
   type Delay_Prescaler_Type is (Delay_1, Delay_3, Delay_5, Delay_7);
   type Baud_Prescaler_Type is (Scale_2, Scale_3, Scale_5, Scale_7);
   type Scaler_Type is mod 2 ** 4;

   for Clock_Phase_Type use (Positive => 0, Negative => 1);
   for Delay_Prescaler_Type use
     (Delay_1 => 0,
      Delay_3 => 1,
      Delay_5 => 2,
      Delay_7 => 3);
   for Baud_Prescaler_Type use
     (Scale_2 => 0,
      Scale_3 => 1,
      Scale_5 => 2,
      Scale_7 => 3);

   type Clock_Transfer_Attributes_Type is record
      Double_Baud_Rate               : Enable_Type;
      Frame_Size                     : Frame_Size_Type;
      Clock_Polarity                 : Pin_State_Type;
      Clock_Phase                    : Clock_Phase_Type;
      LSB_First                      : Enable_Type;
      PCS_To_SCK_Delay_Prescaler     : Delay_Prescaler_Type;
      After_SCK_Delay_Prescaler      : Delay_Prescaler_Type;
      Delay_After_Transfer_Prescaler : Delay_Prescaler_Type;
      Baud_Rate_Prescaler            : Baud_Prescaler_Type;
      PCS_To_SCK_Delay_Scaler        : Scaler_Type;
      After_SCK_Delay_Scaler         : Scaler_Type;
      Delay_After_Transfer_Scaler    : Scaler_Type;
      Baud_Rate_Scaler               : Scaler_Type;
   end record;

   --  DSPI Status Register (DSPIx_SR)
   type FIFO_Counter is mod 2 ** 4;
   type FIFO_Pointer is mod 2 ** 4;

   type Status_Type is record
      Transfer_Complete_Flag       : Occurred_Type;
      Tx_Rx_Status                 : Enable_Type;
      End_Of_Queue_Flag            : Occurred_Type;
      Transmit_FIFO_Underflow_Flag : Occurred_Type;
      Trasmit_FIFO_Fill_Flag       : Occurred_Type;
      Receive_FIFO_Overflow_Flag   : Occurred_Type;
      Receive_FIFO_Drain_Flag      : Occurred_Type;
      Tx_FIFO_Counter              : FIFO_Counter;
      Transmit_Next_Pointer        : FIFO_Pointer;
      Rx_FIFO_Counter              : FIFO_Counter;
      Pop_Next_Pointer             : FIFO_Pointer;
   end record;

   --  DSPI DMA/Interrupt Request Select and Enable Register (DSPIx_RSER)
   type DMA_Interrupt_Request_Type is record
      Transmission_Complete_Request   : Enable_Type;
      Finish_Request                  : Enable_Type;
      Transmit_FIFO_Underflow_Request : Enable_Type;
      Transmit_FIFO_Fill_Request      : Enable_Type;
      Transmit_FIFO_Fill_Select       : Select_Type;
      Recieve_FIFO_Overflow_Request   : Enable_Type;
      Recieve_FIFO_Drain_Request      : Enable_Type;
      Recieve_FIFO_Drain_Select       : Select_Type;
   end record;

   --  DSPI PUSH TX FIFO Register (DSPIx_PUSHR)
   type Assert_Type is (Negate, Assert);

   type PCS_Type is array (Slave_ID) of Pin_State_Type;

   type Push_Tx_FIFO_Type is record
      Continuous_Peripheral_Chip_Select    : Enable_Type;
      Clock_And_Transfer_Attributes_Select : CTAR_ID;
      End_Of_Queue                         : Yes_No_Type;
      Clear_Transfer_Counter               : Yes_No_Type;
      Peripheral_Chip_Select               : PCS_Type;
      Tx_Data                              : SPI_Data;
   end record;

   --  DSPI POP RX FIFO Register (DSPI_POPR)
   type Pop_Rx_FIFO_Type is record
      Rx_Data : SPI_Data;
   end record;

   ----------------------------------------------------------------------------
   --  Hardware Respresentations
   ----------------------------------------------------------------------------
   pragma Pack (PCSIS_Type);
   for MS_Type use (Slave => 0, Master => 1);
   for Configuration_Type use (SPI => 0, DSI => 1, CSI => 2);

   for Module_Configuration_Type use record
      Master_Slave_Select                   at 0 range 0 .. 0;
      Continuous_Serial_Clock               at 0 range 1 .. 1;
      DSP_Configuration                     at 0 range 2 .. 3;
      Freeze                                at 0 range 4 .. 4;
      Modified_Timing_Format                at 0 range 5 .. 5;
      Peripheral_Chip_Select_Strobe         at 0 range 6 .. 6;
      Recieve_FIFO_Overflow_Overwrite       at 0 range 7 .. 7;
      Peripheral_Chip_Select_Inactive_State at 0 range 10 .. 15;
      Module                                at 0 range 17 .. 17;
      Transmit_FIFO                         at 0 range 18 .. 18;
      Reciever_FIFO                         at 0 range 19 .. 19;
      Clear_Tx_FIFO                         at 0 range 20 .. 20;
      Clear_Rx_FIFO                         at 0 range 21 .. 21;
      Sample_Point                          at 0 range 22 .. 23;
      Halt                                  at 0 range 31 .. 31;
   end record;

   for Transfer_Count_Type use record
      SPI_Transfer_Counter at 0 range 0 .. 16;
   end record;
   for Transfer_Counter_Type'Size use 32;

   for Clock_Transfer_Attributes_Type use record
      Double_Baud_Rate               at 0 range 0 .. 0;
      Frame_Size                     at 0 range 1 .. 4;
      Clock_Polarity                 at 0 range 5 .. 5;
      Clock_Phase                    at 0 range 6 .. 6;
      LSB_First                      at 0 range 7 .. 7;
      PCS_To_SCK_Delay_Prescaler     at 0 range 8 .. 9;
      After_SCK_Delay_Prescaler      at 0 range 10 .. 11;
      Delay_After_Transfer_Prescaler at 0 range 12 .. 13;
      Baud_Rate_Prescaler            at 0 range 14 .. 15;
      PCS_To_SCK_Delay_Scaler        at 0 range 16 .. 19;
      After_SCK_Delay_Scaler         at 0 range 20 .. 23;
      Delay_After_Transfer_Scaler    at 0 range 24 .. 27;
      Baud_Rate_Scaler               at 0 range 28 .. 31;
   end record;

   for Status_Type use record
      Transfer_Complete_Flag       at 0 range 0 .. 0;
      Tx_Rx_Status                 at 0 range 1 .. 1;
      End_Of_Queue_Flag            at 0 range 3 .. 3;
      Transmit_FIFO_Underflow_Flag at 0 range 4 .. 4;
      Trasmit_FIFO_Fill_Flag       at 0 range 6 .. 6;
      Receive_FIFO_Overflow_Flag   at 0 range 12 .. 12;
      Receive_FIFO_Drain_Flag      at 0 range 14 .. 14;
      Tx_FIFO_Counter              at 0 range 16 .. 19;
      Transmit_Next_Pointer        at 0 range 20 .. 23;
      Rx_FIFO_Counter              at 0 range 24 .. 27;
      Pop_Next_Pointer             at 0 range 28 .. 31;
   end record;

   for DMA_Interrupt_Request_Type use record
      Transmission_Complete_Request   at 0 range 0 .. 0;
      Finish_Request                  at 0 range 3 .. 3;
      Transmit_FIFO_Underflow_Request at 0 range 4 .. 4;
      Transmit_FIFO_Fill_Request      at 0 range 6 .. 6;
      Transmit_FIFO_Fill_Select       at 0 range 7 .. 7;
      Recieve_FIFO_Overflow_Request   at 0 range 12 .. 12;
      Recieve_FIFO_Drain_Request      at 0 range 14 .. 14;
      Recieve_FIFO_Drain_Select       at 0 range 15 .. 15;
   end record;

   for Assert_Type use (Negate => 0, Assert => 1);
   pragma Pack (PCS_Type);

   for Push_Tx_FIFO_Type use record
      Continuous_Peripheral_Chip_Select    at 0 range 0 .. 0;
      Clock_And_Transfer_Attributes_Select at 0 range 1 .. 3;
      End_Of_Queue                         at 0 range 4 .. 4;
      Clear_Transfer_Counter               at 0 range 5 .. 5;
      Peripheral_Chip_Select               at 0 range 10 .. 15;
      Tx_Data                              at 0 range 16 .. 31;
   end record;

   for Pop_Rx_FIFO_Type use record
      Rx_Data at 0 range 16 .. 31;
   end record;

   ----------------------------------------------------------------------------
   --  DSPI Registers
   ----------------------------------------------------------------------------

   --  DSPI Module Record
   type Clock_Transfer_Attributes_Array is
     array (CTAR_ID) of Clock_Transfer_Attributes_Type;
   for Clock_Transfer_Attributes_Array'Component_Size use 32;

   type DSPI_Module_Type is record
      Module_Configuration_Register          : Module_Configuration_Type;
      Transfer_Count_Register                : Transfer_Counter_Type;
      Clock_And_Transfer_Attributes_Register : Clock_Transfer_Attributes_Array;
      Status_Register                        : Status_Type;
      DMA_Interrupt_Request_Register         : DMA_Interrupt_Request_Type;
      Push_Tx_FIFO_Register                  : Push_Tx_FIFO_Type;
      Pop_Rx_FIFO_Register                   : Pop_Rx_FIFO_Type;
   end record;

   for DSPI_Module_Type use record
      Module_Configuration_Register          at MCR_Offset_Address range
         0 .. 31;
      Transfer_Count_Register                at TCR_Offset_Address range
         0 .. 31;
      Clock_And_Transfer_Attributes_Register at CTAR_Offset_Address range
         0 .. (Integer (CTAR_ID'Last) + 1) * 32 - 1;
      Status_Register                        at SR_Offset_Address range
         0 .. 31;
      DMA_Interrupt_Request_Register         at RSER_Offset_Address range
         0 .. 31;
      Push_Tx_FIFO_Register                  at PUSHR_Offset_Address range
         0 .. 31;
      Pop_Rx_FIFO_Register                   at POPR_Offset_Address range
         0 .. 31;
   end record;

   type Module_Type is array (DSPI_Module_ID) of DSPI_Module_Type;
   for Module_Type'Component_Size use 4000;
   Module : Module_Type;
   for Module'Address use To_Address (DSPI_Base_Address);

end MPC5554.DSPI;
