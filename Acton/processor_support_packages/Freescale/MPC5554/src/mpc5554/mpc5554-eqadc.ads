------------------------------------------------------------------------------------------
--                                                                                      --
--                            OAK PROCESSOR SUPPORT PACKAGE                             --
--                                  FREESCALE MPC5544                                   --
--                                                                                      --
--                                    MPC5554.EQADC                                     --
--                                                                                      --
--                       Copyright (C) 2010-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

with System;
with System.Storage_Elements; use System.Storage_Elements;

package MPC5554.eQADC with Preelaborate is

   ----------------------------------------------------------------------------
   --  Memory Addresses
   ----------------------------------------------------------------------------
   eQADC_Base_Address    : constant Integer_Address := 16#FFF8_0000#;
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
   --  Hardware Features
   ----------------------------------------------------------------------------
   type FIFO_ID_Register is mod 2 ** 4;
   Number_Of_FIFOs : constant FIFO_ID_Register := 6;
   subtype FIFO_ID is FIFO_ID_Register range 0 .. Number_Of_FIFOs;

   Number_Of_ADCs : constant Integer := 3;
   type ADC_ID is mod Number_Of_ADCs;

   ----------------------------------------------------------------------------
   --  eQADC Types
   ---------------------------------------------------------------------------

   --  Common Types
   type External_Device_Message is mod 2 ** 26;
   type eQADC_Data is mod 2 ** 32;
   type eQADC_Command is mod 2 ** 32;
   type eQADC_Counter is mod 2 ** 4;
   type eQADC_Pointer is mod 2 ** 4;

   type CFIFO_Status is (Idle, Waiting, Triggered);

   type Select_Type is (Interrupt, eDMA);
   for Select_Type use (Interrupt => 0, eDMA => 1);
   --  eQADC Module Configuration Register (EQADC_MCR)
   type eQADC_Enable_Type is (
      SSI_Disable,
      SSI_Enable_Trans_Disable,
      SSI_Enable_Trans_Enable);
   type Debug_Type is (Do_Not_Enter, Enter_FCK_Stop, Enter_FCK_Run);

   type Module_Configuration_Type is record
      Synchronous_Serial_Interface : eQADC_Enable_Type;
      Debug                        : Debug_Type;
   end record;

   --  eQADC External Trigger Digital Filter Register (EQADC_ETDFR)
   type External_Trigger_Digital_Filter_Type is mod 2 ** 4;

   --  eQADC CFIFO Control Registers 0-5 (EQADC_CFCRn)
   type Invalidate_Type is (Do_Nothing, Invalidate);
   type Operation_Mode_Type is (
      Disable,
      Software_Trigger_Single_Scan,
      Low_Level_Gated_External_Trigger_Single_Scan,
      High_Level_Gated_External_Trigger_Single_Scan,
      Falling_Edge_External_Trigger_Single_Scan,
      Rising_Edge_External_Trigger_Single_Scan,
      Edge_External_Trigger_Single_Scan,
      Software_Trigger_Continuous_Scan,
      Low_Level_Gated_External_Trigger_Continuous_Scan,
      High_Level_Gated_External_Trigger_Continuous_Scan,
      Falling_Edge_External_Trigger_Continuous_Scan,
      Rising_Edge_External_Trigger_Continuous_Scan,
      Edge_External_Trigger_Continuous_Scan);

   type CFIFO_Control_Type is record
      Single_Scan    : Enable_Type;
      Invalidate     : Invalidate_Type;
      Operation_Mode : Operation_Mode_Type;
   end record;

   --  eQADC Interrupt and eDMA Control Registers 0-5 (EQADC_IDCRn)
   type Interrupt_eDMA_Control_Type is record
      Non_Coherency_Interrupt   : Enable_Type;
      Trigger_Overrun_Interrupt : Enable_Type;
      Paused_Interrupt          : Enable_Type;
      End_Of_Queue_Interrupt    : Enable_Type;
      CFIFO_Underflow_Interrupt : Enable_Type;
      CFIFO_Fill                : Enable_Type;
      CFIFO_Fill_Select         : Select_Type;
      RFIFO_Overflow_Interrupt  : Enable_Type;
      RFIFO_Drain               : Enable_Type;
      RFIFO_Drain_Select        : Select_Type;
   end record;

   --  eQADC FIFO and Interrupt Status Registers 0-5 (EQADC_IDCRn)
   type FIFO_Interrupt_Status_Type is record
      Non_Coherency_Flag           : Occurred_Type;
      Trigger_Overrun_Flag         : Occurred_Type;
      Pause_Flag                   : Occurred_Type;
      End_Of_Queue_Flag            : Occurred_Type;
      CFIFO_Underflow_Flag         : Occurred_Type;
      CFIFO_Single_Scan_Status_Bit : Enable_Type;
      CFIFO_Fill_Flag              : Occurred_Type;
      RFIFO_Overflow_Flag          : Occurred_Type;
      RFIFO_Drain_Flag             : Occurred_Type;
      CFIFO_Entry_Counter          : eQADC_Counter;
      CFIFO_Transfer_Next_Pointer  : eQADC_Pointer;
      RFIFO_Entry_Counter          : eQADC_Counter;
      RFIFO_Pop_Next_Pointer       : eQADC_Pointer;
   end record;

   type Transfer_Counter is mod 2 ** 11;

   --  eQADC CFIFO Status Snapshot Registers 0-2 (EQADC_CFSSRn)
   No_Command : constant FIFO_ID_Register := 2#1111#;

   type CFIFO_Status_Snapshot_Type is record
      CFIFO_0_Status                           : CFIFO_Status;
      CFIFO_1_Status                           : CFIFO_Status;
      CFIFO_2_Status                           : CFIFO_Status;
      CFIFO_3_Status                           : CFIFO_Status;
      CFIFO_4_Status                           : CFIFO_Status;
      CFIFO_5_Status                           : CFIFO_Status;
      Last_CFIFO_To_Transfer                   : FIFO_ID_Register;
      Transfer_Counter_For_Last_CFIFO_Transfer : eQADC_Counter;
   end record;

   --  eQADC CFIFO_Status_Register (EQADC_CFSR)
   type CFIFO_Status_Type is record
      CFIFO_0_Status : CFIFO_Status;
      CFIFO_1_Status : CFIFO_Status;
      CFIFO_2_Status : CFIFO_Status;
      CFIFO_3_Status : CFIFO_Status;
      CFIFO_4_Status : CFIFO_Status;
      CFIFO_5_Status : CFIFO_Status;
   end record;

   --  eQADC SSI Control Register (EQADC_SSICR)
   type Delay_Type is mod 2 ** 3;
   type Buad_Type is mod 2 ** 4;

   type SSI_Control_Type is record
      Min_Delay_After_Transmission : Delay_Type;
      Buad_Rate                    : Buad_Type;
   end record;

   ----------------------------------------------------------------------------
   --  Hardware Respresentations
   ----------------------------------------------------------------------------

   for eQADC_Enable_Type use
     (SSI_Disable              => 2#00#,
      SSI_Enable_Trans_Disable => 2#10#,
      SSI_Enable_Trans_Enable  => 2#11#);
   for Debug_Type use
     (Do_Not_Enter   => 2#00#,
      Enter_FCK_Stop => 2#10#,
      Enter_FCK_Run  => 2#11#);

   for CFIFO_Status use (Idle => 2#00#, Waiting => 2#10#, Triggered => 2#11#);

   for Module_Configuration_Type use record
      Synchronous_Serial_Interface at 0 range 27 .. 28;
      Debug                        at 0 range 30 .. 31;
   end record;

   for Invalidate_Type use (Do_Nothing => 0, Invalidate => 1);
   for Operation_Mode_Type use
     (Disable                                           => 2#0000#,
      Software_Trigger_Single_Scan                      => 2#0001#,
      Low_Level_Gated_External_Trigger_Single_Scan      => 2#0010#,
      High_Level_Gated_External_Trigger_Single_Scan     => 2#0011#,
      Falling_Edge_External_Trigger_Single_Scan         => 2#0100#,
      Rising_Edge_External_Trigger_Single_Scan          => 2#0101#,
      Edge_External_Trigger_Single_Scan                 => 2#0110#,
      Software_Trigger_Continuous_Scan                  => 2#1001#,
      Low_Level_Gated_External_Trigger_Continuous_Scan  => 2#1010#,
      High_Level_Gated_External_Trigger_Continuous_Scan => 2#1011#,
      Falling_Edge_External_Trigger_Continuous_Scan     => 2#1100#,
      Rising_Edge_External_Trigger_Continuous_Scan      => 2#1101#,
      Edge_External_Trigger_Continuous_Scan             => 2#1110#);

   for CFIFO_Control_Type use record
      Single_Scan    at 0 range 5 .. 5;
      Invalidate     at 0 range 6 .. 6;
      Operation_Mode at 0 range 8 .. 11;
   end record;

   for Interrupt_eDMA_Control_Type use record
      Non_Coherency_Interrupt   at 0 range 0 .. 0;
      Trigger_Overrun_Interrupt at 0 range 1 .. 1;
      Paused_Interrupt          at 0 range 2 .. 2;
      End_Of_Queue_Interrupt    at 0 range 3 .. 3;
      CFIFO_Underflow_Interrupt at 0 range 4 .. 4;
      CFIFO_Fill                at 0 range 6 .. 6;
      CFIFO_Fill_Select         at 0 range 7 .. 7;
      RFIFO_Overflow_Interrupt  at 0 range 12 .. 12;
      RFIFO_Drain               at 0 range 14 .. 14;
      RFIFO_Drain_Select        at 0 range 15 .. 15;
   end record;

   for FIFO_Interrupt_Status_Type use record
      Non_Coherency_Flag           at 0 range 0 .. 0;
      Trigger_Overrun_Flag         at 0 range 1 .. 1;
      Pause_Flag                   at 0 range 2 .. 2;
      End_Of_Queue_Flag            at 0 range 3 .. 3;
      CFIFO_Underflow_Flag         at 0 range 4 .. 4;
      CFIFO_Single_Scan_Status_Bit at 0 range 5 .. 5;
      CFIFO_Fill_Flag              at 0 range 6 .. 6;
      RFIFO_Overflow_Flag          at 0 range 12 .. 12;
      RFIFO_Drain_Flag             at 0 range 14 .. 14;
      CFIFO_Entry_Counter          at 0 range 16 .. 19;
      CFIFO_Transfer_Next_Pointer  at 0 range 20 .. 23;
      RFIFO_Entry_Counter          at 0 range 24 .. 27;
      RFIFO_Pop_Next_Pointer       at 0 range 28 .. 31;
   end record;

   for CFIFO_Status_Snapshot_Type use record
      CFIFO_0_Status                           at 0 range 0 .. 1;
      CFIFO_1_Status                           at 0 range 2 .. 3;
      CFIFO_2_Status                           at 0 range 4 .. 5;
      CFIFO_3_Status                           at 0 range 6 .. 7;
      CFIFO_4_Status                           at 0 range 8 .. 9;
      CFIFO_5_Status                           at 0 range 10 .. 11;
      Last_CFIFO_To_Transfer                   at 0 range 17 .. 20;
      Transfer_Counter_For_Last_CFIFO_Transfer at 0 range 21 .. 31;
   end record;

   for CFIFO_Status_Type use record
      CFIFO_0_Status at 0 range 0 .. 1;
      CFIFO_1_Status at 0 range 2 .. 3;
      CFIFO_2_Status at 0 range 4 .. 5;
      CFIFO_3_Status at 0 range 6 .. 7;
      CFIFO_4_Status at 0 range 8 .. 9;
      CFIFO_5_Status at 0 range 10 .. 11;
   end record;

   for SSI_Control_Type use record
      Min_Delay_After_Transmission at 0 range 21 .. 23;
      Buad_Rate                    at 0 range 28 .. 31;
   end record;

   ----------------------------------------------------------------------------
   --  eQADC Registers
   ----------------------------------------------------------------------------

   pragma Warnings (Off, "*alignment*");

   Module_Control_Register : Module_Configuration_Type;
   for Module_Control_Register'Address use
     System'To_Address (eQADC_Base_Address + MCR_Offset_Address);

   Null_Message_Send_Format_Register : External_Device_Message;
   for Null_Message_Send_Format_Register'Address use
     System'To_Address (eQADC_Base_Address + NMSFR_Offset_Address);

   External_Trigger_Digital_Filter_Register :
     External_Trigger_Digital_Filter_Type;
   for External_Trigger_Digital_Filter_Register'Address use
     System'To_Address (eQADC_Base_Address + ETDFR_Offset_Address);

   Command_FIFO_Push_Registers : array (FIFO_ID) of aliased eQADC_Command;
   for Command_FIFO_Push_Registers'Address use
     System'To_Address (eQADC_Base_Address + CFPR_Offset_Address);

   Result_FIFO_Push_Registers : array (FIFO_ID) of aliased eQADC_Data;
   for Result_FIFO_Push_Registers'Address use
     System'To_Address (eQADC_Base_Address + RFPR_Offset_Address);

   Command_FIFO_Control_Registers :
     array (FIFO_ID) of aliased CFIFO_Control_Type;
   for Command_FIFO_Control_Registers'Address use
     System'To_Address (eQADC_Base_Address + CFCR_Offset_Address);

   Interrupt_eDMA_Control_Registers :
     array (FIFO_ID) of aliased Interrupt_eDMA_Control_Type;
   for Interrupt_eDMA_Control_Registers'Address use
     System'To_Address (eQADC_Base_Address + IDCR_Offset_Address);

   FIFO_Interrupt_Status_Registers :
     array (FIFO_ID) of aliased FIFO_Interrupt_Status_Type;
   for FIFO_Interrupt_Status_Registers'Address use
     System'To_Address (eQADC_Base_Address + FISR_Offset_Address);

   CFIFO_Transfer_Counter_Registers :
     array (FIFO_ID) of aliased Transfer_Counter;
   for CFIFO_Transfer_Counter_Registers'Address use
     System'To_Address (eQADC_Base_Address + CFTCR_Offset_Address);

   CFIFO_Status_Snapshot_Registers :
     array (FIFO_ID) of aliased CFIFO_Status_Snapshot_Type;
   for CFIFO_Status_Snapshot_Registers'Address use
     System'To_Address (eQADC_Base_Address + CFSSR_Offset_Address);

   CFIFO_Status_Register : CFIFO_Status_Type;
   for CFIFO_Status_Register'Address use
     System'To_Address (eQADC_Base_Address + CFSR_Offset_Address);

   SSI_Control_Register : SSI_Control_Type;
   for SSI_Control_Register'Address use
     System'To_Address (eQADC_Base_Address + SSICR_Offset_Address);

   SSI_Receive_Data_Register : External_Device_Message;
   for SSI_Receive_Data_Register'Address use
     System'To_Address (eQADC_Base_Address + SSIRDR_Offset_Address);

end MPC5554.eQADC;
