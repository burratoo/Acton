with Interfaces;              use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;

package MPC5554.eTPU with Preelaborate is

   ----------------------------------------------------------------------------
   --  Memory Addresses
   ----------------------------------------------------------------------------
   eTPU_Base_Address   : constant Integer_Address := 16#C3FC_0000#;
   MCR_Offset_Address  : constant Integer_Address := 16#0000#;
   CDCR_Offset_Address : constant Integer_Address := 16#0004#;

   MISCCMPR_Offset_Address    : constant Integer_Address := 16#000C#;
   SCMOFFDATAR_Offset_Address : constant Integer_Address := 16#0010#;
   ECR_A_Offset_Address       : constant Integer_Address := 16#0014#;
   ECR_B_Offset_Address       : constant Integer_Address := 16#0018#;

   TBCR_A_Offset_Address  : constant Integer_Address := 16#0020#;
   TB1R_A_Offset_Address  : constant Integer_Address := 16#0024#;
   TB2R_A_Offset_Address  : constant Integer_Address := 16#0028#;
   REDCR_A_Offset_Address : constant Integer_Address := 16#002C#;

   TBCR_B_Offset_Address  : constant Integer_Address := 16#0040#;
   TB1R_B_Offset_Address  : constant Integer_Address := 16#0044#;
   TB2R_B_Offset_Address  : constant Integer_Address := 16#0048#;
   REDCR_B_Offset_Address : constant Integer_Address := 16#004C#;

   CISR_A_Offset_Address : constant Integer_Address := 16#0200#;
   CISR_B_Offset_Address : constant Integer_Address := 16#0204#;

   CDTRSR_A_Offset_Address : constant Integer_Address := 16#0210#;
   CDTRSR_B_Offset_Address : constant Integer_Address := 16#0214#;

   CIOSR_A_Offset_Address : constant Integer_Address := 16#0220#;
   CIOSR_B_Offset_Address : constant Integer_Address := 16#0224#;

   CDTROSR_A_Offset_Address : constant Integer_Address := 16#0230#;
   CDTROSR_B_Offset_Address : constant Integer_Address := 16#0234#;

   CIER_A_Offset_Address : constant Integer_Address := 16#0240#;
   CIER_B_Offset_Address : constant Integer_Address := 16#0244#;

   CDTRER_A_Offset_Address : constant Integer_Address := 16#0250#;
   CDTRER_B_Offset_Address : constant Integer_Address := 16#0254#;

   CPSSR_A_Offset_Address : constant Integer_Address := 16#0280#;
   CPSSR_B_Offset_Address : constant Integer_Address := 16#0284#;

   CSSR_A_Offset_Address : constant Integer_Address := 16#0290#;
   CSSR_B_Offset_Address : constant Integer_Address := 16#0294#;

   A_Channel_Offset_Address : constant Integer_Address := 16#0400#;
   B_Channel_Offset_Address : constant Integer_Address := 16#0800#;

   SDM_Offset_Address            : constant Integer_Address := 16#8000#;
   SDM_SPE_Mirror_Offset_Address : constant Integer_Address := 16#C000#;
   SCM_Offset_Address            : constant Integer_Address := 16#1_0000#;

   ----------------------------------------------------------------------------
   --  Hardware Features
   ----------------------------------------------------------------------------
   Shared_Data_Size       : constant Unsigned_32 := 3 * 1024;
   Shared_Code_Size       : constant Unsigned_32 := 16 * 1024 / 4;
   Channel_Registers_Size : constant Integer     := 1024 * 8;

   Channel_Configuration_Control_Type_Size : constant Integer := 128;

   type eTPU_ID_Type is range 0 .. 31;
   type eTPU_Engine is (eTPU_A, eTPU_B);

   type Shared_Data_Offset is new Unsigned_32 range 0 .. Shared_Data_Size -
                                                         1;
   type Shared_Code_Offset is mod Shared_Code_Size;

   type Shared_Code_Element_Type is new Unsigned_32;

   ----------------------------------------------------------------------------
   --  eTPU Types
   ---------------------------------------------------------------------------

   --  Common Types

   --  eTPU Module Configuration Register (ETPU_MCR)
   type GEC_Type is (Keep_Global_Exception, Negate_Global_Exception);
   type MGE_Type is (Exception_Not_Pending, Exception_Pending);
   type ILF_Type is (No_Illegal_Instruction, Illegal_Instruction);
   subtype SCMSIZE_Type is Integer range 0 .. 31;
   type SCMMISF_Type is (No_Signature_Mismatch, Signature_Mismatch);
   type VIS_Type is (Not_Visable_To_Slave, Visable_To_Slave);

   type Module_Configuration_Type is record
      Global_Exception_Clear              : GEC_Type;
      Microcode_Global_Exception_Engine_A : MGE_Type;
      Microcode_Global_Exception_Engine_B : MGE_Type;
      Illegal_Instruction_Flag_A          : ILF_Type;
      Illegal_Instruction_Flag_B          : ILF_Type;
      SCM_Size                            : SCMSIZE_Type;
      SCM_MISC_Flag                       : SCMMISF_Type;
      SCM_MISC_Enable                     : Enable_Type;
      SCM_Visability                      : VIS_Type;
      Global_Time_Base_Enable             : Enable_Type;
   end record;

   --  eTPU Coherent Dual-Parameter Controller Register (ETPU_CDCR)
   type STS_Type is (Do_Not_Start, Start);
   type CTBASE_Type is range 0 .. 2#1_1111#;
   type PBASE_Address is range 0 .. 2#11_1111_1111#;
   type PWIDTH_Type is (Transfer_24_Bit, Transfer_32_Bit);
   type PARM_Type is range 0 .. 2#11_1111#;
   type WR_Type is (Read, Write);

   type Coherent_Dual_Parameter_Controller_Type is record
      Start                         : STS_Type;
      Channel_Transfer_Base         : CTBASE_Type;
      Parameter_Buffer_Base_Address : PBASE_Address;
      Parameter_Width               : PWIDTH_Type;
      Channel_Parameter_0           : PARM_Type;
      Read_Write_Select             : WR_Type;
      Channel_Parameter_1           : PARM_Type;
   end record;

   --  eTPU MISC Compare Register (ETPU_MISCCMPR)
   type MISC_Value_Type is new Integer;

   --  eTPU SCM Off-Range Data Register (EPTU_SCMOFFDATAR)
   type SCM_Off_Range_Data_Type is new Integer;

   --  eTPU Engine Configuration Register (ETPU_ECR)
   type FEND_Type is (Normal_Operation, Terminate_Operation);
   type MDIS_Type is (Run_Clocks, Stop_Clocks);
   type STF_Type is (Engine_Operating, Engine_Stopped);
   type HLTF_Type is (Engine_Not_Halted, Engine_Halted);
   type FPSCK_Type is (
      FP_2,
      FP_4,
      FP_8,
      FP_16,
      FP_32,
      FP_64,
      FP_128,
      FP_256);
   type CDFC_Type is (Two_Sample_Mode, Three_Sample_Mode, Continuous_Mode);
   type ETB_Type is mod 2 ** 5;

   type Engine_Configuration_Type is record
      Force_End                      : FEND_Type;
      Module_Disable_Internal_Stop   : MDIS_Type;
      Stop_Flag                      : STF_Type;
      Halt_Mode_Flag                 : HLTF_Type;
      Filter_Prescale_Clock_Control  : FPSCK_Type;
      Channel_Digital_Filter_Control : CDFC_Type;
      Entry_Table_Base               : ETB_Type;
   end record;

   --  eTPU Time Base Configuration Register (ETPU_TBCR)
   type TCR2CTL_Type is (
      Gated_DIV8_Clock,
      Rise_Transition,
      Fall_Transition,
      Rise_Fall_Transition,
      DIV8_Clock,
      Shutdown);
   type TCRCF_Type is (
      DIV2_Clock_Two_Sample,
      Filter_Clock_Two_Sample,
      DIV2_Clock_Integration,
      Filter_Clock_Integration);
   type TCR2P_Type is mod 2 ** 5;
   type TCR1CTL_Type is (TCR_Clock, DIV2_Clock, Shutdown);
   type TCR1P_Type is mod 2 ** 7;

   type Time_Base_Configuration_Type is record
      TCR2_Clock_Gate_Control    : TCR2CTL_Type;
      TCRCLK_Sign_Filter_Control : TCRCF_Type;
      Angle_Mode_Select          : Enable_Type;
      TCR2_Prescaler_Control     : TCR2P_Type;
      TCR1_Clock_Gate_Control    : TCR1CTL_Type;
      TCR1_Prescaler_Control     : TCR1P_Type;
   end record;

   --  eTPU Time Base 1 (TCR1) Visibility Register (ETPU_TB1R) eTPU Time Base 2
   --  (TCR2) Visibility Register (ETPU_TB2R)
   type Time_Count_Register_Type is range 0 .. 16#FF_FFFF#;

   --  STAC Bus Configuration Register (ETPU_REDCR)
   type RSC_Type is (Client, Server);
   type Server_ID_Type is mod 2 ** 4;
   type SRV_Type is mod 2 ** 4;

   type STAC_Bus_Configuration_Type is record
      TCR1_Resource_Client_Server_Operation  : Enable_Type;
      TCR1_Resource_Client_Server_Assignment : RSC_Type;
      STAC_Bus_Address_1                     : Server_ID_Type;
      TCR1_Resource_Server                   : SRV_Type;
      TCR2_Resource_Client_Server_Operation  : Enable_Type;
      TCR2_Resource_Client_Server_Assignment : RSC_Type;
      STAC_Bus_Address_2                     : Server_ID_Type;
      TCR2_Resource_Server                   : SRV_Type;
   end record;

   --  eTPU Channel Interrupt Status Register (ETPU_CISR) eTPU Channel Data
   --  Transfer Request Status Register (ETPU_CDTRSR)
   type Pending_Type is (Not_Pending_Keep, Pending_Clear);

   type Channel_Pending_Type is record
      Channel_0, Channel_1, Channel_2, Channel_3, Channel_4, Channel_5,
Channel_6, Channel_7, Channel_8, Channel_9, Channel_10, Channel_11, Channel_12,
Channel_13, Channel_14, Channel_15, Channel_16, Channel_17, Channel_18,
Channel_19, Channel_20, Channel_21, Channel_22, Channel_23, Channel_24,
Channel_25, Channel_26, Channel_27, Channel_28, Channel_29, Channel_30,
Channel_31 : Pending_Type;
   end record;

   --  eTPU Channel Interrupt Overflow Status Register (ETPU_CIOSR) eTPU
   --  Channel Data Transfer Request Overflow Status Register (ETPU_CDTROSR)
   --  eTPU Channel Pending Service Status Register (ETPU_CPSSR)
   type Channel_Occured_Type is record
      Channel_0, Channel_1, Channel_2, Channel_3, Channel_4, Channel_5,
Channel_6, Channel_7, Channel_8, Channel_9, Channel_10, Channel_11, Channel_12,
Channel_13, Channel_14, Channel_15, Channel_16, Channel_17, Channel_18,
Channel_19, Channel_20, Channel_21, Channel_22, Channel_23, Channel_24,
Channel_25, Channel_26, Channel_27, Channel_28, Channel_29, Channel_30,
Channel_31 : Occurred_Type;
   end record;

   --  eTPU Channel Interrupt Enable Register (ETPU_CIER) eTPU Channel Data
   --  Transfer Request Enable Register (ETPU_CDTRER)
   type Channel_Enable_Type is record
      Channel_0, Channel_1, Channel_2, Channel_3, Channel_4, Channel_5,
Channel_6, Channel_7, Channel_8, Channel_9, Channel_10, Channel_11, Channel_12,
Channel_13, Channel_14, Channel_15, Channel_16, Channel_17, Channel_18,
Channel_19, Channel_20, Channel_21, Channel_22, Channel_23, Channel_24,
Channel_25, Channel_26, Channel_27, Channel_28, Channel_29, Channel_30,
Channel_31 : Occurred_Type;
   end record;

   --  eTPU Channel Service Status Register (EPTU_CSSR)
   type Service_Type is (Not_Being_Serviced, Being_Serviced);
   type Channel_Service_Type is record
      Channel_0, Channel_1, Channel_2, Channel_3, Channel_4, Channel_5,
Channel_6, Channel_7, Channel_8, Channel_9, Channel_10, Channel_11, Channel_12,
Channel_13, Channel_14, Channel_15, Channel_16, Channel_17, Channel_18,
Channel_19, Channel_20, Channel_21, Channel_22, Channel_23, Channel_24,
Channel_25, Channel_26, Channel_27, Channel_28, Channel_29, Channel_30,
Channel_31 : Service_Type;
   end record;

   --  eTPU Channel n Configuration Register (ETPU_CnCR)
   type Priority_Type is (Disabled, Low, Middle, High);
   type ETCS_Type is (Standard, Alternative);
   type Function_Number is mod 2 ** 5;
   type Polarity_Type is (Active_Low, Active_High);
   subtype CPBA_Type is Shared_Data_Offset range 0 .. 2#111_1111_1111#;

   type Channel_Configuration_Type is record
      Interrupt_Enable             : Enable_Type;
      Data_Transfer_Request_Enable : Enable_Type;
      Priority                     : Priority_Type;
      Entry_Table_Condition_Select : ETCS_Type;
      Function_Select              : Function_Number;
      Output_Disable               : Enable_Type;
      Output_Polarity              : Polarity_Type;
      Parameter_Base_Address       : CPBA_Type;
   end record;

   --  eTPU Channel n Status Control Register (ETPU_CnSCR)
   type FM_Type is mod 2 ** 2;

   type Channel_Status_Type is record
      Interrupt_Status                     : Pending_Type;
      Interrupt_Overflow_Status            : Occurred_Type;
      Data_Transfer_Request_Status         : Pending_Type;
      Data_Transfer_Rquest_Overflow_Status : Occurred_Type;
      Input_Pin_State                      : Pin_State_Type;
      Output_Pin_State                     : Pin_State_Type;
      Function_Mode                        : FM_Type;
   end record;

   --  eTPU Channel n Host Service Request Register (ETPU_CnHSRR)
   type HSR_Type is range 0 .. 2#111#;

   type Channel_Host_Service_Request_Type is record
      Host_Service_Request : HSR_Type;
   end record;

   pragma Warnings (Off, "*bits of*unused");
   type Channel_Configuration_Control_Type is record
      Configuration_Register        : Channel_Configuration_Type;
      Status_Register               : Channel_Status_Type;
      Host_Service_Request_Register : Channel_Host_Service_Request_Type;
   end record with Size => Channel_Configuration_Control_Type_Size;
   pragma Warnings (On, "*bits of*unused");

   ----------------------------------------------------------------------------
   --  Hardware Respresentations
   ----------------------------------------------------------------------------
   for GEC_Type use
     (Keep_Global_Exception   => 0,
      Negate_Global_Exception => 1);
   for MGE_Type use (Exception_Not_Pending => 0, Exception_Pending => 1);
   for ILF_Type use (No_Illegal_Instruction => 0, Illegal_Instruction => 1);
   for SCMMISF_Type use (No_Signature_Mismatch => 0, Signature_Mismatch => 1);
   for VIS_Type use (Not_Visable_To_Slave => 0, Visable_To_Slave => 1);

   for Module_Configuration_Type use record
      Global_Exception_Clear              at 0 range 0 .. 0;
      Microcode_Global_Exception_Engine_A at 0 range 4 .. 4;
      Microcode_Global_Exception_Engine_B at 0 range 5 .. 5;
      Illegal_Instruction_Flag_A          at 0 range 6 .. 6;
      Illegal_Instruction_Flag_B          at 0 range 7 .. 7;
      SCM_Size                            at 0 range 11 .. 15;
      SCM_MISC_Flag                       at 0 range 21 .. 21;
      SCM_MISC_Enable                     at 0 range 22 .. 22;
      SCM_Visability                      at 0 range 25 .. 25;
      Global_Time_Base_Enable             at 0 range 31 .. 31;
   end record;

   for STS_Type use (Do_Not_Start => 0, Start => 1);
   for PWIDTH_Type use (Transfer_24_Bit => 0, Transfer_32_Bit => 1);
   for WR_Type use (Read => 0, Write => 1);

   for Coherent_Dual_Parameter_Controller_Type use record
      Start                         at 0 range 0 .. 0;
      Channel_Transfer_Base         at 0 range 1 .. 5;
      Parameter_Buffer_Base_Address at 0 range 6 .. 15;
      Parameter_Width               at 0 range 16 .. 16;
      Channel_Parameter_0           at 0 range 17 .. 23;
      Read_Write_Select             at 0 range 24 .. 24;
      Channel_Parameter_1           at 0 range 25 .. 31;
   end record;

   for FEND_Type use (Normal_Operation => 0, Terminate_Operation => 1);
   for MDIS_Type use (Run_Clocks => 0, Stop_Clocks => 1);
   for STF_Type use (Engine_Operating => 0, Engine_Stopped => 1);
   for HLTF_Type use (Engine_Not_Halted => 0, Engine_Halted => 1);
   for FPSCK_Type use
     (FP_2   => 2#000#,
      FP_4   => 2#001#,
      FP_8   => 2#010#,
      FP_16  => 2#011#,
      FP_32  => 2#100#,
      FP_64  => 2#101#,
      FP_128 => 2#110#,
      FP_256 => 2#111#);
   for CDFC_Type use
     (Two_Sample_Mode   => 2#00#,
      Three_Sample_Mode => 2#10#,
      Continuous_Mode   => 2#11#);

   for Engine_Configuration_Type use record
      Force_End                      at 0 range 0 .. 0;
      Module_Disable_Internal_Stop   at 0 range 1 .. 1;
      Stop_Flag                      at 0 range 3 .. 3;
      Halt_Mode_Flag                 at 0 range 8 .. 8;
      Filter_Prescale_Clock_Control  at 0 range 13 .. 15;
      Channel_Digital_Filter_Control at 0 range 16 .. 17;
      Entry_Table_Base               at 0 range 27 .. 31;
   end record;

   for TCR2CTL_Type use
     (Gated_DIV8_Clock     => 2#000#,
      Rise_Transition      => 2#001#,
      Fall_Transition      => 2#010#,
      Rise_Fall_Transition => 2#011#,
      DIV8_Clock           => 2#100#,
      Shutdown             => 2#111#);
   for TCRCF_Type use
     (DIV2_Clock_Two_Sample    => 2#00#,
      Filter_Clock_Two_Sample  => 2#01#,
      DIV2_Clock_Integration   => 2#10#,
      Filter_Clock_Integration => 2#11#);
   for TCR1CTL_Type use
     (TCR_Clock  => 2#00#,
      DIV2_Clock => 2#10#,
      Shutdown   => 2#11#);

   for Time_Base_Configuration_Type use record
      TCR2_Clock_Gate_Control    at 0 range 0 .. 2;
      TCRCLK_Sign_Filter_Control at 0 range 3 .. 4;
      Angle_Mode_Select          at 0 range 6 .. 6;
      TCR2_Prescaler_Control     at 0 range 10 .. 15;
      TCR1_Clock_Gate_Control    at 0 range 16 .. 17;
      TCR1_Prescaler_Control     at 0 range 24 .. 31;
   end record;

   for RSC_Type use (Client => 0, Server => 1);

   for STAC_Bus_Configuration_Type use record
      TCR1_Resource_Client_Server_Operation  at 0 range 0 .. 0;
      TCR1_Resource_Client_Server_Assignment at 0 range 1 .. 1;
      STAC_Bus_Address_1                     at 0 range 4 .. 7;
      TCR1_Resource_Server                   at 0 range 12 .. 15;
      TCR2_Resource_Client_Server_Operation  at 0 range 16 .. 16;
      TCR2_Resource_Client_Server_Assignment at 0 range 17 .. 17;
      STAC_Bus_Address_2                     at 0 range 20 .. 23;
      TCR2_Resource_Server                   at 0 range 28 .. 31;
   end record;

   for Pending_Type use (Not_Pending_Keep => 0, Pending_Clear => 1);
   for Channel_Pending_Type use record
      Channel_0  at 0 range 31 .. 31;
      Channel_1  at 0 range 30 .. 30;
      Channel_2  at 0 range 29 .. 29;
      Channel_3  at 0 range 28 .. 28;
      Channel_4  at 0 range 27 .. 27;
      Channel_5  at 0 range 26 .. 26;
      Channel_6  at 0 range 25 .. 25;
      Channel_7  at 0 range 24 .. 24;
      Channel_8  at 0 range 23 .. 23;
      Channel_9  at 0 range 22 .. 22;
      Channel_10 at 0 range 21 .. 21;
      Channel_11 at 0 range 20 .. 20;
      Channel_12 at 0 range 19 .. 19;
      Channel_13 at 0 range 18 .. 18;
      Channel_14 at 0 range 17 .. 17;
      Channel_15 at 0 range 16 .. 16;
      Channel_16 at 0 range 15 .. 15;
      Channel_17 at 0 range 14 .. 14;
      Channel_18 at 0 range 13 .. 13;
      Channel_19 at 0 range 12 .. 12;
      Channel_20 at 0 range 11 .. 11;
      Channel_21 at 0 range 10 .. 10;
      Channel_22 at 0 range 9 .. 9;
      Channel_23 at 0 range 8 .. 8;
      Channel_24 at 0 range 7 .. 7;
      Channel_25 at 0 range 6 .. 6;
      Channel_26 at 0 range 5 .. 5;
      Channel_27 at 0 range 4 .. 4;
      Channel_28 at 0 range 3 .. 3;
      Channel_29 at 0 range 2 .. 2;
      Channel_30 at 0 range 1 .. 1;
      Channel_31 at 0 range 0 .. 0;
   end record;

   for Channel_Occured_Type use record
      Channel_0  at 0 range 31 .. 31;
      Channel_1  at 0 range 30 .. 30;
      Channel_2  at 0 range 29 .. 29;
      Channel_3  at 0 range 28 .. 28;
      Channel_4  at 0 range 27 .. 27;
      Channel_5  at 0 range 26 .. 26;
      Channel_6  at 0 range 25 .. 25;
      Channel_7  at 0 range 24 .. 24;
      Channel_8  at 0 range 23 .. 23;
      Channel_9  at 0 range 22 .. 22;
      Channel_10 at 0 range 21 .. 21;
      Channel_11 at 0 range 20 .. 20;
      Channel_12 at 0 range 19 .. 19;
      Channel_13 at 0 range 18 .. 18;
      Channel_14 at 0 range 17 .. 17;
      Channel_15 at 0 range 16 .. 16;
      Channel_16 at 0 range 15 .. 15;
      Channel_17 at 0 range 14 .. 14;
      Channel_18 at 0 range 13 .. 13;
      Channel_19 at 0 range 12 .. 12;
      Channel_20 at 0 range 11 .. 11;
      Channel_21 at 0 range 10 .. 10;
      Channel_22 at 0 range 9 .. 9;
      Channel_23 at 0 range 8 .. 8;
      Channel_24 at 0 range 7 .. 7;
      Channel_25 at 0 range 6 .. 6;
      Channel_26 at 0 range 5 .. 5;
      Channel_27 at 0 range 4 .. 4;
      Channel_28 at 0 range 3 .. 3;
      Channel_29 at 0 range 2 .. 2;
      Channel_30 at 0 range 1 .. 1;
      Channel_31 at 0 range 0 .. 0;
   end record;

   for Channel_Enable_Type use record
      Channel_0  at 0 range 31 .. 31;
      Channel_1  at 0 range 30 .. 30;
      Channel_2  at 0 range 29 .. 29;
      Channel_3  at 0 range 28 .. 28;
      Channel_4  at 0 range 27 .. 27;
      Channel_5  at 0 range 26 .. 26;
      Channel_6  at 0 range 25 .. 25;
      Channel_7  at 0 range 24 .. 24;
      Channel_8  at 0 range 23 .. 23;
      Channel_9  at 0 range 22 .. 22;
      Channel_10 at 0 range 21 .. 21;
      Channel_11 at 0 range 20 .. 20;
      Channel_12 at 0 range 19 .. 19;
      Channel_13 at 0 range 18 .. 18;
      Channel_14 at 0 range 17 .. 17;
      Channel_15 at 0 range 16 .. 16;
      Channel_16 at 0 range 15 .. 15;
      Channel_17 at 0 range 14 .. 14;
      Channel_18 at 0 range 13 .. 13;
      Channel_19 at 0 range 12 .. 12;
      Channel_20 at 0 range 11 .. 11;
      Channel_21 at 0 range 10 .. 10;
      Channel_22 at 0 range 9 .. 9;
      Channel_23 at 0 range 8 .. 8;
      Channel_24 at 0 range 7 .. 7;
      Channel_25 at 0 range 6 .. 6;
      Channel_26 at 0 range 5 .. 5;
      Channel_27 at 0 range 4 .. 4;
      Channel_28 at 0 range 3 .. 3;
      Channel_29 at 0 range 2 .. 2;
      Channel_30 at 0 range 1 .. 1;
      Channel_31 at 0 range 0 .. 0;
   end record;

   for Priority_Type use
     (Disabled => 2#00#,
      Low      => 2#01#,
      Middle   => 2#10#,
      High     => 2#11#);
   for ETCS_Type use (Standard => 0, Alternative => 1);
   for Polarity_Type use (Active_Low => 0, Active_High => 1);

   for Channel_Configuration_Type use record
      Interrupt_Enable             at 0 range 0 .. 0;
      Data_Transfer_Request_Enable at 0 range 1 .. 1;
      Priority                     at 0 range 2 .. 3;
      Entry_Table_Condition_Select at 0 range 7 .. 7;
      Function_Select              at 0 range 11 .. 15;
      Output_Disable               at 0 range 16 .. 16;
      Output_Polarity              at 0 range 17 .. 17;
      Parameter_Base_Address       at 0 range 21 .. 31;
   end record;

   for Channel_Status_Type use record
      Interrupt_Status                     at 0 range 0 .. 0;
      Interrupt_Overflow_Status            at 0 range 1 .. 1;
      Data_Transfer_Request_Status         at 0 range 8 .. 8;
      Data_Transfer_Rquest_Overflow_Status at 0 range 9 .. 9;
      Input_Pin_State                      at 0 range 16 .. 16;
      Output_Pin_State                     at 0 range 17 .. 17;
      Function_Mode                        at 0 range 30 .. 31;
   end record;

   for Channel_Host_Service_Request_Type use record
      Host_Service_Request at 0 range 29 .. 31;
   end record;

   for Channel_Configuration_Control_Type use record
      Configuration_Register        at 0 range 0 .. 31;
      Status_Register               at 4 range 0 .. 31;
      Host_Service_Request_Register at 8 range 0 .. 31;
   end record;

   ----------------------------------------------------------------------------
   --  eTPU Registers
   ----------------------------------------------------------------------------

   Module_Configuration_Register : Module_Configuration_Type;
   for Module_Configuration_Register'Address use
     System'To_Address (eTPU_Base_Address + MCR_Offset_Address);

   Coherent_Dual_Parameter_Controller_Register :
     Coherent_Dual_Parameter_Controller_Type;
   for Coherent_Dual_Parameter_Controller_Register'Address use
     System'To_Address (eTPU_Base_Address + CDCR_Offset_Address);

   MISC_Compare_Register : MISC_Value_Type;
   for MISC_Compare_Register'Address use
     System'To_Address (eTPU_Base_Address + MISCCMPR_Offset_Address);

   SCM_Off_Range_Data_Register : SCM_Off_Range_Data_Type;
   for SCM_Off_Range_Data_Register'Address use
     System'To_Address (eTPU_Base_Address + SCMOFFDATAR_Offset_Address);

   Engine_Configuration_Register_A : Engine_Configuration_Type;
   for Engine_Configuration_Register_A'Address use
     System'To_Address (eTPU_Base_Address + ECR_A_Offset_Address);
   Engine_Configuration_Register_B : Engine_Configuration_Type;
   for Engine_Configuration_Register_B'Address use
     System'To_Address (eTPU_Base_Address + ECR_B_Offset_Address);

   Time_Base_Configuration_Register_A : Time_Base_Configuration_Type;
   for Time_Base_Configuration_Register_A'Address use
     System'To_Address (eTPU_Base_Address + TBCR_A_Offset_Address);
   Time_Base_1_Register_A : Time_Count_Register_Type;
   for Time_Base_1_Register_A'Address use
     System'To_Address (eTPU_Base_Address + TB1R_A_Offset_Address);
   Time_Base_2_Register_A : Time_Count_Register_Type;
   for Time_Base_2_Register_A'Address use
     System'To_Address (eTPU_Base_Address + TB2R_A_Offset_Address);
   STAC_Bus_Configuration_Register_A : STAC_Bus_Configuration_Type;
   for STAC_Bus_Configuration_Register_A'Address use
     System'To_Address (eTPU_Base_Address + REDCR_A_Offset_Address);

   Time_Base_Configuration_Register_B : Time_Base_Configuration_Type;
   for Time_Base_Configuration_Register_B'Address use
     System'To_Address (eTPU_Base_Address + TBCR_B_Offset_Address);
   Time_Base_1_Register_B : Time_Count_Register_Type;
   for Time_Base_1_Register_B'Address use
     System'To_Address (eTPU_Base_Address + TB1R_B_Offset_Address);
   Time_Base_2_Register_B : Time_Count_Register_Type;
   for Time_Base_2_Register_B'Address use
     System'To_Address (eTPU_Base_Address + TB2R_B_Offset_Address);
   STAC_Bus_Configuration_Register_B : STAC_Bus_Configuration_Type;
   for STAC_Bus_Configuration_Register_B'Address use
     System'To_Address (eTPU_Base_Address + REDCR_B_Offset_Address);

   Interrupt_Status_A : Channel_Pending_Type;
   for Interrupt_Status_A'Address use
     System'To_Address (eTPU_Base_Address + CISR_A_Offset_Address);
   Interrupt_Status_B : Channel_Pending_Type;
   for Interrupt_Status_B'Address use
     System'To_Address (eTPU_Base_Address + CISR_B_Offset_Address);

   Channel_Data_Transfer_Request_Status_A : Channel_Pending_Type;
   for Channel_Data_Transfer_Request_Status_A'Address use
     System'To_Address (eTPU_Base_Address + CDTRSR_A_Offset_Address);
   Channel_Data_Transfer_Request_Status_B : Channel_Pending_Type;
   for Channel_Data_Transfer_Request_Status_B'Address use
     System'To_Address (eTPU_Base_Address + CDTRSR_B_Offset_Address);

   Interrupt_Overflow_Status_A : Channel_Occured_Type;
   for Interrupt_Overflow_Status_A'Address use
     System'To_Address (eTPU_Base_Address + CIOSR_A_Offset_Address);
   Interrupt_Overflow_Status_B : Channel_Occured_Type;
   for Interrupt_Overflow_Status_B'Address use
     System'To_Address (eTPU_Base_Address + CIOSR_B_Offset_Address);

   Channel_Data_Transfer_Request_Overflow_Status_A : Channel_Occured_Type;
   for Channel_Data_Transfer_Request_Overflow_Status_A'Address use
     System'To_Address (eTPU_Base_Address + CDTROSR_A_Offset_Address);
   Channel_Data_Transfer_Request_Overflow_Status_B : Channel_Occured_Type;
   for Channel_Data_Transfer_Request_Overflow_Status_B'Address use
     System'To_Address (eTPU_Base_Address + CDTROSR_B_Offset_Address);

   Interrupt_Enable_Register_A : Channel_Enable_Type;
   for Interrupt_Enable_Register_A'Address use
     System'To_Address (eTPU_Base_Address + CIER_A_Offset_Address);
   Interrupt_Enable_Register_B : Channel_Enable_Type;
   for Interrupt_Enable_Register_B'Address use
     System'To_Address (eTPU_Base_Address + CIER_B_Offset_Address);

   Data_Transfer_Request_Enable_Register_A : Channel_Enable_Type;
   for Data_Transfer_Request_Enable_Register_A'Address use
     System'To_Address (eTPU_Base_Address + CDTRER_A_Offset_Address);
   Data_Transfer_Request_Enable_Register_B : Channel_Enable_Type;
   for Data_Transfer_Request_Enable_Register_B'Address use
     System'To_Address (eTPU_Base_Address + CDTRER_B_Offset_Address);

   Channel_Pending_Service_Status_Register_A : Channel_Pending_Type;
   for Channel_Pending_Service_Status_Register_A'Address use
     System'To_Address (eTPU_Base_Address + CPSSR_A_Offset_Address);
   Channel_Pending_Service_Status_Register_B : Channel_Pending_Type;
   for Channel_Pending_Service_Status_Register_B'Address use
     System'To_Address (eTPU_Base_Address + CPSSR_B_Offset_Address);

   pragma Warnings (Off, "*component of*padded*");

   type Channel_Config_Control_Array is
     array (eTPU_ID_Type) of Channel_Configuration_Control_Type;
   type Channel_Registers_Array_Type is
     array (eTPU_Engine) of Channel_Config_Control_Array;
   for Channel_Registers_Array_Type'Component_Size use Channel_Registers_Size;

   pragma Warnings (On, "*component of*padded*");

   Channel : Channel_Registers_Array_Type;
   for Channel'Address use
     System'To_Address (eTPU_Base_Address + A_Channel_Offset_Address);

   type Shared_Code_Type is
     array (Shared_Code_Offset range <>) of Shared_Code_Element_Type;
   Shared_Code_Memory : Shared_Code_Type (Shared_Code_Offset);
   for Shared_Code_Memory'Address use
     System'To_Address (eTPU_Base_Address + SCM_Offset_Address);

   type Shared_Data_Type_32 is
     array (Shared_Data_Offset range <>) of Unsigned_32;
   Shared_Data_Memory_32 : Shared_Data_Type_32 (
      0 .. Shared_Data_Offset'Last / 4);
   for Shared_Data_Memory_32'Address use
     System'To_Address (eTPU_Base_Address + SDM_Offset_Address);

   Shared_Data_Memory_16 :
     array (0 .. Shared_Data_Offset'Last / 2) of Unsigned_16;
   for Shared_Data_Memory_16'Address use Shared_Data_Memory_32'Address;
   pragma Import (Ada, Shared_Data_Memory_16);

   Shared_Data_Memory_8 : array (Shared_Data_Offset) of Unsigned_8;
   for Shared_Data_Memory_8'Address use Shared_Data_Memory_32'Address;
   pragma Import (Ada, Shared_Data_Memory_8);

   pragma Warnings (Off, "*alignment*");

   Shared_Data_Extended_Memory_32 : array (Shared_Data_Offset) of Integer_32;
   for Shared_Data_Extended_Memory_32'Address use
     System'To_Address (eTPU_Base_Address + SDM_SPE_Mirror_Offset_Address);
   pragma Import (Ada, Shared_Data_Extended_Memory_32);

end MPC5554.eTPU;
