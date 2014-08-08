with Interfaces; use Interfaces;
with System;     use System;

package Atmel.AT91SAM7S.PIO with Preelaborate is

   --------------------------
   -- PIO Memory Addresses --
   --------------------------

   PIO_Base_Address    : constant  := 16#FFFF_F400#;
   PER_Offset_Address  : constant  := 16#0000#;
   PDR_Offset_Address  : constant  := 16#0004#;
   PSR_Offset_Address  : constant  := 16#0008#;
   OER_Offset_Address  : constant  := 16#0010#;
   ODR_Offset_Address  : constant  := 16#0014#;
   OSR_Offset_Address  : constant  := 16#0018#;
   IFER_Offset_Address : constant  := 16#0020#;
   IFDR_Offset_Address : constant  := 16#0024#;
   IFSR_Offset_Address : constant  := 16#0028#;
   SODR_Offset_Address : constant  := 16#0030#;
   CODR_Offset_Address : constant  := 16#0034#;
   ODSR_Offset_Address : constant  := 16#0038#;
   PDSR_Offset_Address : constant  := 16#003C#;
   IER_Offset_Address  : constant  := 16#0040#;
   IDR_Offset_Address  : constant  := 16#0044#;
   IMR_Offset_Address  : constant  := 16#0048#;
   ISR_Offset_Address  : constant  := 16#004C#;
   MDER_Offset_Address : constant  := 16#0050#;
   MDDR_Offset_Address : constant  := 16#0054#;
   MDSR_Offset_Address : constant  := 16#0058#;
   PUDR_Offset_Address : constant  := 16#0060#;
   PUER_Offset_Address : constant  := 16#0064#;
   PUSR_Offset_Address : constant  := 16#0068#;
   ASR_Offset_Address  : constant  := 16#0070#;
   BSR_Offset_Address  : constant  := 16#0074#;
   ABSR_Offset_Address : constant  := 16#0078#;
   OWER_Offset_Address : constant  := 16#00A0#;
   OWDR_Offset_Address : constant  := 16#00A4#;
   OWSR_Offset_Address : constant  := 16#00A8#;

   -----------------------
   -- Hardware Features --
   -----------------------

   ---------------
   -- PIO Types --
   ---------------

   type Active_Status_Type is (Inactive, Active);
   type Output_Status_Type is (Pure_Input, Output_Enabled);
   type Change_Type is (No_Change, Change_Occured);
   type Select_Type is (No_Change, Use_Peripheral);
   type Peripheral_Assigment_Type is (A, B);

   type Enable_Set is array (PIO_Lines) of Enable_No_Change_Type
     with Pack, Size => Register_Size, Alignment => 4;
   type Disable_Set is array (PIO_Lines) of Disable_No_Change_Type
     with Pack, Size => Register_Size, Alignment => 4;
   type Active_Status_Set is array (PIO_Lines) of Active_Status_Type
     with Pack, Size => Register_Size, Alignment => 4;
   type Output_Status_Set is array (PIO_Lines) of Output_Status_Type
     with Pack, Size => Register_Size, Alignment => 4;
   type Enabled_Set is array (PIO_Lines) of Enabled_Type
     with Pack, Size => Register_Size, Alignment => 4;
   type Binary_Set is array (PIO_Lines) of Boolean
     with Pack, Size => Register_Size, Alignment => 4;
   type Change_Set is array (PIO_Lines) of Change_Type
     with Pack, Size => Register_Size, Alignment => 4;
   type Select_Set is array (PIO_Lines) of Select_Type
     with Pack, Size => Register_Size, Alignment => 4;
   type Peripheral_Assigment_Set is
     array (PIO_Lines) of Peripheral_Assigment_Type
     with Pack, Size => Register_Size, Alignment => 4;

   ------------------------------
   -- Hardware Representations --
   ------------------------------

   for Active_Status_Type use (Inactive => 0, Active => 1);
   for Output_Status_Type use (Pure_Input => 0, Output_Enabled => 1);
   for Change_Type use (No_Change => 0, Change_Occured => 1);
   for Select_Type use (No_Change => 0, Use_Peripheral => 1);
   for Peripheral_Assigment_Type use (A => 0, B => 1);

   -------------------
   -- PIO Registers --
   -------------------

   PIO_Enable_Register : Enable_Set
     with Address => System'To_Address (PIO_Base_Address + PER_Offset_Address);

   PIO_Disable_Register : Disable_Set
     with Address => System'To_Address (PIO_Base_Address + PDR_Offset_Address);

   PIO_Status_Register : Active_Status_Set
     with Address => System'To_Address (PIO_Base_Address + PSR_Offset_Address);

   Output_Enable_Register : Enable_Set
     with Address => System'To_Address (PIO_Base_Address + OER_Offset_Address);

   Output_Disable_Register : Disable_Set
     with Address => System'To_Address (PIO_Base_Address + ODR_Offset_Address);

   Output_Status_Register  : Output_Status_Set
     with Address => System'To_Address (PIO_Base_Address + OSR_Offset_Address);

   Input_Filter_Enable_Register : Enable_Set
     with Address =>
       System'To_Address (PIO_Base_Address + IFER_Offset_Address);

   Input_Filter_Disable_Register : Disable_Set
     with Address =>
       System'To_Address (PIO_Base_Address + IFDR_Offset_Address);

   Input_Filter_Status_Register  : Enabled_Set
     with Address =>
       System'To_Address (PIO_Base_Address + IFSR_Offset_Address);

   Set_Output_Data_Register : Enable_Set
     with Address =>
       System'To_Address (PIO_Base_Address + SODR_Offset_Address);

   Clear_Output_Data_Register : Enable_Set
     with Address =>
       System'To_Address (PIO_Base_Address + CODR_Offset_Address);

   Output_Data_Status_Register  : Binary_Set
     with Address =>
       System'To_Address (PIO_Base_Address + ODSR_Offset_Address);

   Output_Data_Status_Register_Word : Unsigned_32
     with Address =>
       System'To_Address (PIO_Base_Address + ODSR_Offset_Address);

   Pin_Data_Status_Register : Binary_Set
     with Address =>
       System'To_Address (PIO_Base_Address + PDSR_Offset_Address);

   Pin_Data_Status_Register_Word : Unsigned_32
     with Address =>
       System'To_Address (PIO_Base_Address + PDSR_Offset_Address);

   Interrupt_Enable_Register  : Enable_Set
     with Address => System'To_Address (PIO_Base_Address + IER_Offset_Address);

   Interrupt_Disable_Register : Disable_Set
     with Address => System'To_Address (PIO_Base_Address + IDR_Offset_Address);

   Interrupt_Mask_Register : Enabled_Set
     with Address => System'To_Address (PIO_Base_Address + IMR_Offset_Address);

   Interrupt_Status_Register : Change_Set
     with Address => System'To_Address (PIO_Base_Address + ISR_Offset_Address);

   Multi_Driver_Enable_Register : Enable_Set
     with Address =>
       System'To_Address (PIO_Base_Address + MDER_Offset_Address);

   Multi_Driver_Disable_Register : Disable_Set
     with Address =>
       System'To_Address (PIO_Base_Address + MDDR_Offset_Address);

   Multi_Driver_Status_Register : Enabled_Set
     with Address =>
       System'To_Address (PIO_Base_Address + MDSR_Offset_Address);

   Pull_Up_Disable_Register : Disable_Set
     with Address =>
       System'To_Address (PIO_Base_Address + PUDR_Offset_Address);

   Pull_Up_Enable_Register : Enable_Set
     with Address =>
       System'To_Address (PIO_Base_Address + PUER_Offset_Address);

   Pull_Up_Status_Register : Enabled_Set
     with Address =>
       System'To_Address (PIO_Base_Address + PUER_Offset_Address);

   Peripheral_A_Select_Register : Select_Set
     with Address => System'To_Address (PIO_Base_Address + ASR_Offset_Address);

   Peripheral_B_Select_Register : Select_Set
     with Address => System'To_Address (PIO_Base_Address + BSR_Offset_Address);

   Peripheral_Status_Register : Peripheral_Assigment_Type
     with Address =>
       System'To_Address (PIO_Base_Address + ABSR_Offset_Address);

   Output_Write_Enable_Register : Enable_Set
     with Address =>
       System'To_Address (PIO_Base_Address + OWER_Offset_Address);

   Output_Write_Disable_Register : Disable_Set
     with Address =>
       System'To_Address (PIO_Base_Address + OWDR_Offset_Address);

   Output_Write_Status_Register : Enabled_Set
     with Address =>
       System'To_Address (PIO_Base_Address + OWSR_Offset_Address);
end Atmel.AT91SAM7S.PIO;
