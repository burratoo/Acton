with Interfaces; use Interfaces;
with System;     use System;

package Atmel.AT91SAM7S.AIC with Preelaborate is

   --------------------------
   -- AIC Memory Addresses --
   --------------------------

   AIC_Base_Address     : constant := 16#FFFF_F000#;
   SMR_Offset_Address   : constant := 16#000#;
   SVR_Offset_Address   : constant := 16#080#;
   IVR_Offset_Address   : constant := 16#100#;
   FVR_Offset_Address   : constant := 16#104#;
   ISR_Offset_Address   : constant := 16#108#;
   IPR_Offset_Address   : constant := 16#10C#;
   IMR_Offset_Address   : constant := 16#110#;
   CISR_Offset_Address  : constant := 16#114#;
   IECR_Offset_Address  : constant := 16#120#;
   IDCR_Offset_Address  : constant := 16#124#;
   ICCR_Offset_Address  : constant := 16#128#;
   ISCR_Offset_Address  : constant := 16#12C#;
   EOICR_Offset_Address : constant := 16#130#;
   SPU_Offset_Address   : constant := 16#134#;
   DCR_Offset_Address   : constant := 16#138#;
   FFER_Offset_Address  : constant := 16#140#;
   FFDR_Offset_Address  : constant := 16#144#;
   FFSR_Offset_Address  : constant := 16#148#;

   -----------------------
   -- Hardware Features --
   -----------------------

   type AIC_Interrupt_Priority is range 0 .. 7 with Size => 3;
   type Interrupt_Source_Type is
     (Low_Level_Sensitive,
      Negative_Edge_Triggered,
      High_Level_Sensitive,
      Positive_Edge_Triggered);

   ---------------
   -- AIC Types --
   ---------------

   type Source_Mode_Type is record
      Priority_Level   : AIC_Interrupt_Priority;
      Interrupt_Source : Interrupt_Source_Type;
   end record with Size => Standard'Word_Size;

   type Interrupt_Status_Type is record
      Current_Interrupt : Peripheral_Id;
   end record with Size => Standard'Word_Size;

   type Peripheral_Bit_Field is array (Peripheral_Id) of Boolean
     with Pack, Size => Standard'Word_Size;

   type Interrupt_Pending_Type is record
      Interrupt_Pending : Peripheral_Bit_Field;
   end record with Size => Standard'Word_Size;

   type Interrupt_Enabled_Bit_Field is array (Peripheral_Id) of Enabled_Type
     with Pack, Size => Standard'Word_Size;

   type Interrupt_Enabled_Type is record
      Interrupt : Interrupt_Enabled_Bit_Field;
   end record with Size => Standard'Word_Size;

   type Active_Type is (Deactive, Active);

   type Core_Interrupt_Status_Type is record
      NFIQ : Active_Type;
      NIRQ : Active_Type;
   end record with Size => Standard'Word_Size;

   type Interrupt_Enable_No_Change_Field is
     array (Peripheral_Id) of Enable_No_Change_Type
     with Pack, Size => Standard'Word_Size;

   type Interrupt_Enable_No_Change_Type is record
      Interrupt : Interrupt_Enable_No_Change_Field;
   end record with Size => Standard'Word_Size;

   type Interrupt_Disable_No_Change_Field is
     array (Peripheral_Id) of Disable_No_Change_Type
     with Pack, Size => Standard'Word_Size;

   type Interrupt_Disable_No_Change_Type is record
      Interrupt : Interrupt_Disable_No_Change_Field;
   end record with Size => Standard'Word_Size;

   type Interrupt_Clear_Type is record
      Clear_Interrupt : Peripheral_Bit_Field;
   end record with Size => Standard'Word_Size;

   type Interrupt_Set_Type is record
      Set_Clear_Interrupt : Peripheral_Bit_Field;
   end record with Size => Standard'Word_Size;

   type Debug_Control_Type is record
      Protection_Mode : Enable_Type;
      General_Mask    : Boolean;
   end record with Size => 32;

   ------------------------------
   -- Hardware Representations --
   ------------------------------

   for Interrupt_Source_Type use
     (Low_Level_Sensitive     => 0,
      Negative_Edge_Triggered => 1,
      High_Level_Sensitive    => 2,
      Positive_Edge_Triggered => 3);

   for Source_Mode_Type use record
      Priority_Level   at 0 range 0 .. 2;
      Interrupt_Source at 0 range 5 .. 6;
   end record;

   for Interrupt_Status_Type use record
      Current_Interrupt at 0 range 0 .. 4;
   end record;

   for Core_Interrupt_Status_Type use record
      NFIQ at 0 range 0 .. 0;
      NIRQ at 0 range 1 .. 1;
   end record;

   for Debug_Control_Type use record
      Protection_Mode at 0 range 0 .. 0;
      General_Mask    at 0 range 1 .. 1;
   end record;

   -------------------
   -- AIC Registers --
   -------------------

   Source_Mode_Register : array (Peripheral_Id) of Source_Mode_Type
     with Alignment => 4,
     Address => System'To_Address (AIC_Base_Address + SMR_Offset_Address);

   Source_Vector_Register : array (Peripheral_Id) of Address
     with Alignment => 4,
     Address => System'To_Address (AIC_Base_Address + SVR_Offset_Address);

   Interrupt_Vector_Register : Address
     with Address => System'To_Address (AIC_Base_Address + IVR_Offset_Address);

   FIQ_Vector_Register : Address
     with Address => System'To_Address (AIC_Base_Address + IVR_Offset_Address);

   Interrupt_Status_Register : Interrupt_Status_Type
     with Address => System'To_Address (AIC_Base_Address + ISR_Offset_Address);

   Interrupt_Pending_Register : Interrupt_Pending_Type
     with Address => System'To_Address (AIC_Base_Address + IPR_Offset_Address);

   Interrupt_Mask_Register : Interrupt_Enabled_Type
     with Address => System'To_Address (AIC_Base_Address + IMR_Offset_Address);

   Core_Interrupt_Status_Register : Core_Interrupt_Status_Type
     with Address => System'To_Address
       (AIC_Base_Address + CISR_Offset_Address);

   Interrupt_Enable_Command_Register : Interrupt_Enable_No_Change_Type
     with Address => System'To_Address
       (AIC_Base_Address + IECR_Offset_Address);

   Interrupt_Disable_Command_Register : Interrupt_Disable_No_Change_Type
     with Address => System'To_Address
       (AIC_Base_Address + IECR_Offset_Address);

   Interrupt_Clear_Command_Register : Interrupt_Clear_Type
     with Address => System'To_Address
       (AIC_Base_Address + ICCR_Offset_Address);

   Interrupt_Set_Command_Register : Interrupt_Set_Type
     with Address => System'To_Address
       (AIC_Base_Address + ICCR_Offset_Address);

   End_Of_Interrupt_Command_Register : Unsigned_32
     with Address => System'To_Address
       (AIC_Base_Address + EOICR_Offset_Address);

   Spurious_Interrupt_Vector_Register : Address
     with Address => System'To_Address (AIC_Base_Address + SPU_Offset_Address);

   Debug_Control_Register : Debug_Control_Type
     with Address => System'To_Address (AIC_Base_Address + DCR_Offset_Address);

   Fast_Forcing_Enable_Register : Interrupt_Enable_No_Change_Type
     with Address => System'To_Address
       (AIC_Base_Address + FFER_Offset_Address);

   Fast_Forcing_Disable_Register : Interrupt_Disable_No_Change_Type
     with Address => System'To_Address
       (AIC_Base_Address + FFDR_Offset_Address);

   Fast_Forcing_Status_Register : Interrupt_Enabled_Type
     with Address => System'To_Address
       (AIC_Base_Address + FFSR_Offset_Address);

end Atmel.AT91SAM7S.AIC;
