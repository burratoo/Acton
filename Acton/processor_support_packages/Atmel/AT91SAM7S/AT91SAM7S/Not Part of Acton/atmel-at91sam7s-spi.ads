------------------------------------------------------------------------------------------
--                                                                                      --
--                            ACTON PROCESSOR SUPPORT PACKAGE                           --
--                                                                                      --
--                                  ATMEL.AT91SAM7S.PDC                                 --
--                                                                                      --
--                       Copyright (C) 2014-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

with Ada.Interrupts; use Ada.Interrupts;
with Interfaces;     use Interfaces;
with System;         use System;

with Atmel.AT91SAM7S.PDC; use Atmel.AT91SAM7S.PDC;

package Atmel.AT91SAM7S.SPI is

   ------------------------------
   -- Public Hardware Features --
   ------------------------------

   type Peripheral_Chip_Select_Pin is (CS0, CS1, CS2, CS3);
   type Peripheral_Chip_Select_Id is mod 2 ** 4;

   Chip_Select_0 : constant Peripheral_Chip_Select_Id := 2#1110#;
   Chip_Select_1 : constant Peripheral_Chip_Select_Id := 2#1101#;
   Chip_Select_2 : constant Peripheral_Chip_Select_Id := 2#1011#;
   Chip_Select_3 : constant Peripheral_Chip_Select_Id := 2#0111#;

   type Chip_Select_Pins is array (Peripheral_Chip_Select_Pin)
     of Peripheral_Pin;

   type Endpoint_Type is (Slave, Master);
   type Peripheral_Select_Type is (Fixed, Variable);
   type Chip_Select_Decode_Type is (Direct, Multiplexed);

   type Mode_Type is record
      Master_Slave_Mode          : Endpoint_Type;
      Peripheral_Select          : Peripheral_Select_Type;
      Chip_Select_Decode         : Chip_Select_Decode_Type;
      Mode_Fault_Detection       : Enable_Type;
      Local_Loopback             : Enable_Type;
      Peripheral_Chip_Select     : Peripheral_Chip_Select_Id;
      Delay_Between_Chip_Selects : Unsigned_8;
   end record with Size => Standard'Word_Size, Volatile;

   type Clock_Polarity_Type is (Inactive_Low, Inactive_High);
   type Clock_Phase_Type is (Data_Changed_First, Data_Captured_First);

   pragma Warnings (Off, "*size clause*");
   type Bits_Per_Transfer_Type is range 8 .. 16 with Size => 4;
   pragma Warnings (On, "*size clause*");

   type Chip_Select_Type is record
      Clock_Polarity                    : Clock_Polarity_Type;
      Clock_Phase                       : Clock_Phase_Type;
      Chip_Select_Active_After_Transfer : Boolean;
      Bits_Per_Transfer                 : Bits_Per_Transfer_Type;
      Serial_Clock_Divider              : Unsigned_8;
      DLYBS                             : Unsigned_8;
      DLYBCT                            : Unsigned_8;
   end record with Size => 32;

   ----------------------
   -- Public Interface --
   ----------------------

   procedure Transmit_Data
     (To_Device              : in Peripheral_Chip_Select_Id;
      Send_Message           : in Address;
      Send_Message_Length    : in Unsigned_16;
      Recieve_Message        : in Address;
      Recieve_Message_Length : in Unsigned_16);

   procedure Initialise_Interface
     (SPI_Settings                : Mode_Type;
      Chip_Select_Pin_Assignments : in Chip_Select_Pins);

   procedure Setup_Chip_Select_Pin
     (For_Pin              : in Peripheral_Chip_Select_Pin;
      Chip_Select_Settings : in Chip_Select_Type);

private

   --------------------------
   -- SPI Memory Addresses --
   --------------------------

   SPI_Base_Address    : constant  := 16#FFFE_0000#;
   CR_Offset_Address   : constant  := 16#0000#;
   MR_Offset_Address   : constant  := 16#0004#;
   RDR_Offset_Address  : constant  := 16#0008#;
   TDR_Offset_Address  : constant  := 16#000C#;
   SR_Offset_Address   : constant  := 16#0010#;
   IER_Offset_Address  : constant  := 16#0014#;
   IDR_Offset_Address  : constant  := 16#0018#;
   IMR_Offset_Address  : constant  := 16#001C#;
   CSR0_Offset_Address : constant  := 16#0030#;
   CSR1_Offset_Address : constant  := 16#0034#;
   CSR2_Offset_Address : constant  := 16#0038#;
   CSR3_Offset_Address : constant  := 16#003C#;

   ---------------
   -- SPI Types --
   ---------------

   type Control_Register_Type is record
      SPI_Enable     : Enable_No_Change_Type;
      SPI_Disable    : Disable_No_Change_Type;
      Software_Reset : Reset_Type;
      Last_Transfer  : Boolean;
   end record with Size => Standard'Word_Size;

   type Receive_Data_Type is record
      Data                   : Unsigned_16;
      Peripheral_Chip_Select : Peripheral_Chip_Select_Id;
   end record with Size => Standard'Word_Size;

   type Transmit_Data_Type is record
      Data                   : Unsigned_16;
      Peripheral_Chip_Select : Peripheral_Chip_Select_Id;
      Last_Transfer          : Boolean;
   end record with Size => Standard'Word_Size;

   type Status_Type is record
      Receive_Data_Register_Full   : Boolean;
      Transmit_Data_Register_Empty : Boolean;
      Mode_Fault_Error             : Occured_Type;
      Overrun_Error                : Occured_Type;
      End_Of_Receive_Buffer        : Boolean;
      End_Of_Transmit_Buffer       : Boolean;
      Receive_Buffer_Full          : Boolean;
      Transmit_Buffer_Full         : Boolean;
      NSS_Rising_Edge              : Occured_Type;
      Transmission_Registers_Empty : Boolean;
      SPI_Enabled                  : Boolean;
   end record with Size => Standard'Word_Size;

   type Interrupt_Enable_Type is record
      Receive_Data_Register_Full   : Enable_No_Change_Type;
      Transmit_Data_Register_Empty : Enable_No_Change_Type;
      Mode_Fault_Error             : Enable_No_Change_Type;
      Overrun_Error                : Enable_No_Change_Type;
      End_Of_Receive_Buffer        : Enable_No_Change_Type;
      End_Of_Transmit_Buffer       : Enable_No_Change_Type;
      Receive_Buffer_Full          : Enable_No_Change_Type;
      Transmit_Buffer_Full         : Enable_No_Change_Type;
      NSS_Rising_Edge              : Enable_No_Change_Type;
      Transmission_Registers_Empty : Enable_No_Change_Type;
   end record with Size => Standard'Word_Size;

   type Interrupt_Disable_Type is record
      Receive_Data_Register_Full   : Disable_No_Change_Type;
      Transmit_Data_Register_Empty : Disable_No_Change_Type;
      Mode_Fault_Error             : Disable_No_Change_Type;
      Overrun_Error                : Disable_No_Change_Type;
      End_Of_Receive_Buffer        : Disable_No_Change_Type;
      End_Of_Transmit_Buffer       : Disable_No_Change_Type;
      Receive_Buffer_Full          : Disable_No_Change_Type;
      Transmit_Buffer_Full         : Disable_No_Change_Type;
      NSS_Rising_Edge              : Disable_No_Change_Type;
      Transmission_Registers_Empty : Disable_No_Change_Type;
   end record with Size => Standard'Word_Size;

   type Interrupt_Mask_Type is record
      Receive_Data_Register_Full   : Enabled_Type;
      Transmit_Data_Register_Empty : Enabled_Type;
      Mode_Fault_Error             : Enabled_Type;
      Overrun_Error                : Enabled_Type;
      End_Of_Receive_Buffer        : Enabled_Type;
      End_Of_Transmit_Buffer       : Enabled_Type;
      Receive_Buffer_Full          : Enabled_Type;
      Transmit_Buffer_Full         : Enabled_Type;
      NSS_Rising_Edge              : Enabled_Type;
      Transmission_Registers_Empty : Enabled_Type;
   end record with Size => Standard'Word_Size;

   ------------------------------
   -- Hardware Representations --
   ------------------------------

   for Control_Register_Type use record
      SPI_Enable     at 0 range 0 .. 0;
      SPI_Disable    at 0 range 1 .. 1;
      Software_Reset at 0 range 7 .. 7;
      Last_Transfer  at 0 range 24 .. 24;
   end record;

   for Endpoint_Type use (Slave => 0, Master => 1);
   for Peripheral_Select_Type use (Fixed => 0, Variable => 1);
   for Chip_Select_Decode_Type use (Direct => 0, Multiplexed => 1);

   for Mode_Type use record
      Master_Slave_Mode          at 0 range 0 .. 0;
      Peripheral_Select          at 0 range 1 .. 1;
      Chip_Select_Decode         at 0 range 2 .. 2;
      Mode_Fault_Detection       at 0 range 4 .. 4;
      Local_Loopback             at 0 range 7 .. 7;
      Peripheral_Chip_Select     at 0 range 16 .. 19;
      Delay_Between_Chip_Selects at 0 range 24 .. 31;
   end record;

   for Receive_Data_Type use record
      Data                   at 0 range 0 .. 15;
      Peripheral_Chip_Select at 0 range 16 .. 19;
   end record;

   for Transmit_Data_Type use record
      Data                   at 0 range 0 .. 15;
      Peripheral_Chip_Select at 0 range 16 .. 19;
      Last_Transfer          at 0 range 24 .. 24;
   end record;

   for Status_Type use record
      Receive_Data_Register_Full   at 0 range 0 .. 0;
      Transmit_Data_Register_Empty at 0 range 1 .. 1;
      Mode_Fault_Error             at 0 range 2 .. 2;
      Overrun_Error                at 0 range 3 .. 3;
      End_Of_Receive_Buffer        at 0 range 4 .. 4;
      End_Of_Transmit_Buffer       at 0 range 5 .. 5;
      Receive_Buffer_Full          at 0 range 6 .. 6;
      Transmit_Buffer_Full         at 0 range 7 .. 7;
      NSS_Rising_Edge              at 0 range 8 .. 8;
      Transmission_Registers_Empty at 0 range 9 .. 9;
      SPI_Enabled                  at 0 range 16 .. 16;
   end record;

   for Interrupt_Enable_Type use record
      Receive_Data_Register_Full   at 0 range 0 .. 0;
      Transmit_Data_Register_Empty at 0 range 1 .. 1;
      Mode_Fault_Error             at 0 range 2 .. 2;
      Overrun_Error                at 0 range 3 .. 3;
      End_Of_Receive_Buffer        at 0 range 4 .. 4;
      End_Of_Transmit_Buffer       at 0 range 5 .. 5;
      Receive_Buffer_Full          at 0 range 6 .. 6;
      Transmit_Buffer_Full         at 0 range 7 .. 7;
      NSS_Rising_Edge              at 0 range 8 .. 8;
      Transmission_Registers_Empty at 0 range 9 .. 9;
   end record;

   for Interrupt_Disable_Type use record
      Receive_Data_Register_Full   at 0 range 0 .. 0;
      Transmit_Data_Register_Empty at 0 range 1 .. 1;
      Mode_Fault_Error             at 0 range 2 .. 2;
      Overrun_Error                at 0 range 3 .. 3;
      End_Of_Receive_Buffer        at 0 range 4 .. 4;
      End_Of_Transmit_Buffer       at 0 range 5 .. 5;
      Receive_Buffer_Full          at 0 range 6 .. 6;
      Transmit_Buffer_Full         at 0 range 7 .. 7;
      NSS_Rising_Edge              at 0 range 8 .. 8;
      Transmission_Registers_Empty at 0 range 9 .. 9;
   end record;

   for Interrupt_Mask_Type use record
      Receive_Data_Register_Full   at 0 range 0 .. 0;
      Transmit_Data_Register_Empty at 0 range 1 .. 1;
      Mode_Fault_Error             at 0 range 2 .. 2;
      Overrun_Error                at 0 range 3 .. 3;
      End_Of_Receive_Buffer        at 0 range 4 .. 4;
      End_Of_Transmit_Buffer       at 0 range 5 .. 5;
      Receive_Buffer_Full          at 0 range 6 .. 6;
      Transmit_Buffer_Full         at 0 range 7 .. 7;
      NSS_Rising_Edge              at 0 range 8 .. 8;
      Transmission_Registers_Empty at 0 range 9 .. 9;
   end record;

   for Clock_Polarity_Type use (Inactive_Low => 0, Inactive_High => 1);
   for Clock_Phase_Type use (Data_Changed_First  => 0,
                             Data_Captured_First => 1);

   for Chip_Select_Type use record
      Clock_Polarity                    at 0 range 0 .. 0;
      Clock_Phase                       at 0 range 1 .. 1;
      Chip_Select_Active_After_Transfer at 0 range 3 .. 3;
      Bits_Per_Transfer                 at 0 range 4 .. 7;
      Serial_Clock_Divider              at 0 range 8 .. 15;
      DLYBS                             at 0 range 16 .. 23;
      DLYBCT                            at 0 range 24 .. 31;
   end record;

   -------------------
   -- SPI Registers --
   -------------------

   Control_Register : Control_Register_Type
     with Address => System'To_Address (SPI_Base_Address + CR_Offset_Address);

   Mode_Register : Mode_Type
     with Address => System'To_Address (SPI_Base_Address + MR_Offset_Address);

   Receive_Data_Register : Receive_Data_Type
     with Address => System'To_Address (SPI_Base_Address + RDR_Offset_Address);

   Transmit_Data_Register : Transmit_Data_Type
     with Address => System'To_Address (SPI_Base_Address + TDR_Offset_Address);

   Status_Register : Status_Type
     with Address => System'To_Address (SPI_Base_Address + SR_Offset_Address);

   Interrupt_Enable_Register : Interrupt_Enable_Type
     with Address => System'To_Address (SPI_Base_Address + IER_Offset_Address);

   Interrupt_Disable_Register : Interrupt_Disable_Type
     with Address => System'To_Address (SPI_Base_Address + IDR_Offset_Address);

   Interrupt_Mask_Register : Interrupt_Mask_Type
     with Address => System'To_Address (SPI_Base_Address + IMR_Offset_Address);

   pragma Warnings (Off, "*alignment*");
   Chip_Select_Register : array (Peripheral_Chip_Select_Pin)
     of Chip_Select_Type
       with Address =>
         System'To_Address (SPI_Base_Address + CSR0_Offset_Address);

   Receive_Pointer_Register : Address
     with Address => System'To_Address (SPI_Base_Address + RPR_Offset_Address);

   Receive_Counter_Register : Unsigned_16
     with Address => System'To_Address (SPI_Base_Address + RCR_Offset_Address);

   Transmit_Pointer_Register : Address
     with Address => System'To_Address (SPI_Base_Address + TPR_Offset_Address);

   Transmit_Counter_Register : Unsigned_16
     with Address => System'To_Address (SPI_Base_Address + TCR_Offset_Address);

   Receive_Next_Pointer_Register : Address
     with Address =>
       System'To_Address (SPI_Base_Address + RNPR_Offset_Address);

   Receive_Next_Counter_Register : Unsigned_16
     with Address =>
       System'To_Address (SPI_Base_Address + RNCR_Offset_Address);

   Transmit_Next_Pointer_Register : Address
     with Address =>
       System'To_Address (SPI_Base_Address + TNPR_Offset_Address);

   Transmit_Next_Counter_Register : Unsigned_16
     with Address =>
       System'To_Address (SPI_Base_Address + TNCR_Offset_Address);

   Transfer_Control_Register : Transfer_Control_Type
     with Address =>
       System'To_Address (SPI_Base_Address + PTCR_Offset_Address);

   Transfer_Status_Register : Transfer_Status_Type
     with Address =>
       System'To_Address (SPI_Base_Address + PTSR_Offset_Address);

   -------------------------------------
   -- Internal Driver Data Structures --
   -------------------------------------

   protected Serial_Peripheral_Interface is

      procedure Initialise_Interface
        (SPI_Settings                : Mode_Type;
         Chip_Select_Pin_Assignments : in Chip_Select_Pins);

      procedure Exchange_Data
        (With_Device            : in Peripheral_Chip_Select_Id;
         Send_Message           : in Address;
         Send_Message_Length    : in Unsigned_16;
         Recieve_Message        : in Address;
         Recieve_Message_Length : in Unsigned_16);

      procedure Setup_Chip_Select_Pin
        (For_Pin              : in Peripheral_Chip_Select_Pin;
         Chip_Select_Settings : in Chip_Select_Type);

      entry Wait_For_Transmission;

   private

      procedure Interface_Handler;
      pragma Attach_Handler (Interface_Handler, P_SPI);

      Transmit_Buffer        : Address;
      Transmit_Buffer_Length : Natural;
      Recieve_Buffer         : Address;
      Recieve_Buffer_Length  : Natural;

      Transfer_Completed : Boolean := False;

   end Serial_Peripheral_Interface;

end Atmel.AT91SAM7S.SPI;
