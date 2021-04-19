------------------------------------------------------------------------------------------
--                                                                                      --
--                            ACTON PROCESSOR SUPPORT PACKAGE                           --
--                                                                                      --
--                                  ATMEL.AT91SAM7S.TWI                                 --
--                                                                                      --
--                       Copyright (C) 2014-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

with Interfaces; use Interfaces;
with System;     use System;

with Ada.Interrupts; use Ada.Interrupts;

package Atmel.AT91SAM7S.TWI is

   -----------------------
   -- Hardware Features --
   -----------------------

   type TWI_Device_Address is mod 2 ** 7;
   type TWI_Internal_Address is mod 2 ** 32 with Size => 32;
   type Internal_Device_Address_Range is range 0 .. 3 with Size => 2;
   type Communication_Direction is (Write, Read);
   type Clock_Divider_Type is mod 2 ** 3;

   No_Internal_Address : constant TWI_Internal_Address := 0;

   for Communication_Direction use (Write => 0, Read => 1);

   ----------------------
   -- Public Interface --
   ----------------------

   procedure Send_Message
     (To               : in TWI_Device_Address;
      Internal_Address : in TWI_Internal_Address;
      Address_Size     : in Internal_Device_Address_Range;
      Message          : in Address;
      Message_Length   : in Natural);

   procedure Receive_Message
     (From             : in TWI_Device_Address;
      Internal_Address : in TWI_Internal_Address;
      Address_Size     : in Internal_Device_Address_Range;
      Message          : in Address;
      Message_Length   : in Natural);

   procedure Initialise_Interface
     (Clock_Divider      : Clock_Divider_Type;
      Clock_Low_Divider  : Unsigned_8;
      Clock_High_Divider : Unsigned_8);

private

   --------------------------
   -- TWI Memory Addresses --
   --------------------------

   TWI_Base_Address    : constant  := 16#FFFB_8000#;
   CR_Offset_Address   : constant  := 16#0000#;
   MMR_Offset_Address  : constant  := 16#0004#;
   IADR_Offset_Address : constant  := 16#000C#;
   CWGR_Offset_Address : constant  := 16#0010#;
   SR_Offset_Address   : constant  := 16#0020#;
   IER_Offset_Address  : constant  := 16#0024#;
   IDR_Offset_Address  : constant  := 16#0028#;
   IMR_Offset_Address  : constant  := 16#002C#;
   RHR_Offset_Address  : constant  := 16#0030#;
   THR_Offset_Address  : constant  := 16#0034#;

   ---------------
   -- TWI Types --
   ---------------

   --  TWI Control Register (TWI_CR)

   type Send_Condition is (No, Yes);

   type Control_Register_Type is record
      Start                    : Send_Condition;
      Stop                     : Send_Condition;
      Master_Transfer_Enabled  : Boolean;
      Master_Transfer_Disabled : Boolean;
      Software_Reset           : Boolean;
   end record with Size => Standard'Word_Size;

   --  TWI Master Mode Register (TWI_MMR)

   type Master_Mode_Type is record
      Internal_Device_Address_Size : Internal_Device_Address_Range;
      Master_Read_Direction        : Communication_Direction;
      Device_Address               : TWI_Device_Address;
   end record with Size => Standard'Word_Size;

   --  TWI Internal Address Register (TWI_IADR)

   type Internal_Address_Type is record
      Internal_Address :  TWI_Internal_Address;
   end record with Size => Standard'Word_Size;

   --  TWI Clock Waveform Generator Register (TWI_CWGR)

   type Clock_Waveform_Generator_Type is record
      Clock_Low_Divider  : Unsigned_8;
      Clock_High_Divider : Unsigned_8;
      Clock_Divider      : Clock_Divider_Type;
   end record with Size => Standard'Word_Size;

   --  TWI Status Register (TWI_SR)

   type Status_Register_Type is record
      Transmission_Completed          : Boolean;
      Receive_Holding_Register_Ready  : Boolean;
      Transmit_Holding_Register_Ready : Boolean;
      Not_Acknowledged                : Boolean;
   end record with Size => Register_Size;

   --  TWI Interrupt Enable Register (TWI_IER)

   type Interrupt_Enable_Type is record
      Transmission_Completed          : Enable_No_Change_Type;
      Receive_Holding_Register_Ready  : Enable_No_Change_Type;
      Transmit_Holding_Register_Ready : Enable_No_Change_Type;
      Not_Acknowledged                : Enable_No_Change_Type;
   end record with Size => Standard'Word_Size;

   --  TWI Interrupt Disable Register (TWI_IDR)

   type Interrupt_Disable_Type is record
      Transmission_Completed          : Disable_No_Change_Type;
      Receive_Holding_Register_Ready  : Disable_No_Change_Type;
      Transmit_Holding_Register_Ready : Disable_No_Change_Type;
      Not_Acknowledged                : Disable_No_Change_Type;
   end record with Size => Standard'Word_Size;

   --  TWI Receive Holding Register

   type Receive_Holding_Type is record
      Data : Unsigned_8;
   end record with Size => Standard'Word_Size;

   --  TWI Transmit Holding Register

   type Transmit_Holding_Type is record
      Data : Unsigned_8;
   end record with Size => Standard'Word_Size;

   ------------------------------
   -- Hardware Representations --
   ------------------------------

   for Send_Condition use (No => 0, Yes => 1);

   for Control_Register_Type use record
      Start                    at 0 range 0 .. 0;
      Stop                     at 0 range 1 .. 1;
      Master_Transfer_Enabled  at 0 range 2 .. 2;
      Master_Transfer_Disabled at 0 range 3 .. 3;
      Software_Reset           at 0 range 7 .. 7;
   end record;

   for Master_Mode_Type use record
      Internal_Device_Address_Size at 0 range 8 .. 9;
      Master_Read_Direction        at 0 range 12 .. 12;
      Device_Address               at 0 range 16 .. 22;
   end record;

   for Internal_Address_Type use record
      Internal_Address at 0 range 0 .. 31;
   end record;

   for Clock_Waveform_Generator_Type use record
      Clock_Low_Divider  at 0 range 0 .. 7;
      Clock_High_Divider at 0 range 8 .. 15;
      Clock_Divider      at 0 range 16 .. 18;
   end record;

   for Status_Register_Type use record
      Transmission_Completed          at 0 range 0 .. 0;
      Receive_Holding_Register_Ready  at 0 range 1 .. 1;
      Transmit_Holding_Register_Ready at 0 range 2 .. 2;
      Not_Acknowledged                at 0 range 8 .. 8;
   end record;

   for Interrupt_Enable_Type use record
      Transmission_Completed          at 0 range 0 .. 0;
      Receive_Holding_Register_Ready  at 0 range 1 .. 1;
      Transmit_Holding_Register_Ready at 0 range 2 .. 2;
      Not_Acknowledged                at 0 range 8 .. 8;
   end record;

   for Interrupt_Disable_Type use record
      Transmission_Completed          at 0 range 0 .. 0;
      Receive_Holding_Register_Ready  at 0 range 1 .. 1;
      Transmit_Holding_Register_Ready at 0 range 2 .. 2;
      Not_Acknowledged                at 0 range 8 .. 8;
   end record;

   for Receive_Holding_Type use record
      Data at 0 range 0 .. 7;
   end record;

   for Transmit_Holding_Type use record
      Data at 0 range 0 .. 7;
   end record;

   -------------------
   -- TWI Registers --
   -------------------

   Control_Register : Control_Register_Type
     with Address => System'To_Address (TWI_Base_Address + CR_Offset_Address);

   Master_Mode_Register : Master_Mode_Type
     with Address => System'To_Address (TWI_Base_Address + MMR_Offset_Address);

   Internal_Address_Register : Internal_Address_Type
     with Address =>
       System'To_Address (TWI_Base_Address + IADR_Offset_Address);

   Clock_Waveform_Generator_Register : Clock_Waveform_Generator_Type
     with Address =>
       System'To_Address (TWI_Base_Address + CWGR_Offset_Address);

   Status_Register : Status_Register_Type
     with Address => System'To_Address (TWI_Base_Address + SR_Offset_Address);

   Interrupt_Enable_Register : Interrupt_Enable_Type
     with Address => System'To_Address (TWI_Base_Address + IER_Offset_Address);

   Interrupt_Disable_Register : Interrupt_Disable_Type
     with Address => System'To_Address (TWI_Base_Address + IDR_Offset_Address);

   Interrupt_Mask_Register : Status_Register_Type
     with Address => System'To_Address (TWI_Base_Address + IMR_Offset_Address);

   Receive_Holding_Register : Receive_Holding_Type
     with Address => System'To_Address (TWI_Base_Address + RHR_Offset_Address);

   Transmit_Holding_Register : Transmit_Holding_Type
     with Address => System'To_Address (TWI_Base_Address + THR_Offset_Address);

   -------------------------------------
   -- Internal Driver Data Structures --
   -------------------------------------

   protected Two_Wire_Interface
--       with Interrupt_Priority => System.Max_Interrupt_Priority
   is

      procedure Initialise_Interface
        (Clock_Divider      : Clock_Divider_Type;
         Clock_Low_Divider  : Unsigned_8;
         Clock_High_Divider : Unsigned_8);

      procedure Transmit_Data
        (With_Device      : in TWI_Device_Address;
         Internal_Address : in TWI_Internal_Address;
         Address_Size     : in Internal_Device_Address_Range;
         Data             : in Address;
         Data_Length      : in Natural;
         Direction        : in Communication_Direction);

      entry Wait_For_Transmission;

   private
      pragma Interrupt_Priority (System.Max_Interrupt_Priority);

      procedure Interface_Handler;
      pragma Attach_Handler (Interface_Handler, P_TWI);

      Buffer        : Address;
      Buffer_Length : Natural;

      Transfer_Completed : Boolean := False;

   end Two_Wire_Interface;

   function "and" (L, R : Status_Register_Type) return Status_Register_Type;

end Atmel.AT91SAM7S.TWI;
