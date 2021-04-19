------------------------------------------------------------------------------------------
--                                                                                      --
--                            ACTON PROCESSOR SUPPORT PACKAGE                           --
--                                                                                      --
--                                ATMEL.AT91SAM7S.USART                                 --
--                                                                                      --
--                       Copyright (C) 2014-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

with Interfaces; use Interfaces;
with System;     use System;
with Atmel.AT91SAM7S.PDC; use Atmel.AT91SAM7S.PDC;
with Ada.Interrupts; use Ada.Interrupts;

package Atmel.AT91SAM7S.USART is

   ------------------------------
   -- Public Hardware Features --
   ------------------------------

   type USART_Id is range 0 .. 1;

   pragma Warnings (Off, "*biased*");
   type USART_Modes is (Normal, RS485, Hardware_Handshaking, Modem, IS07816_T0,
                        IS07816_T1, IrDA);
   type Clock_Selections is (Master_Clock, Master_Clock_Divided, Serial_Clock);
   type Character_Length_Bits is range 5 .. 8 with Size => 2;
   type Synchronous_Type is (Asynchronous_Mode, Synchronous_Mode);
   type Parity_Type is (Even, Odd, Force_0, Force_1, None, Multidrop);
   type Stop_Bits_Quantity is (One, One_And_Half, Two);
   type Channel_Type is (Normal, Automatic_Echo,
                         Local_Loopback, Remote_Loopback);
   type Serial_Bit_Order is (Low_Order_First, High_Order_First);
   type Clock_Driver is (Not_Driven, Driven);
   type Oversampling_Type is (O16_Bits, O8bits);
   type Iterations is mod 2 ** 3;
   pragma Warnings (On, "*biased*");

   type Mode_Type is record
      USART_Mode                         : USART_Modes;
      Clock_Selection                    : Clock_Selections;
      Character_Length                   : Character_Length_Bits;
      Synchronous_Mode_Select            : Synchronous_Type;
      Parity                             : Parity_Type;
      Number_Of_Stop_Bits                : Stop_Bits_Quantity;
      Channel_Mode                       : Channel_Type;
      Order_Of_Bits                      : Serial_Bit_Order;
      Nine_Bit_Character_Length          : Boolean;
      Clock_Output_Select                : Clock_Driver;
      Oversampling_Mode                  : Oversampling_Type;
      Inhibit_Non_Acknowledge            : Boolean;
      Disable_Successive_Non_Acknowledge : Boolean;
      Max_Iterations                     : Iterations;
      Infrared_Receive_Line_Filter       : Enable_Type;
   end record with Size => Standard'Word_Size;

   type Fractional_Part_Type is mod 2 ** 3;

   type Baud_Rate_Generator_Type is record
      Clock_Divider   : Unsigned_16;
      Fractional_Part : Fractional_Part_Type;
   end record with Size => Standard'Word_Size;

   ----------------------
   -- Public Interface --
   ----------------------

   procedure Initialise_Interface
     (Interface_Id     : in USART_Id;
      USART_Settings   : in Mode_Type;
      Receiver_Timeout : in Unsigned_16;
      Baud_Rate        : in Baud_Rate_Generator_Type);

   procedure Reset_Interface (Interface_Id : in USART_Id);

   procedure Exchange_Data
     (Using_Interface        : in USART_Id;
      Send_Message           : in Address;
      Send_Message_Length    : in Unsigned_16;
      Recieve_Message        : in Address;
      Recieve_Message_Length : in Unsigned_16);

   function Interface_Is_Ready (Interface_Id : USART_Id) return Boolean;

   function Receive_Bytes_Remaining (On_Interface : USART_Id) return Natural;

private

   ----------------------------
   -- USART Memory Addresses --
   ----------------------------

   USART_Base_Address  : constant  := 16#FFFC_0000#;
   CR_Offset_Address   : constant  := 16#0000#;
   MR_Offset_Address   : constant  := 16#0004#;
   IER_Offset_Address  : constant  := 16#0008#;
   IDR_Offset_Address  : constant  := 16#000C#;
   IMR_Offset_Address  : constant  := 16#0010#;
   CSR_Offset_Address  : constant  := 16#0014#;
   RHR_Offset_Address  : constant  := 16#0018#;
   THR_Offset_Address  : constant  := 16#001C#;
   BRGR_Offset_Address : constant  := 16#0020#;
   RTOR_Offset_Address : constant  := 16#0024#;
   TTGR_Offset_Address : constant  := 16#0028#;
   FIDI_Offset_Address : constant  := 16#0040#;
   NER_Offset_Address  : constant  := 16#0044#;
   IF_Offset_Address   : constant  := 16#004C#;

   -----------------
   -- USART Types --
   -----------------

   type Wide_Start_Stop is (No_Change, Start, Stop);
   for Wide_Start_Stop use (No_Change => 2#00#, Start => 2#01#, Stop => 2#10#);

   type Control_Register_Type is record
      Reset_Receiver            : Boolean;
      Reset_Transmitter         : Boolean;
      Receiver_State            : Wide_Enable_Type;
      Transmitter_State         : Wide_Enable_Type;
      Reset_Status_Bits         : Boolean;
      Break                     : Wide_Start_Stop;
      Start_Time_Out            : Boolean;
      Send_Address              : Boolean;
      Reset_Iterations          : Boolean;
      Reset_Non_Acknowledge     : Boolean;
      Rearm_Time_Out            : Boolean;
      Data_Terminal_Ready_State : Wide_Enable_Type;
      Request_To_Send_State     : Wide_Enable_Type;
   end record with Size => Standard'Word_Size;

   type Interrupt_Enable_Type is record
      Receive_Ready                    : Enable_No_Change_Type;
      Transmit_Ready                   : Enable_No_Change_Type;
      Receiver_Break                   : Enable_No_Change_Type;
      End_Of_Receive_Transfer          : Enable_No_Change_Type;
      End_Of_Transmitter_Transfer      : Enable_No_Change_Type;
      Overrun_Error                    : Enable_No_Change_Type;
      Framing_Error                    : Enable_No_Change_Type;
      Parity_Error                     : Enable_No_Change_Type;
      Time_Out                         : Enable_No_Change_Type;
      Transmit_Empty                   : Enable_No_Change_Type;
      Iteration                        : Enable_No_Change_Type;
      Transmit_Buffer_Empty            : Enable_No_Change_Type;
      Receive_Buffer_Full              : Enable_No_Change_Type;
      Non_Acknowledge                  : Enable_No_Change_Type;
      Ring_Indicator_Input_Change      : Enable_No_Change_Type;
      Data_Set_Ready_Input_Change      : Enable_No_Change_Type;
      Data_Carrier_Detect_Input_Change : Enable_No_Change_Type;
      Clear_To_Send_Input_Change       : Enable_No_Change_Type;
   end record with Size => Standard'Word_Size;

   type Interrupt_Disable_Type is record
      Receive_Ready                    : Disable_No_Change_Type;
      Transmit_Ready                   : Disable_No_Change_Type;
      Receiver_Break                   : Disable_No_Change_Type;
      End_Of_Receive_Transfer          : Disable_No_Change_Type;
      End_Of_Transmitter_Transfer      : Disable_No_Change_Type;
      Overrun_Error                    : Disable_No_Change_Type;
      Framing_Error                    : Disable_No_Change_Type;
      Parity_Error                     : Disable_No_Change_Type;
      Time_Out                         : Disable_No_Change_Type;
      Transmit_Empty                   : Disable_No_Change_Type;
      Iteration                        : Disable_No_Change_Type;
      Transmit_Buffer_Empty            : Disable_No_Change_Type;
      Receive_Buffer_Full              : Disable_No_Change_Type;
      Non_Acknowledge                  : Disable_No_Change_Type;
      Ring_Indicator_Input_Change      : Disable_No_Change_Type;
      Data_Set_Ready_Input_Change      : Disable_No_Change_Type;
      Data_Carrier_Detect_Input_Change : Disable_No_Change_Type;
      Clear_To_Send_Input_Change       : Disable_No_Change_Type;
   end record with Size => Standard'Word_Size;

   type Interrupt_Mask_Type is record
      Receive_Ready                    : Enabled_Type;
      Transmit_Ready                   : Enabled_Type;
      Receiver_Break                   : Enabled_Type;
      End_Of_Receive_Transfer          : Enabled_Type;
      End_Of_Transmitter_Transfer      : Enabled_Type;
      Overrun_Error                    : Enabled_Type;
      Framing_Error                    : Enabled_Type;
      Parity_Error                     : Enabled_Type;
      Time_Out                         : Enabled_Type;
      Transmit_Empty                   : Enabled_Type;
      Iteration                        : Enabled_Type;
      Transmit_Buffer_Empty            : Enabled_Type;
      Receive_Buffer_Full              : Enabled_Type;
      Non_Acknowledge                  : Enabled_Type;
      Ring_Indicator_Input_Change      : Enabled_Type;
      Data_Set_Ready_Input_Change      : Enabled_Type;
      Data_Carrier_Detect_Input_Change : Enabled_Type;
      Clear_To_Send_Input_Change       : Enabled_Type;
   end record with Size => Standard'Word_Size;

   type Channel_Status_Type is record
      Receive_Ready                    : Boolean;
      Transmit_Ready                   : Boolean;
      Receiver_Break                   : Boolean;
      End_Of_Receive_Transfer          : Boolean;
      End_Of_Transmitter_Transfer      : Boolean;
      Overrun_Error                    : Boolean;
      Framing_Error                    : Boolean;
      Parity_Error                     : Boolean;
      Time_Out                         : Boolean;
      Transmit_Empty                   : Boolean;
      Iteration                        : Boolean;
      Transmit_Buffer_Empty            : Boolean;
      Receive_Buffer_Full              : Boolean;
      Non_Acknowledge                  : Boolean;
      Ring_Indicator_Input_Change      : Boolean;
      Data_Set_Ready_Input_Change      : Boolean;
      Data_Carrier_Detect_Input_Change : Boolean;
      Clear_To_Send_Input_Change       : Boolean;
      RI                               : Pin_Status;
      DSR                              : Pin_Status;
      DCD                              : Pin_Status;
      CTS                              : Pin_Status;
   end record with Size => Standard'Word_Size;

   type Sync_Type is (Data, Command);

   type Receive_Holding_Type is record
      Received_Character : Unsigned_8;
      Received_Sync      : Sync_Type;
   end record with Size => Standard'Word_Size;

   type Transmit_Holding_Type is record
      Transmit_Character : Unsigned_8;
      Transmit_Sync      : Sync_Type;
   end record with Size => Standard'Word_Size;

   type Receiver_Time_Out_Type is record
      Value : Unsigned_16;
   end record with Size => Standard'Word_Size;

   type Transmitter_Timeguard_Type is record
      Value : Unsigned_16;
   end record with Size => Standard'Word_Size;

   type FIDI_Type is mod 2 ** 11;

   type FI_Over_DI_Ratio_Type is record
      Value : FIDI_Type;
   end record with Size => Standard'Word_Size;

   type Number_Of_Errors_Type is record
      Value : Unsigned_8;
   end record with Size => Standard'Word_Size;

   type IrDA_Filter_Type is record
      Filter : Unsigned_8;
   end record with Size => Standard'Word_Size;

   ------------------------------
   -- Hardware Representations --
   ------------------------------

   for Control_Register_Type use record
      Reset_Receiver            at 0 range 2 .. 2;
      Reset_Transmitter         at 0 range 3 .. 3;
      Receiver_State            at 0 range 4 .. 5;
      Transmitter_State         at 0 range 6 .. 7;
      Reset_Status_Bits         at 0 range 8 .. 8;
      Break                     at 0 range 9 .. 10;
      Start_Time_Out            at 0 range 11 .. 11;
      Send_Address              at 0 range 12 .. 12;
      Reset_Iterations          at 0 range 13 .. 13;
      Reset_Non_Acknowledge     at 0 range 14 .. 14;
      Rearm_Time_Out            at 0 range 15 .. 15;
      Data_Terminal_Ready_State at 0 range 16 .. 17;
      Request_To_Send_State     at 0 range 18 .. 19;
   end record;

   for USART_Modes use (Normal               => 2#0000#,
                        RS485                => 2#0001#,
                        Hardware_Handshaking => 2#0010#,
                        Modem                => 2#0011#,
                        IS07816_T0           => 2#0100#,
                        IS07816_T1           => 2#0110#,
                        IrDA                 => 2#1000#);

   for Clock_Selections use (Master_Clock         => 2#00#,
                             Master_Clock_Divided => 2#01#,
                             Serial_Clock         => 2#11#);

   for Synchronous_Type use (Asynchronous_Mode => 0, Synchronous_Mode => 1);

   for Parity_Type use (Even      => 2#000#,
                        Odd       => 2#001#,
                        Force_0   => 2#010#,
                        Force_1   => 2#011#,
                        None      => 2#100#,
                        Multidrop => 2#110#);

   for Stop_Bits_Quantity use (One => 0, One_And_Half => 1, Two => 2);
   for Channel_Type use (Normal          => 0,
                         Automatic_Echo  => 1,
                         Local_Loopback  => 2,
                         Remote_Loopback => 3);
   for Clock_Driver use (Not_Driven => 0, Driven => 1);
   for Oversampling_Type use (O16_Bits => 0, O8bits => 1);

   for Mode_Type use record
      USART_Mode                         at 0 range 0 .. 3;
      Clock_Selection                    at 0 range 4 .. 5;
      Character_Length                   at 0 range 6 .. 7;
      Synchronous_Mode_Select            at 0 range 8 .. 8;
      Parity                             at 0 range 9 .. 11;
      Number_Of_Stop_Bits                at 0 range 12 .. 13;
      Channel_Mode                       at 0 range 14 .. 15;
      Order_Of_Bits                      at 0 range 16 .. 16;
      Nine_Bit_Character_Length          at 0 range 17 .. 17;
      Clock_Output_Select                at 0 range 18 .. 18;
      Oversampling_Mode                  at 0 range 19 .. 19;
      Inhibit_Non_Acknowledge            at 0 range 20 .. 20;
      Disable_Successive_Non_Acknowledge at 0 range 21 .. 21;
      Max_Iterations                     at 0 range 24 .. 26;
      Infrared_Receive_Line_Filter       at 0 range 28 .. 28;
   end record;

   for Interrupt_Enable_Type use record
      Receive_Ready                    at 0 range 0 .. 0;
      Transmit_Ready                   at 0 range 1 .. 1;
      Receiver_Break                   at 0 range 2 .. 2;
      End_Of_Receive_Transfer          at 0 range 3 .. 3;
      End_Of_Transmitter_Transfer      at 0 range 4 .. 4;
      Overrun_Error                    at 0 range 5 .. 5;
      Framing_Error                    at 0 range 6 .. 6;
      Parity_Error                     at 0 range 7 .. 7;
      Time_Out                         at 0 range 8 .. 8;
      Transmit_Empty                   at 0 range 9 .. 9;
      Iteration                        at 0 range 10 .. 10;
      Transmit_Buffer_Empty            at 0 range 11 .. 11;
      Receive_Buffer_Full              at 0 range 12 .. 12;
      Non_Acknowledge                  at 0 range 13 .. 13;
      Ring_Indicator_Input_Change      at 0 range 16 .. 16;
      Data_Set_Ready_Input_Change      at 0 range 17 .. 17;
      Data_Carrier_Detect_Input_Change at 0 range 18 .. 18;
      Clear_To_Send_Input_Change       at 0 range 19 .. 19;
   end record;

   for Interrupt_Disable_Type use record
      Receive_Ready                    at 0 range 0 .. 0;
      Transmit_Ready                   at 0 range 1 .. 1;
      Receiver_Break                   at 0 range 2 .. 2;
      End_Of_Receive_Transfer          at 0 range 3 .. 3;
      End_Of_Transmitter_Transfer      at 0 range 4 .. 4;
      Overrun_Error                    at 0 range 5 .. 5;
      Framing_Error                    at 0 range 6 .. 6;
      Parity_Error                     at 0 range 7 .. 7;
      Time_Out                         at 0 range 8 .. 8;
      Transmit_Empty                   at 0 range 9 .. 9;
      Iteration                        at 0 range 10 .. 10;
      Transmit_Buffer_Empty            at 0 range 11 .. 11;
      Receive_Buffer_Full              at 0 range 12 .. 12;
      Non_Acknowledge                  at 0 range 13 .. 13;
      Ring_Indicator_Input_Change      at 0 range 16 .. 16;
      Data_Set_Ready_Input_Change      at 0 range 17 .. 17;
      Data_Carrier_Detect_Input_Change at 0 range 18 .. 18;
      Clear_To_Send_Input_Change       at 0 range 19 .. 19;
   end record;

   for Interrupt_Mask_Type use record
      Receive_Ready                    at 0 range 0 .. 0;
      Transmit_Ready                   at 0 range 1 .. 1;
      Receiver_Break                   at 0 range 2 .. 2;
      End_Of_Receive_Transfer          at 0 range 3 .. 3;
      End_Of_Transmitter_Transfer      at 0 range 4 .. 4;
      Overrun_Error                    at 0 range 5 .. 5;
      Framing_Error                    at 0 range 6 .. 6;
      Parity_Error                     at 0 range 7 .. 7;
      Time_Out                         at 0 range 8 .. 8;
      Transmit_Empty                   at 0 range 9 .. 9;
      Iteration                        at 0 range 10 .. 10;
      Transmit_Buffer_Empty            at 0 range 11 .. 11;
      Receive_Buffer_Full              at 0 range 12 .. 12;
      Non_Acknowledge                  at 0 range 13 .. 13;
      Ring_Indicator_Input_Change      at 0 range 16 .. 16;
      Data_Set_Ready_Input_Change      at 0 range 17 .. 17;
      Data_Carrier_Detect_Input_Change at 0 range 18 .. 18;
      Clear_To_Send_Input_Change       at 0 range 19 .. 19;
   end record;

   for Channel_Status_Type use record
      Receive_Ready                    at 0 range 0 .. 0;
      Transmit_Ready                   at 0 range 1 .. 1;
      Receiver_Break                   at 0 range 2 .. 2;
      End_Of_Receive_Transfer          at 0 range 3 .. 3;
      End_Of_Transmitter_Transfer      at 0 range 4 .. 4;
      Overrun_Error                    at 0 range 5 .. 5;
      Framing_Error                    at 0 range 6 .. 6;
      Parity_Error                     at 0 range 7 .. 7;
      Time_Out                         at 0 range 8 .. 8;
      Transmit_Empty                   at 0 range 9 .. 9;
      Iteration                        at 0 range 10 .. 10;
      Transmit_Buffer_Empty            at 0 range 11 .. 11;
      Receive_Buffer_Full              at 0 range 12 .. 12;
      Non_Acknowledge                  at 0 range 13 .. 13;
      Ring_Indicator_Input_Change      at 0 range 16 .. 16;
      Data_Set_Ready_Input_Change      at 0 range 17 .. 17;
      Data_Carrier_Detect_Input_Change at 0 range 18 .. 18;
      Clear_To_Send_Input_Change       at 0 range 19 .. 19;
      RI                               at 0 range 20 .. 20;
      DSR                              at 0 range 21 .. 21;
      DCD                              at 0 range 22 .. 22;
      CTS                              at 0 range 23 .. 23;
   end record;

   for Sync_Type use (Data => 0, Command => 1);

   for Receive_Holding_Type use record
      Received_Character at 0 range 0 .. 7;
      Received_Sync      at 0 range 15 .. 15;
   end record;

   for Transmit_Holding_Type use record
      Transmit_Character at 0 range 0 .. 7;
      Transmit_Sync      at 0 range 15 .. 15;
   end record;

   for Baud_Rate_Generator_Type use record
      Clock_Divider   at 0 range 0 .. 15;
      Fractional_Part at 0 range 16 .. 18;
   end record;

   for Receiver_Time_Out_Type use record
      Value at 0 range 0 .. 15;
   end record;

   for Transmitter_Timeguard_Type use record
      Value at 0 range 0 .. 15;
   end record;

   for FI_Over_DI_Ratio_Type use record
      Value at 0 range 0 .. 10;
   end record;

   for Number_Of_Errors_Type use record
      Value at 0 range 0 .. 7;
   end record;

   for IrDA_Filter_Type use record
      Filter at 0 range 0 .. 7;
   end record;

   ---------------------
   -- USART Registers --
   ---------------------
   pragma Warnings (Off, "*unused*");
   pragma Warnings (Off, "*alignment*");

   type USART_Type is record
      Control_Register               : Control_Register_Type;
      Mode_Register                  : Mode_Type;
      Interrupt_Enable_Register      : Interrupt_Enable_Type;
      Interrupt_Disable_Register     : Interrupt_Disable_Type;
      Interrupt_Mask_Register        : Interrupt_Mask_Type;
      Channel_Status_Register        : Channel_Status_Type;
      Receive_Holding_Register       : Receive_Holding_Type;
      Transmit_Holding_Register      : Transmit_Holding_Type;
      Baud_Rate_Generator_Register   : Baud_Rate_Generator_Type;
      Receiver_Time_Out_Register     : Receiver_Time_Out_Type;
      Transmitter_Timeguard_Register : Transmitter_Timeguard_Type;
      FI_Over_DI_Ratio_Register      : FI_Over_DI_Ratio_Type;
      Number_Of_Errors_Register      : Number_Of_Errors_Type;
      IrDA_Filter_Register           : IrDA_Filter_Type;
      Receive_Pointer_Register       : Address;
      Receive_Counter_Register       : Unsigned_16;
      Transmit_Pointer_Register      : Address;
      Transmit_Counter_Register      : Unsigned_16;
      Receive_Next_Pointer_Register  : Address;
      Receive_Next_Counter_Register  : Unsigned_16;
      Transmit_Next_Pointer_Register : Address;
      Transmit_Next_Counter_Register : Unsigned_16;
      Transfer_Control_Register      : Transfer_Control_Type;
      Transfer_Status_Register       : Transfer_Status_Type;
   end record with Size => 16#4000# * 8;

   for USART_Type use record
      Control_Register               at CR_Offset_Address   range 0 .. 31;
      Mode_Register                  at MR_Offset_Address   range 0 .. 31;
      Interrupt_Enable_Register      at IER_Offset_Address  range 0 .. 31;
      Interrupt_Disable_Register     at IDR_Offset_Address  range 0 .. 31;
      Interrupt_Mask_Register        at IMR_Offset_Address  range 0 .. 31;
      Channel_Status_Register        at CSR_Offset_Address  range 0 .. 31;
      Receive_Holding_Register       at RHR_Offset_Address  range 0 .. 31;
      Transmit_Holding_Register      at THR_Offset_Address  range 0 .. 31;
      Baud_Rate_Generator_Register   at BRGR_Offset_Address range 0 .. 31;
      Receiver_Time_Out_Register     at RTOR_Offset_Address range 0 .. 31;
      Transmitter_Timeguard_Register at TTGR_Offset_Address range 0 .. 31;
      FI_Over_DI_Ratio_Register      at FIDI_Offset_Address range 0 .. 31;
      Number_Of_Errors_Register      at NER_Offset_Address  range 0 .. 31;
      IrDA_Filter_Register           at IF_Offset_Address   range 0 .. 31;
      Receive_Pointer_Register       at RPR_Offset_Address  range 0 .. 31;
      Receive_Counter_Register       at RCR_Offset_Address  range 0 .. 15;
      Transmit_Pointer_Register      at TPR_Offset_Address  range 0 .. 31;
      Transmit_Counter_Register      at TCR_Offset_Address  range 0 .. 15;
      Receive_Next_Pointer_Register  at RNPR_Offset_Address range 0 .. 31;
      Receive_Next_Counter_Register  at RNCR_Offset_Address range 0 .. 15;
      Transmit_Next_Pointer_Register at TNPR_Offset_Address range 0 .. 31;
      Transmit_Next_Counter_Register at TNCR_Offset_Address range 0 .. 15;
      Transfer_Control_Register      at PTCR_Offset_Address range 0 .. 31;
      Transfer_Status_Register       at PTSR_Offset_Address range 0 .. 31;
   end record;

   USART : array (USART_Id) of USART_Type
     with Address => System'To_Address (USART_Base_Address);

   -------------------------------------
   -- Internal Driver Data Structures --
   -------------------------------------

--     protected USART_Interface_0 is
--        procedure Initialise_Interface
--          (USART_Settings : in Mode_Type;
--           Baud_Rate      : in Baud_Rate_Generator_Type);
--
--        procedure Exchange_Data
--          (Send_Message           : in Address;
--           Send_Message_Length    : in Unsigned_16;
--           Recieve_Message        : in Address;
--           Recieve_Message_Length : in Unsigned_16);
--
--        entry Wait_For_Transmission;
--
--     private
--
--        procedure Interface_Handler;
--        pragma Attach_Handler (Interface_Handler, US0_Id);
--
--        Transmit_Buffer        : Address;
--        Transmit_Buffer_Length : Natural;
--        Recieve_Buffer         : Address;
--        Recieve_Buffer_Length  : Natural;
--
--        Transfer_Completed : Boolean := False;
--
--     end USART_Interface_0;

   protected USART_Interface_1 is
      procedure Initialise_Interface
        (USART_Settings   : in Mode_Type;
         Receiver_Timeout : in Unsigned_16;
         Baud_Rate        : in Baud_Rate_Generator_Type);

      procedure Exchange_Data
        (Send_Message           : in Address;
         Send_Message_Length    : in Unsigned_16;
         Recieve_Message        : in Address;
         Recieve_Message_Length : in Unsigned_16);

      entry Wait_For_Transmission;

      procedure Reset_Interface;

   private
      pragma Interrupt_Priority (Max_Interrupt_Priority);

      procedure Interface_Handler;
      pragma Attach_Handler (Interface_Handler, P_US1);

      Transmit_Buffer        : Address;
      Transmit_Buffer_Length : Natural;
      Recieve_Buffer         : Address;
      Recieve_Buffer_Length  : Natural;

      Transfer_Completed : Boolean := False;

   end USART_Interface_1;

end Atmel.AT91SAM7S.USART;
