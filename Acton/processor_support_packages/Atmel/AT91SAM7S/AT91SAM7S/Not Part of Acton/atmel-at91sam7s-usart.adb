------------------------------------------------------------------------------------------
--                                                                                      --
--                            ACTON PROCESSOR SUPPORT PACKAGE                           --
--                                                                                      --
--                                ATMEL.AT91SAM7S.USART                                 --
--                                                                                      --
--                       Copyright (C) 2014-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

with Atmel.AT91SAM7S.AIC;
with Atmel.AT91SAM7S.PIO; use Atmel.AT91SAM7S.PIO;
with Atmel.AT91SAM7S.PMC; use Atmel.AT91SAM7S.PMC;

package body Atmel.AT91SAM7S.USART is
   procedure Initialise_Interface
     (Interface_Id     : in USART_Id;
      USART_Settings   : in Mode_Type;
      Receiver_Timeout : in Unsigned_16;
      Baud_Rate        : in Baud_Rate_Generator_Type) is
   begin
      case Interface_Id is
         when 0 =>
            null;
--              USART_Interface_0.Initialise_Interface
--                (USART_Settings, Baud_Rate);
         when 1 =>
            USART_Interface_1.Initialise_Interface
              (USART_Settings,
               Receiver_Timeout,
               Baud_Rate);
      end case;
   end Initialise_Interface;

   procedure Reset_Interface (Interface_Id : in USART_Id) is
   begin
      case Interface_Id is
         when 0 =>
            null;
            --              USART_Interface_0.Initialise_Interface
            --                (USART_Settings, Baud_Rate);
         when 1 =>
            USART_Interface_1.Reset_Interface;
      end case;
   end Reset_Interface;

   function Interface_Is_Ready (Interface_Id : USART_Id) return Boolean is
   begin
      case Interface_Id is
         when 0 =>
            return False;
--              return PMC.Peripheral_Clock_Status_Register (P_US0) = Enabled;
         when 1 =>
            return PMC.Peripheral_Clock_Status_Register (P_US1) = Enabled;
      end case;
   end Interface_Is_Ready;

   procedure Exchange_Data
     (Using_Interface        : in USART_Id;
      Send_Message           : in Address;
      Send_Message_Length    : in Unsigned_16;
      Recieve_Message        : in Address;
      Recieve_Message_Length : in Unsigned_16) is
   begin
      case Using_Interface is
         when 0 =>
            null;
--              USART_Interface_0.Exchange_Data
--                (Send_Message           => Send_Message,
--                 Send_Message_Length    => Send_Message_Length,
--                 Recieve_Message        => Recieve_Message,
--                 Recieve_Message_Length => Recieve_Message_Length);
--              USART_Interface_0.Wait_For_Transmission;
         when 1 =>
            USART_Interface_1.Exchange_Data
              (Send_Message           => Send_Message,
               Send_Message_Length    => Send_Message_Length,
               Recieve_Message        => Recieve_Message,
               Recieve_Message_Length => Recieve_Message_Length);
            USART_Interface_1.Wait_For_Transmission;
      end case;
   end Exchange_Data;

   function Receive_Bytes_Remaining (On_Interface : USART_Id) return Natural is
   begin
      case On_Interface is
         when 0 =>
            return 0;
         when 1 =>
            return Natural (USART (1).Receive_Counter_Register);
      end case;
   end Receive_Bytes_Remaining;

--     protected body USART_Interface_0 is
--        procedure Initialise_Interface
--          (USART_Settings : in Mode_Type;
--           Baud_Rate      : in Baud_Rate_Generator_Type) is
--        begin
--           --  Turn on USART Clock
--           PMC.Peripheral_Clock_Enable_Register :=
--             (P_US0  => Enable,
--              others => No_Change);
--
--           --  Setup Pins
--
--           PIO.PIO_Disable_Register :=
--             (IO_Line_A_USART_RXD0 => Disable,
--              IO_Line_A_USART_TXD0 => Disable,
--              IO_Line_B_USART_SCK0 => Disable,
--              others               => No_Change);
--
--           PIO.Peripheral_A_Select_Register :=
--             (IO_Line_A_USART_RXD0 => PIO.Use_Peripheral,
--              IO_Line_A_USART_TXD0 => PIO.Use_Peripheral,
--              others               => PIO.No_Change);
--
--           PIO.Peripheral_B_Select_Register :=
--             (IO_Line_B_USART_SCK0 => PIO.Use_Peripheral,
--             others               => PIO.No_Change);
--
--           if USART_Settings.USART_Mode = Hardware_Handshaking then
--              PIO.PIO_Disable_Register :=
--                (IO_Line_A_USART_RTS0 => Disable,
--                 IO_Line_A_USART_CTS0 => Disable,
--                 others               => No_Change);
--
--              PIO.Peripheral_A_Select_Register :=
--                (IO_Line_A_USART_RTS0 => PIO.Use_Peripheral,
--                 IO_Line_A_USART_CTS0 => PIO.Use_Peripheral,
--                 others               => No_Change);
--           end if;
--
--           --  Setup USART unit
--
--           USART (0).Mode_Register := USART_Settings;
--           USART (0).Control_Register :=
--             (Receiver_State            => Enable,
--              Transmitter_State         => Enable,
--              Break                     => No_Change,
--              Data_Terminal_Ready_State => No_Change,
--              Request_To_Send_State     => No_Change,
--              others                    => False);
--
--           USART (0).Transfer_Control_Register :=
--             (Receiver_Transfer_Enable    => Enable,
--              Transmitter_Transfer_Enable => Enable,
--              others                      => No_Change);
--        end Initialise_Interface;
--
--        procedure Exchange_Data
--          (Send_Message           : in Address;
--           Send_Message_Length    : in Unsigned_16;
--           Recieve_Message        : in Address;
--           Recieve_Message_Length : in Unsigned_16) is
--        begin
--           Transfer_Completed := False;
--
--           if Send_Message /= Null_Address and then Send_Message_Length > 0
--     then
--              USART (0).Transmit_Pointer_Register := Send_Message;
--              USART (0).Transmit_Counter_Register := Send_Message_Length;
--              USART (0).Interrupt_Enable_Register :=
--                (End_Of_Transmitter_Transfer => Enable,
--                 others                      => No_Change);
--           end if;
--
--           if Recieve_Message /= Null_Address and then Recieve_Message_Length
--     > 0 then
--              USART (0).Receive_Pointer_Register := Recieve_Message;
--              USART (0).Receive_Counter_Register := Recieve_Message_Length;
--              USART (0).Interrupt_Enable_Register :=
--                (End_Of_Receive_Transfer => Enable,
--                 others                  => No_Change);
--           end if;
--        end Exchange_Data;
--
--        entry Wait_For_Transmission when Transfer_Completed is
--        begin
--           null;
--        end Wait_For_Transmission;
--
--
--        procedure Interface_Handler is
--        begin
--           if USART (0).Channel_Status_Register.End_Of_Transmitter_Transfer
--             and USART (0).Channel_Status_Register.End_Of_Receive_Transfer
--     then
--              USART (0).Interrupt_Disable_Register :=
--                (End_Of_Transmitter_Transfer => Disable,
--                 End_Of_Receive_Transfer     => Disable,
--                 others                      => No_Change);
--              Transfer_Completed := True;
--           end if;
--        end Interface_Handler;
--
--     end USART_Interface_0;

   protected body USART_Interface_1 is
      procedure Initialise_Interface
        (USART_Settings   : in Mode_Type;
         Receiver_Timeout : in Unsigned_16;
         Baud_Rate        : in Baud_Rate_Generator_Type) is
      begin
         --  Turn on USART Clock
         PMC.Peripheral_Clock_Enable_Register :=
           (P_US1  => Enable,
            others => No_Change);

         --  Setup Pins
         PIO.PIO_Disable_Register :=
           (IO_Line_A_USART_RXD1 => Disable,
            IO_Line_A_USART_TXD1 => Disable,
            IO_Line_A_USART_SCK1 => Disable,
            others               => No_Change);

         PIO.Peripheral_A_Select_Register :=
           (IO_Line_A_USART_RXD1 => PIO.Use_Peripheral,
            IO_Line_A_USART_TXD1 => PIO.Use_Peripheral,
            IO_Line_A_USART_SCK1 => PIO.Use_Peripheral,
            others               => PIO.No_Change);

         if USART_Settings.USART_Mode = Hardware_Handshaking then
            PIO.PIO_Disable_Register :=
              (IO_Line_A_USART_RTS1 => Disable,
               IO_Line_A_USART_CTS1 => Disable,
               others               => No_Change);

            PIO.Peripheral_A_Select_Register :=
              (IO_Line_A_USART_RTS1 => PIO.Use_Peripheral,
               IO_Line_A_USART_CTS1 => PIO.Use_Peripheral,
               others               => No_Change);
         end if;

         USART (1).Mode_Register := USART_Settings;
         USART (1).Control_Register :=
           (Receiver_State            => Enable,
            Transmitter_State         => Enable,
            Break                     => No_Change,
            Data_Terminal_Ready_State => No_Change,
            Request_To_Send_State     => No_Change,
            others                    => False);

         USART (1).Transfer_Control_Register :=
           (Receiver_Transfer    => Enable,
            Transmitter_Transfer => Enable);

         USART (1).Baud_Rate_Generator_Register := Baud_Rate;
         USART (1).Receiver_Time_Out_Register.Value := Receiver_Timeout;

         AIC.Interrupt_Enable_Command_Register.Interrupt :=
           (P_US1  => Enable,
            others => No_Change);
      end Initialise_Interface;

      procedure Exchange_Data
        (Send_Message           : in Address;
         Send_Message_Length    : in Unsigned_16;
         Recieve_Message        : in Address;
         Recieve_Message_Length : in Unsigned_16) is
      begin
         Transfer_Completed := False;

         if Send_Message /= Null_Address and then Send_Message_Length > 0 then
            USART (1).Transmit_Pointer_Register := Send_Message;
            USART (1).Transmit_Counter_Register := Send_Message_Length;
            USART (1).Interrupt_Enable_Register :=
              (End_Of_Transmitter_Transfer => Enable,
               others                      => No_Change);
         end if;

         if Recieve_Message /= Null_Address
           and then Recieve_Message_Length > 0
         then
            USART (1).Receive_Pointer_Register := Recieve_Message;
            USART (1).Receive_Counter_Register := Recieve_Message_Length;
            USART (1).Interrupt_Enable_Register :=
              (End_Of_Receive_Transfer => Enable,
               Time_Out                => Enable,
               others                  => No_Change);
            USART (1).Control_Register :=
              (Rearm_Time_Out            => False,
               Start_Time_Out            => True,
               Receiver_State            => No_Change,
               Transmitter_State         => No_Change,
               Break                     => No_Change,
               Data_Terminal_Ready_State => No_Change,
               Request_To_Send_State     => No_Change,
               others                    => False);
         end if;
      end Exchange_Data;

      entry Wait_For_Transmission when Transfer_Completed is
      begin
         null;
      end Wait_For_Transmission;

      procedure Reset_Interface is
      begin
         USART (1).Control_Register :=
           (Reset_Receiver    => True,
            Reset_Transmitter => True,
            Receiver_State            => No_Change,
            Transmitter_State         => No_Change,
            Break                     => No_Change,
            Data_Terminal_Ready_State => No_Change,
            Request_To_Send_State     => No_Change,
            others                    => False);

         Transfer_Completed := True;
      end Reset_Interface;

      procedure Interface_Handler is
      begin
         if USART (1).Channel_Status_Register.End_Of_Transmitter_Transfer
           and USART (1).Channel_Status_Register.End_Of_Receive_Transfer then
            USART (1).Interrupt_Disable_Register :=
              (End_Of_Transmitter_Transfer => Disable,
               End_Of_Receive_Transfer     => Disable,
               Time_Out                    => Disable,
               others                      => No_Change);
            Transfer_Completed := True;
         elsif USART (1).Channel_Status_Register.Time_Out then
            USART (1).Interrupt_Disable_Register := (others => No_Change);
            Transfer_Completed := True;
         end if;
      end Interface_Handler;

   end USART_Interface_1;

end Atmel.AT91SAM7S.USART;
