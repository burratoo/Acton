with Atmel.AT91SAM7S.AIC;
with Atmel.AT91SAM7S.PIO;

with Atmel.AT91SAM7S.PMC;

package body Atmel.AT91SAM7S.SPI is
   procedure Transmit_Data
     (To_Device              : in Peripheral_Chip_Select_Id;
      Send_Message           : in Address;
      Send_Message_Length    : in Unsigned_16;
      Recieve_Message        : in Address;
      Recieve_Message_Length : in Unsigned_16) is
   begin
      Serial_Peripheral_Interface.Exchange_Data
        (With_Device            => To_Device,
         Send_Message           => Send_Message,
         Send_Message_Length    => Send_Message_Length,
         Recieve_Message        => Recieve_Message,
         Recieve_Message_Length => Recieve_Message_Length);
      Serial_Peripheral_Interface.Wait_For_Transmission;
   end Transmit_Data;

   procedure Initialise_Interface
     (SPI_Settings                : Mode_Type;
      Chip_Select_Pin_Assignments : in Chip_Select_Pins) is
   begin
      Serial_Peripheral_Interface.Initialise_Interface
        (SPI_Settings                => SPI_Settings,
         Chip_Select_Pin_Assignments => Chip_Select_Pin_Assignments);
   end Initialise_Interface;

   procedure Setup_Chip_Select_Pin
     (For_Pin              : in Peripheral_Chip_Select_Pin;
      Chip_Select_Settings : in Chip_Select_Type) is
   begin
      Serial_Peripheral_Interface.Setup_Chip_Select_Pin
        (For_Pin, Chip_Select_Settings);
   end Setup_Chip_Select_Pin;

   protected body Serial_Peripheral_Interface is

      procedure Initialise_Interface
        (SPI_Settings                : Mode_Type;
         Chip_Select_Pin_Assignments : in Chip_Select_Pins) is
      begin
         --  Turn on SPI clock
         PMC.Peripheral_Clock_Enable_Register :=
           (P_SPI => Enable,
            others => No_Change);

         --  Setup Pins

         PIO.PIO_Disable_Register :=
           (IO_Line_A_SPI_Clock => Disable,
            IO_Line_A_SPI_MOSI  => Disable,
            IO_Line_A_SPI_MISO  => Disable,
            others              => No_Change);

         PIO.Peripheral_A_Select_Register :=
           (IO_Line_A_SPI_Clock => PIO.Use_Peripheral,
            IO_Line_A_SPI_MOSI  => PIO.Use_Peripheral,
            IO_Line_A_SPI_MISO  => PIO.Use_Peripheral,
            others              => PIO.No_Change);

         for P of Chip_Select_Pin_Assignments loop
            declare
               Disable_Reg    : PIO.Disable_Set := (others => No_Change);
               Peripheral_Set : PIO.Select_Set  := (others => PIO.No_Change);
            begin
               case P.Pin_Function is
               when A =>
                  Disable_Reg (P.Pin) := Disable;
                  PIO.PIO_Disable_Register := Disable_Reg;
                  Peripheral_Set (P.Pin) := PIO.Use_Peripheral;
                  PIO.Peripheral_A_Select_Register := Peripheral_Set;
               when B =>
                  Disable_Reg (P.Pin) := Disable;
                  PIO.PIO_Disable_Register := Disable_Reg;
                  Peripheral_Set (P.Pin) := PIO.Use_Peripheral;
                  PIO.Peripheral_B_Select_Register := Peripheral_Set;
               when PIO_Function =>
                  null;
               end case;
            end;
         end loop;

         --  Setup SPI unit

         Control_Register :=
           (SPI_Enable     => Enable,
            SPI_Disable    => No_Change,
            Software_Reset => No_Change,
            Last_Transfer  => False);

         Mode_Register := SPI_Settings;

         Transfer_Control_Register :=
           (Receiver_Transfer    => Enable,
            Transmitter_Transfer => Enable);

         AIC.Interrupt_Enable_Command_Register.Interrupt :=
           (P_SPI  => Enable,
            others => No_Change);
      end Initialise_Interface;

      procedure Exchange_Data
        (With_Device            : in Peripheral_Chip_Select_Id;
         Send_Message           : in Address;
         Send_Message_Length    : in Unsigned_16;
         Recieve_Message        : in Address;
         Recieve_Message_Length : in Unsigned_16) is
         MR : Mode_Type := Mode_Register;
      begin
         Transfer_Completed := False;

         if Mode_Register.Peripheral_Select = Variable then
            raise Program_Error with
              "Variable peripheral selection not supported";
         end if;

         MR.Peripheral_Chip_Select := With_Device;
         Mode_Register := MR;

         if Send_Message /= Null_Address
           and then Send_Message_Length > 0
         then
            Transmit_Pointer_Register := Send_Message;
            Transmit_Counter_Register := Send_Message_Length;
         end if;

         if Recieve_Message /= Null_Address
           and then Recieve_Message_Length > 0
         then
            Receive_Pointer_Register := Recieve_Message;
            Receive_Counter_Register := Recieve_Message_Length;
         end if;

         if Send_Message_Length >= Recieve_Message_Length then
            Interrupt_Enable_Register :=
              (End_Of_Transmit_Buffer => Enable,
               others                 => No_Change);
         else
            Interrupt_Enable_Register :=
              (End_Of_Receive_Buffer => Enable,
               others                => No_Change);
         end if;
      end Exchange_Data;

      procedure Setup_Chip_Select_Pin
        (For_Pin              : in Peripheral_Chip_Select_Pin;
         Chip_Select_Settings : in Chip_Select_Type) is
      begin
         Chip_Select_Register (For_Pin) := Chip_Select_Settings;
      end Setup_Chip_Select_Pin;

      entry Wait_For_Transmission when Transfer_Completed is
      begin
         null;
      end Wait_For_Transmission;

      procedure Interface_Handler is
      begin
         --  We wait for the transmission buffers to become empty and then
         --  wait for the last transmission to complete.

         if Status_Register.End_Of_Receive_Buffer
           or Status_Register.End_Of_Transmit_Buffer
         then
--              Interrupt_Enable_Register :=
--                (Transmission_Registers_Empty => Enable,
--                 others                       => No_Change);
            Interrupt_Disable_Register :=
              (End_Of_Receive_Buffer  => Disable,
               End_Of_Transmit_Buffer => Disable,
               others                 => No_Change);
            Transfer_Completed := True;
--           elsif Status_Register.Transmission_Registers_Empty then
--              Transfer_Completed := True;
--              Interrupt_Disable_Register :=
--                (Transmission_Registers_Empty => Disable,
--                 others                       => No_Change);
         end if;

      end Interface_Handler;

   end Serial_Peripheral_Interface;
end Atmel.AT91SAM7S.SPI;
