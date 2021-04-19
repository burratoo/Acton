------------------------------------------------------------------------------------------
--                                                                                      --
--                            ACTON PROCESSOR SUPPORT PACKAGE                           --
--                                                                                      --
--                                  ATMEL.AT91SAM7S.TWI                                 --
--                                                                                      --
--                       Copyright (C) 2014-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;
with System.Address_To_Access_Conversions;

with Atmel.AT91SAM7S.AIC;
with Atmel.AT91SAM7S.PIO;
with Atmel.AT91SAM7S.PMC;

with System.Storage_Elements; use System.Storage_Elements;

package body Atmel.AT91SAM7S.TWI is

   package Access_Buffer_Address is new
     System.Address_To_Access_Conversions (Unsigned_8);
   use Access_Buffer_Address;

   procedure Send_Message
     (To               : in TWI_Device_Address;
      Internal_Address : in TWI_Internal_Address;
      Address_Size     : in Internal_Device_Address_Range;
      Message          : in Address;
      Message_Length   : in Natural) is
   begin
      Two_Wire_Interface.Transmit_Data
        (With_Device      => To,
         Internal_Address => Internal_Address,
         Address_Size     => Address_Size,
         Data             => Message,
         Data_Length      => Message_Length,
         Direction        => Write);
      Two_Wire_Interface.Wait_For_Transmission;
   end Send_Message;

   procedure Receive_Message
     (From             : in TWI_Device_Address;
      Internal_Address : in TWI_Internal_Address;
      Address_Size     : in Internal_Device_Address_Range;
      Message          : in Address;
      Message_Length   : in Natural) is
   begin
      Two_Wire_Interface.Transmit_Data
        (With_Device      => From,
         Internal_Address => Internal_Address,
         Address_Size     => Address_Size,
         Data             => Message,
         Data_Length      => Message_Length,
         Direction        => Read);
      Two_Wire_Interface.Wait_For_Transmission;
   end Receive_Message;

   procedure Initialise_Interface
     (Clock_Divider      : Clock_Divider_Type;
      Clock_Low_Divider  : Unsigned_8;
      Clock_High_Divider : Unsigned_8) is
   begin
      Two_Wire_Interface.Initialise_Interface
        (Clock_Divider      => Clock_Divider,
         Clock_Low_Divider  => Clock_Low_Divider,
         Clock_High_Divider => Clock_High_Divider);
   end Initialise_Interface;

   protected body Two_Wire_Interface is

      procedure Initialise_Interface
        (Clock_Divider      : Clock_Divider_Type;
         Clock_Low_Divider  : Unsigned_8;
         Clock_High_Divider : Unsigned_8) is
      begin
         AIC.Interrupt_Disable_Command_Register.Interrupt :=
           (P_TWI  => Disable,
            others => No_Change);
         Interrupt_Disable_Register := (others => Disable);

         --  Turn on TWI clocks

         PMC.Peripheral_Clock_Enable_Register :=
           (P_TWI  => Enable,
            P_PIOA => Enable,
            others => No_Change);

         --  Set up pins

         PIO.Multi_Driver_Enable_Register :=
           (IO_Line_A_TWI_Data  => Enable,
            IO_Line_A_TWI_Clock => Enable,
            others              => No_Change);

         PIO.PIO_Disable_Register :=
           (IO_Line_A_TWI_Data  => Disable,
            IO_Line_A_TWI_Clock => Disable,
            others              => No_Change);

         PIO.Peripheral_A_Select_Register :=
           (IO_Line_A_TWI_Data  => PIO.Use_Peripheral,
            IO_Line_A_TWI_Clock => PIO.Use_Peripheral,
            others              => PIO.No_Change);

         --  Setup TWI Hardware

         Control_Register :=
           (Start                    => No,
            Stop                     => No,
            Master_Transfer_Enabled  => False,
            Master_Transfer_Disabled => True,
            Software_Reset           => True);

         Clock_Waveform_Generator_Register :=
           (Clock_Low_Divider  => Clock_Low_Divider,
            Clock_High_Divider => Clock_High_Divider,
            Clock_Divider      => Clock_Divider);

         Control_Register :=
           (Start                    => No,
            Stop                     => No,
            Master_Transfer_Enabled  => True,
            Master_Transfer_Disabled => False,
            Software_Reset           => False);

         --  Setup Interrupts

         AIC.Source_Mode_Register (P_TWI) :=
           (Priority_Level   => AIC.AIC_Interrupt_Priority'Last,
            Interrupt_Source => AIC.High_Level_Sensitive);

         AIC.Interrupt_Enable_Command_Register.Interrupt :=
           (P_TWI  => Enable,
            others => No_Change);

         Transfer_Completed := True;
      end Initialise_Interface;

      procedure Transmit_Data
        (With_Device      : in TWI_Device_Address;
         Internal_Address : in TWI_Internal_Address;
         Address_Size     : in Internal_Device_Address_Range;
         Data             : in Address;
         Data_Length      : in Natural;
         Direction        : in Communication_Direction) is
      begin

         --  Setup the protected object for the transfer

         Transfer_Completed := False;

         Buffer := Data;
         Buffer_Length := Data_Length;

         --  Setup the TWI hardware for the transfer

         Master_Mode_Register :=
           (Internal_Device_Address_Size => Address_Size,
            Master_Read_Direction        => Direction,
            Device_Address               => With_Device);

         if Address_Size > 0 then
            Internal_Address_Register.Internal_Address := Internal_Address;
         end if;

         --  Start the transfer
--           Control_Register :=
--             (Start                    => Yes,
--              Stop                     => No,
--              Master_Transfer_Enabled  => True,
--              Master_Transfer_Disabled => False,
--              Software_Reset           => False);
         case Direction is
            when Read =>
               Control_Register :=
                 (Start                    => Yes,
                  Stop                     => No,
                  Master_Transfer_Enabled  => False,
                  Master_Transfer_Disabled => False,
                  Software_Reset           => False);
               Interrupt_Enable_Register :=
                 (Receive_Holding_Register_Ready  => Enable,
                  Not_Acknowledged                => Enable,
                  others                          => No_Change);
            when Write =>
               Transmit_Holding_Register.Data := To_Pointer (Buffer).all;
               Buffer := Buffer + 1;
               Buffer_Length := Buffer_Length - 1;
               Interrupt_Enable_Register :=
                 (Transmit_Holding_Register_Ready => Enable,
--                    Transmission_Completed          => Enable,
                  Not_Acknowledged                => Enable,
                  others                          => No_Change);
         end case;
      end Transmit_Data;

      entry Wait_For_Transmission when Transfer_Completed is
      begin
         null;
      end Wait_For_Transmission;

      procedure Interface_Handler is
         ST : constant Status_Register_Type := Status_Register;
         TWI_Status : constant Status_Register_Type :=
                        (ST and Interrupt_Mask_Register);
      begin
         if TWI_Status.Receive_Holding_Register_Ready then
            --  We have recieved a byte

            if Buffer_Length > 0 then
               To_Pointer (Buffer).all := Receive_Holding_Register.Data;
               Buffer := Buffer + 1;
               Buffer_Length := Buffer_Length - 1;

               if Buffer_Length = 1 then
                  Control_Register :=
                    (Start                    => No,
                     Stop                     => Yes,
                     Master_Transfer_Enabled  => False,
                     Master_Transfer_Disabled => False,
                     Software_Reset           => False);
               elsif Buffer_Length = 0 then
                  --  Recieve transfer complete
                  Interrupt_Disable_Register := (others => Disable);
                  Transfer_Completed := True;
               end if;
            elsif Buffer_Length = 0 then
               raise Program_Error;
            end if;

         elsif TWI_Status.Transmit_Holding_Register_Ready then
            if Buffer_Length > 0 then
               if Buffer_Length = 1 then
                  Control_Register :=
                    (Start                    => No,
                     Stop                     => Yes,
                     Master_Transfer_Enabled  => False,
                     Master_Transfer_Disabled => False,
                     Software_Reset           => False);
--                    Interrupt_Disable_Register :=
--                      (Transmit_Holding_Register_Ready => Disable,
--                       others                          => No_Change);
               end if;

               Transmit_Holding_Register.Data := To_Pointer (Buffer).all;
               Buffer := Buffer + 1;
               Buffer_Length := Buffer_Length - 1;

            else
               Transfer_Completed := True;
               Interrupt_Disable_Register :=
                 (Transmit_Holding_Register_Ready => Disable,
                  others                          => No_Change);
            end if;

--           elsif TWI_Status.Transmission_Completed then
--              if Buffer_Length > 0 then
--                 raise Program_Error;
--              end if;
--              Transfer_Completed := True;
--              Interrupt_Disable_Register := (others => Disable);
         end if;

         if TWI_Status.Not_Acknowledged then
            Transfer_Completed := True;
            Interrupt_Disable_Register := (others => Disable);
--              raise Program_Error;
         end if;
      end Interface_Handler;

   end Two_Wire_Interface;

   function "and" (L, R : Status_Register_Type) return Status_Register_Type is
      function To_Register is new Ada.Unchecked_Conversion
        (Source => Status_Register_Type,
         Target => Register);
      function To_Status_Type is new Ada.Unchecked_Conversion
        (Source => Register,
         Target => Status_Register_Type);
   begin
      return To_Status_Type (To_Register (L) and To_Register (R));
   end "and";

end Atmel.AT91SAM7S.TWI;
