------------------------------------------------------------------------------------------
--                                                                                      --
--                            ACTON PROCESSOR SUPPORT PACKAGE                           --
--                                                                                      --
--                                   ATMEL AT91SAM7S                                    --
--                                                                                      --
--                       Copyright (C) 2014-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

package Atmel.AT91SAM7S with Pure is

   Register_Size : constant := 32;
   type Register is mod 2 ** Register_Size;

   --  Peripheral Identifiers

--     type Peripheral_Id is
--       (P_FIQ, P_SYSC, P_PIOA, P_ADC, P_SPI, P_US0, P_US1, P_SSC,
--        P_TWI, P_PWMC, P_UDP,  P_TC0, P_TC1, P_TC2,
--        IRQ0,  IRQ1);

   type Peripheral_Id is
     (P_FIQ, P_SYSC, P_PIOA, PID3,  P_ADC, P_SPI, P_US0, P_US1, P_SSC,
      P_TWI, P_PWMC, P_UDP,  P_TC0, P_TC1, P_TC2, PID15, PID16, PID17,
      PID18,  PID19,  PID20, PID21, PID22, PID23, PID24, PID25, PID26,
      PID27,  PID28,  PID29, IRQ0,  IRQ1);

   FIQ_Id  : constant := 0;
   SYSC_Id : constant := 1;
   PIOA_Id : constant := 2;
   ADC_Id  : constant := 4;
   SPI_Id  : constant := 5;
   US0_Id  : constant := 6;
   US1_Id  : constant := 7;
   SSC_Id  : constant := 8;
   TWI_Id  : constant := 9;
   PWMC_Id : constant := 10;
   UDP_Id  : constant := 11;
   TC1_Id  : constant := 12;
   TC2_Id  : constant := 13;
   TC3_Id  : constant := 14;
   IRQ0_Id : constant := 30;
   IRQ1_Id : constant := 31;

   --  Peripherals

   type PIO_Lines is mod 2 ** 5;

   type Peripheral_Pin_Function is (PIO_Function, A, B);
   type Peripheral_Pin is record
      Pin          : PIO_Lines;
      Pin_Function : Peripheral_Pin_Function;
   end record;

   --  Peripheral A Pins

   IO_Line_A_TWI_Data   : constant := 3;
   IO_Line_A_TWI_Clock  : constant := 4;
   IO_Line_A_USART_RXD0 : constant := 5;
   IO_Line_A_USART_TXD0 : constant := 6;
   IO_Line_A_USART_RTS0 : constant := 7;
   IO_Line_A_USART_CTS0 : constant := 8;
   IO_Line_A_SPI_CS0    : constant := 11;
   IO_Line_A_SPI_MISO   : constant := 12;
   IO_Line_A_SPI_MOSI   : constant := 13;
   IO_Line_A_SPI_Clock  : constant := 14;
   IO_Line_A_TD         : constant := 17;
   IO_Line_A_USART_RXD1 : constant := 21;
   IO_Line_A_USART_TXD1 : constant := 22;
   IO_Line_A_USART_SCK1 : constant := 23;
   IO_Line_A_USART_RTS1 : constant := 24;
   IO_Line_A_USART_CTS1 : constant := 25;
   IO_Line_A_USART_DCD1 : constant := 26;
   IO_Line_A_USART_DTR1 : constant := 27;
   IO_Line_A_USART_DSR1 : constant := 28;
   IO_Line_A_USART_RI1  : constant := 29;
   IO_Line_A_SPI_CS1    : constant := 31;

   --  Peripheral B Pins
   IO_Line_B_USART_SCK0     : constant := 2;
   IO_Line_B_SP1_CS3_Pin_3  : constant := 3;
   IO_Line_B_SP1_CS3_Pin_5  : constant := 5;
   IO_Line_B_SPI_CS1        : constant := 9;
   IO_Line_B_SPI_CS2_Pin_10 : constant := 10;
   IO_Line_B_SP1_CS3_Pin_22 : constant := 22;
   IO_Line_B_SPI_CS2_Pin_30 : constant := 30;

   -----------------------------
   -- Hardware Representation --
   -----------------------------

--     for Peripheral_Id use
--       (P_FIQ => 0,  P_SYSC => 1,  P_PIOA => 2,  P_ADC => 4, P_SPI  => 5,
--        P_US0 => 6,  P_US1  => 7,  P_SSC  => 8,  P_TWI => 9, P_PWMC => 10,
--        P_UDP => 11, P_TC0  => 12, P_TC1  => 13, P_TC2 => 14,
--        IRQ0  => 30, IRQ1   => 31);

   for Peripheral_Id use
     (P_FIQ  => 0,  P_SYSC => 1,  P_PIOA => 2,  PID3  => 3,  P_ADC => 4,
      P_SPI  => 5,  P_US0  => 6,  P_US1  => 7,  P_SSC => 8,  P_TWI => 9,
      P_PWMC => 10, P_UDP  => 11, P_TC0  => 12, P_TC1 => 13, P_TC2 => 14,
      PID15  => 15, PID16  => 16, PID17  => 17, PID18 => 18, PID19 => 19,
      PID20  => 20, PID21  => 21, PID22  => 22, PID23 => 23, PID24 => 24,
      PID25  => 25, PID26  => 26, PID27  => 27, PID28 => 28, PID29 => 29,
      IRQ0   => 30, IRQ1   => 31);
end Atmel.AT91SAM7S;
