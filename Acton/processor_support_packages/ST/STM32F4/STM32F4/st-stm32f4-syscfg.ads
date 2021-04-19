------------------------------------------------------------------------------------------
--                                                                                      --
--                            OAK PROCESSOR SUPPORT PACKAGE                             --
--                                      ST STM32F4                                      --
--                                                                                      --
--                                  ST.STM32F4.SYSCFG                                   --
--                                                                                      --
--                       Copyright (C) 2014-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

with ST.STM32F4.GPIO; use ST.STM32F4.GPIO;

with System; use System;

package ST.STM32F4.SYSCFG with Preelaborate is

   -----------------------------
   -- SYSCFG Memory Addresses --
   -----------------------------

   SYSCFG_Base_Address   : constant := 16#4001_3800#;
   MEMRM_Offset_Address  : constant := 16#00#;
   PMC_Offset_Address    : constant := 16#04#;
   EXTICR_Offset_Address : constant := 16#08#;
   CMPCR_Offset_Address  : constant := 16#20#;

   -----------------------
   -- Hardware Features --
   -----------------------

   ------------------
   -- SYSCFG Types --
   ------------------

   type Memory_Selection is (Main_Flash, System_Flash,
                             FSMC_Bank, Embedded_SRAM);

   type Memory_Remap is record
      Selection : Memory_Selection;
   end record;

   type Eth_PYH_Interface_Selection is (MII, RMII_PHY);

   type Peripheral_Mode_Configuration is record
      Ethernet_PYH_Interface : Eth_PYH_Interface_Selection;
   end record;

   type External_Interrupt_Configuration is record
      EXTI0  : GPIO_Ports;
      EXTI1  : GPIO_Ports;
      EXTI2  : GPIO_Ports;
      EXTI3  : GPIO_Ports;
      EXTI4  : GPIO_Ports;
      EXTI5  : GPIO_Ports;
      EXTI6  : GPIO_Ports;
      EXTI7  : GPIO_Ports;
      EXTI8  : GPIO_Ports;
      EXTI9  : GPIO_Ports;
      EXTI10 : GPIO_Ports;
      EXTI11 : GPIO_Ports;
      EXTI12 : GPIO_Ports;
      EXTI13 : GPIO_Ports;
      EXTI14 : GPIO_Ports;
      EXTI15 : GPIO_Ports;
   end record;

   type Compensation_Cell_Control is record
      Ready  : Boolean;
      Enable : Enable_Type;
   end record;

   ------------------------------
   -- Hardware Representations --
   ------------------------------

   for Memory_Remap use record
      Selection at 0 range 0 .. 1;
   end record;

   for Peripheral_Mode_Configuration use record
      Ethernet_PYH_Interface at 0 range 23 .. 23;
   end record;

   for External_Interrupt_Configuration use record
      EXTI0  at 0 range  0 .. 3;
      EXTI1  at 0 range  4 .. 7;
      EXTI2  at 0 range  8 .. 11;
      EXTI3  at 0 range 12 .. 15;
      EXTI4  at 4 range  0 .. 3;
      EXTI5  at 4 range  4 .. 7;
      EXTI6  at 4 range  8 .. 11;
      EXTI7  at 4 range 12 .. 15;
      EXTI8  at 8 range  0 .. 3;
      EXTI9  at 8 range  4 .. 7;
      EXTI10 at 8 range  8 .. 11;
      EXTI11 at 8 range 12 .. 15;
      EXTI12 at 12 range  0 .. 3;
      EXTI13 at 12 range  4 .. 7;
      EXTI14 at 12 range  8 .. 11;
      EXTI15 at 12 range 12 .. 15;
   end record;

   for Compensation_Cell_Control use record
      Ready  at 0 range 8 .. 8;
      Enable at 0 range 0 .. 0;
   end record;

   ----------------------
   -- SYSCFG Registers --
   ----------------------

   Memory_Remap_Register : Memory_Remap
     with Address =>
       System'To_Address (SYSCFG_Base_Address + MEMRM_Offset_Address);

   External_Interrupt_Config_Register : External_Interrupt_Configuration
     with Address =>
       System'To_Address (SYSCFG_Base_Address + EXTICR_Offset_Address);

   Compensation_Cell_Control_Register : Compensation_Cell_Control
     with Address =>
       System'To_Address (SYSCFG_Base_Address + CMPCR_Offset_Address);
end ST.STM32F4.SYSCFG;
