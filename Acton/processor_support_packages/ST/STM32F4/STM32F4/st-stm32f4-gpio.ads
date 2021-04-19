------------------------------------------------------------------------------------------
--                                                                                      --
--                            OAK PROCESSOR SUPPORT PACKAGE                             --
--                                      ST STM32F4                                      --
--                                                                                      --
--                                   ST.STM32F4.GPIO                                    --
--                                                                                      --
--                       Copyright (C) 2014-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

with System; use System;

package ST.STM32F4.GPIO with Preelaborate is

   ---------------------------
   -- GPIO Memory Addresses --
   ---------------------------

   GPIO_Base_Address      : constant := 16#4002_0000#;
   MODER_Offset_Address   : constant := 16#00#;
   OTYPER_Offset_Address  : constant := 16#04#;
   OSPEEDR_Offset_Address : constant := 16#08#;
   PUPDR_Offset_Address   : constant := 16#0C#;
   IDR_Offset_Address     : constant := 16#10#;
   ODR_Offset_Address     : constant := 16#14#;
   BSRR_Offset_Address    : constant := 16#18#;
   LCKR_Offset_Address    : constant := 16#1C#;
   AFR_Offset_Address     : constant := 16#20#;

   -----------------------
   -- Hardware Features --
   -----------------------

   type GPIO_Ports is (Port_A, Port_B, Port_C, Port_D, Port_E,
                       Port_F, Port_G, Port_H, Port_I);

   type Pins is mod 16;

   -----------------
   -- GPIO Types --
   -----------------

   type IO_Mode is (Input, Output, Alternative_Function, Analog);
   type IO_Mode_Set is array (Pins) of IO_Mode with Pack;

   type IO_Mode_Pins is record
      Pin : IO_Mode_Set;
   end record;

   type Output_Type is (Push_Pull, Open_Drain);
   type Output_Type_Set is array (Pins) of Output_Type with Pack;

   type Output_Driver_Pins is record
      Pin : Output_Type_Set;
   end record;

   type Speeds is (Low, Medium, Fast, High);
   type Speeds_Set is array (Pins) of Speeds with Pack;

   type Speeds_Pins is record
      Pin : Speeds_Set;
   end record;

   type Pull_Direction is (No_Pull, Pull_Up, Pull_Down);
   type Pull_Direction_Set is array (Pins) of Pull_Direction with Pack;

   type Pull_Direction_Pins is record
      Pin : Pull_Direction_Set;
   end record;

   type GPIO_State is (Low, High);
   type GPIO_State_Set is array (Pins) of GPIO_State with Pack;

   type GPIO_State_Pins is record
      Pin : GPIO_State_Set;
   end record;

   type High_State is (No_Change, High);
   type Low_State is (No_Change, Low);

   type High_Set is array (Pins) of High_State with Pack;
   type Low_Set is array (Pins) of Low_State with Pack;

   type GPIO_Set_Clear is record
      Pin_To_Set_High : High_Set;
      Pin_To_Set_Low  : Low_Set;
   end record;

   type Active_Type is (Inactive, Active);
   type Lock_Type is (Unlocked, Locked);
   type Lock_Set is array (Pins) of Lock_Type with Pack;

   type Configuration_Lock is record
      Lock_Key : Active_Type;
      Pin      : Lock_Set;
   end record;

   type Alternative_Function_Type is mod 2 ** 4;
   type Alternative_Function_Set is array (Pins) of Alternative_Function_Type
     with Pack;

   type Pin_Alternative_Function is record
      Pin : Alternative_Function_Set;
   end record;

   type GPIO_Port_Registers is record
      IO_Mode_Register              : IO_Mode_Pins;
      Output_Driver_Register        : Output_Driver_Pins;
      Output_Pin_Speed_Register     : Speeds_Set;
      Pull_Direction_Register       : Pull_Direction_Pins;
      Input_State_Register          : GPIO_State_Pins;
      Output_State_Register         : GPIO_State_Pins;
      Set_Pin_State_Register        : GPIO_Set_Clear;
      Configuration_Lock_Register   : Configuration_Lock;
      Alternative_Function_Register : Pin_Alternative_Function;
   end record;

   ------------------------------
   -- Hardware Representations --
   ------------------------------

   for GPIO_Set_Clear use record
      Pin_To_Set_High at 0 range  0 .. 15;
      Pin_To_Set_Low  at 0 range 16 .. 31;
   end record;

   for Configuration_Lock use record
      Lock_Key at 0 range 16 .. 16;
      Pin      at 0 range  0 .. 15;
   end record;

   for GPIO_Port_Registers use record
      IO_Mode_Register              at MODER_Offset_Address   range 0 .. 31;
      Output_Driver_Register        at OTYPER_Offset_Address  range 0 .. 15;
      Output_Pin_Speed_Register     at OSPEEDR_Offset_Address range 0 .. 31;
      Pull_Direction_Register       at PUPDR_Offset_Address   range 0 .. 31;
      Input_State_Register          at IDR_Offset_Address     range 0 .. 15;
      Output_State_Register         at ODR_Offset_Address     range 0 .. 15;
      Set_Pin_State_Register        at BSRR_Offset_Address    range 0 .. 31;
      Configuration_Lock_Register   at LCKR_Offset_Address    range 0 .. 16;
      Alternative_Function_Register at AFR_Offset_Address     range 0 .. 63;
   end record;

   --------------------
   -- GPIO Registers --
   --------------------

   pragma Warnings (Off, "*component of*");
   type GPIO_Port_Set is array (GPIO_Ports) of GPIO_Port_Registers
     with Component_Size => 16#400# * Storage_Unit;
   pragma Warnings (On, "*component of*");

   GPIO_Port_Register : GPIO_Port_Set
     with Address => System'To_Address (GPIO_Base_Address);

end ST.STM32F4.GPIO;
