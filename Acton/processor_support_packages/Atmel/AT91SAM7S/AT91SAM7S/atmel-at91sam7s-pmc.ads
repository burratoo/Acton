------------------------------------------------------------------------------------------
--                                                                                      --
--                            ACTON PROCESSOR SUPPORT PACKAGE                           --
--                                                                                      --
--                                  ATMEL.AT91SAM7S.PMC                                 --
--                                                                                      --
--                       Copyright (C) 2014-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

with Interfaces; use Interfaces;
with System;     use System;

package Atmel.AT91SAM7S.PMC with Preelaborate is

   --------------------------
   -- PMC Memory Addresses --
   --------------------------

   PMC_Base_Address    : constant  := 16#FFFF_FC00#;
   SCER_Offset_Address : constant  := 16#0000#;
   SCDR_Offset_Address : constant  := 16#0004#;
   SCSR_Offset_Address : constant  := 16#0008#;
   PCER_Offset_Address : constant  := 16#0010#;
   PCDR_Offset_Address : constant  := 16#0014#;
   PCSR_Offset_Address : constant  := 16#0018#;
   MOR_Offset_Address  : constant  := 16#0020#;
   MCFR_Offset_Address : constant  := 16#0024#;
   PLLR_Offset_Address : constant  := 16#002C#;
   MCKR_Offset_Address : constant  := 16#0030#;
   PCK0_Offset_Address : constant  := 16#0040#;
   PCK1_Offset_Address : constant  := 16#0044#;
   PCK2_Offset_Address : constant  := 16#0048#;
   IER_Offset_Address  : constant  := 16#0060#;
   IDR_Offset_Address  : constant  := 16#0064#;
   SR_Offset_Address   : constant  := 16#0068#;
   IMR_Offset_Address  : constant  := 16#006C#;

   ---------------
   -- PMC Types --
   ---------------

   type System_Clock_Enable_Type is record
      Processor_Clock             : Enable_No_Change_Type;
      USB_Device_Port_Clock       : Enable_No_Change_Type;
      Programmable_Clock_0_Output : Enable_No_Change_Type;
      Programmable_Clock_1_Output : Enable_No_Change_Type;
      Programmable_Clock_2_Output : Enable_No_Change_Type;
   end record with Size => Register_Size;

   type System_Clock_Disable_Type is record
      Processor_Clock             : Disable_No_Change_Type;
      USB_Device_Port_Clock       : Disable_No_Change_Type;
      Programmable_Clock_0_Output : Disable_No_Change_Type;
      Programmable_Clock_1_Output : Disable_No_Change_Type;
      Programmable_Clock_2_Output : Disable_No_Change_Type;
   end record with Size => Register_Size;

   type System_Clock_Status_Type is record
      Processor_Clock             : Enabled_Type;
      USB_Device_Port_Clock       : Enabled_Type;
      Programmable_Clock_0_Output : Enabled_Type;
      Programmable_Clock_1_Output : Enabled_Type;
      Programmable_Clock_2_Output : Enabled_Type;
   end record with Size => Register_Size;

   type Peripheral_Enable_Set is array (Peripheral_Id) of Enable_No_Change_Type
     with Pack, Size => Register_Size, Alignment => 4;
   type Peripheral_Disable_Set is
     array (Peripheral_Id) of Disable_No_Change_Type
     with Pack, Size => Register_Size, Alignment => 4;
   type Peripheral_Status_Set is array (Peripheral_Id) of Enabled_Type
     with Pack, Size => Register_Size, Alignment => 4;

   type Clock_Generator_Main_Oscillator_Type is record
      Main_Oscillator               : Enable_Type;
      Oscillator_Bypass             : Enable_Type;
      Main_Oscillator_Start_Up_Time : Unsigned_8;
   end record with Size => Register_Size;

   type Clock_Generator_Main_Clock_Frequency_Type is record
      Main_Clock_Frequency : Unsigned_16;
      Main_Clock_Ready     : Boolean;
   end record with Size => Register_Size;

   type PLL_Counter_Type is mod 2 ** 6;
   type PLL_Clock_Frequency_Range_Type is mod 2 ** 2;
   type PLL_Multiplier_Type is mod 2 ** 11;
   type Divider_For_USB_Clock_Type is (Divide_1, Divide_2, Divide_4);

   type Clock_Generator_PLL_Type is record
      Divider                   : Unsigned_8;
      PLL_Counter               : PLL_Counter_Type;
      PLL_Clock_Frequency_Range : PLL_Clock_Frequency_Range_Type;
      PLL_Multiplier            : PLL_Multiplier_Type;
      Divider_For_USB_Clock     : Divider_For_USB_Clock_Type;
   end record with Size => Register_Size;

   type Master_Clock_Selection_Type is (Slow_Clock, Main_Clock, PLL_Clock);
   type Clock_Prescaler_Type is
     (Divide_1, Divide_2, Divide_4, Divide_8, Divide_16, Divide_32, Divide_64);

   type Master_Clock_Type is record
      Master_Clock_Selection    : Master_Clock_Selection_Type;
      Processor_Clock_Prescaler : Clock_Prescaler_Type;
   end record with Size => Register_Size;

   type Programmable_Clock_Type is record
      Master_Clock_Selection       : Master_Clock_Selection_Type;
      Programmable_Clock_Prescaler : Clock_Prescaler_Type;
   end record with Size => Register_Size;

   type Interrupt_Enable_Type is record
      Main_Oscillator_Status_Interrupt     : Enable_No_Change_Type;
      PLL_Lock_Interrupt                   : Enable_No_Change_Type;
      Master_Clock_Ready_Interrupt         : Enable_No_Change_Type;
      Programmable_Clock_Ready_0_Interrupt : Enable_No_Change_Type;
      Programmable_Clock_Ready_1_Interrupt : Enable_No_Change_Type;
      Programmable_Clock_Ready_2_Interrupt : Enable_No_Change_Type;
   end record with Size => Register_Size;

   type Interrupt_Disable_Type is record
      Main_Oscillator_Status_Interrupt     : Disable_No_Change_Type;
      PLL_Lock_Interrupt                   : Disable_No_Change_Type;
      Master_Clock_Ready_Interrupt         : Disable_No_Change_Type;
      Programmable_Clock_Ready_0_Interrupt : Disable_No_Change_Type;
      Programmable_Clock_Ready_1_Interrupt : Disable_No_Change_Type;
      Programmable_Clock_Ready_2_Interrupt : Disable_No_Change_Type;
   end record with Size => Register_Size;

   type Stabilized_Type is (Not_Stabilized, Stabilized);
   type Locked_Type is (Not_Locked, Locked);
   type Ready_Type is (Not_Ready, Ready);

   type Status_Type is record
      Main_Oscillator            : Stabilized_Type;
      PLL_Lock                   : Locked_Type;
      Master_Clock               : Ready_Type;
      Programmable_Clock_0_Ready : Ready_Type;
      Programmable_Clock_1_Ready : Ready_Type;
      Programmable_Clock_2_Ready : Ready_Type;
   end record with Size => Register_Size;

   type Interrupt_Mask_Type is record
      Main_Oscillator            : Enabled_Type;
      PLL_Lock                   : Enabled_Type;
      Master_Clock               : Enabled_Type;
      Programmable_Clock_0_Ready : Enabled_Type;
      Programmable_Clock_1_Ready : Enabled_Type;
      Programmable_Clock_2_Ready : Enabled_Type;
   end record with Size => Register_Size;

   ------------------------------
   -- Hardware Representations --
   ------------------------------

   for System_Clock_Enable_Type use record
      Processor_Clock             at 0 range 0 .. 0;
      USB_Device_Port_Clock       at 0 range 7 .. 7;
      Programmable_Clock_0_Output at 0 range 8 .. 8;
      Programmable_Clock_1_Output at 0 range 9 .. 9;
      Programmable_Clock_2_Output at 0 range 10 .. 10;
   end record;

   for System_Clock_Disable_Type use record
      Processor_Clock             at 0 range 0 .. 0;
      USB_Device_Port_Clock       at 0 range 7 .. 7;
      Programmable_Clock_0_Output at 0 range 8 .. 8;
      Programmable_Clock_1_Output at 0 range 9 .. 9;
      Programmable_Clock_2_Output at 0 range 10 .. 10;
   end record;

   for System_Clock_Status_Type use record
      Processor_Clock             at 0 range 0 .. 0;
      USB_Device_Port_Clock       at 0 range 7 .. 7;
      Programmable_Clock_0_Output at 0 range 8 .. 8;
      Programmable_Clock_1_Output at 0 range 9 .. 9;
      Programmable_Clock_2_Output at 0 range 10 .. 10;
   end record;

   for Clock_Generator_Main_Oscillator_Type use record
      Main_Oscillator               at 0 range 0 .. 0;
      Oscillator_Bypass             at 0 range 1 .. 1;
      Main_Oscillator_Start_Up_Time at 0 range 8 .. 15;
   end record;

   for Clock_Generator_Main_Clock_Frequency_Type use record
      Main_Clock_Frequency at 0 range 0 .. 15;
      Main_Clock_Ready     at 0 range 16 .. 16;
   end record;

   for Divider_For_USB_Clock_Type use (Divide_1 => 0,
                                       Divide_2 => 1,
                                       Divide_4 => 2);

   for Clock_Generator_PLL_Type use record
      Divider                   at 0 range 0 .. 7;
      PLL_Counter               at 0 range 8 .. 13;
      PLL_Clock_Frequency_Range at 0 range 14 .. 15;
      PLL_Multiplier            at 0 range 16 .. 26;
      Divider_For_USB_Clock     at 0 range 28 .. 29;
   end record;

   for Master_Clock_Selection_Type use (Slow_Clock => 0,
                                        Main_Clock => 1,
                                        PLL_Clock  => 3);

   for Clock_Prescaler_Type use
     (Divide_1  => 0, Divide_2 => 1, Divide_4 => 2, Divide_8 => 3,
      Divide_16 => 4, Divide_32 => 5, Divide_64 => 6);

   for Master_Clock_Type use record
      Master_Clock_Selection    at 0 range 0 .. 1;
      Processor_Clock_Prescaler at 0 range 2 .. 4;
   end record;

   for Programmable_Clock_Type use record
      Master_Clock_Selection       at 0 range 0 .. 1;
      Programmable_Clock_Prescaler at 0 range 2 .. 4;
   end record;

   for Interrupt_Enable_Type use record
      Main_Oscillator_Status_Interrupt     at 0 range 0 .. 0;
      PLL_Lock_Interrupt                   at 0 range 2 .. 2;
      Master_Clock_Ready_Interrupt         at 0 range 3 .. 3;
      Programmable_Clock_Ready_0_Interrupt at 0 range 8 .. 8;
      Programmable_Clock_Ready_1_Interrupt at 0 range 9 .. 9;
      Programmable_Clock_Ready_2_Interrupt at 0 range 10 .. 10;
   end record;

   for Interrupt_Disable_Type use record
      Main_Oscillator_Status_Interrupt     at 0 range 0 .. 0;
      PLL_Lock_Interrupt                   at 0 range 2 .. 2;
      Master_Clock_Ready_Interrupt         at 0 range 3 .. 3;
      Programmable_Clock_Ready_0_Interrupt at 0 range 8 .. 8;
      Programmable_Clock_Ready_1_Interrupt at 0 range 9 .. 9;
      Programmable_Clock_Ready_2_Interrupt at 0 range 10 .. 10;
   end record;

   for Stabilized_Type use (Not_Stabilized => 0, Stabilized => 1);
   for Locked_Type use (Not_Locked => 0, Locked => 1);
   for Ready_Type use (Not_Ready => 0, Ready => 1);

   for Status_Type use record
      Main_Oscillator            at 0 range 0 .. 0;
      PLL_Lock                   at 0 range 2 .. 2;
      Master_Clock               at 0 range 3 .. 3;
      Programmable_Clock_0_Ready at 0 range 8 .. 8;
      Programmable_Clock_1_Ready at 0 range 9 .. 9;
      Programmable_Clock_2_Ready at 0 range 10 .. 10;
   end record;

   for Interrupt_Mask_Type use record
      Main_Oscillator            at 0 range 0 .. 0;
      PLL_Lock                   at 0 range 2 .. 2;
      Master_Clock               at 0 range 3 .. 3;
      Programmable_Clock_0_Ready at 0 range 8 .. 8;
      Programmable_Clock_1_Ready at 0 range 9 .. 9;
      Programmable_Clock_2_Ready at 0 range 10 .. 10;
   end record;

   -------------------
   -- PMC Registers --
   -------------------

   pragma Warnings (Off, "*alignment*");
   System_Clock_Enable_Register : System_Clock_Enable_Type
     with Address =>
       System'To_Address (PMC_Base_Address + SCER_Offset_Address);

   System_Clock_Disable_Register : System_Clock_Disable_Type
     with Address =>
       System'To_Address (PMC_Base_Address + SCDR_Offset_Address);

   System_Clock_Status_Register : System_Clock_Enable_Type
     with Address =>
       System'To_Address (PMC_Base_Address + SCSR_Offset_Address);

   Peripheral_Clock_Enable_Register : Peripheral_Enable_Set
     with Address =>
       System'To_Address (PMC_Base_Address + PCER_Offset_Address);

   Peripheral_Clock_Disable_Register : Peripheral_Disable_Set
     with Address =>
       System'To_Address (PMC_Base_Address + PCDR_Offset_Address);

   Peripheral_Clock_Status_Register : Peripheral_Status_Set
     with Address =>
       System'To_Address (PMC_Base_Address + PCSR_Offset_Address);

   Clock_Generator_Main_Oscillator_Register :
   Clock_Generator_Main_Oscillator_Type
     with Address =>
       System'To_Address (PMC_Base_Address + MOR_Offset_Address);

   Clock_Generator_Main_Clock_Frequency_Register :
   Clock_Generator_Main_Clock_Frequency_Type
     with Address =>
       System'To_Address (PMC_Base_Address + MCFR_Offset_Address);

   Clock_Generator_PLL_Register : Clock_Generator_PLL_Type
     with Address =>
       System'To_Address (PMC_Base_Address + PLLR_Offset_Address);

   Master_Clock_Register : Master_Clock_Type
     with Address =>
       System'To_Address (PMC_Base_Address + MCKR_Offset_Address);

   Programmable_Clock_0_Register : Programmable_Clock_Type
     with Address =>
       System'To_Address (PMC_Base_Address + PCK0_Offset_Address);

   Programmable_Clock_1_Register : Programmable_Clock_Type
     with Address =>
       System'To_Address (PMC_Base_Address + PCK1_Offset_Address);

   Programmable_Clock_2_Register : Programmable_Clock_Type
     with Address =>
       System'To_Address (PMC_Base_Address + PCK2_Offset_Address);

   Interrupt_Enable_Register : Interrupt_Enable_Type
     with Address => System'To_Address (PMC_Base_Address + IER_Offset_Address);

   Interrupt_Disable_Register : Interrupt_Disable_Type
     with Address => System'To_Address (PMC_Base_Address + IDR_Offset_Address);

   Status_Register : Status_Type
     with Address => System'To_Address (PMC_Base_Address + SR_Offset_Address);

   Interrupt_Mask_Register : Interrupt_Mask_Type
     with Address => System'To_Address (PMC_Base_Address + IMR_Offset_Address);

end Atmel.AT91SAM7S.PMC;
