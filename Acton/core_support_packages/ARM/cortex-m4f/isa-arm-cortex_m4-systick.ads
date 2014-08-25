with System; use System;

package ISA.ARM.Cortex_M4.SysTick with Preelaborate is

   ------------------------------
   -- SysTick Memory Addresses --
   ------------------------------

   SYST_Base_Address    : constant := 16#E000_E010#;
   CSR_Offset_Address   : constant := 16#0#;
   RVR_Offset_Address   : constant := 16#4#;
   CVR_Offset_Address   : constant := 16#8#;
   CALIB_Offset_Address : constant := 16#C#;

   -----------------------
   -- Hardware Features --
   -----------------------

   type Sys_Tick is mod 2 ** 23;

   -------------------
   -- SysTick Types --
   -------------------

   type Clock_Source_Type is (External, Processor);

   type Control_Status_Type is record
      Counter      : Enable_Type;
      Interrupt    : Enable_Type;
      Clock_Source : Clock_Source_Type;
      Count_Flag   : Boolean;
   end record with Size => Word_Size;

   type Calibration_Value_Type is record
      Value              : Sys_Tick;
      Skewed             : Boolean;
      No_Reference_Clock : Boolean;
   end record;

   ------------------------------
   -- Hardware Representations --
   ------------------------------

   for Clock_Source_Type use (External => 0, Processor => 1);

   for Control_Status_Type use record
      Counter      at 0 range 0 .. 0;
      Interrupt    at 0 range 1 .. 1;
      Clock_Source at 0 range 2 .. 2;
      Count_Flag   at 0 range 16 .. 16;
   end record;

   for Calibration_Value_Type use record
      Value              at 0 range 0 .. 23;
      Skewed             at 0 range 30 .. 30;
      No_Reference_Clock at 0 range 31 .. 31;
   end record;

   -----------------------
   -- SysTick Registers --
   -----------------------

   Control_And_Status_Register : Control_Status_Type
     with Address => System'To_Address (SYST_Base_Address +
                                          CSR_Offset_Address);

   Reload_Value_Register : Sys_Tick
     with Address => System'To_Address (SYST_Base_Address +
                                          RVR_Offset_Address);

   Current_Value_Register : Sys_Tick
     with Volatile,
       Address => System'To_Address (SYST_Base_Address +
                                          CVR_Offset_Address);

   Calibration_Value_Register : Calibration_Value_Type
     with Address => System'To_Address (SYST_Base_Address +
                                          CALIB_Offset_Address);
end ISA.ARM.Cortex_M4.SysTick;
