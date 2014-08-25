with System; use System;

package ST.STM32F4.Power with Preelaborate is

   ----------------------------
   -- Power Memory Addresses --
   ----------------------------

   PWR_Base_Address   : constant := 16#4000_7000#;
   CR_Offset_Address  : constant := 16#00#;
   CSR_Offset_Address : constant := 16#04#;

   -----------------------
   -- Hardware Features --
   -----------------------

   -----------------
   -- Power Types --
   -----------------

   type Scaling_Selection is (Scale_2, Scale_1);
   type PVD_Level is (PVD_2V0, PVD_2V1, PVD_2V3, PVD_2V5,
                      PVD_2V6, PVD_2V7, PVD_2V8, PVD_2V9);
   type Deepsleep_Options is (Stop_Mode, Standby_Mode);
   type Deepsleep_Regulator_Options is (On, Low_Power);

   type Power_Control is record
      Regulator_Voltage_Scaling_Output : Scaling_Selection;
      Flash_Power_Down_In_Stop_Mode    : Boolean;
      Backup_Domain_Writable           : Boolean;
      Power_Voltage_Dectector_Level    : PVD_Level;
      Power_Voltage_Dectector          : Enable_Type;
      Standby_Flag                     : Clear_Type;
      Wakeup_Flag                      : Clear_Type;
      Deepsleep_Mode                   : Deepsleep_Options;
      Voltage_Regulator_In_Deepsleep   : Deepsleep_Regulator_Options;
   end record;

   type PVD_State is (VDD_Higher, VDD_Lower);

   type Control_Status is record
      Regulator_Voltage_Scaling_Output_Selection_Ready : Boolean;
      Backup_Regulator                                 : Enable_Type;
      Wakeup_Pin                                       : Enable_Type;
      Backup_Regulator_Ready                           : Boolean;
      Power_Voltage_Dectector                          : PVD_State;
      Was_In_Standby                                   : Boolean;
      Wake_Up_Event_Occurred                           : Boolean;
   end record;

   ------------------------------
   -- Hardware Representations --
   ------------------------------

   for Power_Control use record
      Regulator_Voltage_Scaling_Output at 0 range 14 .. 14;
      Flash_Power_Down_In_Stop_Mode    at 0 range  9 .. 9;
      Backup_Domain_Writable           at 0 range  8 .. 8;
      Power_Voltage_Dectector_Level    at 0 range  5 .. 7;
      Power_Voltage_Dectector          at 0 range  4 .. 4;
      Standby_Flag                     at 0 range  3 .. 3;
      Wakeup_Flag                      at 0 range  2 .. 2;
      Deepsleep_Mode                   at 0 range  1 .. 1;
      Voltage_Regulator_In_Deepsleep   at 0 range  0 .. 0;
   end record;

   for Control_Status use record
      Regulator_Voltage_Scaling_Output_Selection_Ready at 0 range 14 .. 14;
      Backup_Regulator                                 at 0 range  9 .. 9;
      Wakeup_Pin                                       at 0 range  8 .. 8;
      Backup_Regulator_Ready                           at 0 range  3 .. 3;
      Power_Voltage_Dectector                          at 0 range  2 .. 2;
      Was_In_Standby                                   at 0 range  1 .. 1;
      Wake_Up_Event_Occurred                           at 0 range  0 .. 0;
   end record;

   ---------------------
   -- Power Registers --
   ---------------------

   Power_Control_Register : Power_Control
     with Address => System'To_Address (PWR_Base_Address + CR_Offset_Address);

   Control_Status_Register : Control_Status
     with Address => System'To_Address (PWR_Base_Address + CSR_Offset_Address);

end ST.STM32F4.Power;
