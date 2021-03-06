with MPC5554;                                    use MPC5554;
with MPC5554.SIU;                                use MPC5554.SIU;
with MPC5554.EBI;                                use MPC5554.EBI;
with MPC5554.PBRIDGE;                            use MPC5554.PBRIDGE;
with MPC5554.XBAR;                               use MPC5554.XBAR;
with ISA;                                        use ISA;
with ISA.Power.e200.Processor_Control_Registers;
use  ISA.Power.e200.Processor_Control_Registers;

with System.Machine_Code; use System.Machine_Code;

package body MPC5554.Init is

   procedure Setup is
      MSR : Machine_State_Register_Type;
   begin
      --  Read the Machine State Register
      Asm
        ("mfmsr  %0",
         Outputs  => (Machine_State_Register_Type'Asm_Output ("=r", MSR)),
         Volatile => True);
      MSR.Signal_Processing   := Enable;
      MSR.External_Interrupts := Enable;
      --  Write the Time Control Register
      Asm
        ("mtmsr  %0",
         Inputs   => (Machine_State_Register_Type'Asm_Input ("r", MSR)),
         Volatile => True);
      Setup_External_Clocks;
      Setup_External_Bus;
      Setup_PBRIDGE;
      Setup_XBAR;
   end Setup;

   procedure Setup_External_Clocks is
      Engineering_Clock_Pad : constant SIU.Pad_Configuration_Pointer :=
         SIU.Pad_Configuration_Register_Array (214)'Access;

      Clock_Out_Pad : constant SIU.Pad_Configuration_Pointer :=
         SIU.Pad_Configuration_Register_Array (229)'Access;

   begin
      Engineering_Clock_Pad.Output_Buffer_Enable   := Enable;
      Engineering_Clock_Pad.Drive_Strength_Control := ds_10pf;

      Clock_Out_Pad.Output_Buffer_Enable   := Enable;
      Clock_Out_Pad.Drive_Strength_Control := ds_10pf;

      SIU.External_Clock_Control_Register :=
        (Engineering_Clock_Division_Factor => 33,
         External_Bus_Tap_Select           => Zero_Hold,
         External_Bus_Division_Factor      => Divide_By_4);

   end Setup_External_Clocks;

   procedure Setup_External_Bus is
      subtype Address_Pads_ID_Type is SIU.Pad_ID_Type range 4 .. 27;
      subtype Data_Pads_ID_Type is SIU.Pad_ID_Type range 28 .. 59;

      CS0_Base_Address : constant Sixteen_Bit_Address_Type := 16#3FF8#;
      CS1_Base_Address : constant Sixteen_Bit_Address_Type := 16#2000#;
      CS2_Base_Address : constant Sixteen_Bit_Address_Type := 16#2080#;
      CS3_Base_Address : constant Sixteen_Bit_Address_Type := 16#3000#;

      Chip_Select_0_Register_Pad_No : constant SIU.Pad_ID_Type := 0;
      Chip_Select_1_Register_Pad_No : constant SIU.Pad_ID_Type := 1;
      Chip_Select_2_Register_Pad_No : constant SIU.Pad_ID_Type := 2;
      Chip_Select_3_Register_Pad_No : constant SIU.Pad_ID_Type := 3;

      Address_Pads_Config : constant SIU.Pad_Configuration_Type :=
        (Pin_Assignment           => Primary_Function,
         Output_Buffer_Enable     => Disable,
         Input_Buffer_Enable      => Disable,
         Drive_Strength_Control   => ds_20pf,
         Open_Drain_Output_Enable => Disable,
         Input_Hysteresis         => Disable,
         Slew_Rate_Control        => Minimum,
         Weak_Pullup_Down_Enable  => Disable,
         Weak_Pullup_Down_Select  => Pulldown);

      Data_Pads_Config : constant SIU.Pad_Configuration_Type :=
        (Pin_Assignment           => Primary_Function,
         Output_Buffer_Enable     => Disable,
         Input_Buffer_Enable      => Disable,
         Drive_Strength_Control   => ds_20pf,
         Open_Drain_Output_Enable => Disable,
         Input_Hysteresis         => Disable,
         Slew_Rate_Control        => Minimum,
         Weak_Pullup_Down_Enable  => Disable,
         Weak_Pullup_Down_Select  => Pulldown);

      External_Bus_Control_1_Pads : constant SIU.Pad_Configuration_Type :=
        (Pin_Assignment           => Primary_Function,
         Output_Buffer_Enable     => Disable,
         Input_Buffer_Enable      => Disable,
         Drive_Strength_Control   => ds_20pf,
         Open_Drain_Output_Enable => Disable,
         Input_Hysteresis         => Disable,
         Slew_Rate_Control        => Minimum,
         Weak_Pullup_Down_Enable  => Disable,
         Weak_Pullup_Down_Select  => Pulldown);

      External_Bus_Control_2_Pads : constant SIU.Pad_Configuration_Type :=
        (Pin_Assignment           => Primary_Function,
         Output_Buffer_Enable     => Disable,
         Input_Buffer_Enable      => Disable,
         Drive_Strength_Control   => ds_20pf,
         Open_Drain_Output_Enable => Disable,
         Input_Hysteresis         => Disable,
         Slew_Rate_Control        => Minimum,
         Weak_Pullup_Down_Enable  => Enable,
         Weak_Pullup_Down_Select  => Pullup);
   begin
      for Pad_ID in Address_Pads_ID_Type loop
         SIU.Pad_Configuration_Register_Array (Pad_ID) := Address_Pads_Config;
      end loop;

      for Pad_ID in Data_Pads_ID_Type loop
         SIU.Pad_Configuration_Register_Array (Pad_ID) := Data_Pads_Config;
      end loop;

      SIU.Pad_Configuration_Register_Array (Chip_Select_0_Register_Pad_No) :=
        External_Bus_Control_1_Pads;
      SIU.Pad_Configuration_Register_Array (Chip_Select_1_Register_Pad_No) :=
        External_Bus_Control_2_Pads;
      SIU.Pad_Configuration_Register_Array (Chip_Select_2_Register_Pad_No) :=
        External_Bus_Control_2_Pads;
      SIU.Pad_Configuration_Register_Array (Chip_Select_3_Register_Pad_No) :=
        External_Bus_Control_2_Pads;

      EBI.Module_Configuration_Register :=
        (Size_Enable                           => Disable,
         Size                                  => Four_Byte,
         Automatic_CLKOUT_Gating_Enable        => Disable,
         External_Master_Mode                  => Disable,
         External_Arbitration                  => Disable,
         External_Arbitration_Requrest_Priorty => Equal,
         Module_Disable_Mode                   => Disable,
         Data_Bus_Mode                         => S32_Bit);

      EBI.Base_Register_Bank_0   :=
        (Base_Address                  => CS0_Base_Address,
         Port_Size                     => S32_Bit,
         Burst_Length                  => Four_Word,
         Write_Enable_Byte_Select      => Write_Enable,
         Toggle_Burst_Data_In_Progress => Burst,
         Burst_Inhibit                 => Disable,
         Valid_Bit                     => Valid);
      EBI.Option_Register_Bank_0 :=
        (Address_Mask           => AM_512k,
         Cycle_Length           => 0,
         Burst_Beat_Length_Type => 0);

      EBI.Base_Register_Bank_1   :=
        (Base_Address                  => CS1_Base_Address,
         Port_Size                     => S32_Bit,
         Burst_Length                  => Eight_Word,
         Write_Enable_Byte_Select      => Write_Enable,
         Toggle_Burst_Data_In_Progress => Burst,
         Burst_Inhibit                 => Disable,
         Valid_Bit                     => Valid);
      EBI.Option_Register_Bank_1 :=
        (Address_Mask           => AM_4M,
         Cycle_Length           => 4,
         Burst_Beat_Length_Type => 0);

      EBI.Base_Register_Bank_2   :=
        (Base_Address                  => CS2_Base_Address,
         Port_Size                     => S32_Bit,
         Burst_Length                  => Four_Word,
         Write_Enable_Byte_Select      => Write_Enable,
         Toggle_Burst_Data_In_Progress => Burst,
         Burst_Inhibit                 => Disable,
         Valid_Bit                     => Invalid);
      EBI.Option_Register_Bank_2 :=
        (Address_Mask           => AM_512k,
         Cycle_Length           => 4,
         Burst_Beat_Length_Type => 1);

      EBI.Base_Register_Bank_3   :=
        (Base_Address                  => CS3_Base_Address,
         Port_Size                     => S32_Bit,
         Burst_Length                  => Four_Word,
         Write_Enable_Byte_Select      => Write_Enable,
         Toggle_Burst_Data_In_Progress => Burst,
         Burst_Inhibit                 => Enable,
         Valid_Bit                     => Invalid);
      EBI.Option_Register_Bank_3 :=
        (Address_Mask           => AM_512k,
         Cycle_Length           => 4,
         Burst_Beat_Length_Type => 1);

   end Setup_External_Bus;

   procedure Setup_PBRIDGE is

      PBRIDGE_Access_Settings      : constant
        PBRIDGE.PACR_OPACR_Access_Field_Type :=
        (Buffer_Writes      => Not_Bufferable,
         Supervisor_Protect => Enable,
         Write_Protect      => Disable,
         Trusted_Protect    => Enable);
      Off_Platform_Access_Settings : constant
        PBRIDGE.PACR_OPACR_Access_Field_Type :=
        (Buffer_Writes      => Not_Bufferable,
         Supervisor_Protect => Enable,
         Write_Protect      => Disable,
         Trusted_Protect    => Disable);

      Default_Master_Privilege_Setting : constant
        MPCR_Access_Field_Type :=
        (Buffer_Writes   => Disable,
         Trusted_Reads   => Trusted,
         Trusted_Writes  => Trusted,
         Privilege_Level => Not_Forced);

      PBRIDGE_Master_Privilege_Control : constant
        Master_Privilege_Control_Type :=
        (CPU   => Default_Master_Privilege_Setting,
         Nexus => Default_Master_Privilege_Setting,
         eDMA  => Default_Master_Privilege_Setting,
         EBI   => Default_Master_Privilege_Setting);

   begin
      PBRIDGE.A_Master_Privilege_Control_Register :=
        PBRIDGE_Master_Privilege_Control;

      PBRIDGE.B_Master_Privilege_Control_Register :=
        PBRIDGE_Master_Privilege_Control;

      PBRIDGE.A_Peripheral_Access_Control_Register_0 :=
        (PBRIDGE_A => PBRIDGE_Access_Settings);
      PBRIDGE.A_Off_Platform_Peripheral_Access_Control_Register_0 :=
        (FMPLL         => Off_Platform_Access_Settings,
         EBI_Control   => Off_Platform_Access_Settings,
         Flash_Control => Off_Platform_Access_Settings,
         SIU           => Off_Platform_Access_Settings);
      PBRIDGE.A_Off_Platform_Peripheral_Access_Control_Register_1 :=
        (eMIOS => Off_Platform_Access_Settings);
      PBRIDGE.A_Off_Platform_Peripheral_Access_Control_Register_2 :=
        (eTPU             => Off_Platform_Access_Settings,
         eTPU_PRAM        => Off_Platform_Access_Settings,
         eTPU_PRAM_Mirror => Off_Platform_Access_Settings,
         eTPU_SCM         => Off_Platform_Access_Settings);

      PBRIDGE.B_Peripheral_Access_Control_Register_0 :=
        (PBRIDGE_B => PBRIDGE_Access_Settings,
         XBAR      => PBRIDGE_Access_Settings);
      PBRIDGE.B_Peripheral_Access_Control_Register_2 :=
        (ESCM   => Off_Platform_Access_Settings,
         eDMA   => Off_Platform_Access_Settings,
         INTC   => Off_Platform_Access_Settings);
      PBRIDGE.B_Off_Platform_Peripheral_Access_Control_Register_0 :=
        (eQADC  => Off_Platform_Access_Settings,
         DSPI_A => Off_Platform_Access_Settings,
         DSPI_B => Off_Platform_Access_Settings,
         DSPI_C => Off_Platform_Access_Settings,
         DSPI_D => Off_Platform_Access_Settings);
      PBRIDGE.B_Off_Platform_Peripheral_Access_Control_Register_1 :=
        (eSCI_A => Off_Platform_Access_Settings,
         eSCI_B => Off_Platform_Access_Settings);
      PBRIDGE.B_Off_Platform_Peripheral_Access_Control_Register_2 :=
        (FlexCAN_A => Off_Platform_Access_Settings,
         FlexCAN_B => Off_Platform_Access_Settings,
         FlexCAN_C => Off_Platform_Access_Settings);
      PBRIDGE.B_Off_Platform_Peripheral_Access_Control_Register_3 :=
        (BAM => Off_Platform_Access_Settings);

   end Setup_PBRIDGE;

   procedure Setup_XBAR is
   begin
      XBAR.Master_Priority_Register_0 :=
        (Master_3_Priority => Lowest,
         Master_2_Priority => Third_Highest,
         Master_1_Priority => Second_Highest,
         Master_0_Priority => Highest);

      XBAR.GP_Control_Register_0 :=
        (Read_Only        => Disable,
         Arbitration_Mode => Fixed,
         Parking_Control  => PARK,
         Park             => Master_Port_0);

      XBAR.Master_Priority_Register_1 :=
        (Master_3_Priority => Lowest,
         Master_2_Priority => Third_Highest,
         Master_1_Priority => Second_Highest,
         Master_0_Priority => Highest);

      XBAR.GP_Control_Register_1 :=
        (Read_Only        => Disable,
         Arbitration_Mode => Fixed,
         Parking_Control  => PARK,
         Park             => Master_Port_0);

      XBAR.Master_Priority_Register_3 :=
        (Master_3_Priority => Lowest,
         Master_2_Priority => Third_Highest,
         Master_1_Priority => Second_Highest,
         Master_0_Priority => Highest);

      XBAR.GP_Control_Register_3 :=
        (Read_Only        => Disable,
         Arbitration_Mode => Fixed,
         Parking_Control  => PARK,
         Park             => Master_Port_0);

      XBAR.Master_Priority_Register_6 :=
        (Master_3_Priority => Lowest,
         Master_2_Priority => Third_Highest,
         Master_1_Priority => Second_Highest,
         Master_0_Priority => Highest);

      XBAR.GP_Control_Register_6 :=
        (Read_Only        => Disable,
         Arbitration_Mode => Fixed,
         Parking_Control  => PARK,
         Park             => Master_Port_0);

      XBAR.Master_Priority_Register_7 :=
        (Master_3_Priority => Lowest,
         Master_2_Priority => Third_Highest,
         Master_1_Priority => Second_Highest,
         Master_0_Priority => Highest);

      XBAR.GP_Control_Register_7 :=
        (Read_Only        => Disable,
         Arbitration_Mode => Fixed,
         Parking_Control  => PARK,
         Park             => Master_Port_0);

   end Setup_XBAR;

end MPC5554.Init;
