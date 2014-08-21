with ST;               use ST;
with ST.STM32F4;       use ST.STM32F4;
with ST.STM32F4.Flash; use ST.STM32F4.Flash;
with ST.STM32F4.RCC;   use ST.STM32F4.RCC;

package body Oak.Project_Support_Package.Microcontroller is

   procedure Setup_Clocks;

   procedure Setup is
   begin
      Setup_Clocks;
   end Setup;

   procedure Setup_Clocks is
   begin
      --  Enable Power Interface
      RCC.APB1_Peripheral_Clock_Enable_Register :=
        (Power_Interface => Enable,
         others          => Disable);

      --  Enable high-speed internal clock. Used as fall back if PLL for the
      --  high-speed external clock fails.

      RCC.Clock_Control_Register.High_Speed_Internal_Clock := Enable;

      loop
         exit when RCC.Clock_Control_Register.High_Speed_Internal_Clock_Ready;
      end loop;

      --  Enable high-speed external clock

      RCC.Clock_Control_Register.High_Speed_External_Clock := Enable;

      loop
         exit when RCC.Clock_Control_Register.High_Speed_External_Clock_Ready;
      end loop;

      --  Setup low-speed internal clock

      RCC.Clock_Control_And_Status_Register.Low_Speed_Internal_Clock :=
        Enable;

      loop
         exit when RCC.Clock_Control_And_Status_Register.
           Low_Speed_Internal_Clock_Ready;
      end loop;

      --  Setup the PLL used by the high-speed external clock
      --  Set for a 168MHz clock using an external 8MHz crystal

      RCC.PLL_Configuration_Register :=
        (Peripheral_Clock_Divider => 7,
         Clock_Source             => High_Speed_External_Clock,
         System_Clock_Divider     => Divide_By_2,
         VCO_Multiplier           => 336,
         Input_Clock_Divider      => 8);

      RCC.Clock_Control_Register.PLL_Main := Enable;

      loop
         exit when RCC.Clock_Control_Register.PLL_Main_Locked;
      end loop;

      --  Configure flash states before attempting to use the PLL as the system
      --  clock, otherwise access to the flash will be lost.

      Flash.Access_Control_Register :=
        (Data_Cache        => Enable,
         Instruction_Cache => Enable,
         Prefetch          => Enable,
         Wait_States       => 5,
         others            => Do_Not_Reset);

      --  Setup derived clocks for similar reason

      RCC.Clock_Configuration_Register :=
        (Microcontroller_Clock_Output_2           => System_Clock,
         Microcontroller_Clock_Output_2_Prescaler => Divide_By_5,
         Microcontroller_Clock_Output_1_Prescaler => No_Division,
         I2S_Clock_Selection                      => PLL_I2S,
         Microcontroller_Clock_Output_1           => High_Speed_Internal_Clock,
         HSE_Clock_Divider_For_RTC                => 0,
         APB1_Prescaler                           => Divide_By_2,
         APB2_Prescaler                           => Divide_By_4,
         AHB_Prescaler                            => No_Division,
         System_Clock_Status                      => PLL, -- not written
         System_Clock_Source                      => PLL);

      loop
         exit when
           RCC.Clock_Configuration_Register.System_Clock_Status = PLL;
      end loop;
   end Setup_Clocks;

end Oak.Project_Support_Package.Microcontroller;
