package ISA.Power.e200.z6.HID with Pure is

   type Branch_Prediction_Type is (
      Enable,
      Disable_Backwards,
      Disable_Forward,
      Disable);
   type Hardware_Reset_Type is (Occurred, Not_Occurred);
   type Time_Base_Source is (Processor_Clock, P_Tbclk);

   type Hardware_Implementation_Dependent_Register_0_Type is record
      Machine_Check_Signal                  : Enable_Type;
      Branch_Prediction_Control             : Branch_Prediction_Type;
      Doze_Mode                             : Enable_Type;
      Nap_Mode                              : Enable_Type;
      Sleep_Mode                            : Enable_Type;
      Interrupt_Inputs_Clear_Reservation    : Decision_Type;
      Hardware_Reset                        : Hardware_Reset_Type;
      Time_Base                             : Enable_Type;
      Select_Time_Base_Clock                : Time_Base_Source;
      Debug_Interrupt_Clears_MSR_EE         : Decision_Type;
      Debug_Interrupt_Clears_MSR_CE         : Decision_Type;
      Critical_Interrupt_Clears_MSR_DE      : Decision_Type;
      Machine_Check_Interrupt_Clears_MSR_DE : Decision_Type;
      Debug_APU                             : Enable_Type;
   end record with Size => Standard'Word_Size;

   for Branch_Prediction_Type use
     (Enable            => 2#00#,
      Disable_Backwards => 2#01#,
      Disable_Forward   => 2#10#,
      Disable           => 2#11#);
   for Hardware_Reset_Type use (Occurred => 0, Not_Occurred => 1);
   for Time_Base_Source use (Processor_Clock => 0, P_Tbclk => 1);

   for Hardware_Implementation_Dependent_Register_0_Type use record
      Machine_Check_Signal                  at 0 range 0 .. 0;
      Branch_Prediction_Control             at 0 range 6 .. 7;
      Doze_Mode                             at 0 range 8 .. 8;
      Nap_Mode                              at 0 range 9 .. 9;
      Sleep_Mode                            at 0 range 10 .. 10;
      Interrupt_Inputs_Clear_Reservation    at 0 range 14 .. 14;
      Hardware_Reset                        at 0 range 15 .. 15;
      Time_Base                             at 0 range 17 .. 17;
      Select_Time_Base_Clock                at 0 range 18 .. 18;
      Debug_Interrupt_Clears_MSR_EE         at 0 range 19 .. 19;
      Debug_Interrupt_Clears_MSR_CE         at 0 range 20 .. 20;
      Critical_Interrupt_Clears_MSR_DE      at 0 range 21 .. 21;
      Machine_Check_Interrupt_Clears_MSR_DE at 0 range 22 .. 22;
      Debug_APU                             at 0 range 23 .. 23;
   end record;

end ISA.Power.e200.z6.HID;
