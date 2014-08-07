with Atmel.AT91SAM7S.PMC;

package body Atmel.AT91SAM7S.ADC is

   procedure Initialise_Interface (Settings : Mode_Type) is
   begin
      --  Turn on ADC clock
      PMC.Peripheral_Clock_Enable_Register :=
        (P_ADC  => Enable,
         others => No_Change);

      Mode_Register := Settings;

      Interface_Ready := True;
   end Initialise_Interface;

   procedure Enable_Channel (Channel : ADC_Channel_Id) is
      Channel_To_Enable : ADC_Enable_Set := (others => No_Change);
   begin
      Channel_To_Enable (Channel) := Enable;
      Channel_Enable_Register.Channel := Channel_To_Enable;
   end Enable_Channel;

   procedure Disable_Channel (Channel : ADC_Channel_Id) is
      Channel_To_Disable : ADC_Disable_Set := (others => No_Change);
   begin
      Channel_To_Disable (Channel) := Disable;
      Channel_Disable_Register.Channel := Channel_To_Disable;
   end Disable_Channel;

end Atmel.AT91SAM7S.ADC;
