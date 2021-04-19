------------------------------------------------------------------------------------------
--                                                                                      --
--                            ACTON PROCESSOR SUPPORT PACKAGE                           --
--                                                                                      --
--                                  ATMEL.AT91SAM7S.TC                                  --
--                                                                                      --
--                       Copyright (C) 2014-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

with Atmel.AT91SAM7S.PMC;

package body Atmel.AT91SAM7S.TC is

   procedure Initialise_Interface (Settings : Block_Mode_Type) is
   begin
      Block_Mode_Register := Settings;
      Interface_Ready     := True;
   end Initialise_Interface;

   procedure Initialise_Channel
     (Channel  : Timer_Counter_Channel_Id;
      Settings   : Channel_Mode_Type;
      Register_A : Unsigned_16;
      Register_B : Unsigned_16;
      Register_C : Unsigned_16) is
   begin
      case Channel is
         when 0 =>
            PMC.Peripheral_Clock_Enable_Register :=
              (P_TC0  => Enable,
               others => No_Change);
         when 1 =>
            PMC.Peripheral_Clock_Enable_Register :=
              (P_TC1  => Enable,
               others => No_Change);
         when 2 =>
            PMC.Peripheral_Clock_Enable_Register :=
              (P_TC2  => Enable,
               others => No_Change);
      end case;

      Timer_Channel (Channel).Channel_Mode_Register := Settings;
      Timer_Channel (Channel).Register_A := Register_A;
      Timer_Channel (Channel).Register_B := Register_B;
      Timer_Channel (Channel).Register_C := Register_C;
   end Initialise_Channel;

   procedure Enable_Channel (Channel : Timer_Counter_Channel_Id) is
   begin
      Timer_Channel (Channel).Channel_Control_Register :=
        (Counter_Clock_Enable  => Enable,
         Counter_Clock_Disable => No_Change,
         Software_Trigger      => False);
   end Enable_Channel;

   procedure Disable_Channel (Channel : Timer_Counter_Channel_Id) is
   begin
      Timer_Channel (Channel).Channel_Control_Register :=
        (Counter_Clock_Enable  => No_Change,
         Counter_Clock_Disable => Disable,
         Software_Trigger      => False);
   end Disable_Channel;

   procedure Software_Trigger (Channel : Timer_Counter_Channel_Id) is
   begin
      Timer_Channel (Channel).Channel_Control_Register :=
        (Counter_Clock_Enable  => No_Change,
         Counter_Clock_Disable => No_Change,
         Software_Trigger      => True);
   end Software_Trigger;

   procedure Start_Timer (Channel : Timer_Counter_Channel_Id) is
   begin
      Timer_Channel (Channel).Channel_Control_Register :=
        (Counter_Clock_Enable  => Enable,
         Counter_Clock_Disable => No_Change,
         Software_Trigger      => True);
   end Start_Timer;

end Atmel.AT91SAM7S.TC;
