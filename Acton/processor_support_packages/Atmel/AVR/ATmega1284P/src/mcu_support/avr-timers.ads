with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;

package AVR.Timers with Preelaborate is

   ----------------------------------------------------------------------------
   --  Memory Address
   ----------------------------------------------------------------------------

   TCCR2A_Address : constant Integer_Address := 16#B0#;
   TCCR2B_Address : constant Integer_Address := 16#B1#;
   TCNT2_Address  : constant Integer_Address := 16#B2#;
   OCR2A_Address  : constant Integer_Address := 16#B3#;
   OCR2B_Address  : constant Integer_Address := 16#B4#;
   ASSR_Address   : constant Integer_Address := 16#B6#;
   TIMSK2_Address : constant Integer_Address := 16#70#;
   TIFR2_Address  : constant Integer_Address := 16#17#;
   GTCCR_Address  : constant Integer_Address := 16#23#;

   ----------------------------------------------------------------------------
   --  Timer Types
   ----------------------------------------------------------------------------

   type General_Timer_Counter_Control_Type is record
      Timer_Counter_Synchronization_Mode : Enable_Type;
      Prescaler_Reset_Timer_Counter_2    : Boolean;
      Prescaler_Reset                    : Boolean;
   end record;

   type COM_Type is (Normal, Toggle, Clear, Set);
   --  Compare Output Mode, non-PWM mode
   --     Normal => Normal port operation OC0x disconnected.
   --     Toggle => Toggle OCxy on compare match
   --     Clear  => Clear OCxy on compare match
   --     Set    => Set OCxy on compare match
   --
   --  Compare Output Mode, fast PWM mode
   --     Normal => Normal port operation OC0x disconnected.
   --     Toggle => WGMx2 = 0: Normal port operation OC0x disconnected.
   --            => WGMx2 = 1: Toggle OC2x on compare match
   --     Clear  => Clear OCxy on compare match, set OCxy at BOTTOM
   --     Set    => Set OCxy on compare match, clear OCxy at BOTTOM
   --
   --  Compare Output Mode, phase corrent PWM mode
   --     Normal => Normal port operation OC0x disconnected.
   --     Toggle => WGMx2 = 0: Normal port operation OC0x disconnected.
   --            => WGMx2 = 1: Toggle OCxy on compare match
   --     Clear  => Clear OCxy on compare match when counting up,
   --            => Set OCxy on compare match when counting down.
   --     Set    => Set OCxy on compare match when counting up,
   --            => Clear OCxy on compare match when counting down.

   type Timer_Counter_Control_A_Type is record
      Compare_Match_Output_A_Mode : COM_Type;
      Compare_Match_Output_B_Mode : COM_Type;
      Waveform_Generation_Mode_0  : Boolean;
      Waveform_Generation_Mode_1  : Boolean;
   end record;

   type Clock_Select_Bits is
     (No_Source,
      No_Scaling,
      Divide_8,
      Divide_32,
      Divide_64,
      Divide_128,
      Divide_256,
      Divide_1024);

   type Timer_Counter_Control_B_Type is record
      Force_Output_Compare_A     : Boolean;
      Force_Output_Compare_B     : Boolean;
      Waveform_Generation_Mode_2 : Boolean;
      Clock_Select               : Clock_Select_Bits;
   end record;

   type Asynchronous_Status_Type is record
      External_Clock_Input                   : Enable_Type;
      Async_Timer_Counter_2                  : Enable_Type;
      Async_Timer_Counter_2_Update_Busy      : Boolean;
      Output_Compare_Register_2A_Update_Busy : Boolean;
      Output_Compare_Register_2B_Update_Busy : Boolean;
      TC_Control_Register_2A_Update_Busy     : Boolean;
      TC_Control_Register_2B_Update_Busy     : Boolean;
   end record;

   type Timer_Counter_Interrupt_Mask_Type is record
      Output_Compare_Match_B_Interrupt : Enable_Type;
      Output_Compare_Match_A_Interrupt : Enable_Type;
      Overflow_Interrupt               : Enable_Type;
   end record;

   type Timer_Counter_Flag_Type is record
      Output_Compare_B : Interrupt_Flag;
      Output_Compare_A : Interrupt_Flag;
      Overflow         : Interrupt_Flag;
   end record;

   ----------------------------------------------------------------------------
   --  Hardware Respresentations
   ----------------------------------------------------------------------------

   for General_Timer_Counter_Control_Type use record
      Timer_Counter_Synchronization_Mode at 0 range 7 .. 7;
      Prescaler_Reset_Timer_Counter_2    at 0 range 1 .. 1;
      Prescaler_Reset                    at 0 range 0 .. 0;
   end record;

   for COM_Type use (Normal => 2#00#, Toggle => 2#01#,
                     Clear  => 2#10#, Set    => 2#11#);
   for Timer_Counter_Control_A_Type use record
      Compare_Match_Output_A_Mode at 0 range 6 .. 7;
      Compare_Match_Output_B_Mode at 0 range 4 .. 5;
      Waveform_Generation_Mode_0  at 0 range 1 .. 1;
      Waveform_Generation_Mode_1  at 0 range 0 .. 0;
   end record;

   for Clock_Select_Bits use
     (No_Source   => 2#000#,
      No_Scaling  => 2#001#,
      Divide_8    => 2#010#,
      Divide_32   => 2#011#,
      Divide_64   => 2#100#,
      Divide_128  => 2#101#,
      Divide_256  => 2#110#,
      Divide_1024 => 2#111#);

   for Timer_Counter_Control_B_Type use record
      Force_Output_Compare_A     at 0 range 7 .. 7;
      Force_Output_Compare_B     at 0 range 6 .. 6;
      Waveform_Generation_Mode_2 at 0 range 3 .. 3;
      Clock_Select               at 0 range 0 .. 2;
   end record;

   for Asynchronous_Status_Type use record
      External_Clock_Input                   at 0 range 6 .. 6;
      Async_Timer_Counter_2                  at 0 range 5 .. 5;
      Async_Timer_Counter_2_Update_Busy      at 0 range 4 .. 4;
      Output_Compare_Register_2A_Update_Busy at 0 range 3 .. 3;
      Output_Compare_Register_2B_Update_Busy at 0 range 2 .. 2;
      TC_Control_Register_2A_Update_Busy     at 0 range 1 .. 1;
      TC_Control_Register_2B_Update_Busy     at 0 range 0 .. 0;
   end record;

   for Timer_Counter_Interrupt_Mask_Type use record
      Output_Compare_Match_B_Interrupt at 0 range 2 .. 2;
      Output_Compare_Match_A_Interrupt at 0 range 1 .. 1;
      Overflow_Interrupt               at 0 range 0 .. 0;
   end record;

   for Timer_Counter_Flag_Type use record
      Output_Compare_B at 0 range 2 .. 2;
      Output_Compare_A at 0 range 1 .. 1;
      Overflow         at 0 range 0 .. 0;
   end record;

   ----------------------------------------------------------------------------
   --  Timer Registers
   ----------------------------------------------------------------------------

   General_Timer_Counter_Control_Register : General_Timer_Counter_Control_Type;
   for General_Timer_Counter_Control_Register'Address use
     System'To_Address (GTCCR_Address);

   Timer_Counter2_Control_Register_A : Timer_Counter_Control_A_Type;
   for Timer_Counter2_Control_Register_A'Address use
     System'To_Address (TCCR2A_Address);

   Timer_Counter2_Control_Register_B :  Timer_Counter_Control_B_Type;
   for Timer_Counter2_Control_Register_B'Address use
     System'To_Address (TCCR2B_Address);

   Timer_Counter2_Register : Unsigned_8;
   for Timer_Counter2_Register'Address use System'To_Address (TCNT2_Address);

   Output_Compare2_Register_A : Unsigned_8;
   for Output_Compare2_Register_A'Address use
     System'To_Address (OCR2A_Address);

   Output_Compare2_Register_B : Unsigned_8;
   for Output_Compare2_Register_B'Address use
     System'To_Address (OCR2B_Address);

   Asynchronous_Status_Register : Asynchronous_Status_Type;
   for Asynchronous_Status_Register'Address use
     System'To_Address (ASSR_Address);

   Timer_Counter2_Interrupt_Mask_Register : Timer_Counter_Interrupt_Mask_Type;
   for Timer_Counter2_Interrupt_Mask_Register'Address use
     System'To_Address (TIMSK2_Address);

   Timer_Counter2_Interrupt_Flag_Register : Timer_Counter_Flag_Type;
   for Timer_Counter2_Interrupt_Flag_Register'Address use
     System'To_Address (TIFR2_Address);
end AVR.Timers;
