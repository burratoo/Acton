with Interfaces; use Interfaces;
with System;     use System;
package Atmel.AT91SAM7S.TC with Preelaborate is

   ----------------------------------------
   -- Public Hardware Features and Types --
   ----------------------------------------

   type Timer_Counter_Channel_Id is range 0 .. 2;

   type External_Clock_Pin is (TCLK0, None, TCLK1, TCLK2);
   type External_Clock_Pin_Set is array (Timer_Counter_Channel_Id) of
     External_Clock_Pin with Pack;

   type Block_Mode_Type is record
      External_Clock_Signal : External_Clock_Pin_Set;
   end record with Size => Standard'Word_Size;

   type Counter_Type is (Capture_Mode, Waveform_Mode);
   for Counter_Type use (Capture_Mode => 0, Waveform_Mode => 1);

   type Clock_Selection_Options is
     (Timer_Clock1, Timer_Clock2, Timer_Clock3,
      Timer_Clock4, Timer_Clock5, XC0, XC1, XC2);

   type Clock_Edge is (Rising, Falling);
   type Burst_Signal_Selection_Type is (None, XC0, XC1, XC2);
   type Edge_Type is (None, Rising_Edge, Falling_Edge, Each_Edge);
   type External_Trigger_Pin is (TIOA, TIOB);
   type External_Event_Signal is (TIOB, XC0, XC1, XC2);
   type Waveform_Type is
     (UP_Mode_Without_Automatic_Tigger_On_Register_C_Compare,
      UP_Mode_With_Automatic_Tigger_On_Register_C_Compare,
      UPDOWN_Mode_Without_Automatic_Tigger_On_Register_C_Compare,
      UPDOWN_Mode_With_Automatic_Tigger_On_Register_C_Compare);
   type Event_Effect is (None, Set, Clear, Toggle);

   type Channel_Mode_Type
     (Counter_Mode : Counter_Type := Capture_Mode) is record
      Clock_Selection           : Clock_Selection_Options;
      Counter_Increment_On_Edge : Clock_Edge;
      Burst_Signal_Selection    : Burst_Signal_Selection_Type;

      case Counter_Mode is
         when Capture_Mode =>
            Counter_Clock_Stopped_When_RegB_Loading  : Boolean;
            Counter_Clock_Disabled_When_RegB_Loading : Boolean;
            External_Trigger_Edge                    : Edge_Type;
            External_Trigger_Source                  : External_Trigger_Pin;
            Register_C_Compare_Trigger               : Enable_Type;
            Register_A_Loading_Event                 : Edge_Type;
            Register_B_Loading_Event                 : Edge_Type;

         when Waveform_Mode =>
            Counter_Clock_Stopped_When_RegC_Compare  : Boolean;
            Counter_Clock_Disabled_When_RegC_Compare : Boolean;
            External_Event_Edge_Selection            : Edge_Type;
            External_Event_Signal_Selection          : External_Event_Signal;
            External_Event_Trigger                   : Enable_Type;
            Waveform_Selection                       : Waveform_Type;
            Register_A_Compare_Effect_On_TIOA        : Event_Effect;
            Register_C_Compare_Effect_On_TIOA        : Event_Effect;
            External_Event_Effect_On_TIOA            : Event_Effect;
            Software_Trigger_Effect_On_TIOA          : Event_Effect;
            Register_A_Compare_Effect_On_TIOB        : Event_Effect;
            Register_C_Compare_Effect_On_TIOB        : Event_Effect;
            External_Event_Effect_On_TIOB            : Event_Effect;
            Software_Trigger_Effect_On_TIOB          : Event_Effect;
      end case;
   end record with Size => Standard'Word_Size;

   ----------------------
   -- Public Interface --
   ----------------------

   procedure Initialise_Interface (Settings : Block_Mode_Type);
   procedure Initialise_Channel (Channel    : Timer_Counter_Channel_Id;
                                 Settings   : Channel_Mode_Type;
                                 Register_A : Unsigned_16;
                                 Register_B : Unsigned_16;
                                 Register_C : Unsigned_16);

   function Interface_Is_Ready return Boolean;

   procedure Disable_Channel (Channel : Timer_Counter_Channel_Id) with Inline;
   procedure Enable_Channel (Channel : Timer_Counter_Channel_Id) with Inline;
   procedure Software_Trigger (Channel : Timer_Counter_Channel_Id) with Inline;
   procedure Start_Timer (Channel : Timer_Counter_Channel_Id) with Inline;

private

   -------------------------
   -- TC Memory Addresses --
   -------------------------

   TC_Base_Address    : constant  := 16#FFFA_0000#;
   CCR_Offset_Address : constant  := 16#00#;
   CMR_Offset_Address : constant  := 16#04#;
   CV_Offset_Address  : constant  := 16#10#;
   RA_Offset_Address  : constant  := 16#14#;
   RB_Offset_Address  : constant  := 16#18#;
   RC_Offset_Address  : constant  := 16#1C#;
   SR_Offset_Address  : constant  := 16#20#;
   IER_Offset_Address : constant  := 16#24#;
   IDR_Offset_Address : constant  := 16#28#;
   IMR_Offset_Address : constant  := 16#2C#;
   BCR_Offset_Address : constant  := 16#C0#;
   BMR_Offset_Address : constant  := 16#C4#;

   -----------------------
   -- Hardware Features --
   -----------------------

   --------------
   -- TC Types --
   --------------

   type Block_Control_Register_Type is record
      Send_Synchro_Command : Boolean;
   end record with Size => Standard'Word_Size;

   type Channel_Control_Type is record
      Counter_Clock_Enable  : Enable_No_Change_Type;
      Counter_Clock_Disable : Disable_No_Change_Type;
      Software_Tigger       : Boolean;
   end record;

   type Channel_Status_Type is record
      Counter_Overflow   : Occured_Type;
      Load_Overrun       : Occured_Type;
      Register_A_Compare : Occured_Type;
      Register_B_Compare : Occured_Type;
      Register_C_Compare : Occured_Type;
      Register_A_Load    : Occured_Type;
      Register_B_Load    : Occured_Type;
      External_Trigger   : Occured_Type;
      Clock              : Enabled_Type;
      TIOA               : Pin_Status;
      TIOB               : Pin_Status;
   end record with Size => Standard'Word_Size;

   type Interrupt_Enable_Type is record
      Counter_Overflow   : Enable_No_Change_Type;
      Load_Overrun       : Enable_No_Change_Type;
      Register_A_Compare : Enable_No_Change_Type;
      Register_B_Compare : Enable_No_Change_Type;
      Register_C_Compare : Enable_No_Change_Type;
      Register_A_Load    : Enable_No_Change_Type;
      Register_B_Load    : Enable_No_Change_Type;
      External_Trigger   : Enable_No_Change_Type;
   end record with Size => Standard'Word_Size;

   type Interrupt_Disable_Type is record
      Counter_Overflow   : Disable_No_Change_Type;
      Load_Overrun       : Disable_No_Change_Type;
      Register_A_Compare : Disable_No_Change_Type;
      Register_B_Compare : Disable_No_Change_Type;
      Register_C_Compare : Disable_No_Change_Type;
      Register_A_Load    : Disable_No_Change_Type;
      Register_B_Load    : Disable_No_Change_Type;
      External_Trigger   : Disable_No_Change_Type;
   end record with Size => Standard'Word_Size;

   type Interrupt_Mask_Type is record
      Counter_Overflow   : Enabled_Type;
      Load_Overrun       : Enabled_Type;
      Register_A_Compare : Enabled_Type;
      Register_B_Compare : Enabled_Type;
      Register_C_Compare : Enabled_Type;
      Register_A_Load    : Enabled_Type;
      Register_B_Load    : Enabled_Type;
      External_Trigger   : Enabled_Type;
   end record with Size => Standard'Word_Size;

   pragma Warnings (Off, "*unused*");
   type Timer_Channel_Type is record
      Channel_Control_Register   : Channel_Control_Type;
      Channel_Mode_Register      : Channel_Mode_Type;
      Counter_Value              : Unsigned_16;
      Register_A                 : Unsigned_16;
      Register_B                 : Unsigned_16;
      Register_C                 : Unsigned_16;
      Status_Register            : Channel_Status_Type;
      Interrupt_Enable_Register  : Interrupt_Enable_Type;
      Interrupt_Disable_Register : Interrupt_Disable_Type;
      Interrupt_Mask_Register    : Interrupt_Mask_Type;
      Padding_At_End             : Boolean;
   end record with Size => 16#40# * 32;

   ------------------------------
   -- Hardware Representations --
   ------------------------------

   for Block_Control_Register_Type use record
      Send_Synchro_Command at 0 range 0 .. 0;
   end record;

   for Block_Mode_Type use record
      External_Clock_Signal at 0 range 0 .. 5;
   end record;

   for External_Clock_Pin use (TCLK0 => 0, None => 1, TCLK1 => 2, TCLK2 => 3);

   for Channel_Control_Type use record
      Counter_Clock_Enable  at 0 range 0 .. 0;
      Counter_Clock_Disable at 0 range 1 .. 1;
      Software_Tigger       at 0 range 2 .. 2;
   end record;

   for Clock_Selection_Options use
     (Timer_Clock1 => 0, Timer_Clock2 => 1, Timer_Clock3 => 2,
      Timer_Clock4 => 3, Timer_Clock5 => 4, XC0 => 5, XC1 => 6, XC2 => 7);

   for Clock_Edge use (Rising => 0, Falling => 1);
   for Burst_Signal_Selection_Type use (None => 0,
                                        XC0  => 1,
                                        XC1  => 2,
                                        XC2  => 3);
   for Edge_Type use (None        => 0,
                      Rising_Edge  => 1,
                      Falling_Edge => 2,
                      Each_Edge    => 3);

   for External_Trigger_Pin use (TIOA => 0, TIOB => 1);
   for External_Event_Signal use (TIOB => 0, XC0 => 1, XC1 => 2, XC2 => 3);
   for Waveform_Type use
     (UP_Mode_Without_Automatic_Tigger_On_Register_C_Compare     => 0,
      UP_Mode_With_Automatic_Tigger_On_Register_C_Compare        => 1,
      UPDOWN_Mode_Without_Automatic_Tigger_On_Register_C_Compare => 2,
      UPDOWN_Mode_With_Automatic_Tigger_On_Register_C_Compare    => 3);
   for Event_Effect use (None => 0, Set => 1, Clear => 2, Toggle => 3);

   for Channel_Mode_Type use record
      Clock_Selection           at 0 range 0 .. 2;
      Counter_Increment_On_Edge at 0 range 3 .. 3;
      Burst_Signal_Selection    at 0 range 4 .. 5;
      Counter_Mode              at 0 range 15 .. 15;

      Counter_Clock_Stopped_When_RegB_Loading  at 0 range 6 .. 6;
      Counter_Clock_Disabled_When_RegB_Loading at 0 range 7 .. 7;
      External_Trigger_Edge                    at 0 range 8 .. 9;
      External_Trigger_Source                  at 0 range 10 .. 10;
      Register_C_Compare_Trigger               at 0 range 14 .. 14;
      Register_A_Loading_Event                 at 0 range 16 .. 17;
      Register_B_Loading_Event                 at 0 range 18 .. 19;

      Counter_Clock_Stopped_When_RegC_Compare  at 0 range 6 .. 6;
      Counter_Clock_Disabled_When_RegC_Compare at 0 range 7 .. 7;
      External_Event_Edge_Selection            at 0 range 8 .. 9;
      External_Event_Signal_Selection          at 0 range 10 .. 11;
      External_Event_Trigger                   at 0 range 12 .. 12;
      Waveform_Selection                       at 0 range 13 .. 14;
      Register_A_Compare_Effect_On_TIOA        at 0 range 16 .. 17;
      Register_C_Compare_Effect_On_TIOA        at 0 range 18 .. 19;
      External_Event_Effect_On_TIOA            at 0 range 20 .. 21;
      Software_Trigger_Effect_On_TIOA          at 0 range 22 .. 23;
      Register_A_Compare_Effect_On_TIOB        at 0 range 24 .. 25;
      Register_C_Compare_Effect_On_TIOB        at 0 range 26 .. 27;
      External_Event_Effect_On_TIOB            at 0 range 28 .. 29;
      Software_Trigger_Effect_On_TIOB          at 0 range 30 .. 31;
   end record;

   for Channel_Status_Type use record
      Counter_Overflow   at 0 range 0 .. 0;
      Load_Overrun       at 0 range 1 .. 1;
      Register_A_Compare at 0 range 2 .. 2;
      Register_B_Compare at 0 range 3 .. 3;
      Register_C_Compare at 0 range 4 .. 4;
      Register_A_Load    at 0 range 5 .. 5;
      Register_B_Load    at 0 range 6 .. 6;
      External_Trigger   at 0 range 7 .. 7;
      Clock              at 0 range 16 .. 16;
      TIOA               at 0 range 17 .. 17;
      TIOB               at 0 range 18 .. 18;
   end record;

   for Interrupt_Enable_Type use record
      Counter_Overflow   at 0 range 0 .. 0;
      Load_Overrun       at 0 range 1 .. 1;
      Register_A_Compare at 0 range 2 .. 2;
      Register_B_Compare at 0 range 3 .. 3;
      Register_C_Compare at 0 range 4 .. 4;
      Register_A_Load    at 0 range 5 .. 5;
      Register_B_Load    at 0 range 6 .. 6;
      External_Trigger   at 0 range 7 .. 7;
   end record;

   for Interrupt_Disable_Type use record
      Counter_Overflow   at 0 range 0 .. 0;
      Load_Overrun       at 0 range 1 .. 1;
      Register_A_Compare at 0 range 2 .. 2;
      Register_B_Compare at 0 range 3 .. 3;
      Register_C_Compare at 0 range 4 .. 4;
      Register_A_Load    at 0 range 5 .. 5;
      Register_B_Load    at 0 range 6 .. 6;
      External_Trigger   at 0 range 7 .. 7;
   end record;

   for Interrupt_Mask_Type use record
      Counter_Overflow   at 0 range 0 .. 0;
      Load_Overrun       at 0 range 1 .. 1;
      Register_A_Compare at 0 range 2 .. 2;
      Register_B_Compare at 0 range 3 .. 3;
      Register_C_Compare at 0 range 4 .. 4;
      Register_A_Load    at 0 range 5 .. 5;
      Register_B_Load    at 0 range 6 .. 6;
      External_Trigger   at 0 range 7 .. 7;
   end record;

   for Timer_Channel_Type use record
      Channel_Control_Register   at CCR_Offset_Address range 0 .. 31;
      Channel_Mode_Register      at CMR_Offset_Address range 0 .. 31;
      Counter_Value              at CV_Offset_Address  range 0 .. 15;
      Register_A                 at RA_Offset_Address  range 0 .. 15;
      Register_B                 at RB_Offset_Address  range 0 .. 15;
      Register_C                 at RC_Offset_Address  range 0 .. 15;
      Status_Register            at SR_Offset_Address  range 0 .. 31;
      Interrupt_Enable_Register  at IER_Offset_Address range 0 .. 31;
      Interrupt_Disable_Register at IDR_Offset_Address range 0 .. 31;
      Interrupt_Mask_Register    at IMR_Offset_Address range 0 .. 31;
      Padding_At_End             at IMR_Offset_Address + 4 range 0 .. 512;
   end record;

   -------------------
   -- TC Registers --
   -------------------

   type Timer_Channel_Set_Type is array (Timer_Counter_Channel_Id) of
     Timer_Channel_Type
     with Alignment => 4, Component_Size => 16#40# * 32;

   Block_Control_Register : Block_Control_Register_Type
     with Address => System'To_Address (TC_Base_Address + BCR_Offset_Address);

   Block_Mode_Register    : Block_Mode_Type
     with Address => System'To_Address (TC_Base_Address + BMR_Offset_Address);

   Timer_Channel_0 : Timer_Channel_Type
     with Alignment => 4, Address => System'To_Address (TC_Base_Address),
          Import, Convention => Ada;

   Timer_Channel_1 : Timer_Channel_Type
     with Alignment => 4,
          Address => System'To_Address (TC_Base_Address + 16#40#),
          Import, Convention => Ada;

   Timer_Channel_2 : Timer_Channel_Type
     with Alignment => 4,
          Address => System'To_Address (TC_Base_Address + 2 * 16#40#),
          Import, Convention => Ada;

   -------------------
   -- Internal Data --
   -------------------

   Interface_Ready : Boolean := False;

   --------------------------
   -- Expression Functions --
   --------------------------

   function Interface_Is_Ready return Boolean is
     (Interface_Ready);

end Atmel.AT91SAM7S.TC;
