package ISA.Power.e200.Timer_Registers is

   pragma Pure;
   -------
   --  Time Control Register Type
   -------
   type WP_Type is mod 2 ** 2;
   type WTRC_Type is mod 2 ** 2;
   type FITP_Type is mod 2 ** 2;
   type WPEXT_Type is mod 2 ** 4;
   type FPEXT_Type is mod 2 ** 4;

   type Timer_Control_Register_Type is record
      Watchdog_Timer_Period                 : WP_Type;
      Watchdog_Timer_Reset_Control          : WTRC_Type;
      Watchdog_Timer_Interrupt              : Enable_Type;
      Decrementer_Interrupt                 : Enable_Type;
      Fixed_Interval_Timer_Period           : FITP_Type;
      Fixed_Interval_Interrupt              : Enable_Type;
      Auto_Reload                           : Enable_Type;
      Watchdog_Timer_Period_Extension       : WPEXT_Type;
      Fixed_Interval_Timer_Period_Extension : FPEXT_Type;
   end record;

   type Event_Status is (Not_Occurred, Occurred);
   type Watchgdog_Status_Type is mod 2 ** 2;

   type Timer_Status_Register_Type is record
      Next_Watchdog_Time       : Enable_Type;
      Watchdog_Timer_Interrupt : Event_Status;
      Watchdog_Timer_Reset     : Watchgdog_Status_Type;
      Decrement_Interrupt      : Event_Status;
      Fixed_Interval_Interrupt : Event_Status;
   end record;

   ---------------
   --  Hardware Representation
   ---------------

   for Timer_Control_Register_Type use record
      Watchdog_Timer_Period                 at 0 range 0 .. 1;
      Watchdog_Timer_Reset_Control          at 0 range 2 .. 3;
      Watchdog_Timer_Interrupt              at 0 range 4 .. 4;
      Decrementer_Interrupt                 at 0 range 5 .. 5;
      Fixed_Interval_Timer_Period           at 0 range 6 .. 7;
      Fixed_Interval_Interrupt              at 0 range 8 .. 8;
      Auto_Reload                           at 0 range 9 .. 9;
      Watchdog_Timer_Period_Extension       at 0 range 11 .. 14;
      Fixed_Interval_Timer_Period_Extension at 0 range 15 .. 18;
   end record;

   for Timer_Control_Register_Type'Size use Register_Size_32;

   for Event_Status use (Not_Occurred => 0, Occurred => 1);

   for Timer_Status_Register_Type use record
      Next_Watchdog_Time       at 0 range 0 .. 0;
      Watchdog_Timer_Interrupt at 0 range 1 .. 1;
      Watchdog_Timer_Reset     at 0 range 2 .. 3;
      Decrement_Interrupt      at 0 range 4 .. 4;
      Fixed_Interval_Interrupt at 0 range 5 .. 5;
   end record;

   for Timer_Status_Register_Type'Size use Register_Size_32;

end ISA.Power.e200.Timer_Registers;
