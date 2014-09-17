with System; use System;

package ST.STM32F4.EXTI with Preelaborate is

   ---------------------------
   -- EXTI Memory Addresses --
   ---------------------------

   EXTI_Base_Address    : constant := 16#4001_3C00#;
   IMR_Offset_Address   : constant := 16#00#;
   EMR_Offset_Address   : constant := 16#04#;
   RTSR_Offset_Address  : constant := 16#08#;
   FTSR_Offset_Address  : constant := 16#0C#;
   SWIER_Offset_Address : constant := 16#10#;
   PR_Offset_Address    : constant := 16#14#;

   -----------------------
   -- Hardware Features --
   -----------------------

   type EXTI_Lines is mod 23;

   ----------------
   -- EXTI Types --
   ----------------

   type EXTI_Boolean_Set is array (EXTI_Lines) of Boolean with Pack;

   type Interrupt_Mask is record
      Interrupt_Request : EXTI_Boolean_Set;
   end record;

   type Event_Mask is record
      Event_Request : EXTI_Boolean_Set;
   end record;

   type Rising_Trigger_Selection is record
      Rising_Trigger : EXTI_Boolean_Set;
   end record;

   type Falling_Trigger_Selection is record
      Falling_Trigger : EXTI_Boolean_Set;
   end record;

   type Software_Interrupt is record
      Interrupt_Occurred : EXTI_Boolean_Set;
   end record;

   type EXTI_Pending is record
      Event_Occurred : EXTI_Boolean_Set;
   end record;

   ------------------------------
   -- Hardware Representations --
   ------------------------------

   for Interrupt_Mask use record
      Interrupt_Request at 0 range 0 .. 22;
   end record;

   for Event_Mask use record
      Event_Request at 0 range 0 .. 22;
   end record;

   for Rising_Trigger_Selection use record
      Rising_Trigger at 0 range 0 .. 22;
   end record;

   for Falling_Trigger_Selection use record
      Falling_Trigger at 0 range 0 .. 22;
   end record;

   for Software_Interrupt use record
      Interrupt_Occurred at 0 range 0 .. 22;
   end record;

   for EXTI_Pending use record
      Event_Occurred at 0 range 0 .. 22;
   end record;

   --------------------
   -- EXTI Registers --
   --------------------

   Interrupt_Mask_Register : Interrupt_Mask
     with Address =>
       System'To_Address (EXTI_Base_Address + IMR_Offset_Address);

   Event_Mask_Register : Event_Mask
     with Address =>
       System'To_Address (EXTI_Base_Address + EMR_Offset_Address);

   Rising_Trigger_Selection_Register : Rising_Trigger_Selection
     with Address =>
       System'To_Address (EXTI_Base_Address + RTSR_Offset_Address);

   Falling_Trigger_Selection_Register : Falling_Trigger_Selection
     with Address =>
       System'To_Address (EXTI_Base_Address + FTSR_Offset_Address);

   Software_Interrupt_Register : Software_Interrupt
     with Address =>
       System'To_Address (EXTI_Base_Address + SWIER_Offset_Address);

   Interrupt_Pending_Register : EXTI_Pending
     with Address =>
       System'To_Address (EXTI_Base_Address + PR_Offset_Address);

end ST.STM32F4.EXTI;
