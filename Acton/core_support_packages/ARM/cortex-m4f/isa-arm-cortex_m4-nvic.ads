------------------------------------------------------------------------------------------
--                                                                                      --
--                               OAK CORE SUPPORT PACKAGE                               --
--                                    ARM CORTEX M4F                                    --
--                                                                                      --
--                                ISA.ARM.CORTEX_M4.NVIC                                --
--                                                                                      --
--                       Copyright (C) 2014-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

with ISA.ARM.Cortex_M4.Exceptions; use ISA.ARM.Cortex_M4.Exceptions;

with System; use System;

package ISA.ARM.Cortex_M4.NVIC with Preelaborate is

   ---------------------------
   -- NVIC Memory Addresses --
   ---------------------------

   NVIC_Base_Address   : constant := 16#E000_E000#;
   ISER_Offset_Address : constant := 16#100#;
   ISCR_Offset_Address : constant := 16#180#;
   ISPR_Offset_Address : constant := 16#200#;
   ICPR_Offset_Address : constant := 16#280#;
   IABR_Offset_Address : constant := 16#300#;
   IPR_Offset_Address  : constant := 16#400#;
   STIR_Offset_Address : constant := 16#F00#;

   -----------------------
   -- Hardware Features --
   -----------------------

   subtype NVIC_Interrupt_Id is Exception_Id range 0 .. 240;

   ----------------
   -- NVIC Types --
   ----------------

   type Interrupt_Enable_Set is array (NVIC_Interrupt_Id) of
     Enable_No_Change_Type with Pack;

   type Interrupt_Disable_Set is array (NVIC_Interrupt_Id) of
     Disable_No_Change_Type with Pack;

   type Interrupt_Enabled_Set is array (NVIC_Interrupt_Id) of Enabled_Type
     with Pack;

   type Set_Type is (No_Change, Set);

   type Interrupt_Set_Set is array (NVIC_Interrupt_Id) of Set_Type
     with Pack;

   type Clear_Type is (No_Change, Clear);

   type Interrupt_Clear_Set is array (NVIC_Interrupt_Id) of Clear_Type
     with Pack;

   type Pending_Type is (Not_Pending, Pending);

   type Interrupt_Pending_Set is array (NVIC_Interrupt_Id) of Pending_Type
     with Pack;

   type Active_Type is (Inactive, Active);

   type Interrupt_Active_Set is array (NVIC_Interrupt_Id) of Active_Type
     with Pack;

   type Software_Trigger_Interrupt_Type is record
      Interrupt : NVIC_Interrupt_Id;
   end record with Size => Word_Size;

   ------------------------------
   -- Hardware Representations --
   ------------------------------

   for Software_Trigger_Interrupt_Type use record
      Interrupt at 0 range 0 .. 31;
      --  This should be 0 .. 8, but need to force a 32 bit access to the field
   end record;

   --------------------
   -- NVIC Registers --
   --------------------

   Interrupt_Enable_Register : Interrupt_Enable_Set
     with Address => System'To_Address (NVIC_Base_Address +
                                          ISER_Offset_Address);

   Interrupt_Disable_Register : Interrupt_Disable_Set
     with Address => System'To_Address (NVIC_Base_Address +
                                          ISCR_Offset_Address);

   Interrupt_Enabled_Register : Interrupt_Enabled_Set
     with Address => System'To_Address (NVIC_Base_Address +
                                          ISER_Offset_Address);

   Interrupt_Set_Pending_Register : Interrupt_Set_Set
     with Address => System'To_Address (NVIC_Base_Address +
                                          ISPR_Offset_Address);

   Interrupt_Clear_Pending_Register : Interrupt_Clear_Set
     with Address => System'To_Address (NVIC_Base_Address +
                                          ICPR_Offset_Address);

   Interrupt_Pending_Register : Interrupt_Pending_Set
     with Address => System'To_Address (NVIC_Base_Address +
                                          ISPR_Offset_Address);

   Interrupt_Active_Register : Interrupt_Active_Set
     with Address => System'To_Address (NVIC_Base_Address +
                                          IABR_Offset_Address);

   Interrupt_Priority_Register : array (NVIC_Interrupt_Id) of
     Exception_Priority
     with Address => System'To_Address (NVIC_Base_Address +
                                          IPR_Offset_Address);

   Software_Trigger_Interrupt_Register : Software_Trigger_Interrupt_Type
     with Address => System'To_Address (NVIC_Base_Address +
                                          STIR_Offset_Address);
end ISA.ARM.Cortex_M4.NVIC;
