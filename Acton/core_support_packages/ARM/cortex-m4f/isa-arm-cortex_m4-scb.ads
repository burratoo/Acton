------------------------------------------------------------------------------------------
--                                                                                      --
--                               OAK CORE SUPPORT PACKAGE                               --
--                                    ARM CORTEX M4F                                    --
--                                                                                      --
--                                 ISA.ARM.CORTEX_M4.SCB                                --
--                                                                                      --
--                       Copyright (C) 2014-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

with ISA.ARM.Cortex_M4.Exceptions; use ISA.ARM.Cortex_M4.Exceptions;
with ISA.ARM.Cortex_M4.NVIC;       use ISA.ARM.Cortex_M4.NVIC;

with System; use System;

package ISA.ARM.Cortex_M4.SCB with Preelaborate is

   --------------------------
   -- SCB Memory Addresses --
   ---------------------------

   SCB_Base_Address     : constant := 16#E000_E000#;
   ACTLR_Offset_Address : constant := 16#008#;
   CPUID_Offset_Address : constant := 16#D00#;
   ICSR_Offset_Address  : constant := 16#D04#;
   VTOR_Offset_Address  : constant := 16#D08#;
   AIRCR_Offset_Address : constant := 16#D0C#;
   SCR_Offset_Address   : constant := 16#D10#;
   CCR_Offset_Address   : constant := 16#D14#;
   SHPR_Offset_Address  : constant := 16#D18#;
   SHCRS_Offset_Address : constant := 16#D24#;
   CFSR_Offset_Address  : constant := 16#D28#;
   HFSR_Offset_Address  : constant := 16#D2C#;
   MMAR_Offset_Address  : constant := 16#D34#;
   BFAR_Offset_Address  : constant := 16#D38#;
   AFSR_Offset_Address  : constant := 16#D3C#;

   -----------------------
   -- Hardware Features --
   -----------------------

   ---------------
   -- SCB Types --
   ---------------

   type Pending_Type is (Not_Pending, Pending);
   type Clear_Type is (No_Change, Clear);

   type Interrupt_Control_And_State is record
      Active_Exception       : Exception_Id;
      No_Preempted_Exception : Boolean;
      Pending_Exception      : Exception_Id;
      Interrupt_Pending      : Boolean;
      Clear_Systick_Pending  : Clear_Type;
      Systick_Interrupt      : Pending_Type;
      Clear_PendSV_Pending   : Clear_Type;
      PendSV_Interrupt       : Pending_Type;
      NMI_Interrupt          : Pending_Type;
   end record;

   type Veckey_Type is (Write, Read);
   type Endian is (Little, Big);
   type Priority_Group_Type is mod 2 ** 3;

   type Application_Interrupt_And_Reset_Control is record
      Veckey               : Veckey_Type;
      Endianness           : Endian;
      Priority_Group       : Priority_Group_Type;
      System_Reset_Request : Boolean;
   end record;

   type Alignment is (Four_Bytes, Eight_Bytes);
   type Thread_Mode_Access_Type is (No_Exceptions_Active, Any_Level);

   type Configuration_And_Control is record
      Stack_Alignment                 : Alignment;
      Flault_Handlers_Ignore_BusFault : Boolean;
      Trap_Divide_By_0                : Boolean;
      Trap_Unaligned_Access           : Boolean;
      Unpriviledged_Soft_Intr_Trigger : Boolean;
      Thread_Mode_Access              : Thread_Mode_Access_Type;
   end record with Size => Word_Size;

   type System_Handler_Priority_Set is array (System_Exception_Id) of
     Exception_Priority with Pack;

   type System_Handler_Control_And_State is record
      Usage_Fault          : Enable_Type;
      Bus_Fault            : Enable_Type;
      Mem_Manage           : Enable_Type;
      SVCall_Pending       : Boolean;
      Bus_Fault_Pending    : Boolean;
      Mem_Manage_Pending   : Boolean;
      Usage_Fault_Pending  : Boolean;
      SysTick_Active       : Boolean;
      PendSV_Active        : Boolean;
      Debug_Monitor_Active : Boolean;
      SVCall_Active        : Boolean;
      Usage_Fault_Active   : Boolean;
      Bus_Fault_Active     : Boolean;
      Mem_Manage_Active    : Boolean;
   end record;

   ------------------------------
   -- Hardware Representations --
   ------------------------------

   for Pending_Type use (Not_Pending => 0, Pending => 1);
   for Clear_Type use (No_Change => 0, Clear => 1);

   for Interrupt_Control_And_State use record
      Active_Exception       at 0 range 0 .. 8;
      No_Preempted_Exception at 0 range 11 .. 11;
      Pending_Exception      at 0 range 12 .. 20;
      Interrupt_Pending      at 0 range 22 .. 22;
      Clear_Systick_Pending  at 0 range 25 .. 25;
      Systick_Interrupt      at 0 range 26 .. 26;
      Clear_PendSV_Pending   at 0 range 27 .. 27;
      PendSV_Interrupt       at 0 range 28 .. 28;
      NMI_Interrupt          at 0 range 31 .. 31;
   end record;

   for Veckey_Type use (Write => 16#5FA#, Read => 16#FA05#);
   for Endian use (Little => 0, Big => 1);

   for Application_Interrupt_And_Reset_Control use record
      Veckey               at 0 range 16 .. 31;
      Endianness           at 0 range 15 .. 15;
      Priority_Group       at 0 range 8 .. 10;
      System_Reset_Request at 0 range 2 .. 2;
   end record;

   for Configuration_And_Control use record
      Stack_Alignment                 at 0 range 9 .. 9;
      Flault_Handlers_Ignore_BusFault at 0 range 8 .. 8;
      Trap_Divide_By_0                at 0 range 4 .. 4;
      Trap_Unaligned_Access           at 0 range 3 .. 3;
      Unpriviledged_Soft_Intr_Trigger at 0 range 1 .. 1;
      Thread_Mode_Access              at 0 range 0 .. 0;
   end record;

   for System_Handler_Control_And_State use record
      Usage_Fault          at 0 range 18 .. 18;
      Bus_Fault            at 0 range 17 .. 17;
      Mem_Manage           at 0 range 16 .. 16;
      SVCall_Pending       at 0 range 15 .. 15;
      Bus_Fault_Pending    at 0 range 14 .. 14;
      Mem_Manage_Pending   at 0 range 13 .. 13;
      Usage_Fault_Pending  at 0 range 12 .. 12;
      SysTick_Active       at 0 range 11 .. 11;
      PendSV_Active        at 0 range 10 .. 10;
      Debug_Monitor_Active at 0 range 8 .. 8;
      SVCall_Active        at 0 range 7 .. 7;
      Usage_Fault_Active   at 0 range 3 .. 3;
      Bus_Fault_Active     at 0 range 1 .. 1;
      Mem_Manage_Active    at 0 range 0 .. 0;
   end record;

   -------------------
   -- SCB Registers --
   -------------------

   Interrupt_Control_And_State_Register : Interrupt_Control_And_State
     with Address => System'To_Address (SCB_Base_Address +
                                          ICSR_Offset_Address);

   Vector_Table_Offset_Register : Address
     with Address => System'To_Address (SCB_Base_Address +
                                          VTOR_Offset_Address);
   --  Note that while this register is of type Address, there are restrictions
   --  on the least significat bits. See the device's Cortex user manual for
   --  more information.

   Application_Interrupt_And_Reset_Control_Register
                                : Application_Interrupt_And_Reset_Control
     with Address => System'To_Address (SCB_Base_Address +
                                          AIRCR_Offset_Address);

   Configuration_And_Control_Register : Configuration_And_Control
     with Address => System'To_Address (SCB_Base_Address +
                                          CCR_Offset_Address);

   System_Handler_Priority_Register : System_Handler_Priority_Set
     with Address => System'To_Address (SCB_Base_Address +
                                          SHPR_Offset_Address);

   System_Handler_Control_And_State_Register : System_Handler_Control_And_State
     with Address => System'To_Address (SCB_Base_Address +
                                          SHCRS_Offset_Address);
end ISA.ARM.Cortex_M4.SCB;
