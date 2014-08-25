with System; use System;

package ISA.ARM.Cortex_M4.FPU with Preelaborate is

   --------------------------
   -- FPU Memory Addresses --
   --------------------------

   FPU_Base_Address      : constant := 16#E000_EF30#;
   FPCCR_Offset_Address  : constant := 16#4#;
   FPCAR_Offset_Address  : constant := 16#8#;
   FPDSCR_Offset_Address : constant := 16#C#;

   -----------------------
   -- Hardware Features --
   -----------------------

   ---------------
   -- FPU Types --
   ---------------

   type Context_Control is record
      Automatic_State_Preservation   : Enable_Type;
      Lazy_State_Preservation        : Enable_Type;
      Debug_Monitor_Ready            : Boolean;
      Bus_Fault_Ready                : Boolean;
      Mem_Manage_Ready               : Boolean;
      Hard_Fault_Ready               : Boolean;
      Thread_Mode                    : Boolean;
      User_Mode                      : Boolean;
      Lazy_State_Preservation_Active : Boolean;
   end record;

   type Half_Precision_Type is (IEEE_Format, Alternative_Format);
   type Not_A_Number_Handling_Type is (Propagate, Default_NaN);
   type Rounding_Mode_Type is (Round_To_Nearest,
                               Round_Towards_Plus_Infinity,
                               Round_Towards_Minus_Infinity,
                               Round_Towards_Zero);

   type Default_Status_Control is record
      Half_Precision_Mode   : Half_Precision_Type;
      Not_A_Number_Handling : Not_A_Number_Handling_Type;
      Flush_To_Zero_Mode    : Enable_Type;
      Rounding_Mode         : Rounding_Mode_Type;
   end record;

   ------------------------------
   -- Hardware Representations --
   ------------------------------

   for Context_Control use record
      Automatic_State_Preservation   at 0 range 31 .. 31;
      Lazy_State_Preservation        at 0 range 30 .. 30;
      Debug_Monitor_Ready            at 0 range  8 .. 8;
      Bus_Fault_Ready                at 0 range  6 .. 6;
      Mem_Manage_Ready               at 0 range  5 .. 5;
      Hard_Fault_Ready               at 0 range  4 .. 4;
      Thread_Mode                    at 0 range  3 .. 3;
      User_Mode                      at 0 range  1 .. 1;
      Lazy_State_Preservation_Active at 0 range  0 .. 0;
   end record;

   for Default_Status_Control use record
      Half_Precision_Mode   at 0 range 26 .. 26;
      Not_A_Number_Handling at 0 range 25 .. 25;
      Flush_To_Zero_Mode    at 0 range 24 .. 24;
      Rounding_Mode         at 0 range 22 .. 23;
   end record;

   -------------------
   -- FPU Registers --
   -------------------

   Floating_Point_Context_Control_Register : Context_Control
     with Address =>
       System'To_Address (FPU_Base_Address + FPCCR_Offset_Address);

   Floating_Point_Context_Address_Register : Address
     with Address =>
       System'To_Address (FPU_Base_Address + FPCAR_Offset_Address);

   Floating_Point_Default_Status_Control_Register : Default_Status_Control
     with Address =>
       System'To_Address (FPU_Base_Address + FPDSCR_Offset_Address);
end ISA.ARM.Cortex_M4.FPU;
