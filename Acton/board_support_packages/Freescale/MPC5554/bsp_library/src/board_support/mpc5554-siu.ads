with System.Storage_Elements; use System.Storage_Elements;

package MPC5554.SIU is

   ----------------------------------------------------------------------------
   --  Memory Addresses
   ----------------------------------------------------------------------------
   SIU_Base_Address                 : constant Integer_Address :=
      16#C3F9_0000#;
   EISR_Offset_Address              : constant Integer_Address := 16#0014#;
   DIRER_Offset_Address             : constant Integer_Address := 16#0018#;
   IREER_Offset_Address             : constant Integer_Address := 16#0028#;
   IFEER_Offset_Address             : constant Integer_Address := 16#002C#;
   Pad_Configuration_Offset_Address : constant Integer_Address := 16#0040#;
   GPO_Offset_Address               : constant Integer_Address := 16#0600#;
   GPI_Offset_Address               : constant Integer_Address := 16#0800#;
   ECCR_Offset_Address              : constant Integer_Address := 16#0984#;

   ----------------------------------------------------------------------------
   --  Hardware Features
   ----------------------------------------------------------------------------
   type Pad_ID_Type is range 0 .. 230;
   type GPIO_ID_Type is range 0 .. 213;
   GPIO_Register_Size : constant Integer := 8;

   IRQ_Size : constant Integer := 16;
   type IRQ_ID_Type is range 0 .. IRQ_Size - 1;

   ----------------------------------------------------------------------------
   --  SIU Types
   ---------------------------------------------------------------------------

   --  Common Types

   type IRQ_Occured_Type is record
      IRQ15 : Occurred_Type;
      IRQ14 : Occurred_Type;
      IRQ13 : Occurred_Type;
      IRQ12 : Occurred_Type;
      IRQ11 : Occurred_Type;
      IRQ10 : Occurred_Type;
      IRQ9  : Occurred_Type;
      IRQ8  : Occurred_Type;
      IRQ7  : Occurred_Type;
      IRQ6  : Occurred_Type;
      IRQ5  : Occurred_Type;
      IRQ4  : Occurred_Type;
      IRQ3  : Occurred_Type;
      IRQ2  : Occurred_Type;
      IRQ1  : Occurred_Type;
      IRQ0  : Occurred_Type;
   end record;

   type IRQ_Enable_Type is record
      IRQ15 : Enable_Type;
      IRQ14 : Enable_Type;
      IRQ13 : Enable_Type;
      IRQ12 : Enable_Type;
      IRQ11 : Enable_Type;
      IRQ10 : Enable_Type;
      IRQ9  : Enable_Type;
      IRQ8  : Enable_Type;
      IRQ7  : Enable_Type;
      IRQ6  : Enable_Type;
      IRQ5  : Enable_Type;
      IRQ4  : Enable_Type;
      IRQ3  : Enable_Type;
      IRQ2  : Enable_Type;
      IRQ1  : Enable_Type;
      IRQ0  : Enable_Type;
   end record;

   --  Pad Configuration Registers
   type PA_Type is (
      GPIO,
      Primary_Function,
      Alternate_Function_1,
      Main_Primary_Function,
      Alternate_Function_2);

   type DSC_Type is (ds_10pf, ds_20pf, ds_30pf, ds_50pf);

   type SRC_Type is (Minimum, Medium, Maximum);

   type WPS_Type is (Pulldown, Pullup);

   type Pad_Configuration_Type is record
      Pin_Assignment           : PA_Type;
      Output_Buffer_Enable     : Enable_Type;
      Input_Buffer_Enable      : Enable_Type;
      Drive_Strength_Control   : DSC_Type;
      Open_Drain_Output_Enable : Enable_Type;
      Input_Hysteresis         : Enable_Type;
      Slew_Rate_Control        : SRC_Type;
      Weak_Pullup_Down_Enable  : Enable_Type;
      Weak_Pullup_Down_Select  : WPS_Type;
   end record;

   --  GPIO Registers
   type GPIO_Pin_State_Type is (Low, High);

   subtype GPO_Data_Register_Type is GPIO_Pin_State_Type;
   subtype GPI_Data_Register_Type is GPIO_Pin_State_Type;

   --  External Clock Control Register

   type ENGDIV_Type is range 1 .. 63;
   type EBTS_Type is (Zero_Hold, Non_Zero_Hold);
   type EBDF_Type is (Divide_By_2, Divide_By_4);

   type External_Clock_Control_Type is record
      Engineering_Clock_Division_Factor : ENGDIV_Type;
      External_Bus_Tap_Select           : EBTS_Type;
      External_Bus_Division_Factor      : EBDF_Type;
   end record;

   ----------------------------------------------------------------------------
   --  Hardware Respresentations
   ----------------------------------------------------------------------------

   for IRQ_Occured_Type use record
      IRQ15 at 0 range 16 .. 16;
      IRQ14 at 0 range 17 .. 17;
      IRQ13 at 0 range 18 .. 18;
      IRQ12 at 0 range 19 .. 19;
      IRQ11 at 0 range 20 .. 20;
      IRQ10 at 0 range 21 .. 21;
      IRQ9  at 0 range 22 .. 22;
      IRQ8  at 0 range 23 .. 23;
      IRQ7  at 0 range 24 .. 24;
      IRQ6  at 0 range 25 .. 25;
      IRQ5  at 0 range 26 .. 26;
      IRQ4  at 0 range 27 .. 27;
      IRQ3  at 0 range 28 .. 28;
      IRQ2  at 0 range 29 .. 29;
      IRQ1  at 0 range 30 .. 30;
      IRQ0  at 0 range 31 .. 31;
   end record;

   for IRQ_Enable_Type use record
      IRQ15 at 0 range 16 .. 16;
      IRQ14 at 0 range 17 .. 17;
      IRQ13 at 0 range 18 .. 18;
      IRQ12 at 0 range 19 .. 19;
      IRQ11 at 0 range 20 .. 20;
      IRQ10 at 0 range 21 .. 21;
      IRQ9  at 0 range 22 .. 22;
      IRQ8  at 0 range 23 .. 23;
      IRQ7  at 0 range 24 .. 24;
      IRQ6  at 0 range 25 .. 25;
      IRQ5  at 0 range 26 .. 26;
      IRQ4  at 0 range 27 .. 27;
      IRQ3  at 0 range 28 .. 28;
      IRQ2  at 0 range 29 .. 29;
      IRQ1  at 0 range 30 .. 30;
      IRQ0  at 0 range 31 .. 31;
   end record;

   for PA_Type use
     (GPIO                  => 2#000#,
      Primary_Function      => 2#001#,
      Alternate_Function_1  => 2#010#,
      Main_Primary_Function => 2#011#,
      Alternate_Function_2  => 2#100#);

   for DSC_Type use
     (ds_10pf => 2#00#,
      ds_20pf => 2#01#,
      ds_30pf => 2#10#,
      ds_50pf => 2#11#);

   for SRC_Type use (Minimum => 2#00#, Medium => 2#01#, Maximum => 2#11#);

   for WPS_Type use (Pulldown => 0, Pullup => 1);

   for Pad_Configuration_Type use record
      Pin_Assignment           at 0 range 3 .. 5;
      Output_Buffer_Enable     at 0 range 6 .. 6;
      Input_Buffer_Enable      at 0 range 7 .. 7;
      Drive_Strength_Control   at 0 range 8 .. 9;
      Open_Drain_Output_Enable at 0 range 10 .. 10;
      Input_Hysteresis         at 0 range 11 .. 11;
      Slew_Rate_Control        at 0 range 12 .. 13;
      Weak_Pullup_Down_Enable  at 0 range 14 .. 14;
      Weak_Pullup_Down_Select  at 0 range 15 .. 15;
   end record;

   for GPIO_Pin_State_Type use (Low => 0, High => 1);

   for GPIO_Pin_State_Type'Size use GPIO_Register_Size;

   for EBTS_Type use (Zero_Hold => 0, Non_Zero_Hold => 1);
   for EBDF_Type use (Divide_By_2 => 2#01#, Divide_By_4 => 2#11#);

   for External_Clock_Control_Type use record
      Engineering_Clock_Division_Factor at 0 range 18 .. 23;
      External_Bus_Tap_Select           at 0 range 28 .. 28;
      External_Bus_Division_Factor      at 0 range 30 .. 31;
   end record;

   ----------------------------------------------------------------------------
   --  SIU Registers
   ----------------------------------------------------------------------------

   External_Interrupt_State_Register : IRQ_Occured_Type;
   for External_Interrupt_State_Register'Address use
     To_Address (SIU_Base_Address + EISR_Offset_Address);

   DMA_Interrupt_Request_Enable_Register : IRQ_Enable_Type;
   for DMA_Interrupt_Request_Enable_Register'Address use
     To_Address (SIU_Base_Address + DIRER_Offset_Address);

   IRQ_Rising_Edge_Event_Enable_Register : IRQ_Enable_Type;
   for IRQ_Rising_Edge_Event_Enable_Register'Address use
     To_Address (SIU_Base_Address + IREER_Offset_Address);

   IRQ_Falling_Edge_Event_Enable_Register : IRQ_Enable_Type;
   for IRQ_Falling_Edge_Event_Enable_Register'Address use
     To_Address (SIU_Base_Address + IFEER_Offset_Address);

   Pad_Configuration_Register_Array :
     array (Pad_ID_Type) of aliased Pad_Configuration_Type;
   for Pad_Configuration_Register_Array'Address use
     To_Address (SIU_Base_Address + Pad_Configuration_Offset_Address);
   type Pad_Configuration_Pointer is access all Pad_Configuration_Type;

   GPO_Data_Register_Array :
     array (GPIO_ID_Type) of aliased GPO_Data_Register_Type;
   for GPO_Data_Register_Array'Address use
     To_Address (SIU_Base_Address + GPO_Offset_Address);
   type GPO_Data_Register_Pointer is access all GPO_Data_Register_Type;

   GPI_Data_Register_Array :
     array (GPIO_ID_Type) of aliased GPI_Data_Register_Type;
   for GPI_Data_Register_Array'Address use
     To_Address (SIU_Base_Address + GPI_Offset_Address);
   type GPI_Data_Register_Pointer is access all GPI_Data_Register_Type;

   External_Clock_Control_Register : External_Clock_Control_Type;
   for External_Clock_Control_Register'Address use
     To_Address (SIU_Base_Address + ECCR_Offset_Address);

end MPC5554.SIU;
