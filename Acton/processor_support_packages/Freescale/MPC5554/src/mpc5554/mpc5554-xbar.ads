------------------------------------------------------------------------------------------
--                                                                                      --
--                            OAK PROCESSOR SUPPORT PACKAGE                             --
--                                  FREESCALE MPC5544                                   --
--                                                                                      --
--                                     MPC5554.XBAR                                     --
--                                                                                      --
--                       Copyright (C) 2010-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

with System.Storage_Elements; use System.Storage_Elements;

package MPC5554.XBAR with Preelaborate is

   ----------------------------------------------------------------------------
   --  Memory Addresses
   ----------------------------------------------------------------------------
   XBAR_Base_Address     : constant Integer_Address := 16#FFF0_4000#;
   MPR0_Offset_Address   : constant Integer_Address := 16#0000#;
   SGPCR0_Offset_Address : constant Integer_Address := 16#0010#;
   MPR1_Offset_Address   : constant Integer_Address := 16#0100#;
   SGPCR1_Offset_Address : constant Integer_Address := 16#0110#;
   MPR3_Offset_Address   : constant Integer_Address := 16#0300#;
   SGPCR3_Offset_Address : constant Integer_Address := 16#0310#;
   MPR6_Offset_Address   : constant Integer_Address := 16#0600#;
   SGPCR6_Offset_Address : constant Integer_Address := 16#0610#;
   MPR7_Offset_Address   : constant Integer_Address := 16#0700#;
   SGPCR7_Offset_Address : constant Integer_Address := 16#0710#;

   ----------------------------------------------------------------------------
   --  Hardware Features
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   --  XBAR Types
   ---------------------------------------------------------------------------

   --  Common Types
   type Priority_Type is (Highest, Second_Highest, Third_Highest, Lowest);

   --  Master Priority Register XBAR_MPRn
   type Master_Priority_Type is record
      Master_3_Priority, Master_2_Priority : Priority_Type;
      Master_1_Priority, Master_0_Priority : Priority_Type;
   end record with Size => Standard'Word_Size;

   --  Slave General-Purpose Control Registers
   type ARB_Type is (Fixed, Round_Robin);
   type PCTL_Type is (PARK, Park_On_Last, Low_Power_Park);
   type Park_Type is (Master_Port_0, Master_Port_1, Master_Port_2);

   type Slave_GP_Control_Type is record
      Read_Only        : Enable_Type;
      Arbitration_Mode : ARB_Type;
      Parking_Control  : PCTL_Type;
      Park             : Park_Type;
   end record with Size => Standard'Word_Size;

   ----------------------------------------------------------------------------
   --  Hardware Respresentations
   ----------------------------------------------------------------------------
   for Priority_Type use
     (Highest        => 2#00#,
      Second_Highest => 2#01#,
      Third_Highest  => 2#10#,
      Lowest         => 2#11#);

   for Master_Priority_Type use record
      Master_3_Priority at 0 range 18 .. 19;
      Master_2_Priority at 0 range 22 .. 23;
      Master_1_Priority at 0 range 26 .. 27;
      Master_0_Priority at 0 range 30 .. 31;
   end record;

   for ARB_Type use (Fixed => 0, Round_Robin => 1);
   for PCTL_Type use
     (PARK           => 2#00#,
      Park_On_Last   => 2#01#,
      Low_Power_Park => 2#10#);
   for Park_Type use
     (Master_Port_0 => 2#000#,
      Master_Port_1 => 2#001#,
      Master_Port_2 => 2#010#);

   for Slave_GP_Control_Type use record
      Read_Only        at 0 range 0 .. 0;
      Arbitration_Mode at 0 range 22 .. 23;
      Parking_Control  at 0 range 26 .. 27;
      Park             at 0 range 29 .. 31;
   end record;

   ----------------------------------------------------------------------------
   --  EBI Registers
   ----------------------------------------------------------------------------

   Master_Priority_Register_0 : Master_Priority_Type;
   for Master_Priority_Register_0'Address use
     System'To_Address (XBAR_Base_Address + MPR0_Offset_Address);

   GP_Control_Register_0 : Slave_GP_Control_Type;
   for GP_Control_Register_0'Address use
     System'To_Address (XBAR_Base_Address + SGPCR0_Offset_Address);

   Master_Priority_Register_1 : Master_Priority_Type;
   for Master_Priority_Register_1'Address use
     System'To_Address (XBAR_Base_Address + MPR0_Offset_Address);

   GP_Control_Register_1 : Slave_GP_Control_Type;
   for GP_Control_Register_1'Address use
     System'To_Address (XBAR_Base_Address + SGPCR1_Offset_Address);

   Master_Priority_Register_3 : Master_Priority_Type;
   for Master_Priority_Register_3'Address use
     System'To_Address (XBAR_Base_Address + MPR3_Offset_Address);

   GP_Control_Register_3 : Slave_GP_Control_Type;
   for GP_Control_Register_3'Address use
     System'To_Address (XBAR_Base_Address + SGPCR3_Offset_Address);

   Master_Priority_Register_6 : Master_Priority_Type;
   for Master_Priority_Register_6'Address use
     System'To_Address (XBAR_Base_Address + MPR6_Offset_Address);

   GP_Control_Register_6 : Slave_GP_Control_Type;
   for GP_Control_Register_6'Address use
     System'To_Address (XBAR_Base_Address + SGPCR6_Offset_Address);

   Master_Priority_Register_7 : Master_Priority_Type;
   for Master_Priority_Register_7'Address use
     System'To_Address (XBAR_Base_Address + MPR7_Offset_Address);

   GP_Control_Register_7 : Slave_GP_Control_Type;
   for GP_Control_Register_7'Address use
     System'To_Address (XBAR_Base_Address + SGPCR7_Offset_Address);
end MPC5554.XBAR;
