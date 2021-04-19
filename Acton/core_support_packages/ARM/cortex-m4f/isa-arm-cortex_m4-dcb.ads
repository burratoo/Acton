------------------------------------------------------------------------------------------
--                                                                                      --
--                               OAK CORE SUPPORT PACKAGE                               --
--                                    ARM CORTEX M4F                                    --
--                                                                                      --
--                                ISA.ARM.CORTEX_M4.DCB                                 --
--                                                                                      --
--                       Copyright (C) 2014-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

with System; use System;

package ISA.ARM.Cortex_M4.DCB with Preelaborate is
   --  Not complete

   --------------------------
   -- DCB Memory Addresses --
   --------------------------

   DCB_Base_Address     : constant := 16#E000_EDF0#;
   DHCSR_Offset_Address : constant := 16#0#;
   DCRSR_Offset_Address : constant := 16#4#;
   DCRDR_Offset_Address : constant := 16#8#;
   DEMCR_Offset_Address : constant := 16#C#;

   -----------------------
   -- Hardware Features --
   -----------------------

   ---------------
   -- DCB Types --
   ---------------

   type Exception_And_Monitor_Control is record
      Trace                     : Enable_Type;
      Monitor_Semaphore         : Boolean;
      Monitor_Step              : Boolean;
      Monitor_Exception_Pending : Boolean;
      Monitor_Exception         : Enable_Type;
   end record;

   ------------------------------
   -- Hardware Representations --
   ------------------------------

   for Exception_And_Monitor_Control use record
      Trace                     at 0 range 24 .. 24;
      Monitor_Semaphore         at 0 range 19 .. 19;
      Monitor_Step              at 0 range 18 .. 18;
      Monitor_Exception_Pending at 0 range 17 .. 17;
      Monitor_Exception         at 0 range 16 .. 16;
   end record;

   -------------------
   -- DCB Registers --
   -------------------

   Debug_Exception_And_Monitor_Control_Register : Exception_And_Monitor_Control
     with Address =>
       System'To_Address (DCB_Base_Address + DEMCR_Offset_Address);
end ISA.ARM.Cortex_M4.DCB;
