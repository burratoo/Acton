with System; use System;

package ISA.ARM.Cortex_M4.Coprocessor with Preelaborate is

   ----------------------------------
   -- Coprocessor Memory Addresses --
   ----------------------------------

   Co_Base_Address      : constant := 16#E000ED88#;
   CPACR_Offset_Address : constant := 16#0#;

   -----------------------
   -- Hardware Features --
   -----------------------

   type Coprocessor_Access is (No_Access, Priviledged_Access, Full_Access);

   -----------------------
   -- Coprocessor Types --
   -----------------------

   type Coprocessor_Access_Set is array (0 .. 15) of Coprocessor_Access
     with Pack;

   type Coprocessor_Access_Control is record
      Coprocessor : Coprocessor_Access_Set;
   end record;

   ------------------------------
   -- Hardware Representations --
   ------------------------------

   for Coprocessor_Access use (No_Access          => 0,
                               Priviledged_Access => 1,
                               Full_Access        => 3);

   for Coprocessor_Access_Control use record
      Coprocessor at 0 range 0 .. 31;
   end record;

   ---------------------------
   -- Coprocessor Registers --
   ---------------------------

   Access_Control_Register : Coprocessor_Access_Control
     with Address =>
       System'To_Address (Co_Base_Address + CPACR_Offset_Address);

end ISA.ARM.Cortex_M4.Coprocessor;
