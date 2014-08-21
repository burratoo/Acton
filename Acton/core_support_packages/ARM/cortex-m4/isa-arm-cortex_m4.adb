------------------------------------------------------------------------------
--                                                                          --
--                         OAK CORE SUPPORT PACKAGE                         --
--                               ARM CORTEX M4                              --
--                                                                          --
--                             ISA.ARM.CORTEX_M4                            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                 Copyright (C) 2014-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

with System.Machine_Code; use System.Machine_Code;

package body ISA.ARM.Cortex_M4 is

   ------------------------
   -- Disable_Interrupts --
   ------------------------

   procedure Disable_Interrupts is
   begin
      Asm ("cpsid i", Volatile => True);
   end Disable_Interrupts;

   -----------------------
   -- Enable_Interrupts --
   -----------------------

   procedure Enable_Interrupts is
   begin
      Asm ("cpsie i", Volatile => True);
   end Enable_Interrupts;

   -------------------------
   -- Data_Memory_Barrier --
   -------------------------

   procedure Data_Memory_Barrier is
   begin
      Asm ("dmb", Volatile => True);
   end Data_Memory_Barrier;

   ----------------------------------
   -- Data_Synchronization_Barrier --
   ----------------------------------

   procedure Data_Synchronization_Barrier is
   begin
      Asm ("dsb", Volatile => True);
   end Data_Synchronization_Barrier;

   -----------------------------------------
   -- Instruction_Synchronization_Barrier --
   -----------------------------------------

   procedure Instruction_Synchronization_Barrier is
   begin
      Asm ("isb", Volatile => True);
   end Instruction_Synchronization_Barrier;

   ---------------------
   -- Clear_Exclusive --
   ---------------------

   procedure Clear_Exclusive_Lock is
   begin
      Asm ("clrex", Volatile => True);
   end Clear_Exclusive_Lock;

end ISA.ARM.Cortex_M4;
