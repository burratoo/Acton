------------------------------------------------------------------------------------------
--                                                                                      --
--                               OAK CORE SUPPORT PACKAGE                               --
--                                    FREESCALE e200                                    --
--                                                                                      --
--                                      ISA.POWER                                       --
--                                                                                      --
--                       Copyright (C) 2010-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

with System.Machine_Code; use System.Machine_Code;

package body ISA.Power is

   procedure Memory_Barrier is
   begin
      Asm ("mbar", Volatile => True);
   end Memory_Barrier;

   procedure Instruction_Synchronize is
   begin
      Asm ("isync", Volatile => True);
   end Instruction_Synchronize;

   procedure Memory_Synchronize is
   begin
      Asm ("msync", Volatile => True);
   end Memory_Synchronize;
end ISA.Power;
