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

package ISA.Power with Pure is

   procedure Memory_Barrier with Inline_Always;
   procedure Instruction_Synchronize with Inline_Always;
   procedure Memory_Synchronize with Inline_Always;

   procedure Mbar renames Memory_Barrier;
   procedure Isync renames Instruction_Synchronize;
   procedure MSync renames Memory_Synchronize;

end ISA.Power;
