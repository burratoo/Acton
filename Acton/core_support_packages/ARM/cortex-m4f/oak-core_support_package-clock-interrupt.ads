------------------------------------------------------------------------------------------
--                                                                                      --
--                               OAK CORE SUPPORT PACKAGE                               --
--                                    ARM CORTEX M4F                                    --
--                                                                                      --
--                       OAK.CORE_SUPPORT_PACKAGE.CLOCK.INTERRUPT                       --
--                                                                                      --
--                       Copyright (C) 2014-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

package Oak.Core_Support_Package.Clock.Interrupt with Preelaborate is
--  Note that this package exists so as to force gcc to use only the saved
--  registers so that the stack is not implicitly used.

   procedure Update_Clock;
   pragma Machine_Attribute
     (Update_Clock, "naked");

end Oak.Core_Support_Package.Clock.Interrupt;
