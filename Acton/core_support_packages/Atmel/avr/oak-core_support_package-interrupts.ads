------------------------------------------------------------------------------------------
--                                                                                      --
--                               OAK CORE SUPPORT PACKAGE                               --
--                                      ATMEL AVR                                       --
--                                                                                      --
--                          OAK.CORE_SUPPORT_PACKAGE.INTERRUPTS                         --
--                                                                                      --
--                       Copyright (C) 2012-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

package Oak.Core_Support_Package.Interrupts with Preelaborate is

   procedure Enable_Interrupts  with Inline_Always;
   procedure Disable_Interrupts with Inline_Always;
   procedure Set_Up_Interrupts;

private

end Oak.Core_Support_Package.Interrupts;
