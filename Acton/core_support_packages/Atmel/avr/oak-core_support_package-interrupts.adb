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

with System;                                     use System;
with System.Machine_Code;                        use System.Machine_Code;

package body Oak.Core_Support_Package.Interrupts is

   procedure Enable_Interrupts is
   begin
      Asm ("sei", Volatile => True);
   end Enable_Interrupts;

   procedure Disable_Interrupts is
   begin
      Asm ("cli", Volatile => True);
   end Disable_Interrupts;

   procedure Set_Up_Interrupts is
   begin
      null;
   end Set_Up_Interrupts;

end Oak.Core_Support_Package.Interrupts;
