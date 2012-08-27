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

end Oak.Core_Support_Package.Interrupts;
