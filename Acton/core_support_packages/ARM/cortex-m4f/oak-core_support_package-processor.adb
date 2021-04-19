------------------------------------------------------------------------------------------
--                                                                                      --
--                               OAK CORE SUPPORT PACKAGE                               --
--                                    ARM CORTEX M4F                                    --
--                                                                                      --
--                          OAK.CORE_SUPPORT_PACKAGE.PROCESSOR                          --
--                                                                                      --
--                       Copyright (C) 2014-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

package body Oak.Core_Support_Package.Processor is

   -----------------------
   -- Get_Proccessor_Id --
   -----------------------

   function Proccessor_Id return Kernel_Id is
   begin
      return 1;
   end Proccessor_Id;

end Oak.Core_Support_Package.Processor;
