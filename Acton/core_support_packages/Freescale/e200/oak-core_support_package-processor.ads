------------------------------------------------------------------------------------------
--                                                                                      --
--                               OAK CORE SUPPORT PACKAGE                               --
--                                    FREESCALE e200                                    --
--                                                                                      --
--                          OAK.CORE_SUPPORT_PACKAGE.PROCESSOR                          --
--                                                                                      --
--                       Copyright (C) 2010-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

with Oak.Agent; use Oak.Agent;

package Oak.Core_Support_Package.Processor with Pure is

   function Proccessor_Id return Kernel_Id with Inline_Always;

end Oak.Core_Support_Package.Processor;
