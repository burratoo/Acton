------------------------------------------------------------------------------------------
--                                                                                      --
--                               OAK CORE SUPPORT PACKAGE                               --
--                                     ARM ARM7TDMI                                     --
--                                                                                      --
--                          OAK.CORE_SUPPORT_PACKAGE.PROCESSOR                          --
--                                                                                      --
--                       Copyright (C) 2014-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

with Oak.Agent; use Oak.Agent;

package Oak.Core_Support_Package.Processor with Pure is

   function Proccessor_Id return Kernel_Id with Inline_Always;

end Oak.Core_Support_Package.Processor;
