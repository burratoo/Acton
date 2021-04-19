------------------------------------------------------------------------------------------
--                                                                                      --
--                              OAK PROJECT SUPPORT PACKAGE                             --
--                                      ST STM32F4                                      --
--                                                                                      --
--                     OAK.PROJECT_SUPPORT_PACKAGE.MICROCONTROLLER                      --
--                                                                                      --
--                       Copyright (C) 2014-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

package Oak.Project_Support_Package.Microcontroller with Preelaborate is
   procedure Setup
     with Export, Convention => Assembly,
     External_Name => "microcontroller_setup";
end Oak.Project_Support_Package.Microcontroller;
