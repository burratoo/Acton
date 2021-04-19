------------------------------------------------------------------------------------------
--                                                                                      --
--                            OAK PROCESSOR SUPPORT PACKAGE                             --
--                                  FREESCALE MPC5544                                   --
--                                                                                      --
--                            OAK.PROCESSOR_SUPPORT_PACKAGE                             --
--                                                                                      --
--                       Copyright (C) 2010-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

package Oak.Processor_Support_Package with Pure is
   Number_Of_Processors : constant := 1;
   type Processors is range 1 .. Number_Of_Processors;
   --  Defines the number of processors used by the processor package
   --  (yes, overloading the term processors here).

end Oak.Processor_Support_Package;
