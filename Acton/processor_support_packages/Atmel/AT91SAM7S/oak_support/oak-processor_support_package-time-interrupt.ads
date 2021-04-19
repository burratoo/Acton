------------------------------------------------------------------------------------------
--                                                                                      --
--                            OAK PROCESSOR SUPPORT PACKAGE                             --
--                                   ATMEL AT91SAM7S                                    --
--                                                                                      --
--                     OAK.PROCESSOR_SUPPORT_PACKAGE.TIME.INTERRUPT                     --
--                                                                                      --
--                       Copyright (C) 2014-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

--  In a seperate file so register restrictions can be placed on the FIQ
--  handler.

package Oak.Processor_Support_Package.Time.Interrupt with Preelaborate is
   procedure Update_Clock
     with Export, Convention => Ada, External_Name => "fiq_handler";
   pragma Machine_Attribute
     (Update_Clock, "naked");
end Oak.Processor_Support_Package.Time.Interrupt;
