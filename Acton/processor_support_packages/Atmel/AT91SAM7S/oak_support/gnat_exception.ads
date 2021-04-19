------------------------------------------------------------------------------------------
--                                                                                      --
--                            ACTON PROCESSOR SUPPORT PACKAGE                           --
--                                   ATMEL AT91SAM7S                                    --
--                                                                                      --
--                                   GNAT_EXCEPTION                                     --
--                                                                                      --
--                       Copyright (C) 2014-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

with System;

package GNAT_Exception with Pure is
   procedure Last_Chance_Handler (Msg : System.Address; Line : Integer)
     with Export, Convention => C,
     External_Name => "__gnat_last_chance_handler";

end GNAT_Exception;
