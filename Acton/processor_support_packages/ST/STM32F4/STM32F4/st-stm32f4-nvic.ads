------------------------------------------------------------------------------------------
--                                                                                      --
--                            OAK PROCESSOR SUPPORT PACKAGE                             --
--                                      ST STM32F4                                      --
--                                                                                      --
--                                   ST.STM32F4.NVIC                                    --
--                                                                                      --
--                       Copyright (C) 2014-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

with ISA.ARM.Cortex_M4.NVIC; use ISA.ARM.Cortex_M4.NVIC;

package ST.STM32F4.NVIC with Preelaborate is
   subtype ST32F4_Interrupt_Id is NVIC_Interrupt_Id range 0 .. 82;
end ST.STM32F4.NVIC;
