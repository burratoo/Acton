------------------------------------------------------------------------------------------
--                                                                                      --
--                                  OAKLAND COMPONENTS                                  --
--                                                                                      --
--                                  OAKLAND.INTERRUPTS                                  --
--                                                                                      --
--                       Copyright (C) 2011-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

with Oak.Interrupts; use Oak.Interrupts;

package Oakland.Interrupts with Preelaborate is
   procedure Attach_Handlers
     (Handlers  : in Interrupt_Handler_Array);
end Oakland.Interrupts;
