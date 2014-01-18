------------------------------------------------------------------------------
--                                                                          --
--                            OAKLAND COMPONENTS                            --
--                                                                          --
--                            OAKLAND.INTERRUPTS                            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                 Copyright (C) 2011-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

with Oak.Agent;      use Oak.Agent;
with Oak.Interrupts; use Oak.Interrupts;

package Oakland.Interrupts with Preelaborate is
   procedure Attach_Handlers
     (PO        : in Protected_Id;
      Handlers  : in Interrupt_Handler_Array);
end Oakland.Interrupts;
