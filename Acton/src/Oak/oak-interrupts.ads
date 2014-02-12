--                                                                          --
--                              OAK COMPONENTS                              --
--                                                                          --
--                              OAK.INTERRUPTS                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                 Copyright (C) 2012-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

--  This package provides Oak's platform independent interrupt services.

with Oak.Processor_Support_Package.Interrupts;
use Oak.Processor_Support_Package.Interrupts;

package Oak.Interrupts with Preelaborate is

   type Interrupt_Handler_Pair is record
      Interrupt : External_Interrupt_Id;
      Handler   : Parameterless_Handler;
   end record;
   --  A type used to map an external interrupt id with its associated handler.

   type Interrupt_Handler_Array
     is array
     (Positive range <>) of Interrupt_Handler_Pair;
   --  An array of interrupt handling pairs.

   procedure Attach_Handler (Handler : in Interrupt_Handler_Pair);
   --  Attaches the handlers provided in the handler array to the specified
   --  protected object.

end Oak.Interrupts;
