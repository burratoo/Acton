--                                                                          --
--                              OAK COMPONENTS                              --
--                                                                          --
--                              OAK.INTERRUPTS                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                 Copyright (C) 2012-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

with Oak.Agent.Oak_Agent;         use Oak.Agent.Oak_Agent;
with Oak.Agent.Protected_Objects; use Oak.Agent.Protected_Objects;

package body Oak.Interrupts is

   procedure Attach_Handler
     (Handler           : in  Interrupt_Handler_Pair;
      Current_Agent     : in  Task_Id;
      Next_Agent_To_Run : out Oak_Agent_Id)
   is
   begin
         Oak.Processor_Support_Package.Interrupts.Attach_Handler
           (Interrupt => Handler.Interrupt,
            Handler   => Handler.Handler,
            Priority  =>
              Normal_Priority
                (Protected_Object_From_Access
                     (Parameterless_Access (Handler.Handler))));
      Next_Agent_To_Run := Current_Agent;
   end Attach_Handler;

end Oak.Interrupts;
