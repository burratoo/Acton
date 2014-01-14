--                                                                          --
--                              OAK COMPONENTS                              --
--                                                                          --
--                              OAK.INTERRUPTS                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                 Copyright (C) 2012-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

with Oak.Agent.Oak_Agent; use Oak.Agent.Oak_Agent;

package body Oak.Interrupts is

   procedure Attach_Handlers
     (Handlers          : in  Interrupt_Handler_Array;
      Handler_PO        : in  Protected_Id;
      Current_Agent     : in  Task_Id;
      Next_Agent_To_Run : out Oak_Agent_Id)
   is
   begin
      for Handler of Handlers loop
         Oak.Processor_Support_Package.Interrupts.Attach_Handler
           (Interrupt => Handler.Interrupt,
            Handler   => Handler.Handler,
            Priority  => Normal_Priority (Agent => Handler_PO));
      end loop;
      Next_Agent_To_Run := Current_Agent;
   end Attach_Handlers;

end Oak.Interrupts;
