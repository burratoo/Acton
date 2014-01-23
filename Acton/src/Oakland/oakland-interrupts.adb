------------------------------------------------------------------------------
--                                                                          --
--                            OAKLAND COMPONENTS                            --
--                                                                          --
--                            OAKLAND.INTERRUPTS                            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                 Copyright (C) 2011-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

with Oak.Message;    use Oak.Message;
with Oak.States;     use Oak.States;

with Oakland.Tasks;  use Oakland.Tasks;

package body Oakland.Interrupts is
   procedure Attach_Handlers
     (Handlers  : in Interrupt_Handler_Array)
   is
      Message : Oak_Message;
   begin
      for Handler of Handlers loop
         Message := (Message_Type   => Attach_Interrupt_Handler,
                     Attach_Handler => Handler);
         Yield_Processor_To_Kernel (With_Message => Message);
      end loop;
   end Attach_Handlers;

end Oakland.Interrupts;
