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
     (PO        : in Protected_Id;
      Handlers  : in Interrupt_Handler_Array)
   is
      Message : constant Oak_Message :=
                  (Message_Type      => Attach_Interrupt_Handlers,
                   L                 => Handlers'Length,
                   Attach_Handlers   => Handlers,
                   Attach_Handler_PO => PO);
   begin
      Yield_Processor_To_Kernel (With_Message => Message);
   end Attach_Handlers;

end Oakland.Interrupts;
