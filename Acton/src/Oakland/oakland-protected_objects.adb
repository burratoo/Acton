------------------------------------------------------------------------------------------
--                                                                                      --
--                                  OAKLAND COMPONENTS                                  --
--                                                                                      --
--                               OAKLAND.PROTECTED_OBJECTS                              --
--                                                                                      --
--                       Copyright (C) 2011-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

with Oak.Agent;           use Oak.Agent;
with Oak.Agent.Kernel;    use Oak.Agent.Kernel;
with Oak.Agent.Oak_Agent; use Oak.Agent.Oak_Agent;

with Oak.Brokers.Protected_Objects; use Oak.Brokers.Protected_Objects;

with Oak.States;  use Oak.States;

with Oakland.Tasks; use Oakland.Tasks;

package body Oakland.Protected_Objects is

   procedure Enter_Protected_Object
     (PO              : in Protected_Id;
      Subprogram_Kind : in Protected_Subprogram_Type;
      Entry_Id        : in Entry_Index := No_Entry)
   is
      Self    : constant Oak_Agent_Id := Current_Agent (This_Oak_Kernel);
      Message : Oak_Message := (Message_Type       => Entering_PO,
                                PO_Enter           => PO,
                                Subprogram_Kind    => Subprogram_Kind,
                                Entry_Id_Enter     => Entry_Id);
   begin
      if Self in Interrupt_Id then
         return;
      else
         Yield_Processor_To_Kernel (With_Message => Message);

         if State (Self) = Enter_PO_Refused then
            raise Program_Error;
         elsif State (Self) = Entering_PO then
            raise Program_Error;
         end if;
      end if;
   end Enter_Protected_Object;

   procedure Exit_Protected_Object (PO : Protected_Id)
   is
      Self    : constant Oak_Agent_Id := Current_Agent (This_Oak_Kernel);
      Message : Oak_Message := (Message_Type  => Exiting_PO,
                                PO_Exit       => PO);
   begin
      if Self in Interrupt_Id then
         return;
      else
         Yield_Processor_To_Kernel (With_Message => Message);

         if State (Self) = Exit_PO_Error then
            raise Program_Error;
         end if;
      end if;
   end Exit_Protected_Object;

   function Entry_Count
     (PO       : in Protected_Id;
      Entry_Id : Entry_Index) return Natural is
   begin
      return Entry_Queue_Length (PO =>  PO, Entry_Id => Entry_Id);
   end Entry_Count;

end Oakland.Protected_Objects;
