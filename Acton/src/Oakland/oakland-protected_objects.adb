with Oak.Core;
with Oakland.Tasks;

with Oak.Agent.Tasks; use Oak.Agent.Tasks;
with Oak.Agent.Tasks.Protected_Objects; use Oak.Agent.Tasks.Protected_Objects;
package body Oakland.Protected_Objects is

   procedure Enter_Protected_Object
     (PO              : not null access Protected_Agent'Class;
      Subprogram_Kind : in Oak.Protected_Objects.Protected_Subprogram_Type;
      Entry_Id        : in Entry_Index := No_Entry)
   is
      Self : constant access Task_Agent'Class := Oak.Core.Current_Task;
      Message : constant Oak_Task_Message :=
                (Message_Type    => Entering_PO,
                 PO_Enter        => PO,
                 Subprogram_Kind => Subprogram_Kind,
                 Entry_Id_Enter  => Entry_Id);
   begin
      Tasks.Yield_Processor_To_Kernel (Task_Message => Message);
      if Self.State = Enter_PO_Refused then
         raise Program_Error;
      end if;
   end Enter_Protected_Object;

   procedure Exit_Protected_Object
     (PO : not null access Protected_Agent'Class)
   is
      Self     : constant access Task_Agent'Class := Oak.Core.Current_Task;
      Message  : constant Oak_Task_Message :=
                (Message_Type  => Exiting_PO,
                 PO_Exit       => PO);
   begin
      Tasks.Yield_Processor_To_Kernel (Task_Message => Message);
      if Self.State = Exit_PO_Error then
         raise Program_Error;
      end if;
   end Exit_Protected_Object;

   function Entry_Count
     (PO       : not null access Protected_Agent'Class;
      Entry_Id : Entry_Index) return Natural is
   begin
      return PO.Entry_Queue_Length (Entry_Id => Entry_Id);
   end Entry_Count;

end Oakland.Protected_Objects;