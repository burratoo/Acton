with Oak.Core;
with ARPART.Tasks;

with Oak.Agent.Tasks; use Oak.Agent.Tasks;

package body ARPART.Atomic_Actions is
   procedure Enter_Action
     (Atomic_Action : not null access Atomic_Object;
      Action_Id     : Action_Index)
   is
      Self : constant access Task_Agent'Class := Oak.Core.Current_Task;
      Message : constant Oak_Task_Message :=
                  (Message_Type    => Entering_Atomic_Action,
                   AA_Enter        => Atomic_Action,
                   Action_Id_Enter => Action_Id);
   begin
      Tasks.Yield_Processor_To_Kernel (Task_Message => Message);
      if Self.State = Enter_Atomic_Action_Refused then
         raise Program_Error;
      end if;
   end Enter_Action;

   procedure Action_End_Barrier
     (Atomic_Action    : not null access Atomic_Object;
      Action_Id        : Action_Index;
      Exception_Raised : Boolean)
   is
      Self : constant access Task_Agent'Class := Oak.Core.Current_Task;
      Message : constant Oak_Task_Message :=
                  (Message_Type     => Entering_Exit_Barrier,
                   AA_EB            => Atomic_Action,
                   Action_Id_EB     => Action_Id,
                   Exception_Raised => Exception_Raised);
   begin
      Tasks.Yield_Processor_To_Kernel (Task_Message => Message);
      if Self.State = Exit_Atomic_Action_Error then
         raise Program_Error;
      end if;
   end Action_End_Barrier;

   procedure Exit_Action
     (Atomic_Action    : not null access Atomic_Object;
      Action_Id        : Action_Index;
      Exception_Raised : Boolean)
   is
      Self : constant access Task_Agent'Class := Oak.Core.Current_Task;
      Message : constant Oak_Task_Message :=
                  (Message_Type     => Exiting_Atomic_Action,
                   AA_Exit          => Atomic_Action,
                   Action_Id_Exit   => Action_Id,
                   Atomic_Exception => Exception_Raised);
   begin
      Tasks.Yield_Processor_To_Kernel (Task_Message => Message);
      if Self.State = Exit_Atomic_Action_Error then
         raise Program_Error;
      elsif Self.State = Atomic_Action_Error then
         raise Atomic_Error;
      end if;
   end Exit_Action;

end ARPART.Atomic_Actions;
