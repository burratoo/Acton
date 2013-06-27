with Oak.Indices; use Oak.Indices;
with Oak.Oak_Time;
with Oak.States; use Oak.States;

limited with Oak.Agent.Tasks;
limited with Oak.Agent.Tasks.Protected_Objects;
limited with Oak.Atomic_Actions;
limited with Oak.Interrupts;

package Oak.Message with Preelaborate is

   type Protected_Subprogram_Type is
     (Protected_Function,
      Protected_Procedure,
      Protected_Entry);

   type Oak_Message (Message_Type : Agent_State := No_State) is record
      case Message_Type is
         when Sleeping =>
            Wake_Up_At : Oak_Time.Time := Oak_Time.Time_Last;
         when Change_Cycle_Period =>
            New_Cycle_Period : Oak_Time.Time_Span := Oak_Time.Time_Span_Zero;
         when Change_Relative_Deadline =>
            New_Deadline_Span : Oak_Time.Time_Span :=
                                  Oak_Time.Time_Span_Zero;
         when Release_Task =>
            Task_To_Release   : not null access Oak.Agent.Tasks.Task_Agent;

         when Entering_PO =>
            PO_Enter          : not null access
              Oak.Agent.Tasks.Protected_Objects.Protected_Agent'Class;
            Subprogram_Kind  : Protected_Subprogram_Type;
            Entry_Id_Enter   : Indices.Entry_Index;
         when Exiting_PO =>
            PO_Exit           : not null access
              Oak.Agent.Tasks.Protected_Objects.Protected_Agent'Class;
         when Attach_Interrupt_Handlers =>
            Attach_Handlers   : access Oak.Interrupts.Interrupt_Handler_Array;
            Attach_Handler_PO : not null access
              Oak.Agent.Tasks.Protected_Objects.Protected_Agent'Class;
         when Entering_Atomic_Action =>
            AA_Enter          : not null access
              Atomic_Actions.Atomic_Object;
            Action_Id_Enter   : Indices.Action_Index;
         when Entering_Exit_Barrier =>
            AA_EB             : not null access
              Atomic_Actions.Atomic_Object;
            Action_Id_EB      : Indices.Action_Index;
            Exception_Raised  : Boolean;
         when Exiting_Atomic_Action =>
            AA_Exit           : not null access
              Atomic_Actions.Atomic_Object;
            Action_Id_Exit    : Indices.Action_Index;
            Atomic_Exception  : Boolean;
         when others =>
            null;
      end case;
   end record;

   type Yielded_State is (Timer, Interrupt, Voluntary);

   type Oak_Message_Store is record
      Yield_Status : Yielded_State;
      Message      : Oak_Message;
   end record;

   type Oak_Message_Location is access all Oak_Message_Store;

end Oak.Message;
