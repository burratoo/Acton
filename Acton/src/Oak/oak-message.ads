with Oak.Oak_Time;

with Oak.Indices; use Oak.Indices;
with Oak.States;  use Oak.States;

limited with Oak.Agent;
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
            Wake_Up_At              : Oak_Time.Time;
            Remove_From_Charge_List : Boolean;
         when Change_Cycle_Period =>
            New_Cycle_Period  : Oak_Time.Time_Span;
            Cycle_Period_Task : not null access
              Oak.Agent.Tasks.Task_Agent'Class;
         when Change_Relative_Deadline =>
            New_Deadline_Span : Oak_Time.Time_Span :=
                                  Oak_Time.Time_Span_Zero;
            Deadline_Task     : not null access
              Oak.Agent.Tasks.Task_Agent'Class;
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
         when Selecting_Next_Agent =>
            null;
         when Adding_Agent =>
            Agent_To_Add        : not null access Oak.Agent.Oak_Agent'Class;
         when Removing_Agent =>
            Agent_To_Remove     : not null access Oak.Agent.Oak_Agent'Class;
         when Agent_State_Change =>
            Agent_That_Changed  : not null access Oak.Agent.Oak_Agent'Class;
         when Scheduler_Agent_Done =>
            Next_Agent          : access Oak.Agent.Oak_Agent'Class;
            Wake_Scheduler_At   : Oak_Time.Time;
            Keep_In_Charge_List : Boolean;
         when Adding_Agent_To_Scheduler =>
            Agent_To_Add_To_Scheduler : not null access
              Oak.Agent.Oak_Agent'Class;
         when Continue_Sleep =>
            Remain_In_Charge_List : Boolean;
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
