with Oak.Oak_Time;

with Oak.Agent;
with Oak.Brokers;
with Oak.Indices;    use Oak.Indices;
with Oak.Interrupts; use Oak.Interrupts;
with Oak.States;     use Oak.States;
with System;         use System;

package Oak.Message with Preelaborate is

   type Protected_Subprogram_Type is
     (Protected_Function,
      Protected_Procedure,
      Protected_Entry);

   type Task_Property_Kind is (Cycle_Period, Relative_Deadline);

   type Task_Property (Property : Task_Property_Kind := Cycle_Period) is record
      case Property is
         when Cycle_Period =>
            Cycle_Period  : Oak_Time.Time_Span;
         when Relative_Deadline =>
            Deadline_Span : Oak_Time.Time_Span;
      end case;
   end record;

   type Queue_End is (Back, Front);

   type Oak_Message (Message_Type : Agent_State := No_Message) is record
      case Message_Type is
         when Activation_Pending =>
            Activation_List : Oak.Agent.Task_List;

         when Sleeping =>
            Wake_Up_At              : Oak_Time.Time;
            Remove_From_Charge_List : Boolean;

         when Update_Task_Property =>
            Property_To_Update  : Task_Property;
            Update_Task         : Oak.Agent.Task_Id;

         when Release_Task =>
            Task_To_Release : Oak.Agent.Task_Id;

         when Entering_PO =>
            PO_Enter        : Oak.Brokers.Protected_Id;
            Subprogram_Kind : Protected_Subprogram_Type;
            Entry_Id_Enter  : Indices.Entry_Index;

         when Exiting_PO =>
            PO_Exit : Oak.Brokers.Protected_Id;

         when Attach_Interrupt_Handler =>
            Attach_Handler : Interrupt_Handler_Pair;

         when Selecting_Next_Agent =>
            null;

         when Adding_Agent =>
            Agent_To_Add : Oak.Agent.Oak_Agent_Id;
            Place_At     : Queue_End;

         when Adding_Agents =>
            Agents_To_Add : Oak.Agent.Oak_Agent_Id;

         when Removing_Agent =>
            Agent_To_Remove : Oak.Agent.Oak_Agent_Id;

         when Agent_State_Change =>
            Agent_That_Changed : Oak.Agent.Oak_Agent_Id;

         when Scheduler_Agent_Done =>
            Wake_Scheduler_At : Oak_Time.Time;
            Wake_Priority     : Any_Priority;
            Next_Agent        : Oak.Agent.Oak_Agent_Id;

         when Initialising_Agents =>
            Agents_To_Init : Oak.Agent.Oak_Agent_Id;

         when Wake_Agent =>
            Agent_To_Wake : Oak.Agent.Oak_Agent_Id;

         when others =>
            null;
      end case;
   end record;

   type Yielded_State is (Timer, Interrupt, Voluntary);

end Oak.Message;
