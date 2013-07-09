with Oak.Message; use Oak.Message;

with Oak.Agent;                         use Oak.Agent;
with Oak.Agent.Tasks;                   use Oak.Agent.Tasks;
with Oak.Agent.Tasks.Protected_Objects; use Oak.Agent.Tasks.Protected_Objects;
with Oak.Indices;                       use Oak.Indices;
with Oak.Scheduler;                     use Oak.Scheduler;

package Oak.Protected_Objects with Preelaborate is

   procedure Process_Enter_Request
     (Scheduler_Info    : in out Oak_Scheduler_Info;
      Entering_Agent    : in Agent_Handler;
      PO                : not null access Protected_Agent'Class;
      Subprogram_Kind   : in Protected_Subprogram_Type;
      Entry_Id          : in Entry_Index;
      Next_Agent_To_Run : out Agent_Handler);

   procedure Process_Exit_Request
     (Scheduler_Info    : in out Oak_Scheduler_Info;
      Exiting_Agent     : not null access Oak_Agent'Class;
      PO                : not null access Protected_Agent'Class;
      Next_Agent_To_Run : out Agent_Handler);

   procedure Acquire_Protected_Object_For_Interrupt
     (PO : not null access
        Agent.Tasks.Protected_Objects.Protected_Agent'Class);

   procedure Release_Protected_Object_For_Interrupt
     (PO : not null access
        Agent.Tasks.Protected_Objects.Protected_Agent'Class);

end Oak.Protected_Objects;
