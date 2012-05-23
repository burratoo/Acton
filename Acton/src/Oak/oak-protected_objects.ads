limited with Oak.Agent.Tasks;
limited with Oak.Agent.Tasks.Protected_Objects;
limited with Oak.Entries;
limited with Oak.Scheduler;

package Oak.Protected_Objects with Preelaborate is

   type Protected_Subprogram_Type is
     (Protected_Function,
      Protected_Procedure,
      Protected_Entry);

   procedure Process_Enter_Request
     (Scheduler_Info  : in out Scheduler.Oak_Scheduler_Info;
      T               : not null access Agent.Tasks.Task_Agent'Class;
      PO              : not null access
        Agent.Tasks.Protected_Objects.Protected_Agent'Class;
      Subprogram_Kind : in Protected_Subprogram_Type;
      Entry_Id        : in Entries.Entry_Index;
      Chosen_Task     : out Agent.Tasks.Task_Handler);

   procedure Process_Exit_Request
     (Scheduler_Info : in out Scheduler.Oak_Scheduler_Info;
      T              : not null access Agent.Tasks.Task_Agent'Class;
      PO             : not null access
        Agent.Tasks.Protected_Objects.Protected_Agent'Class;
      Chosen_Task    : out Agent.Tasks.Task_Handler);

end Oak.Protected_Objects;
