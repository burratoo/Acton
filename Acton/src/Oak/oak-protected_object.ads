with Oak.Scheduler;

package Oak.Protected_Object with Preelaborate is

   type Protected_Subprogram_Type is
     (Protected_Function,
      Protected_Procedure,
      Protected_Entry);

   procedure Process_Enter_Request
     (Scheduler_Info  : in out Scheduler.Oak_Scheduler_Info;
      T               : in Oak_Task_Handler;
      PO              : in Oak_Task_Handler;
      Subprogram_Kind : in Protected_Subprogram_Type;
      Entry_Id        : in Entry_Index;
      Chosen_Task     : out Oak_Task_Handler);

   procedure Process_Exit_Request
     (Scheduler_Info    : in out Scheduler.Oak_Scheduler_Info;
      T                 : in Oak_Task_Handler;
      PO                : in Oak_Task_Handler;
      Chosen_Task       : out Oak_Task_Handler);

end Oak.Protected_Object;
