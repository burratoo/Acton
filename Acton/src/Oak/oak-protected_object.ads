with Oak.Oak_Task; use Oak.Oak_Task;
with Oak.Scheduler;

package Oak.Protected_Object is
   pragma Preelaborate;

   procedure Process_Enter_Request
     (Scheduler_Info  : in out Scheduler.Oak_Scheduler_Info;
      T               : in Oak_Task_Handler;
      PO              : in Oak_Task_Handler;
      Subprogram_Kind : in Protected_Subprogram_Type;
      Entry_Id        : in Entry_Index;
      Chosen_Task     : out Oak_Task_Handler);

   procedure Process_Exit_Request
     (Scheduler_Info : in out Scheduler.Oak_Scheduler_Info;
      T              : in Oak_Task_Handler;
      PO             : in Oak_Task_Handler;
      Chosen_Task    : out Oak_Task_Handler);

end Oak.Protected_Object;
