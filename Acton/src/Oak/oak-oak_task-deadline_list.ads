package Oak.Oak_Task.Deadline_List is

   pragma Preelaborate;

   procedure Insert_Task
     (List_Head   : in out Oak_Task_Handler;
      Task_To_Add : Oak_Task_Handler);
   procedure Remove_Task
     (List_Head      : in out Oak_Task_Handler;
      Task_To_Remove : Oak_Task_Handler);
   procedure Task_Deadline_Updated
     (List_Head    : in out Oak_Task_Handler;
      Updated_Task : Oak_Task_Handler);
   function Get_Earliest_Deadline
     (List_Head : Oak_Task_Handler)
      return      Time;

end Oak.Oak_Task.Deadline_List;
