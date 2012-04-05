package Oak.Oak_Task.Queue is

   pragma Preelaborate;

   type Queue_End_Point is (Head, Tail);

   procedure Add_Task_Before (Queue     : in out Oak_Task_Handler;
                              T         : in Oak_Task_Handler;
                              Before    : in Oak_Task_Handler;
                              Queue_End : in Queue_End_Point := Head);
   procedure Add_Task_After (Queue : in out Oak_Task_Handler;
                       T     : in Oak_Task_Handler;
                       After : in Oak_Task_Handler);
   procedure Add_Task_To_Head (Queue : in out Oak_Task_Handler;
                     T : in Oak_Task_Handler);
   procedure Add_Task_To_Tail (Queue : in out Oak_Task_Handler;
                     T : in Oak_Task_Handler);
   procedure Remove_Task (Queue : in out Oak_Task_Handler;
                          T     : in Oak_Task_Handler);
   procedure Remove_Task_From_Head (Queue : in out Oak_Task_Handler);
   procedure Remove_Task_From_Tail (Queue : in out Oak_Task_Handler);

   function Get_Next_Task
     (T    : Oak_Task_Handler)
      return Oak_Task_Handler;
   function Get_Prev_Task
     (T    : Oak_Task_Handler)
      return Oak_Task_Handler;
   procedure Set_Next_Task (T, Next : in Oak_Task_Handler);
   procedure Set_Prev_Task (T, Prev : in Oak_Task_Handler);
   procedure Set_Queue_Link (T, Prev, Next : in Oak_Task_Handler);
end Oak.Oak_Task.Queue;
