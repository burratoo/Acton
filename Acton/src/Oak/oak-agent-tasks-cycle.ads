package Oak.Agent.Tasks.Cycle with Preelaborate is
   procedure Setup_Cyclic_Section (T : in Task_Handler);
   procedure New_Cycle (T : in out Task_Handler);
   procedure Release_Task
     (Task_To_Release, Releasing_Task : in Task_Handler;
      Next_Task                       : out Task_Handler);

   procedure Task_Released
     (Released_Task : access Task_Agent'Class);
   --  Called when a sporadic task is released. Is called internally and
   --  indirectly by scheduler agents through the Destination_On_Wake_Up call.

end Oak.Agent.Tasks.Cycle;
