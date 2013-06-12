package Oak.Agent.Tasks.Cycle with Preelaborate is
   procedure Setup_Cyclic_Section (T : in Task_Handler);
   procedure New_Cycle (T : in out Task_Handler);
   procedure Release_Task
     (Task_To_Release, Releasing_Task : in Task_Handler;
      Next_Task                       : out Task_Handler);

end Oak.Agent.Tasks.Cycle;
