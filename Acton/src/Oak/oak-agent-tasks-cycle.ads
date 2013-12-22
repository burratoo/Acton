package Oak.Agent.Tasks.Cycle with Preelaborate is
   procedure Setup_Cyclic_Section (T : in out Task_Agent'Class);
   procedure New_Cycle (T                : access Task_Agent'Class;
                        Next_Task_To_Run : out Agent_Handler);
   procedure Release_Task
     (Task_To_Release  : access Task_Agent'Class;
      Releasing_Task   : in Agent_Handler;
      Next_Task_To_Run : out Agent_Handler);

   procedure Task_Release
     (Released_Task : access Task_Agent'Class);
   --  Called when a sporadic task is released. Is called internally and
   --  indirectly by scheduler agents through the Destination_On_Wake_Up call.

end Oak.Agent.Tasks.Cycle;
