package Oak.Oak_Task.Activation is
   pragma Preelaborate;

   function Continue_Activation
     (Activator : Oak_Task_Handler)
      return      Oak_Task_Handler;
   procedure Finish_Activation (Activator : Oak_Task_Handler);

end Oak.Oak_Task.Activation;
