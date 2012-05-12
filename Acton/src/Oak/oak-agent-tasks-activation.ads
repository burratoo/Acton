package Oak.Agent.Tasks.Activation with Preelaborate is

   function Continue_Activation
     (Activator : access Task_Agent'Class)
      return access Task_Agent'Class;

   procedure Finish_Activation (Activator : in out Task_Agent'Class);

end Oak.Agent.Tasks.Activation;
