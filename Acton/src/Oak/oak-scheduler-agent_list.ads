package Oak.Scheduler.Agent_List with Preelaborate is

   procedure Add_Scheduler_Agent
     (Scheduler_Info : in out Oak_Scheduler_Info;
      New_Agent      : not null access Scheduler_Agent'Class);

end Oak.Scheduler.Agent_List;
