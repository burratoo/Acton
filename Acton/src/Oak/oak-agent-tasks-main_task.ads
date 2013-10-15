package Oak.Agent.Tasks.Main_Task with Preelaborate is
   procedure Initialise_Main_Task
     (Stack_Size      : in Storage_Elements.Storage_Count;
      Name            : in String;
      Normal_Priority : in Integer;
      Run_Loop        : in Address);
end Oak.Agent.Tasks.Main_Task;
