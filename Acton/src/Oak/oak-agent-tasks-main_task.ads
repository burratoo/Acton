------------------------------------------------------------------------------
--                                                                          --
--                              OAK COMPONENTS                              --
--                                                                          --
--                         OAK.AGENT.TASKS.MAIN_TASK                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                 Copyright (C) 2012-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

package Oak.Agent.Tasks.Main_Task with Preelaborate is

   procedure Setup_Main_Task
     (Agent           : out Task_Id;
      Stack_Size      : in  Storage_Elements.Storage_Count;
      Name            : in  String;
      Normal_Priority : in  Integer;
      Run_Loop        : in  Address);
   --  Setups up Acton's first task. This is the task that runs the user's main
   --  program.

end Oak.Agent.Tasks.Main_Task;
