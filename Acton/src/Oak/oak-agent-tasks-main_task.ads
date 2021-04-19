------------------------------------------------------------------------------------------
--                                                                                      --
--                                    OAK COMPONENTS                                    --
--                                                                                      --
--                               OAK.AGENT.TASKS.MAIN_TASK                              --
--                                                                                      --
--                       Copyright (C) 2011-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

package Oak.Agent.Tasks.Main_Task with Preelaborate is

   procedure Setup_Main_Task
     (Stack_Size      : in  Storage_Elements.Storage_Count;
      Name            : in  String;
      Normal_Priority : in  Integer;
      Run_Loop        : in  Address);
   --  Setups up Acton's first task. This is the task that runs the user's main
   --  program. Automatically adds it to the scheduler system.

end Oak.Agent.Tasks.Main_Task;
