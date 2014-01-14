------------------------------------------------------------------------------
--                                                                          --
--                              OAK COMPONENTS                              --
--                                                                          --
--                        OAK.AGENT.TASKS.ACTIVATION                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                 Copyright (C) 2011-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

--  This package handles task activation.

package Oak.Agent.Tasks.Activation with Preelaborate is

   procedure Continue_Activation
     (Activator        : in Task_Id;
      Next_Task_To_Run : out Task_Id);
   --  Continues the activation process.
   --  The procedure will select an unactivated task once each call until all
   --  the tasks in the list have been activated, in which case the Activator
   --  task will be selected to run. If a task terminates instead of
   --  successfully activating, all tasks are terminated.

   procedure Finish_Activation
     (Activator        : in Task_Id;
      Next_Task_To_Run : out Task_Id);
   --  Called when all tasks on the activation chain have been activated.
   --  Releases the tasks on the chain (i.e. they get added to their scheduler
   --  agents) and the Activator is allowed to continue on its merry way.

   procedure Purge_Activation_List
     (Activator        : in Task_Id;
      Activation_List  : in Task_List;
      Next_Task_To_Run : out Task_Id);
   --  Called when a task within the activation list fails at either its
   --  elaboration or activation. Causes any storage associated with the tasks
   --  to be deallocated.

   procedure Start_Activation
     (Activator        : in Task_Id;
      Activation_List  : in Task_List;
      Next_Task_To_Run : out Task_Id);
   --  Starts the activation of a list of tasks within the activation list. The
   --  task that is activating a set of tasks contained within the activation
   --  chain is called the Activator.

end Oak.Agent.Tasks.Activation;
