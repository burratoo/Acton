------------------------------------------------------------------------------
--                                                                          --
--                              OAK COMPONENTS                              --
--                                                                          --
--                        OAK.AGENT.TASKS.ACTIVATION                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                 Copyright (C) 2011-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

with Oak.Oak_Time;

with Oak.Agent.Oak_Agent; use Oak.Agent.Oak_Agent;
with Oak.Scheduler;       use Oak.Scheduler;
with Oak.States;          use Oak.States;

package body Oak.Agent.Tasks.Activation is

   -------------------------
   -- Continue_Activation --
   -------------------------

   procedure Continue_Activation
     (Activator        : in Task_Id;
      Activation_List  : in Task_List;
      Next_Task_To_Run : out Task_Id)
   is
      T : Task_Id;
   begin
      --  Possibly redundant check to make sure that the Activator has tasks to
      --  activate
      if Activation_List = No_Agent then
         raise Program_Error with "Activator has no tasks to activate!";
      end if;

      --  Loop through activation list to find the first task whose state is
      --  Activation_Pending or Terminated. If we reach the end of the list
      --  then all tasks have activated successfully.

      T := Activation_List;

      while T /= No_Agent
        and then (State (T) = Activation_Pending or State (T) = Terminated)
      loop
         T := Next_Agent (T);
      end loop;

      --  Deal with the selected task (or no task) and select the next task to
      --  run.

      if T = No_Agent then
         --  Activation has been successful. Notify and run the Activator.
         Set_State (For_Agent => Activator, State => Activation_Successful);
         Next_Task_To_Run  := Activator;

      else
         --  Deal with the task we've come across.

         case State (T) is
            when Activation_Pending =>
               --  The task is waiting to be activated. So we activate.
               Next_Task_To_Run := T;

            when Terminated =>
               --  The task has terminate! This means it was not successful
               --  in activating and thus the whole activation list fails.

               Purge_Activation_List
                 (Activator        => Activator,
                  Activation_List  => Activation_List,
                  Next_Task_To_Run => Next_Task_To_Run);

            when others =>
               --  Should never get here.
               raise Program_Error;
         end case;
      end if;
   end Continue_Activation;

   -----------------------
   -- Finish_Activation --
   -----------------------

   procedure Finish_Activation
     (Activator        : in Task_Id;
      Activation_List  : in Task_List;
      Next_Task_To_Run : out Task_Id)
   is
      T, Prev_T : Task_Id := Activation_List;
   begin
      --  Release each task in the activation list since if we got this far all
      --  tasks will have been activated.

      while T /= No_Agent loop
         Set_State (For_Agent => T, State => Runnable);
         Set_Wake_Time (For_Agent => T, Wake_Time => Oak_Time.Clock);
         Set_Next_Deadline_For_Task (T, Using => Wake_Up_Time);

         Add_Agent_To_Scheduler (T);

         Prev_T := T;
         T := Next_Agent (T);

         Set_Next_Agent (For_Agent =>  T, Next_Agent => No_Agent);
      end loop;

      --  The activator can now continue.

      Set_State (For_Agent => Activator, State => Runnable);
   end Finish_Activation;

   ---------------------------
   -- Purge_Activation_List --
   ---------------------------

   procedure Purge_Activation_List
     (Activator        : in Task_Id;
      Activation_List  : in Task_List;
      Next_Task_To_Run : out Task_Id)
   is
      T, Delete_T : Task_Id := Activation_List;
   begin
      while T /= No_Agent loop

         Delete_T := T;
         T := Next_Agent (T);

         --  Delete the task's Oak Agent Record
         Delete_Agent (Delete_T);

         --  Delete the task's Task Agent record
         Deallocate_Agent (Delete_T);
      end loop;

      Set_State (For_Agent => Activator, State => Activation_Failed);

   end Purge_Activation_List;

end Oak.Agent.Tasks.Activation;
