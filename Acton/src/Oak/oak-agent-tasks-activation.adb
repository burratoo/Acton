with Oak.Oak_Time;
with Oak.Scheduler;

package body Oak.Agent.Tasks.Activation is

   function Next_Task
     (Current_Task : access Task_Agent'Class)
      return access Task_Agent'Class;

   function Next_Task
     (Current_Task : access Task_Agent'Class)
      return access Task_Agent'Class
   is
   begin
      return Current_Task.Activation_List;
   end Next_Task;

   -------------------------
   -- Continue_Activation --
   -------------------------

   function Continue_Activation
     (Activator : access Task_Agent'Class)
      return      access Task_Agent'Class
   is
      TP : access Task_Agent'Class;
   begin
      --  Possibly redundant check to make sure that the Activator has tasks to
      --  activate
      if Activator.Activation_List = null then
         raise Program_Error with "Activator has no tasks to activate!";
      end if;

      --  Loop through activation list to find the first task whose state is
      --  Activation_Pending or Terminated. If we reach the end of the list
      --  then all tasks have activated successfully.

      TP := Next_Task (Activator);
      while TP /= null
        and then (TP.State = Activation_Pending or TP.State = Terminated)
      loop
         TP := Next_Task (TP);
      end loop;

      if TP = null then
         Activator.State := Activation_Successful;
      else
         case TP.State is
            when Activation_Pending =>
               null;
            when Terminated =>
               TP := Next_Task (Activator);
               while TP /= null loop
                  if TP.State = Activation_Pending then
                     TP.State := Terminated;
                  end if;
                  TP := Next_Task (TP);
               end loop;
               Activator.State := Activation_Failed;
            when others =>
               raise Program_Error;
         end case;
      end if;

      --  TP returns with the next task to activate or null if all tasks have
      --  activated or if activation has failed.
      return TP;
   end Continue_Activation;

   -----------------------
   -- Finish_Activation --
   -----------------------

   procedure Finish_Activation (Activator : in out Task_Agent'Class) is
      T : access Task_Agent'Class := Next_Task (Activator'Access);
   begin
      while T /= null loop
         T.State          := Runnable;
         T.Wake_Time      := Oak_Time.Clock;

         Set_Next_Deadline_For_Task (T.all, Using => Wake_Up_Time);

         Oak.Scheduler.Add_Agent_To_Scheduler (T);
         T := Next_Task (T);
      end loop;
      Activator.State           := Runnable;
      Activator.Activation_List := null;
   end Finish_Activation;

end Oak.Agent.Tasks.Activation;
