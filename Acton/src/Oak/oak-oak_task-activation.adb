with Oak.Core;
with Oak.Scheduler;
with Ada.Real_Time;

package body Oak.Oak_Task.Activation is

   function Next_Task
     (Current_Task : Oak_Task_Handler)
      return         Oak_Task_Handler;

   End_Of_List : constant Oak_Task_Handler := null;

   function Next_Task
     (Current_Task : Oak_Task_Handler)
      return         Oak_Task_Handler
   is
   begin
      return Current_Task.Activation_List;
   end Next_Task;

   -------------------------
   -- Continue_Activation --
   -------------------------

   function Continue_Activation
     (Activator : Oak_Task_Handler)
      return      Oak_Task_Handler
   is
      TP : Oak_Task_Handler;
   begin
      --  Possibly redundant check to make sure that the Activator has tasks to
      --  activate
      if Activator.Activation_List = End_Of_List then
         raise Program_Error with "Activator has no tasks to activate!";
      end if;

      --  Loop through activation list to find the first task whose state is
      --  Activation_Pending or Terminated. If we reach the end of the list
      --  then all tasks have activated successfully.

      TP := Next_Task (Activator);
      while TP /= End_Of_List
        and then (TP.State = Activation_Pending or
                  TP.State = Terminated)
      loop
         TP := Next_Task (TP);
      end loop;

      if TP = End_Of_List then
         Activator.State := Activation_Successful;
      else
         case TP.State is
            when Activation_Pending =>
               null;
            when Terminated =>
               TP := Next_Task (Activator);
               while TP /= End_Of_List loop
                  if TP.State = Activation_Pending then
                     TP.State := Terminated;
                  end if;
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

   procedure Finish_Activation (Activator : Oak_Task_Handler) is
      OI        : constant access Oak.Core.Oak_Data                :=
         Oak.Core.Get_Oak_Instance;
      Scheduler : constant access Oak.Scheduler.Oak_Scheduler_Info :=
                    Oak.Core.Get_Scheduler_Info (OI);

      TP              : Oak_Task_Handler := Next_Task (Activator);
      Activation_Time : constant Time    := Ada.Real_Time.Clock;
   begin
      while TP /= End_Of_List loop
         TP.State          := Runnable;
         TP.Wake_Time      := Activation_Time;
         TP.Next_Run_Cycle := Activation_Time + TP.Cycle_Period;

         --  If the deadline is set to zero, disable the deadline by setting
         --  Next_Deadline to last possible time.
         if TP.Deadline = Time_Span_Zero then
            TP.Next_Deadline := Ada.Real_Time.Time_Last;
         else
            TP.Next_Deadline  := Activation_Time + TP.Deadline;
         end if;

         Oak.Scheduler.Add_Task_To_Scheduler
           (Scheduler_Info => Scheduler.all,
            T              => TP);
         TP := Next_Task (TP);
      end loop;
      Activator.State := Runnable;
      Activator.Activation_List := End_Of_List;
   end Finish_Activation;

end Oak.Oak_Task.Activation;
