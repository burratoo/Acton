with Oak.Oak_Task.Scheduler_Agent;

package body Oak.Scheduler.Agent_List is

   package SA_Ops renames Oak.Oak_Task.Scheduler_Agent;

   -------------------------
   -- Add_Scheduler_Agent --
   -------------------------

   procedure Add_Scheduler_Agent
     (Scheduler_Info : in out Oak_Scheduler_Info;
      New_Agent      : in Oak_Task_Handler)
   is
      Agent      : Oak_Task_Handler := Scheduler_Info.Scheduler_Agent_Table;
      Prev_Agent : Oak_Task_Handler;
   begin
      if Agent = null then
         Scheduler_Info.Scheduler_Agent_Table := New_Agent;
         SA_Ops.Set_Next_Agent (T => New_Agent, Next_Agent => null);
      elsif SA_Ops.Get_Lowest_Priority (New_Agent) >
            SA_Ops.Get_Highest_Priority (Agent)
      then
         Scheduler_Info.Scheduler_Agent_Table := New_Agent;
         SA_Ops.Set_Next_Agent (T => New_Agent, Next_Agent => Agent);
      else
         while Agent /= null
           and then SA_Ops.Get_Lowest_Priority (Agent) >
                    SA_Ops.Get_Highest_Priority (New_Agent)
         loop
            Prev_Agent := Agent;
            Agent      := SA_Ops.Get_Next_Agent (Agent);
         end loop;
         SA_Ops.Set_Next_Agent (T => Prev_Agent, Next_Agent => New_Agent);
         SA_Ops.Set_Next_Agent (T => New_Agent, Next_Agent => Agent);
      end if;
   end Add_Scheduler_Agent;

end Oak.Scheduler.Agent_List;
