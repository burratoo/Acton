package body Oak.Scheduler.Agent_List is

   -------------------------
   -- Add_Scheduler_Agent --
   -------------------------

   procedure Add_Scheduler_Agent
     (Scheduler_Info : in out Oak_Scheduler_Info;
      New_Agent      : not null access Scheduler_Agent'Class)
   is
      Agent      : access Scheduler_Agent'Class :=
                     Scheduler_Info.Scheduler_Agent_Table;
      Prev_Agent : access Scheduler_Agent'Class;
   begin
      if Agent = null then
         Scheduler_Info.Scheduler_Agent_Table := New_Agent;
         New_Agent.Set_Next_Agent (null);
      elsif New_Agent.Lowest_Priority > New_Agent.Highest_Priority then
         Scheduler_Info.Scheduler_Agent_Table := New_Agent;
         New_Agent.Set_Next_Agent (Agent);
      else
         while Agent /= null
           and then Agent.Lowest_Priority > New_Agent.Highest_Priority
         loop
            Prev_Agent := Agent;
            Agent      := Agent.Next_Agent;
         end loop;
         Prev_Agent.Set_Next_Agent (Next_Agent => New_Agent);
         New_Agent.Set_Next_Agent (Next_Agent => Agent);
      end if;
   end Add_Scheduler_Agent;

end Oak.Scheduler.Agent_List;
