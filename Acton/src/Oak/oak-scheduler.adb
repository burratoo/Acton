with Oak.Core; use Oak.Core;
with Oak.Agent.Queue; use Oak.Agent.Queue;
with Oak.Oak_Time;    use Oak.Oak_Time;
with Oak.States; use Oak.States;

package body Oak.Scheduler is

   type SH is access all Scheduler_Agent'Class;

   procedure Add_Agent_To_Scheduler (Agent : not null access Oak_Agent'Class)
   is
   begin
      if Agent.Scheduler_Agent_For_Agent = null then
         raise Program_Error;
      end if;

      --  TODO: Add a check to see if the scheduler agent has been initialised
      --  or not.

      Run_Scheduler_Agent
        (Agent  => Agent.Scheduler_Agent_For_Agent,
         Reason => (Message_Type => Adding_Agent, Agent_To_Add  => Agent));
   end Add_Agent_To_Scheduler;

   procedure Add_Scheduler_To_Scheduler_Table
     (Scheduler_Info : in out Oak_Scheduler_Info;
      Scheduler      : access Scheduler_Agent'Class)
   is
      Agent : access Scheduler_Agent'Class :=
                     Scheduler_Info.Scheduler_Agent_Table;
   begin
      if Agent = null or else
        Scheduler.Lowest_Priority > Scheduler.Highest_Priority
      then
         Add_Agent_To_Head (Scheduler_Info.Scheduler_Agent_Table, Scheduler);
      else
         while Agent /= SH (End_Of_Queue
                            (Scheduler_Info.Scheduler_Agent_Table))
           and then Agent.Lowest_Priority > Scheduler.Highest_Priority
         loop
            Agent := SH (Next_Agent (Agent));
         end loop;
         Add_Agent_Before
           (Queue     => Scheduler_Info.Scheduler_Agent_Table,
            Agent     => Scheduler,
            Before    => Agent,
            Queue_End => Tail);
      end if;
   end Add_Scheduler_To_Scheduler_Table;

   procedure Check_Sechduler_Agents_For_Next_Task_To_Run
     (Scheduler_Info   : in out Oak_Scheduler_Info;
      Next_Task_To_Run : out Agent_Handler)
   is
   begin
      Check_Sechduler_Agents_For_Next_Task_To_Run
        (Next_Task_To_Run     => Next_Task_To_Run,
         From_Scheduler_Agent => Scheduler_Info.Scheduler_Agent_Table);
   end Check_Sechduler_Agents_For_Next_Task_To_Run;

   procedure Check_Sechduler_Agents_For_Next_Task_To_Run
     (From_Scheduler_Agent : access Scheduler_Agent'Class;
      Next_Task_To_Run     : out Agent_Handler)
   is
      Agent : access Scheduler_Agent'Class := From_Scheduler_Agent;
   begin
      Next_Task_To_Run := null;

      loop
         Next_Task_To_Run := Agent.Agent_To_Run;

         Agent       := SH (Next_Agent (Agent));
         exit when Next_Task_To_Run /= null
           or else Agent = From_Scheduler_Agent;
      end loop;

      while Next_Task_To_Run /= null
        and then Next_Task_To_Run.all in Scheduler_Agent'Class
      loop
         Next_Task_To_Run := Scheduler_Handler (Next_Task_To_Run).Agent_To_Run;
      end loop;
   end Check_Sechduler_Agents_For_Next_Task_To_Run;

   function Find_Scheduler_For_System_Priority
     (Priority : Any_Priority;
      CPU      : System.Multiprocessors.CPU_Range)
      return access Scheduler_Agent'Class
   is
      pragma Unreferenced (CPU);

      Head  : constant not null access Scheduler_Agent'Class :=
                Core.Oak_Instance.Scheduler_Info.Scheduler_Agent_Table;
      Agent : not null access Scheduler_Agent'Class := Head;

      --  TODO: update this to support mutliprocessors
   begin
      while Agent /= SH (End_Of_Queue (Head))
        and then Priority < Agent.Lowest_Priority
      loop
         Agent := Scheduler_Handler (Next_Agent (Agent));
      end loop;
      return Agent;
   end Find_Scheduler_For_System_Priority;

   procedure Inform_Scheduler_Agent_Task_Has_Changed_State
     (Changed_Task     : access Task_Agent'Class;
      Next_Task_To_Run : out Agent_Handler)
   is
      Agent : constant access Scheduler_Agent'Class :=
         Changed_Task.Scheduler_Agent_For_Agent;
   begin
      Next_Task_To_Run :=
        Run_Scheduler_Agent
          (Agent  => Agent,
           Reason =>
             (Message_Type       => Agent_State_Change,
              Agent_That_Changed => Changed_Task));

      if Next_Task_To_Run = null then
         Check_Sechduler_Agents_For_Next_Task_To_Run
           (From_Scheduler_Agent => SH (Next_Agent (Agent)),
            Next_Task_To_Run     => Next_Task_To_Run);
      end if;
   end Inform_Scheduler_Agent_Task_Has_Changed_State;

   procedure Remove_Agent_From_Scheduler
     (Agent : not null access Oak_Agent'Class) is
   begin
      Run_Scheduler_Agent
        (Agent  => Agent.Scheduler_Agent_For_Agent,
         Reason => (Message_Type => Removing_Agent, Agent_To_Remove => Agent));
   end Remove_Agent_From_Scheduler;

   function Run_Scheduler_Agent
     (Agent  : not null access Scheduler_Agent'Class;
      Reason : in Oak_Message)
      return access Task_Agent'Class
   is
      Selected_Agent : access Oak_Agent'Class;
   begin
      Run_Scheduler_Agent (Agent => Agent, Reason => Reason);
      Selected_Agent := Agent.Agent_To_Run;
      while Selected_Agent /= null
        and then Selected_Agent.all in Scheduler_Agent'Class
      loop
         Selected_Agent := Scheduler_Handler (Selected_Agent).Agent_To_Run;
      end loop;
      return Task_Handler (Selected_Agent);
   end Run_Scheduler_Agent;

   procedure Run_Scheduler_Agent
     (Agent  : not null access Scheduler_Agent'Class;
      Reason : in Oak_Message)
   is
      SA      : access Scheduler_Agent'Class := Agent;
      Message : Oak_Message                  := Reason;
   begin
      --  Use a loop rather than recursion to limit and protected the stack.
      --  The recursion version of this code suffered from the risk that two
      --  agents could ping-pong between each other indefinitely, leading to
      --  the stack overflowing. By using a loop the system simply enters an
      --  inifinate loop.

      while SA /= null loop
         --  Easier to remove and add agent than to check if agent is present
         --  in charge list.

         Oak_Instance.Remove_Agent_From_Charge_List (SA);
         Oak_Instance.Add_Agent_To_Charge_List (SA);

         SA.Set_Agent_Message (Message);
         Core.Context_Switch_To_Agent (SA);
         case SA.Agent_Message.Message_Type is
            when Scheduler_Agent_Done =>
               SA.Set_Agent_To_Run (SA.Agent_Message.Next_Agent);
               SA.Set_State (Runnable);
               SA.Set_Wake_Time (SA.Desired_Run_Time);
               SA.Scheduler_Timer.Update_Timer (New_Time => SA.Wake_Time);

               if not SA.Agent_Message.Keep_In_Charge_List then
                  Oak_Instance.Remove_Agent_From_Charge_List (SA);
               end if;

               if SA.Agent_To_Run /= null
                 and then SA.Agent_To_Run.all in Scheduler_Agent'Class
               then
                  SA := Scheduler_Handler (SA.Agent_To_Run);
                  Message := (Message_Type => Selecting_Next_Agent);
               else
                  SA := null;
               end if;

            when Sleeping =>
               SA.Set_State (Sleeping);
               SA.Set_Agent_To_Run (null);
               SA.Set_Wake_Time (WT => SA.Agent_Message.Wake_Up_At);
               SA.Scheduler_Timer.Update_Timer (New_Time => Time_Last);

               if SA.Agent_Message.Remove_From_Charge_List then
                  Oak_Instance.Remove_Agent_From_Charge_List (SA);
               end if;

               Message := (Message_Type       => Agent_State_Change,
                           Agent_That_Changed => SA);
               SA := SA.Scheduler_Agent_For_Agent;
            when Continue_Sleep =>
               --  When removing the agent from the charge list, the scheduler
               --  agent's timer should be set to Time_Last to effectively
               --  disable it since we never remove scheduler timers from
               --  the manager. This needs to be done since the execution
               --  timer code may modify it while the task is asleep.

               if not SA.Agent_Message.Remain_In_Charge_List then
                  Oak_Instance.Remove_Agent_From_Charge_List (SA);
                  SA.Scheduler_Timer.Update_Timer (New_Time => Time_Last);
               end if;
               SA := null;
            when others =>
               Oak_Instance.Remove_Agent_From_Charge_List (SA);
               SA := null;
         end case;
      end loop;

   end Run_Scheduler_Agent;

   ------------------------------------------------------------
   -- Run_The_Bloody_Scheduler_Agent_That_Wanted_To_Be_Woken --
   --
   --  Note that we check from the highest priority agent down
   --  as a higher priority agent may wake up in between the
   --  wake up alarm and the lower priority agent actually
   --  running
   ------------------------------------------------------------

   procedure Run_The_Bloody_Scheduler_Agent_That_Wanted_To_Be_Woken
     (Agent            : access Scheduler_Agent'Class;
      Current_Agent    : in Agent_Handler;
      Next_Task_To_Run : out Agent_Handler) is
   begin
      Next_Task_To_Run :=
         Run_Scheduler_Agent
           (Agent  => Agent,
            Reason => (Message_Type => Selecting_Next_Agent));

      if Next_Task_To_Run = null
        and then Current_Agent.State not in Waiting
        and then Current_Agent.State /= Interrupt_Done
      then
         Next_Task_To_Run := Current_Agent;
      end if;
   end Run_The_Bloody_Scheduler_Agent_That_Wanted_To_Be_Woken;

end Oak.Scheduler;
