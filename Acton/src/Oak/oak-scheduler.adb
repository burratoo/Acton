with Oak.Core;
with Oak.Agent.Queue; use Oak.Agent.Queue;
with Oak.Oak_Time;    use Oak.Oak_Time;
with System;          use System;
with Oak.Agent.Tasks.Interrupts; use Oak.Agent.Tasks.Interrupts;
with Oak.States; use Oak.States;

package body Oak.Scheduler is

   type SH is access all Scheduler_Agent'Class;

   procedure Add_Task_To_Scheduler
     (Scheduler_Info : in out Oak_Scheduler_Info;
      T              : access Task_Agent'Class)
   is
      Task_Priority : constant Any_Priority := T.Normal_Priority;
      Agent         : access Scheduler_Agent'Class :=
        Scheduler_Info.Scheduler_Agent_Table;
      R             : constant Oak_Message :=
                        (Message_Type => Adding_Agent, Agent_To_Add  => T);
   begin
      if Agent = null then
         raise Program_Error;
      end if;

      while Agent /= SH (End_Of_Queue (Scheduler_Info.Scheduler_Agent_Table))
        and then Task_Priority < Agent.Lowest_Priority
      loop
         Agent := Scheduler_Handler (Next_Agent (Agent));
      end loop;
      T.Set_Scheduler_Agent_For_Task (Agent);
      Run_Scheduler_Agent (Agent => Agent, Reason => R);
   end Add_Task_To_Scheduler;

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

   procedure Check_With_Scheduler_Agents_On_Which_Task_To_Run_Next
     (Scheduler_Info : in out Oak_Scheduler_Info;
      Chosen_Task    : out Task_Handler)
   is
   begin
      Check_With_Scheduler_Agents_On_Which_Task_To_Run_Next
        (Chosen_Task          => Chosen_Task,
         From_Scheduler_Agent => Scheduler_Info.Scheduler_Agent_Table);
   end Check_With_Scheduler_Agents_On_Which_Task_To_Run_Next;

   procedure Check_With_Scheduler_Agents_On_Which_Task_To_Run_Next
     (From_Scheduler_Agent : access Scheduler_Agent'Class;
      Chosen_Task          : out Task_Handler)
   is
      Agent : access Scheduler_Agent'Class := From_Scheduler_Agent;
   begin
      Chosen_Task := null;

      loop
         --  Context switch to Manage Queues Routine.
         if Agent.Wake_Time < Clock then
            Chosen_Task :=
              Run_Scheduler_Agent
                (Agent  => Agent,
                 Reason => (Message_Type => Selecting_Next_Agent));
         else
            Chosen_Task := Agent.Agent_To_Run;
         end if;

         Agent       := SH (Next_Agent (Agent));
         exit when Chosen_Task /= null
           or else Agent = From_Scheduler_Agent;
      end loop;
   end Check_With_Scheduler_Agents_On_Which_Task_To_Run_Next;

   procedure Inform_Scheduler_Agent_Task_Has_Changed_State
     (Chosen_Task : in out Task_Handler)
   is
      Agent : constant access Scheduler_Agent'Class :=
         Chosen_Task.Scheduler_Agent_For_Task;
   begin
      if Chosen_Task.all in Interrupt_Agent then
         return;
      end if;

      Chosen_Task :=
        Run_Scheduler_Agent
          (Agent  => Agent,
           Reason =>
             (Message_Type       => Agent_State_Change,
              Agent_That_Changed => Chosen_Task));

      if Chosen_Task = null then
         Check_With_Scheduler_Agents_On_Which_Task_To_Run_Next
           (From_Scheduler_Agent => SH (Next_Agent (Agent)),
            Chosen_Task          => Chosen_Task);
      end if;
   end Inform_Scheduler_Agent_Task_Has_Changed_State;

   procedure Remove_Task_From_Scheduler
     (T : access Task_Agent'Class)
   is
      Agent : constant access Scheduler_Agent'Class :=
                T.Scheduler_Agent_For_Task;
   begin
      Run_Scheduler_Agent
        (Agent  => Agent,
         Reason => (Message_Type => Removing_Agent, Agent_To_Remove => T));
      T.Set_Scheduler_Agent_For_Task (null);
   end Remove_Task_From_Scheduler;

   function Run_Scheduler_Agent
     (Agent  : access Scheduler_Agent'Class;
      Reason : in Oak_Message)
      return access Task_Agent'Class is
   begin
      Run_Scheduler_Agent (Agent => Agent, Reason => Reason);
      return Task_Handler (Agent.Agent_To_Run);
   end Run_Scheduler_Agent;

   procedure Run_Scheduler_Agent
     (Agent  : access Scheduler_Agent'Class;
      Reason : in Oak_Message) is
   begin
      Agent.Set_Agent_Message (Reason);
      Agent.Set_State (Reason.Message_Type);
      Core.Context_Switch_To_Agent (Agent);
      Agent.Set_Wake_Time (Agent.Desired_Run_Time);
      Agent.Scheduler_Timer.Update_Timer (New_Time => Agent.Wake_Time);
   end Run_Scheduler_Agent;

   ------------------------------------------------------------
   -- Run_The_Bloody_Scheduler_Agent_That_Wanted_To_Be_Woken --
   --
   --  Note that we check from the highest priority agent down
   --  as a higher priority agent may wake up inbetween the
   --  wake up alarm and the lower priority agent actually
   --  running
   ------------------------------------------------------------
   procedure Run_The_Bloody_Scheduler_Agent_That_Wanted_To_Be_Woken
     (Scheduler_Info : in out Oak_Scheduler_Info;
      Chosen_Task    : in out Task_Handler)
   is
      Current_Time : constant Time         := Oak_Time.Clock;
      Current_Task : constant Task_Handler := Chosen_Task;
      Agent        : access Scheduler_Agent'Class :=
        Scheduler_Info.Scheduler_Agent_Table;
   begin
      Chosen_Task := null;
      loop
         if Agent.Desired_Run_Time < Current_Time then
            Chosen_Task :=
               Run_Scheduler_Agent
                 (Agent  => Agent,
                  Reason => (Message_Type => Selecting_Next_Agent));
         end if;
         Agent := SH (Next_Agent (Agent));
         exit when Chosen_Task /= null
           or else Agent = Scheduler_Info.Scheduler_Agent_Table
           or else (Current_Task /= null and then
                      Current_Task.Normal_Priority > Agent.Highest_Priority);
      end loop;

      if Chosen_Task = null
        and then Current_Task.State not in Waiting
        and then Current_Task.State /= Interrupt_Done
      then
         Chosen_Task := Current_Task;
      end if;
   end Run_The_Bloody_Scheduler_Agent_That_Wanted_To_Be_Woken;

end Oak.Scheduler;
