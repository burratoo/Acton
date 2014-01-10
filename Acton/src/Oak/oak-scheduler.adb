------------------------------------------------------------------------------
--                                                                          --
--                              OAK COMPONENTS                              --
--                                                                          --
--                              OAK.SCHEDULER                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                 Copyright (C) 2010-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

with Oak.Agent.Oak_Agent;  use Oak.Agent.Oak_Agent;
with Oak.Agent.Schedulers; use Oak.Agent.Schedulers;
with Oak.Core;             use Oak.Core;
with Oak.Message;          use Oak.Message;
with Oak.Oak_Time;         use Oak.Oak_Time;
with Oak.States;           use Oak.States;
with Oak.Timers;           use Oak.Timers;

package body Oak.Scheduler is

   ----------------------------
   -- Add_Agent_To_Scheduler --
   ----------------------------

   procedure Add_Agent_To_Scheduler (Agent : in Oak_Agent_Id)
   is
   begin
      --  If a scheduler agent is schedulable, it must have its scheduler agent
      --  recorded.

      if Scheduler_Agent_For_Agent (Agent) = No_Agent then
         raise Program_Error;
      end if;

      --  TODO: Add a check to see if the scheduler agent has been initialised
      --  or not.

      Run_Scheduler_Agent
        (Agent  => Scheduler_Agent_For_Agent (Agent),
         Reason => (Message_Type => Adding_Agent, Agent_To_Add  => Agent));
   end Add_Agent_To_Scheduler;

   --------------------------------------
   -- Add_Scheduler_To_Scheduler_Table --
   --------------------------------------

   procedure Add_Scheduler_To_Scheduler_Table
     (Table     : in out Scheduler_Table;
      Scheduler : in     Scheduler_Id)
   is
   begin
      --  Find spot to put Agent in table.

      if Table = No_Agent then
         --  No entry in the table.
         Table := Scheduler;

      elsif Lowest_Resposible_Priority (Scheduler) >
        Highest_Resposible_Priority (Table)
      then
         --  This scehduler should be placed at the haed of the table.
         Set_Next_Agent (For_Agent => Scheduler, Next_Agent => Table);
         Table := Scheduler;

      else
         --  Search for spot to insert scheduler.

         Search_For_Spot : declare
            Agent      : Scheduler_Id_With_No := Table;
            Prev_Agent : Scheduler_Id         := Table;
         begin

            while Agent /= No_Agent
              and then Lowest_Resposible_Priority (Agent) >
              Highest_Resposible_Priority (Scheduler)
            loop
               Prev_Agent := Agent;
               Agent      := Next_Agent (Agent);
            end loop;

            Set_Next_Agent (For_Agent => Prev_Agent, Next_Agent => Scheduler);
            Set_Next_Agent (For_Agent => Scheduler,  Next_Agent => Agent);
         end Search_For_Spot;
      end if;
   end Add_Scheduler_To_Scheduler_Table;

   --------------------------------------------------
   -- Check_Sechduler_Agents_For_Next_Agent_To_Run --
   --------------------------------------------------

   procedure Check_Sechduler_Agents_For_Next_Agent_To_Run
     (From_Scheduler_Agent : in  Scheduler_Id;
      Next_Agent_To_Run    : out Oak_Agent_Id)
   is
      Agent : Scheduler_Id_With_No := From_Scheduler_Agent;
   begin
      Next_Agent_To_Run := No_Agent;

      --  Check top-level scheduler agents

      while Next_Agent_To_Run = No_Agent and then Agent /= No_Agent loop
         Next_Task_To_Run := Agent_To_Run (Agent);
         Agent            := Next_Agent (Agent);
      end loop;

      --  If a top-level scheduler agent returns another scheduler agent, we
      --  need to keep searching until we find a non-scheduler agent.

      while Next_Task_To_Run /= No_Agent
        and then Next_Task_To_Run in Scheduler_Id
      loop
         Next_Task_To_Run := Agent_To_Run (Next_Task_To_Run);
      end loop;
   end Check_Sechduler_Agents_For_Next_Agent_To_Run;

   ----------------------------------------
   -- Find_Scheduler_For_System_Priority --
   ----------------------------------------

   function Find_Scheduler_For_System_Priority
     (Priority : Any_Priority;
      CPU      : System.Multiprocessors.CPU_Range)
      return Scheduler_Id_With_No
   is
      pragma Unreferenced (CPU);

      Agent : Scheduler_Id_With_No :=
                Core.Oak_Instance.Scheduler_Info.Scheduler_Agent_Table;

      --  TODO: update this to support mutliprocessors
   begin
      while Agent /= No_Agent
        and then Priority < Lowest_Resposible_Priority (Agent)
      loop
         Agent := Next_Agent (Agent);
      end loop;

      return Agent;
   end Find_Scheduler_For_System_Priority;

   ----------------------------------------------
   -- Inform_Scheduler_Agent_Has_Changed_State --
   ----------------------------------------------

   procedure Inform_Scheduler_Agent_Has_Changed_State
     (Changed_Agent     : in  Oak_Agent_Id;
      Next_Agent_To_Run : out Oak_Agent_Id)
   is
      Agent : constant Scheduler_Id :=
                Scheduler_Agent_For_Agent (Changed_Agent);
   begin
      Run_Scheduler_Agent
        (Agent        => Agent,
         Reason       =>
           (Message_Type       => Agent_State_Change,
            Agent_That_Changed => Changed_Task),
         Agent_To_Run => Next_Agent_To_Run);

      --  If the scheduler agent has no agents to run, check with scheduler
      --  agents responsible for lower priorities.

      if Next_Task_To_Run = No_Agent then
         Check_Sechduler_Agents_For_Next_Task_To_Run
           (From_Scheduler_Agent => Next_Agent (Agent),
            Next_Agent_To_Run    => Next_Agent_To_Run);
      end if;
   end Inform_Scheduler_Agent_Has_Changed_State;

   ---------------------------------
   -- Remove_Agent_From_Scheduler --
   ---------------------------------

   procedure Remove_Agent_From_Scheduler (Agent : in Oak_Agent_Id) is
   begin
      Run_Scheduler_Agent
        (Agent  => Scheduler_Agent_For_Agent (Agent),
         Reason => (Message_Type => Removing_Agent, Agent_To_Remove => Agent));
   end Remove_Agent_From_Scheduler;

   -------------------------
   -- Run_Scheduler_Agent --
   -------------------------

   procedure Run_Scheduler_Agent
     (Agent  : in Scheduler_Id;
      Reason : in Oak_Message)
   is
      SA      : Scheduler_Id_With_No := Agent;
      Message : Oak_Message          := Reason;
   begin
      --  Use a loop rather than recursion to limit and protected the stack.
      --  The recursion version of this code suffered from the risk that two
      --  agents could ping-pong between each other indefinitely, leading to
      --  the stack overflowing. By using a loop the system simply enters an
      --  inifinate loop.

      while SA /= No_Agent loop
         --  Easier to remove and add agent than to check if agent is present
         --  in charge list.

         Remove_Agent_From_Charge_List (Oak_Instance, SA);
         Add_Agent_To_Charge_List (Oak_Instance, SA);

         --  Run Scheduler Agent. ????? This will need fixing for the new stuff

         --  SA.Set_Agent_Message (Message);
         Core.Context_Switch_To_Agent (SA, Message);

         --  React to the scheduler agent's response.

         case Message.Message_Type is
            when Scheduler_Agent_Done =>
               --  The Scheduler Agent has run successfully. Store the contents
               --  of its return message and set up its timer for its next run
               --  so it can serivce its queues.

               Set_Agent_To_Run (SA.Agent_Message.Next_Agent);
               Set_State (For_Agent => SA, State => Runnable);
               SA.Set_Wake_Time (Message.Wake_Scheduler_At);
               Update_Timer (Scheduler_Timer (SA), New_Time => SA.Wake_Time);

               if not Message.Keep_In_Charge_List then
                  Remove_Agent_From_Charge_List (Oak_Instance, SA);
               end if;

               --  If the agent to run is a scheduler agent we will run it now.

               if Agent_To_Run (SA) /= No_Agent
                 and then Agent_To_Run (SA) in Scheduler_Agent'Class
               then
                  SA := Agent_To_Run (SA);
                  Message := (Message_Type => Selecting_Next_Agent);
               else
                  --  Otherwise we are done
                  SA := No_Agent;
               end if;

            when Sleeping =>
               --  The scheduler agent has decided to go to sleep. This occurs
               --  for non-top-level scheduler agents whose execution budgets
               --  have expired.

               Set_State (For_Agent => SA, State => Sleeping);
               Set_Agent_To_Run (For_Agent => SA, Agent_To_Run => No_Agent);
               Set_Wake_Time (For_Agent => SA,
                              Wake_Time => Message.Wake_Up_At);
               Deactivate_Timer (Scheduler_Timer (SA));

               if SA.Agent_Message.Remove_From_Charge_List then
                  Remove_Agent_From_Charge_List (Oak_Instance, SA);
               end if;

               --  Notify the scheduler agent's own scheduler agent that its
               --  state has changed.

               Message := (Message_Type       => Agent_State_Change,
                           Agent_That_Changed => SA);
               SA := Scheduler_Agent_For_Agent (SA);

            when Continue_Sleep =>
               --  A sleeping scheduler agent has run (yes that is possible
               --  so tasks can be added and removed) and is still sleeping.
               --  May need to remove from the charge list in the event the
               --  agent was sleeping while the budget expired.

               if not SA.Agent_Message.Remain_In_Charge_List then
                  Oak_Instance.Remove_Agent_From_Charge_List (SA);
                  Deactivate_Timer (Scheduler_Timer (SA));
               end if;

               --  And we are done.
               SA := No_Agent;

            when others =>
               --  ??? Should not get here.
               Remove_Agent_From_Charge_List (Oak_Instance, SA);
               SA := No_Agent;
         end case;
      end loop;

   end Run_Scheduler_Agent;

   procedure Run_Scheduler_Agent
     (Agent        : in  Scheduler_Id;
      Reason       : in  Oak_Message;
      Agent_To_Run : out Oak_Agent_Id)
   is
   begin
      Run_Scheduler_Agent (Agent => Agent, Reason => Reason);
      Agent_To_Run := Agent_To_Run (Agent);

      --  Check to make sure selected agent is not another scheduler agent,
      --  and check that one if it is the case.

      while Agent_To_Run /= null
        and then Agent_To_Run in Scheduler_Id
      loop
         Agent_To_Run := Agent_To_Run (Selected_Agent);
      end loop;
   end Run_Scheduler_Agent;

   ------------------------------------------------------------
   -- Run_The_Bloody_Scheduler_Agent_That_Wanted_To_Be_Woken --
   ------------------------------------------------------------

   --  Note that we check from the highest priority agent down
   --  as a higher priority agent may wake up in between the
   --  wake up alarm and the lower priority agent actually
   --  running

   procedure Run_The_Bloody_Scheduler_Agent_That_Wanted_To_Be_Woken
     (Scheduler         : in  Scheduler_Id;
      Current_Agent     : in  Oak_Agent_Id;
      Next_Agent_To_Run : out Oak_Agent_Id)
   is
   begin
      Run_Scheduler_Agent
        (Agent              => Agent,
         Reason             => (Message_Type => Selecting_Next_Agent),
         Next_Agent_To_Run  => Next_Agent_To_Run);

      --  This is a curious bit of code. Apparently the current agent will not
      --  be selected if it is a Waiting state or is a completed interrupt
      --  agent. Are these states even possible to get here????

      if Next_Task_To_Run = No_Agent
        and then State (Current_Agent) not in Waiting
        and then State (Current_Agent) /= Interrupt_Done
      then
         Next_Task_To_Run := Current_Agent;
      end if;
   end Run_The_Bloody_Scheduler_Agent_That_Wanted_To_Be_Woken;

end Oak.Scheduler;
