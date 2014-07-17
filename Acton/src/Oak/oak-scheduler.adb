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

with Oak.Agent.Kernel;     use Oak.Agent.Kernel;
with Oak.Agent.Oak_Agent;  use Oak.Agent.Oak_Agent;
with Oak.Agent.Schedulers; use Oak.Agent.Schedulers;
with Oak.Core;             use Oak.Core;
with Oak.Oak_Time;         use Oak.Oak_Time;
with Oak.States;           use Oak.States;
with Oak.Timers;           use Oak.Timers;

package body Oak.Scheduler is

   ----------------------------
   -- Add_Agent_To_Scheduler --
   ----------------------------

   procedure Add_Agent_To_Scheduler (Agent   : in Oak_Agent_Id;
                                     Place_At : in Queue_End := Back) is
   begin
      --  If a scheduler agent is schedulable, it must have its scheduler agent
      --  recorded.

      if Scheduler_Agent_For_Agent (Agent) = No_Agent then
         raise Program_Error;
      end if;

      Push_Scheduler_Op
        (Oak_Kernel => This_Oak_Kernel,
         Scheduler  => Scheduler_Agent_For_Agent (Agent),
         Operation  => (Message_Type => Adding_Agent,
                        Agent_To_Add => Agent,
                        Place_At     => Place_At));

      --  Initialise the the scheduler agent first before it is added to its
      --  scheduler agent ??? Do I really need to do this???

      if Agent in Scheduler_Id and then State (Agent) = Not_Initialised then
         Push_Scheduler_Op
           (Oak_Kernel => This_Oak_Kernel,
            Scheduler  => Agent,
            Operation  => (Message_Type => No_Message));
         Set_State (Agent, Sleeping);
      end if;
   end Add_Agent_To_Scheduler;

   -----------------------------
   -- Add_Agents_To_Scheduler --
   -----------------------------

   procedure Add_Agents_To_Scheduler (Agents : in Oak_Agent_Id) is
   begin
      Push_Scheduler_Op
        (Oak_Kernel => This_Oak_Kernel,
         Scheduler  => Scheduler_Agent_For_Agent (Agents),
         Operation  => (Message_Type  => Adding_Agents,
                        Agents_To_Add => Agents));
   end Add_Agents_To_Scheduler;

   --------------------------------------------------
   -- Check_Sechduler_Agents_For_Next_Agent_To_Run --
   --------------------------------------------------

   procedure Check_Sechduler_Agents_For_Next_Agent_To_Run
     (Next_Agent_To_Run : out Oak_Agent_Id;
      Top_Priority      : out Any_Priority)
   is
      Scheduler : Scheduler_Id_With_No :=
                    Top_Level_Schedulers (This_Oak_Kernel);
   begin
      Next_Agent_To_Run := No_Agent;

      --  Check top-level scheduler agents

      while Next_Agent_To_Run = No_Agent and then Scheduler /= No_Agent loop
         Next_Agent_To_Run := Agent_To_Run (Scheduler);
         Scheduler         := Next_Agent (Scheduler);
      end loop;

      Top_Priority := Normal_Priority (Next_Agent_To_Run);

      --  If Next_Agent is a scheduler agent, find what agent it wishes to
      --  run. Keep checking until a non scheduler agent is selected.

      while Next_Agent_To_Run in Scheduler_Id loop
         Next_Agent_To_Run := Agent_To_Run (Next_Agent_To_Run);
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

      Agent : Scheduler_Id_With_No := Top_Level_Schedulers (This_Oak_Kernel);

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
     (Changed_Agent : in Oak_Agent_Id) is
   begin
      Push_Scheduler_Op
        (Oak_Kernel => This_Oak_Kernel,
         Scheduler  => Scheduler_Agent_For_Agent (Changed_Agent),
         Operation  =>
           (Message_Type       => Agent_State_Change,
            Agent_That_Changed => Changed_Agent));
   end Inform_Scheduler_Agent_Has_Changed_State;

   -------------------------
   -- New_Scheduler_Cycle --
   -------------------------

   procedure New_Scheduler_Cycle (Scheduler : in  Scheduler_Id)
   is
      Replenish_At : constant Time :=
                       Time_Next_Cycle_Commences (Scheduler)
                       + Scheduler_Cycle_Period (Scheduler);
      RD           : constant Time_Span :=
                       Scheduler_Relative_Deadline (Scheduler);
      EB           : constant Time_Span :=
                       Scheduler_Execution_Budget (Scheduler);

   begin
      Remove_Agent_From_Charge_List
        (Oak_Kernel => This_Oak_Kernel,
         Agent      => Scheduler);

      Set_Next_Cycle_Start_Time
        (Scheduler  => Scheduler,
         Start_Time => Replenish_At);

      if RD < Time_Span_Last then
         Set_Absolute_Deadline (Scheduler, Replenish_At + RD);
      end if;

      Set_Remaining_Budget (Scheduler, To_Amount => EB);

      Increment_Execution_Cycle_Count (For_Agent => Scheduler, By => 1);

      if Replenish_At
        < Firing_Time (Timer_For_Scheduler_Agent (Scheduler))
      then
         Update_Timer
           (Timer    => Timer_For_Scheduler_Agent (Scheduler),
            New_Time => Replenish_At);
      end if;

      case State (Scheduler) is
         when Runnable =>
            Set_State (For_Agent => Scheduler, State => Allowance_Exhausted);
            Remove_Agent_From_Scheduler (Scheduler);
         when No_Agent_To_Run =>
            Set_State (For_Agent => Scheduler, State => Allowance_Exhausted);
         when others =>
            raise Program_Error;
      end case;

   end New_Scheduler_Cycle;

   ------------------------------
   -- Post_Run_Scheduler_Agent --
   ------------------------------

   procedure Post_Run_Scheduler_Agent
     (Agent   : in Scheduler_Id;
      Message : in Oak_Message)
   is
      SA           : constant Scheduler_Id_With_No := Agent;
      My_Kernel_Id : constant Kernel_Id            := This_Oak_Kernel;

   begin
      --  The Scheduler Agent has run successfully. Store the contents
      --  of its return message and set up its timer for its next run
      --  so it can serivce its queues.

      --  Store the scehduler agent's selected task inside the agent
      --  data stucture

      Set_Agent_To_Run (SA, Message.Next_Agent);

      --  Update the chosen agent's state to runnable (since the scheduler
      --  agent cannot do it itself). Note the sleep agent is alway runnable
      --  and is equivilent to No_Agent, hence no test needs to be done here.

      if State (Message.Next_Agent) = Sleeping then
         Set_State (Message.Next_Agent, Runnable);
      end if;

      --  The scheduler agent is placed back on the charge list
      --  if its when charge state is not Only_While_Running or
      --  if the scheduler agent does not want to be charged
      --  while it has the No_Agent selected.

      if When_To_Charge (SA) /= Only_While_Running
        and then Is_Scheduler_Active (SA)
        and then not (Message.Next_Agent = No_Agent
                      and then not Charge_While_No_Agent (SA))
      then
         Add_Agent_To_Charge_List
           (Oak_Kernel => My_Kernel_Id,
            Agent      => SA);
      end if;

      if State (SA) /= Allowance_Exhausted then
         --  Fix up the scheduler timer if it is being used to
         --  detect the passing of a scheduler agent's deadline.

         if Scheduler_Active_Till (SA) = Deadline and then
           Absolute_Deadline (SA) <= Message.Wake_Scheduler_At
         then
            Update_Timer
              (Timer    => Timer_For_Scheduler_Agent (SA),
               New_Time => Absolute_Deadline (SA));
         else
            Update_Timer
              (Timer    => Timer_For_Scheduler_Agent (SA),
               New_Time => Message.Wake_Scheduler_At);
         end if;
      else
         --  Special case when the scheduler agent's allowance is exhausted:
         --  the timer is set to no earilier than the start of the agent's next
         --  cycle.

         if Message.Wake_Scheduler_At > Time_Next_Cycle_Commences (SA) then
            --  Set_Wake_Time
            --    (For_Agent => SA,
            --     Wake_Time => Message.Wake_Scheduler_At);
            Update_Timer
              (Timer    => Timer_For_Scheduler_Agent (SA),
               New_Time => Message.Wake_Scheduler_At);
         else
            Update_Timer
              (Timer    => Timer_For_Scheduler_Agent (SA),
               New_Time => Time_Next_Cycle_Commences (SA));
         end if;
      end if;

      --  From here it depends on whether the scheduler agent was runnable or
      --  if it has been suspended since it has exhausted its allowance or has
      --  not agent to run.

      case State (SA) is
         when Runnable =>
            if Scheduler_Agent_For_Agent (SA) /= No_Agent
              and then Message.Next_Agent = No_Agent
              and then Interpret_No_Agent_As (SA) = As_Is
            then
               --  The scheduler agent is a nested agent and has nothing to
               --  dispatch. In this case it is removed its parent scheduler
               --  agent. The agent's timer is kept active so it can
               --  manage its queues and dispatch a task once one becomes
               --  ready.

               Set_State (SA, No_Agent_To_Run);
               Push_Scheduler_Op
                 (Oak_Kernel => My_Kernel_Id,
                  Scheduler  => Scheduler_Agent_For_Agent (SA),
                  Operation  => (Message_Type    => Removing_Agent,
                                 Agent_To_Remove => SA));
            end if;

         when Allowance_Exhausted =>

            --  If the next cycle time has passed, then the agent goes back
            --  onto it's respective runnable queue.

            if Time_Next_Cycle_Commences (SA) < Clock then
               Set_State (SA, Runnable);
               Push_Scheduler_Op
                 (Oak_Kernel => My_Kernel_Id,
                  Scheduler  => Scheduler_Agent_For_Agent (SA),
                  Operation  => (Message_Type => Adding_Agent,
                                 Agent_To_Add => SA,
                                 Place_At     => Back));
            end if;

         when No_Agent_To_Run =>
            if Message.Next_Agent /= No_Agent then
               --  Add the scheduler agent back to its scheduler agent since
               --  it now has an agent to dispatch

               Set_State (SA, Runnable);
               Push_Scheduler_Op
                 (Oak_Kernel => My_Kernel_Id,
                  Scheduler  => Scheduler_Agent_For_Agent (SA),
                  Operation  => (Message_Type => Adding_Agent,
                                 Agent_To_Add => SA,
                                 Place_At     => Back));
            end if;

         when others =>
            --  ??? Should not get here.
            pragma Assert (False);
      end case;
   end Post_Run_Scheduler_Agent;

   ---------------------------------
   -- Remove_Agent_From_Scheduler --
   ---------------------------------

   procedure Remove_Agent_From_Scheduler (Agent : in Oak_Agent_Id) is
   begin
      Push_Scheduler_Op
        (Oak_Kernel => This_Oak_Kernel,
         Scheduler  => Scheduler_Agent_For_Agent (Agent),
         Operation  => (Message_Type    => Removing_Agent,
                        Agent_To_Remove => Agent));
   end Remove_Agent_From_Scheduler;

   -----------------------------------
   -- Service_Scheduler_Agent_Timer --
   -----------------------------------

   procedure Service_Scheduler_Agent_Timer
     (Scheduler : in Scheduler_Id) is
   begin
      --  In the first instance the timer is being used to signal that the
      --  scheduler agent wants to run.

      if Scheduler_Active_Till (Scheduler) = Always_Active
        or else (Scheduler_Active_Till (Scheduler) = Deadline and then
                 Absolute_Deadline (Scheduler) > Clock)
        or else (Scheduler_Active_Till (Scheduler) = Budget_Exhaustion and then
                 Remaining_Budget (Scheduler) > Time_Span_Zero)
      then
         Push_Scheduler_Op
           (Oak_Kernel => This_Oak_Kernel,
            Scheduler  => Scheduler,
            Operation  => (Message_Type => Selecting_Next_Agent));

      else
         --  A new scheduler cycle has started
         New_Scheduler_Cycle (Scheduler);
      end if;

   end Service_Scheduler_Agent_Timer;

end Oak.Scheduler;
