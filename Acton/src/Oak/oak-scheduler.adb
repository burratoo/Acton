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

   procedure Add_Agent_To_Scheduler (Agent : in Oak_Agent_Id) is
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
                        Agent_To_Add => Agent));

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
      New_Wake_Time : constant Time := Time_Next_Cycle_Commences (Scheduler);
      RD            : constant Time_Span :=
                        Scheduler_Relative_Deadline (Scheduler);
      EB            : constant Time_Span :=
                        Scheduler_Execution_Budget (Scheduler);

   begin
      Deactivate_Timer (Timer_For_Scheduler_Agent (Scheduler));
      Remove_Agent_From_Charge_List
        (Oak_Kernel => This_Oak_Kernel,
         Agent      => Scheduler);
      Set_Is_Scheduler_Active (Scheduler, Active => False);

      Set_Wake_Time (Scheduler, New_Wake_Time);
      Set_Next_Cycle_Start_Time
        (Scheduler  => Scheduler,
         Start_Time => New_Wake_Time + Scheduler_Cycle_Period (Scheduler));

      if RD < Time_Span_Last then
         Set_Absolute_Deadline (Scheduler, New_Wake_Time + RD);
      end if;

      Set_Remaining_Budget (Scheduler, To_Amount => EB);

      Increment_Execution_Cycle_Count (For_Agent => Scheduler, By => 1);

      case State (Scheduler) is
         when Runnable =>
            Set_State (For_Agent => Scheduler, State => Sleeping);
            Inform_Scheduler_Agent_Has_Changed_State
              (Changed_Agent => Scheduler);
         when Sleeping =>
               Push_Scheduler_Op
                 (Oak_Kernel => This_Oak_Kernel,
                  Scheduler  => Scheduler_Agent_For_Agent (Scheduler),
                  Operation  => (Message_Type  => Wake_Agent,
                                 Agent_To_Wake => Scheduler));
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
      SA                : constant Scheduler_Id_With_No := Agent;
      My_Kernel_Id      : constant Kernel_Id            := This_Oak_Kernel;
      Agent_Prior_State : Agent_State;

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

      Agent_Prior_State := State (Message.Next_Agent);
      Set_State (Message.Next_Agent, Runnable);

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

      --  From here it depends on whether the scheduler agent was
      --  sleeping or runnable.

      case State (SA) is
         when Runnable =>
            --  The scheduler agent is in a runnable state. Wake_Time
            --  is used to set the scheduler timer to enable the agent
            --  to manage its queues when needed.

            Set_Wake_Time
              (For_Agent => SA,
               Wake_Time => Message.Wake_Scheduler_At);

            Update_Timer
              (Timer    => Timer_For_Scheduler_Agent (SA),
               New_Time => Message.Wake_Scheduler_At);

            --  Fix up the scheduler timer if it is being used to
            --  detect the passing of a scheduler agent's deadline.

            if Scheduler_Active_Till (SA) = Deadline and then
              Absolute_Deadline (SA) <= Message.Wake_Scheduler_At
            then
               Update_Timer
                 (Timer    => Timer_For_Scheduler_Agent (SA),
                  New_Time => Message.Wake_Scheduler_At);
            end if;

            --  Handle the case where the selected agent is itself
            --  another scheduler agent (i.e. a nested scheduler
            --  agent).

            --  First up handle a freshly woken child scheduler.

            if Message.Next_Agent in Scheduler_Id
              and then Agent_Prior_State = Sleeping
            then
               --  The child scheduler agent was sleeping, but is now
               --  runnable. Need to activate its timer and to make
               --  things easier service it now if it needs servicing.
               --  The scheduler's timer is used to determine if it
               --  needs servicing.

               Set_Is_Scheduler_Active (Message.Next_Agent, True);

               --  Activate Timer - timer can already be in activated

               Activate_Timer (Timer_For_Scheduler_Agent (Message.Next_Agent));

               --  Service the scheduler agent

               Push_Scheduler_Op
                 (Oak_Kernel => My_Kernel_Id,
                  Scheduler  => Message.Next_Agent,
                  Operation  => (Message_Type => Selecting_Next_Agent));

               --  Deal with No_Agent. For top level schedulers there is
               --  nothing to be done since Oak will move on to the next
               --  scheduler agent in the top level table. For child
               --  scheduler agents however, it depends on the
               --  Interpret_No_Agent flag. If it is treated as No_Agent
               --  the scheduler agent goes to sleep, otherwise the
               --  Sleep_Agent is deployed in its place. This allows for
               --  execution servers that either allow lower priority
               --  tasks to run when it has nothing to run or prevent
               --  these tasks instead.

            elsif Scheduler_Agent_For_Agent (SA) /= No_Agent
              and then Agent_To_Run (SA) = No_Agent
              and then Interpret_No_Agent_As (SA) = As_Is
            then

               --  The timer only remains active at this point only
               --  if it is being used to end the current cycle of
               --  the scheduler agent. The wake time of the scheduler
               --  is sufficent to allow the scheduler agent to wake
               --  when needed (infact, the timer may interfer with
               --  its operation).

               if Firing_Time (Timer_For_Scheduler_Agent (SA))
                 = Wake_Time (SA)
               then
                  Deactivate_Timer (Timer_For_Scheduler_Agent (SA));
               end if;

               Set_State (SA, Sleeping);
               Push_Scheduler_Op
                 (Oak_Kernel => My_Kernel_Id,
                  Scheduler  => Scheduler_Agent_For_Agent (SA),
                  Operation  => (Message_Type       => Agent_State_Change,
                                 Agent_That_Changed => SA));
            end if;

         when Sleeping =>
            --  The scheduler agent was in a sleeping state when it
            --  was run.

            --  A sleeping scheduler agent can be in two states based
            --  on whether the it is active or not. If active, the
            --  scheduler agent will wake up and be moved back on to
            --  a runnable queue if it now has a task to run.
            --  Otherwise the scheduler agent remains asleep and
            --  updates its inactive timer. Allowing the scheduler
            --  agent to run while sleep enables the kernel to add
            --  and remove tasks at ease.

            if Is_Scheduler_Active (SA)
              and then Message.Next_Agent /= No_Agent
            then
               Set_State (For_Agent => SA, State => Sleeping);
               Set_Wake_Time
                 (For_Agent => SA,
                  Wake_Time => Message.Wake_Scheduler_At);
               Push_Scheduler_Op
                 (Oak_Kernel => My_Kernel_Id,
                  Scheduler  => Scheduler_Agent_For_Agent (SA),
                  Operation  => (Message_Type  => Wake_Agent,
                                 Agent_To_Wake => SA));

            else
               Update_Timer
                 (Timer    => Timer_For_Scheduler_Agent (SA),
                  New_Time => Message.Wake_Scheduler_At);
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
