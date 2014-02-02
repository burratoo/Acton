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
with Oak.Core_Support_Package.Task_Support;
use  Oak.Core_Support_Package.Task_Support;

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

      if Agent in Scheduler_Id and then State (Agent) = Not_Initialised then
         declare
            Message : Oak_Message := (Message_Type => No_Message);
         begin
            Switch_To_Scheduler_Agent
              (Scheduler_Agent => Agent,
               Message         => Message);
         end;
         Set_State (Agent, Sleeping);
      end if;

      --  TODO: Add a check to see if the scheduler agent has been initialised
      --  or not.

      Run_Scheduler_Agent
        (Agent  => Scheduler_Agent_For_Agent (Agent),
         Reason => (Message_Type  => Adding_Agent,
                    Agent_To_Add  => Agent));
   end Add_Agent_To_Scheduler;

   --------------------------------------------------
   -- Check_Sechduler_Agents_For_Next_Agent_To_Run --
   --------------------------------------------------

   procedure Check_Sechduler_Agents_For_Next_Agent_To_Run
     (Next_Agent_To_Run : out Oak_Agent_Id)
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
     (Changed_Agent     : in  Oak_Agent_Id;
      Next_Agent_To_Run : out Oak_Agent_Id)
   is
      Agent : constant Scheduler_Id :=
                Scheduler_Agent_For_Agent (Changed_Agent);
   begin
      Run_Scheduler_Agent
        (Agent             => Agent,
         Reason            =>
           (Message_Type       => Agent_State_Change,
            Agent_That_Changed => Changed_Agent),
         Next_Agent_To_Run => Next_Agent_To_Run);

      --  If the scheduler agent has no agents to run, check with scheduler
      --  agents responsible for lower priorities.

      if Next_Agent_To_Run = No_Agent then
         Check_Sechduler_Agents_For_Next_Agent_To_Run
           (Next_Agent_To_Run => Next_Agent_To_Run);
      end if;
   end Inform_Scheduler_Agent_Has_Changed_State;

   -------------------------
   -- New_Scheduler_Cycle --
   -------------------------

   procedure New_Scheduler_Cycle
     (Scheduler         : in  Scheduler_Id;
      Next_Agent_To_Run : out Oak_Agent_Id)
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
      Set_Timer_Scheduler_Action
        (Timer            => Timer_For_Scheduler_Agent (Scheduler),
         Scheduler        => Scheduler,
         Scheduler_Action => Service);

      if RD < Time_Span_Last then
         Set_Absolute_Deadline (Scheduler, New_Wake_Time + RD);
      end if;

      Set_Remaining_Budget (Scheduler, To_Amount => EB);

      Increment_Execution_Cycle_Count (For_Agent => Scheduler, By => 1);

      case State (Scheduler) is
         when Runnable =>
            Set_State (For_Agent => Scheduler, State => Sleeping);
            Inform_Scheduler_Agent_Has_Changed_State
              (Changed_Agent     => Scheduler,
               Next_Agent_To_Run => Next_Agent_To_Run);
         when Sleeping =>
            declare
               Message : constant Oak_Message :=
                           (Message_Type  => Wake_Agent,
                            Agent_To_Wake => Scheduler);
            begin
               Run_Scheduler_Agent
                 (Agent             => Scheduler,
                  Reason            => Message,
                  Next_Agent_To_Run => Next_Agent_To_Run);
            end;

         when others =>
            raise Program_Error;
      end case;

   end New_Scheduler_Cycle;

   ---------------------------------
   -- Remove_Agent_From_Scheduler --
   ---------------------------------

   procedure Remove_Agent_From_Scheduler (Agent : in Oak_Agent_Id) is
   begin
      Run_Scheduler_Agent
        (Agent  => Scheduler_Agent_For_Agent (Agent),
         Reason => (Message_Type    => Removing_Agent,
                    Agent_To_Remove => Agent));
   end Remove_Agent_From_Scheduler;

   -------------------------
   -- Run_Scheduler_Agent --
   -------------------------

   procedure Run_Scheduler_Agent
     (Agent  : in Scheduler_Id;
      Reason : in Oak_Message)
   is
      SA                : Scheduler_Id_With_No := Agent;
      Message           : Oak_Message          := Reason;
      My_Kernel_Id      : constant Kernel_Id   := This_Oak_Kernel;
      Agent_Prior_State : Agent_State;

   begin
      --  Use a loop rather than recursion to limit and protected the stack.
      --  The recursion version of this code suffered from the risk that two
      --  agents could ping-pong between each other indefinitely, leading to
      --  the stack overflowing. By using a loop the system simply enters an
      --  inifinate loop.

      while SA /= No_Agent loop
         --  Easier to remove and add agent than to check if agent is present
         --  in charge list.

         Remove_Agent_From_Charge_List (My_Kernel_Id, SA);
         Add_Agent_To_Charge_List (My_Kernel_Id, SA);

         --  Run Scheduler Agent.

         Switch_To_Scheduler_Agent (SA, Message);

         --  React to the scheduler agent's response.

         case Message.Message_Type is
            when Scheduler_Agent_Done =>
               --  The Scheduler Agent has run successfully. Store the contents
               --  of its return message and set up its timer for its next run
               --  so it can serivce its queues.

               --  Store the scehduler agent's selected task inside the agent
               --  data stucture

               Set_Agent_To_Run (SA, Message.Next_Agent);

               --  Update the agent's state to runnable (since the scheduler
               --  agent cannot do it itself). Note the sleep agent is alway
               --  runnable and is equivilent to No_Agent, hence no test needs
               --  to be done here.

               Agent_Prior_State := State (Message.Next_Agent);
               Set_State (Message.Next_Agent, Runnable);

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

                     if Scheduler_Active_Till (SA) = Deadline then
                        if Message.Wake_Scheduler_At <
                          Absolute_Deadline (SA) then
                           Update_Timer
                             (Timer    => Timer_For_Scheduler_Agent (SA),
                              New_Time => Message.Wake_Scheduler_At);
                           Set_Timer_Scheduler_Action
                             (Timer            =>
                                Timer_For_Scheduler_Agent (SA),
                              Scheduler        => SA,
                              Scheduler_Action => Service);
                        else
                           Update_Timer
                             (Timer    => Timer_For_Scheduler_Agent (SA),
                              New_Time => Absolute_Deadline (SA));
                           Set_Timer_Scheduler_Action
                             (Timer            =>
                                Timer_For_Scheduler_Agent (SA),
                              Scheduler        => SA,
                              Scheduler_Action => End_Cycle);
                        end if;
                     end if;

                     --  The scheduler agent is only kept on the charge list
                     --  if its when charge state is not Only_While_Running or
                     --  if the scheduler agent does not want to be charged
                     --  while it has the No_Agent selected.

                     if When_To_Charge (SA) = Only_While_Running
                       or else (Message.Next_Agent = No_Agent
                                and then not Charge_While_No_Agent (SA))
                     then
                        Remove_Agent_From_Charge_List
                          (Oak_Kernel => My_Kernel_Id,
                           Agent      => SA);
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

                        --  At this point the child scheduler agent becomes
                        --  the scheduler agent of interest.

                        SA := Message.Next_Agent;

                        --  Signal that the scheduler is active

                        Set_Is_Scheduler_Active (SA, True);

                        --  Activate Timer - timer can already be in activated

                        Activate_Timer (Timer_For_Scheduler_Agent (SA));

                        --  Service the scheduler agent

                        Message :=
                          (Message_Type => Selecting_Next_Agent);

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

                        --  If the selected agent is a nested scheduler agent
                        --  and it has selected No_Agent we need to check to
                        --  see how how the No_Agent is handled, either as a
                        --   No_Agent or as a sleep agent.

                        Set_State (SA, Sleeping);
                        Message :=
                          (Message_Type       => Agent_State_Change,
                           Agent_That_Changed => SA);

                        --  The timer only remains active at this point only
                        --  if it is being used to end the current cycle of
                        --  the scheduler agent. The wake time of the scheduler
                        --  is sufficent to allow the scheduler agent to wake
                        --  when needed (infact, the timer may interfer with
                        --  its operation).

                        if Scheduler_Action (Timer_For_Scheduler_Agent (SA))
                          /= End_Cycle then
                           Deactivate_Timer (Timer_For_Scheduler_Agent (SA));
                        end if;

                        SA := Scheduler_Agent_For_Agent (SA);

                     else
                        SA := No_Agent;
                     end if;

                  when Sleeping =>
                     --  The scheduler agent was in a sleeping state when it
                     --  was run.

                     --  The scheduler agent is only kept on the charge list
                     --  if its when charge state is not Only_While_Running or
                     --  if the scheduler agent does not want to be charged
                     --  while it has the No_Agent selected.

                     if When_To_Charge (SA) = Only_While_Running
                       or else not Is_Scheduler_Active (SA)
                       or else (Message.Next_Agent = No_Agent
                                and then not Charge_While_No_Agent (SA))
                     then
                        Remove_Agent_From_Charge_List
                          (Oak_Kernel => My_Kernel_Id,
                           Agent      => SA);
                     end if;

                     --  A sleeping scheduler agent can be in two states based
                     --  on whether the it is active or not. If active, the
                     --  scheduler agent will wake up and be moved back on to
                     --  a runnable queue if it now has a task to run.
                     --  Otherwise the scheduler agent remains asleep and
                     --  updates its inactive timer. Allowing the scheduler
                     --  agent to run while sleep enables the kernel to add
                     --  and remove tasks at ease.

                     if Is_Scheduler_Active (SA) then
                        Set_State (For_Agent => SA, State => Sleeping);
                        Set_Wake_Time
                          (For_Agent => SA,
                           Wake_Time => Message.Wake_Scheduler_At);
                        Message :=
                          (Message_Type  => Wake_Agent,
                           Agent_To_Wake => SA);
                        SA := Scheduler_Agent_For_Agent (SA);
                     else
                        Update_Timer
                          (Timer    => Timer_For_Scheduler_Agent (SA),
                           New_Time => Message.Wake_Scheduler_At);
                        SA := No_Agent;
                     end if;

                  when others =>
                     --  ??? Should not get here.
                     pragma Assert (False);
               end case;

            when others => pragma Assert (False);
         end case;
      end loop;
   end Run_Scheduler_Agent;

   procedure Run_Scheduler_Agent
     (Agent             : in  Scheduler_Id;
      Reason            : in  Oak_Message;
      Next_Agent_To_Run : out Oak_Agent_Id)
   is
   begin
      Run_Scheduler_Agent (Agent => Agent, Reason => Reason);
      Check_Sechduler_Agents_For_Next_Agent_To_Run
        (Next_Agent_To_Run => Next_Agent_To_Run);
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
      pragma Unreferenced (Current_Agent);
   begin
      Run_Scheduler_Agent
        (Agent              => Scheduler,
         Reason             => (Message_Type => Selecting_Next_Agent),
         Next_Agent_To_Run  => Next_Agent_To_Run);
   end Run_The_Bloody_Scheduler_Agent_That_Wanted_To_Be_Woken;

   -------------------------------
   -- Switch_To_Scheduler_Agent --
   -------------------------------

   procedure Switch_To_Scheduler_Agent
     (Scheduler_Agent : in     Scheduler_Id;
      Message         : in out Oak_Message)
   is
      My_Kernel_Id : constant Kernel_Id := This_Oak_Kernel;
   begin
      Set_Current_Agent
        (Oak_Kernel => My_Kernel_Id, Agent => Scheduler_Agent);
      Update_Exit_Stats (Oak_Kernel => My_Kernel_Id);
      Context_Switch_Will_Be_To_Agent;
      Request_Agent_Service (Message);
      Update_Entry_Stats (Oak_Kernel => My_Kernel_Id);
   end Switch_To_Scheduler_Agent;

end Oak.Scheduler;
