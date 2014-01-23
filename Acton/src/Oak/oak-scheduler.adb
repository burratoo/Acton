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
     (From_Scheduler_Agent : in  Scheduler_Id;
      Next_Agent_To_Run    : out Oak_Agent_Id)
   is
      Agent : Scheduler_Id_With_No := From_Scheduler_Agent;
   begin
      Next_Agent_To_Run := No_Agent;

      --  Check top-level scheduler agents

      while Next_Agent_To_Run = No_Agent and then Agent /= No_Agent loop
         Next_Agent_To_Run := Agent_To_Run (Agent);
         Agent             := Next_Agent (Agent);
      end loop;

      --  If a top-level scheduler agent returns another scheduler agent, we
      --  need to keep searching until we find a non-scheduler agent.

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
           (From_Scheduler_Agent => Next_Agent (Agent),
            Next_Agent_To_Run    => Next_Agent_To_Run);
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
      Set_Wake_Time (Scheduler, New_Wake_Time);
      Set_Next_Cycle_Start_Time
        (Scheduler  => Scheduler,
         Start_Time => New_Wake_Time + Scheduler_Cycle_Period (Scheduler));

      if RD < Time_Span_Last then
         Set_Absolute_Deadline (Scheduler, New_Wake_Time + RD);
      end if;

      Set_Remaining_Budget (Scheduler, To_Amount => EB);

      Increment_Execution_Cycle_Count (For_Agent => Scheduler, By => 1);

      Set_State (For_Agent => Scheduler, State => Sleeping);
      Inform_Scheduler_Agent_Has_Changed_State
        (Changed_Agent     => Scheduler,
         Next_Agent_To_Run => Next_Agent_To_Run);
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

                     --  How the timer is updated depends how long the
                     --  the scheduler is active for.

                     case Scheduler_Active_Till (SA) is
                        when Always_Active =>
                           Update_Timer
                             (Timer    => Timer_For_Scheduler_Agent (SA),
                              New_Time => Message.Wake_Scheduler_At);

                        when Deadline =>
                           if Message.Wake_Scheduler_At <
                             Absolute_Deadline (SA) then
                              Update_Timer
                                (Timer    => Timer_For_Scheduler_Agent (SA),
                                 New_Time => Message.Wake_Scheduler_At);
                              Set_Timer_Scheduler_Action
                                (Timer            =>
                                   Timer_For_Scheduler_Agent (SA),
                                 Scheduler_Action => Service);
                           else
                              Update_Timer
                                (Timer    => Timer_For_Scheduler_Agent (SA),
                                 New_Time => Absolute_Deadline (SA));
                              Set_Timer_Scheduler_Action
                                (Timer            =>
                                   Timer_For_Scheduler_Agent (SA),
                                 Scheduler_Action => End_Cycle);
                           end if;

                        when Budget_Exhaustion =>
                           --  Budget expiration is handled in Oak.Core, using
                           --  the same mechanism as for task agents.

                           Update_Timer
                             (Timer    => Timer_For_Scheduler_Agent (SA),
                              New_Time => Message.Wake_Scheduler_At);
                     end case;

                     --  The scheduler agent is only kept on the charge list
                     --  if its when charge state is not Only_While_Running or
                     --  if the scheduler agent does not want to be charged
                     --  while it has the No_Agent selected.

                     if When_To_Charge (SA) /= Only_While_Running
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

                     if Message.Next_Agent in Scheduler_Id then

                        --  Check the previous state of the scheduler agent
                        --  before it was selected.

                        if Agent_Prior_State = Sleeping then
                           --  The scheduler agent was sleeping, but is now
                           --  runnable. Need to activate its timer and to
                           --  make things easier service it now if it needs
                           --  servicing.

                           --  Activate Timer

                           Activate_Timer (Timer_For_Scheduler_Agent (SA));

                           --  Check the scheduler agent's timer to see if
                           --  it needs servicing

                           if Firing_Time (Timer_For_Scheduler_Agent (SA)) <
                             Clock
                           then
                              --  Has timer that needs servicing

                              SA := Message.Next_Agent;
                              Message :=
                                (Message_Type => Selecting_Next_Agent);
                           else

                              --  Otherwise there is nothing to do

                              SA := No_Agent;
                           end if;

                        else
                           SA := No_Agent;
                        end if;

                        --  If there is no agent to run and the scheduler agent
                        --  is set to treat No_Agent as a No_Agent (and not a
                        --  sleep agent), then sleep the agent, allowing
                        --  another task to run.

                        if SA = No_Agent
                          and then Agent_To_Run (SA) = No_Agent
                          and then Interpret_No_Agent_As (SA) = As_Is
                        then
                           Set_State (Message.Next_Agent, Sleeping);
                           Message :=
                             (Message_Type => Agent_State_Change,
                              Agent_That_Changed => Message.Next_Agent);
                           SA := Scheduler_Agent_For_Agent (SA);
                        end if;
                     else
                        --  The selected agent is not a scheduler agent, so we
                        --  can exit now.

                        SA := No_Agent;

                     end if;

                  when Sleeping =>
                     --  The scheduler agent was in a sleeping state when it
                     --  was run.

                     --  Wake time for the sleeping agent remains unaffected by
                     --  the value return by the agent. The timer is updated,
                     --  but remains inactive. This allows the kernel not to
                     --  have to run the scheduler agent when it wakes up if
                     --  nothing has changed.

                     Update_Timer
                       (Timer    => Timer_For_Scheduler_Agent (SA),
                        New_Time => Message.Wake_Scheduler_At);

                     --  The scheduler agent is removed from the charge list.

                     Remove_Agent_From_Charge_List
                       (Oak_Kernel => My_Kernel_Id, Agent => SA);

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
      Next_Agent_To_Run := Agent_To_Run (Agent);

      --  Check to make sure selected agent is not another scheduler agent,
      --  and check that one if it is the case.

      while Next_Agent_To_Run in Scheduler_Id
      loop
         Next_Agent_To_Run := Agent_To_Run (Next_Agent_To_Run);
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
        (Agent              => Scheduler,
         Reason             => (Message_Type => Selecting_Next_Agent),
         Next_Agent_To_Run  => Next_Agent_To_Run);

      --  This is a curious bit of code. Apparently the current agent will not
      --  be selected if it is a Waiting state or is a completed interrupt
      --  agent. Are these states even possible to get here????

      if State (Current_Agent) not in Waiting
        and then State (Current_Agent) /= Interrupt_Done
      then
         Next_Agent_To_Run := Current_Agent;
      end if;
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
