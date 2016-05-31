--                                                                          --
--                              OAK COMPONENTS                              --
--                                                                          --
--                          OAK.AGENT.TASKS.CYCLIC                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                 Copyright (C) 2013-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

with Oak.Core;

with Ada.Cyclic_Tasks; use Ada.Cyclic_Tasks;

with Oak.Agent.Oak_Agent; use Oak.Agent.Oak_Agent;
with Oak.Scheduler;       use Oak.Scheduler;
with Oak.States;          use Oak.States;
with Oak.Timers;          use Oak.Timers;

package body Oak.Agent.Tasks.Cyclic is

   ----------------
   -- Body Types --
   ----------------

   subtype Event_Based is Behaviour range Aperiodic .. Sporadic;
   subtype Time_Based  is Behaviour range Sporadic  .. Periodic;
   --  These two subtypes categorise the behaviour of the different cyclic
   --  kinds.

   ---------------
   -- New_Cycle --
   ---------------

   procedure New_Cycle (For_Task : in  Task_Id)
   is
      T : Task_Agent_Record renames Agent_Pool (For_Task);
   begin

      --  Update execution statistics

      Increment_Execution_Cycle_Count (For_Agent => For_Task, By => 1);
      Set_Remaining_Budget (For_Task, T.Execution_Budget);

      --  The task at this points sleeps. The wake time of the task will be
      --  used to move the task into a runnable state, even if the task may be
      --  runnable now (for instance for a normal task or a aperiodic task that
      --  has a been release).
      --
      --  The reason for this approach is that it allows aperiodic and sporadic
      --  tasks to use the same code path, with the difference lying in the
      --  wake time chosen for each task (which is the only real fundamental
      --  difference between the two cyclic behaviour).

      Set_State (For_Task, Sleeping);

      --  Determine the the task's wake time.

      case T.Cycle_Behaviour is
         when Time_Based =>
            --  Periodic and Sporadic tasks have to wait until a specific time
            --  determined by the Next_Run_Cycle task component before they
            --  can commence their next cycle.

            Set_Wake_Time (For_Task, Wake_Time => T.Next_Run_Cycle);

            --  Determine the subsquent run time from the new wake time.

            T.Next_Run_Cycle := T.Next_Run_Cycle + T.Cycle_Period;

            --  Skip cycles if their times have passed. Used mainly when
            --  periodic tasks may be hidden in execution server. Probably can
            --  be removed later since people are not really going to place
            --  periodic tasks within execution servers.

            while Wake_Time (For_Task) < Clock loop
               Set_Wake_Time (For_Task, T.Next_Run_Cycle);
               T.Next_Run_Cycle := T.Next_Run_Cycle + T.Cycle_Period;
            end loop;

         when Aperiodic | Normal =>
            --  Aperiodics and Normal tasks can run straight away. It is
            --  achieved by setting their wake time to now.

            Set_Wake_Time (For_Task, Wake_Time => Clock);
      end case;

      --  Move tasks to their appropriate places, update their deadlines and
      --  notify their scheduler agents on what has happen to them.

      case T.Cycle_Behaviour is
         when Periodic | Normal =>
            --  At this point a periodic task has been placed into a sleeping
            --  state with a wake up time set to the start of its next cycle.
            --  All that remains is to calcuate the task's next deadline
            --  (which can be determined now since we know the time the next
            --  cycle is due to start).

            --  A normal task is released straight away. Note that placing it
            --  into a sleep state above should mean the scheduler agent should
            --  place the task at the end of its runnable queue.

            Set_Next_Deadline_For_Task (For_Task, Using => Wake_Up_Time);

            --  Notify the task's scheduler agent that the state of the task
            --  has changed.

            Inform_Scheduler_Agent_Has_Changed_State
              (Changed_Agent => For_Task);

         when Event_Based =>
            --  The handling of Aperiodic and Sporadic tasks depends on if an
            --  event has been raised or not.

            case T.Event_Raised is
               when True =>
                  --  If an event has been raised release the task. Releasing
                  --  the task entails that its deadline and next run time is
                  --  is calculated. The difference between aperiodic and
                  --  sporadic tasks at this point is what their wake time is
                  --  set to: an aperiodic task can run straight away, while
                  --  a sporadic task will have to wait for some future time
                  --  that was calculated above.

                  --  TODO: check to see what happens for aperiodic tasks
                  --  when the next run cycle is attempted to be calculated.

                  T.Event_Raised := False;
                  Set_Next_Deadline_For_Task
                    (For_Task, Using => Wake_Up_Time);
                  T.Next_Run_Cycle :=
                    Wake_Time (For_Task) + T.Cycle_Period;

                  Inform_Scheduler_Agent_Has_Changed_State
                    (Changed_Agent => For_Task);

               when False =>
                  --  While a aperiodic or sporadic task is waiting for an
                  --  event, it is removed from its scheduler agent. Its state
                  --  is set to Waiting_For_Event.

                  Set_State (For_Task, Waiting_For_Event);
                  Remove_Agent_From_Scheduler (For_Task);
                  Deactivate_Timer (T.Deadline_Timer);
            end case;
      end case;
   end New_Cycle;

   ------------------
   -- Release_Task --
   ------------------

   procedure Release_Task
     (Task_To_Release : in Task_Id;
      Releasing_Agent : in Oak_Agent_Id)
   is
      T : Task_Agent_Record renames Agent_Pool (Task_To_Release);
      C : Oak_Time.Time;

   begin
      Set_State (For_Agent => Releasing_Agent, State => Runnable);

      if State (Task_To_Release) = Waiting_For_Event then

         --  As with New_Cycle above, tasks are placed into a sleeping state
         --  to let the wake time of the task determine what state the task
         --  end up in.

         Set_State (For_Agent => Task_To_Release, State => Sleeping);

         --  Set the deadline for the task. To do so, need to determine first
         --  if the task is eligble to run now, or if it is a sporadic task
         --  that still has to wait for its inter-release time to expire.

         --  Store the clock so it only needs to be fetched once.

         C := Clock;

         if T.Cycle_Behaviour /= Sporadic or else
           Wake_Time (Task_To_Release) < C
         then
            --  The task is eligble to run now. The clock is stored as the
            --  task's wake time so we do not have the overhead of fetching the
            --  clock again when setting the deadline for the task.
            Set_Wake_Time (For_Agent => Task_To_Release, Wake_Time => C);
         end if;

         --  Finally update deadline and the next run cycle (only for sporadic
         --  tasks).

         Set_Next_Deadline_For_Task (Task_To_Release, Using => Wake_Up_Time);

         T.Next_Run_Cycle := Wake_Time (Task_To_Release) + T.Cycle_Period;

         --  Return the task to its scheduler agent and figure out who gets to
         --  run next.

         Add_Agent_To_Scheduler (Task_To_Release);
      else
         --  Make a note that the event have been raised in the target task.
         T.Event_Raised := True;
      end if;

   end Release_Task;

   --------------------------
   -- Setup_Cyclic_Section --
   --------------------------

   --  Task state on entry: Setup_Cycle.
   --  Task selected on exit: T.

   procedure Setup_Cyclic_Section (For_Task : in Task_Id)
   is
      T : Task_Agent_Record renames Agent_Pool (For_Task);
   begin
      Set_State (For_Agent => For_Task, State => Runnable);

      T.Next_Run_Cycle := Core.Global_Start_Time + T.Phase;
      T.Event_Raised   := False;

      Deactivate_Timer (T.Deadline_Timer);
   end Setup_Cyclic_Section;

end Oak.Agent.Tasks.Cyclic;
