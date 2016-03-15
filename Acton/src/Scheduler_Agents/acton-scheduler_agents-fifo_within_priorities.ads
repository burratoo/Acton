------------------------------------------------------------------------------
--                                                                          --
--                           ACTON SCHEDULER AGENT                          --
--                                                                          --
--               ACTON.SCHEDULER_AGENTS.FIFO_WITHIN_PRIORITIES              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                 Copyright (C) 2010-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

pragma Restrictions (No_Elaboration_Code);

with Oak.Agent;           use Oak.Agent;
with Oak.Agent.Oak_Agent; use Oak.Agent.Oak_Agent;
with System;              use System;

with Oak.Storage.Slim_Priority_Queue;
with Oak.Storage.Slim_Time_Priority_Queue;

package Acton.Scheduler_Agents.FIFO_Within_Priorities with Preelaborate is

   procedure New_Scheduler_Agent
     (Agent        : out Scheduler_Id;
      Min_Priority : in  Any_Priority;
      Max_Priority : in  Any_Priority);

private

   subtype Schedulable_Agents is
     Oak_Agent_Id range Sleep_Agent .. Task_Id'Last;

   function Priority_Greater_Than  (Left, Right : in Oak_Agent_Id)
                                    return Boolean
     with Inline_Always;

   function Priority_Greater_Than_Equal  (Left, Right : in Oak_Agent_Id)
                                          return Boolean
     with Inline_Always;

   function Wake_Less_Than (Left, Right : in Oak_Agent_Id) return Boolean
     with Inline;

   package Priority_Queue is new Oak.Storage.Slim_Priority_Queue
     (Item_Type     => Schedulable_Agents,
      No_Item       => No_Agent,
      ">"           => Priority_Greater_Than,
      ">="          => Priority_Greater_Than_Equal);

   use Priority_Queue;

   package Time_Queue is new Oak.Storage.Slim_Time_Priority_Queue
     (Item_Type     => Schedulable_Agents,
      Priority_Type => Any_Priority,
      No_Item       => No_Agent,
      "<"           => Wake_Less_Than,
      Priority      => Normal_Priority);

   use Time_Queue;

   Stack_Size : constant := 32 * Schedulable_Agents'Range_Length + 512;
   --   Agent_Name : constant String := "Fixed_Priority_Scheduler";
   Agent_Name : constant String := "";

end Acton.Scheduler_Agents.FIFO_Within_Priorities;
