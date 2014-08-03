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

with Oak.Agent;           use Oak.Agent;
with System;              use System;

with Oak.Project_Support_Package; use Oak.Project_Support_Package;

with Oak.Storage.Slim_Priority_Queue;
with Oak.Storage.Binary_Heap;

package Acton.Scheduler_Agents.FIFO_Within_Priorities_Basic_Sleep
  with Preelaborate is

   procedure New_Scheduler_Agent
     (Agent        : out Scheduler_Id;
      Min_Priority : in  Any_Priority;
      Max_Priority : in  Any_Priority);

   Stack_Size : constant := 1 * 1024;
   Agent_Name : constant String := "Fixed_Priority_Scheduler";

private

   Scheduler_Error : exception;

   function Priority_Greater_Than  (Left, Right : in Oak_Agent_Id)
                                    return Boolean
     with Inline_Always;

   function Priority_Greater_Than_Equal  (Left, Right : in Oak_Agent_Id)
                                          return Boolean
     with Inline_Always;

   function Wake_Greater_Than (Left, Right : in Oak_Agent_Id) return Boolean
     with Inline;

   package Priority_Queue is new Oak.Storage.Slim_Priority_Queue
     (Item_Type     => Oak_Agent_Id,
      No_Item       => No_Agent,
      ">"           => Priority_Greater_Than,
      ">="          => Priority_Greater_Than_Equal);

   use Priority_Queue;

   package Time_Queue is new Oak.Storage.Binary_Heap
     (Item_Type                    => Oak_Agent_Id,
      No_Item                      => No_Agent,
      Size                         => Max_Task_Agents + Max_Scheduler_Agents,
      ">"                          => Wake_Greater_Than);

   use Time_Queue;

end Acton.Scheduler_Agents.FIFO_Within_Priorities_Basic_Sleep;
