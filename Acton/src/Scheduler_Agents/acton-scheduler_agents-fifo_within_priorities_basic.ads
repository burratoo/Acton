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
with System;              use System;

with Oak.Project_Support_Package; use Oak.Project_Support_Package;

with Oak.Storage.Binary_Heap;

package Acton.Scheduler_Agents.FIFO_Within_Priorities_Basic
  with Preelaborate is

   procedure New_Scheduler_Agent
     (Agent        : out Scheduler_Id;
      Min_Priority : in  Any_Priority;
      Max_Priority : in  Any_Priority);

   Agent_Name : constant String := "Fixed_Priority_Scheduler_Basic";
   Stack_Size : constant := 1 * 1024;

private

   subtype Schedulable_Agents is
     Oak_Agent_Id range Sleep_Agent .. Task_Id'Last;

   function Wake_Greater_Than (Left, Right : in Oak_Agent_Id) return Boolean
     with Inline;

   package Time_Queue is new Oak.Storage.Binary_Heap
     (Item_Type                    => Oak_Agent_Id,
      No_Item                      => No_Agent,
      Size                         => Max_Task_Agents + Max_Scheduler_Agents,
      ">"                          => Wake_Greater_Than);

   use Time_Queue;

end Acton.Scheduler_Agents.FIFO_Within_Priorities_Basic;
