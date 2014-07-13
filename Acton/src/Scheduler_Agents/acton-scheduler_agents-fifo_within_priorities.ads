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
with Oak.Agent.Oak_Agent; use Oak.Agent.Oak_Agent;
with System;              use System;

with Oak.Project_Support_Package; use Oak.Project_Support_Package;

with Oak.Storage.Priority_Queue;
with Oak.Storage.Binary_Heap;

--  with Oak.Project_Support_Package; use Oak.Project_Support_Package;

package Acton.Scheduler_Agents.FIFO_Within_Priorities with Preelaborate is

   procedure New_Scheduler_Agent
     (Agent        : out Scheduler_Id;
      Min_Priority : in  Any_Priority;
      Max_Priority : in  Any_Priority);

   Stack_Size : constant := 1 * 1024;
   Agent_Name : constant String := "Fixed_Priority_Scheduler";

private

   Scheduler_Error : exception;

   type Internal_Agent_Id is mod Max_Task_Agents + Max_Scheduler_Agents;

   function Priority_Greater (Left, Right : in Oak_Agent_Id) return Boolean
     with Inline;
   function Priority_Greater_Equal (Left, Right : in Oak_Agent_Id)
                                      return Boolean with Inline;
   function Wake_Greater_Than (Left, Right : in Oak_Agent_Id) return Boolean
     with Inline;

   package Priority_Queue is new Oak.Storage.Priority_Queue
     (Item_Type     => Oak_Agent_Id,
      No_Item       => No_Agent,
      Index_Type    => Internal_Agent_Id,
      ">"           => Priority_Greater,
      ">="          => Priority_Greater_Equal);

   use Priority_Queue;

   package Time_Queue is new Oak.Storage.Binary_Heap
     (Item_Type                    => Oak_Agent_Id,
      No_Item                      => No_Agent,
      Size                         => Max_Task_Agents + Max_Scheduler_Agents,
      ">"                          => Wake_Greater_Than);

   use Time_Queue;

   function Priority_Greater (Left, Right : in Oak_Agent_Id) return Boolean is
     (Normal_Priority (Left) > Normal_Priority (Right));

   function Priority_Greater_Equal (Left, Right : in Oak_Agent_Id)
                                      return Boolean is
     (Normal_Priority (Left) >= Normal_Priority (Right));

end Acton.Scheduler_Agents.FIFO_Within_Priorities;
