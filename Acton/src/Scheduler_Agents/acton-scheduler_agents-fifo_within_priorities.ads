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

with Oak.Agent; use Oak.Agent;
with System;    use System;

with Oak.Project_Support_Package; use Oak.Project_Support_Package;

package Acton.Scheduler_Agents.FIFO_Within_Priorities with Preelaborate is

   procedure Initialise_Scheduler_Agent
     (Min_Priority : Any_Priority;
      Max_Priority : Any_Priority;
      Oak_Kernel   : Kernel_Id);

   Stack_Size : constant := 1 * 1024;

private

   Max_Schedulable_Agents : constant :=
                              Max_Scheduler_Agents + Max_Task_Agents +
                                Max_Protected_Agents;

   type Storage_Id is mod Max_Schedulable_Agents + 1;

   No_Node : constant Storage_Id := Storage_Id'First;

   type Queue is record
      Head, Tail : Oak_Agent_Id;
   end record;

   type Queues is
     array (System.Any_Priority range <>) of Queue;

   type Scheduler_Element is record
      Agent : Oak_Agent_Id;
      Next  : Storage_Id;
   end record;

   type Elements is array (Storage_Id)
     of Scheduler_Element;

   type Scheduler_Storage (Min, Max : Any_Priority) is record
      Agents : Elements;

      Runnable_Queues : Queues (Min .. Max);
      Sleeping_Queues : Queues (Min .. Max);

      Bulk_Free : Storage_Id := No_Node + 1;
      Free_List : Storage_Id;
   end record;

end Acton.Scheduler_Agents.FIFO_Within_Priorities;
