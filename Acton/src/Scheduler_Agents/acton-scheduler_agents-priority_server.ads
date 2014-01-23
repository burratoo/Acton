------------------------------------------------------------------------------
--                                                                          --
--                           ACTON SCHEDULER AGENT                          --
--                                                                          --
--                  ACTON.SCHEDULER_AGENTS.PRIORITY_SERVER                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                 Copyright (C) 2013-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

with Oak.Agent;    use Oak.Agent;
with Oak.Oak_Time; use Oak.Oak_Time;

with System;                 use System;
with System.Multiprocessors; use System.Multiprocessors;

package Acton.Scheduler_Agents.Priority_Server with Preelaborate is

   procedure New_Scheduler_Agent
     (Agent             : out Scheduler_Id;
      Min_Priority      : in  Any_Priority;
      Max_Priority      : in  Any_Priority;
      Budget            : in  Time_Span;
      Period            : in  Time_Span;
      Phase             : in  Time_Span;
      Relative_Deadline : in  Time_Span;
      CPU               : in  CPU_Range);

   Stack_Size : constant := 1 * 1024;
   Agent_Name : constant String := "Priority_Server";

private

   Max_Schedulable_Agents : constant := 10;

   type Storage_Id is mod Max_Schedulable_Agents + 1;

   No_Node : constant Storage_Id := Storage_Id'First;

   type Scheduler_Element is record
      Agent : Oak_Agent_Id;
      Next  : Storage_Id;
   end record;

   type Elements is array (Storage_Id)
     of Scheduler_Element;

   type Queue is record
      Head, Tail : Storage_Id;
   end record;

   Empty_Queue : constant Queue := (Head => No_Node, Tail => No_Node);

   type Queues is
     array (System.Any_Priority range <>) of Queue;

   type Scheduler_Storage (Min, Max : Any_Priority) is record
   --  Storage is currently very similar to the time priority pool when it
   --  comes to allocating from an array.

      Pool : Elements;

      Runnable_Queues : Queues (Min .. Max) :=
                          (others => (No_Node, No_Node));
      Sleeping_Queues : Queues (Min .. Max) :=
                          (others => (No_Node, No_Node));

      Bulk_Free : Storage_Id := No_Node + 1;
      Free_List : Storage_Id;
   end record;

end Acton.Scheduler_Agents.Priority_Server;
