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

with Oak.Storage.Binary_Heap;

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

   Scheduler_Error : exception;

   function Wake_Greater_Than (Left, Right : in Oak_Agent_Id) return Boolean
     with Inline;

   package Queue_Pack is new Oak.Storage.Binary_Heap
     (Item_Type                    => Oak_Agent_Id,
      No_Item                      => No_Agent,
      Size                         => Max_Schedulable_Agents,
      ">"                          => Wake_Greater_Than);

   use Queue_Pack;

end Acton.Scheduler_Agents.Priority_Server;
