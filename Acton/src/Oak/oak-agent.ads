------------------------------------------------------------------------------
--                                                                          --
--                              OAK COMPONENTS                              --
--                                                                          --
--                                OAK.AGENT                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                 Copyright (C) 2010-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

--  This package defines the types that define Oak's agents. They are housed
--  together in this package together to prevent circular dependencies
--  among the Agent children.

--     type Agent_Kind is
--      (Oak_Agent, Oak_Instance, Scheduler_Agent, Task_Agent, Interrupt_Agent,
--        Protected_Agent);
--
--  This type identifies the type of Agent. There are six kinds of Agents
--  used by Oak:
--    Oak_Agent         The base Agent
--    Oak_Instance      Agent that represents an Oak Instance (an instance
--                      of the kernel Oak)
--    Scheduler_Agent   Agent that represents a scheduler
--    Task_Agent        Agent that represents tasks
--    Interrupt_Agent   Agent that represents the tasks used to handle
--                      interrupts
--    Protected_Agent   Agent that represents protected objects.

with Oak.Project_Support_Package; use Oak.Project_Support_Package;

package Oak.Agent with Pure is

   ---------------
   -- Agent Ids --
   ---------------

   --  Each Agent is identified by an id value. The id refers to the record/s
   --  that holds the Agent's information. All agents except for Oak Agents
   --  are made up of two records: A record recording information specific to
   --  that kind of Agent and an Oak Agent record. Each record is held in its
   --  respective store. To make accessing both records easier, the records
   --  share the same id number. It has the added benefit that the Agent kind
   --  can be determined directly from its id.
   --
   --  This sharing of ids does complicate how the ids are defined. Using
   --  subtypes is the easiest route, but this means the details of this ranges
   --  have to be made public here, when they really should be hidden. Becuase
   --  the alternaive is not very pretty - defining a bunch of types and then
   --  having to do frequent type conversions - this package goes down the
   --  public subtype route.

   type Oak_Agent_Id is mod Max_Oak_Agents;
   --  Type use to identify an Oak Agent in the system.

   No_Agent    : constant Oak_Agent_Id := Oak_Agent_Id'First;
   --  Used to indicate that no agent has been selected. It is a synonym of
   --  Sleep_Agent to ensure that in the event of a No_Agent being accidentally
   --  dispatched, there is something valid to run. Because of this, it is safe
   --  to assume that No_Agent = Sleep_Agent. Both exist since there are
   --  contexts better suite to one or the other terms.

   Sleep_Agent : constant Oak_Agent_Id := Oak_Agent_Id'First;
   --  Used to indicate the Sleep Agent. Equal to No_Agent, see above for
   --  details.

   type Agent_List is record
      Head, Tail : Oak_Agent_Id;
   end record;
   --  This type is used to indicate a linked-list of Oak Agents. It points
   --  to the head and tail of the list.

   Empty_List : constant Agent_List := (No_Agent, No_Agent);
   --  Used to indicate an empty list.

   --  Constants used to define the range of the ids of the Agents that extend
   --  Oak Agents.

   Kernel_Id_Low_Bound  : constant := Oak_Agent_Id'Succ (Sleep_Agent);
   Kernel_Id_High_Bound : constant := Oak_Agent_Id'First + Max_Kernel_Agents;

   Scheduler_Id_Low_Bound  : constant :=
                               Kernel_Id_High_Bound + 1;
   Scheduler_Id_High_Bound : constant :=
                               Kernel_Id_High_Bound + Max_Scheduler_Agents;

   Interrupt_Id_Low_Bound  : constant :=
                               Scheduler_Id_High_Bound + 1;
   Interrupt_Id_High_Bound : constant :=
                               Scheduler_Id_High_Bound + Max_Interrupt_Agents;

   Task_Id_Low_Bound       : constant :=
                               Interrupt_Id_High_Bound + 1;
   Task_Id_High_Bound      : constant :=
                               Interrupt_Id_High_Bound + Max_Task_Agents;

   Protected_Id_Low_Bound  : constant :=
                               Task_Id_High_Bound + 1;
   Protected_Id_High_Bound : constant :=
                               Task_Id_High_Bound + Max_Protected_Agents;

   --  Subtype defintions for Agent Ids that derive from Oak Agent.

   subtype Kernel_Id                is Oak_Agent_Id
     range Kernel_Id_Low_Bound        .. Kernel_Id_High_Bound;

   subtype Scheduler_Id             is Oak_Agent_Id
     range Scheduler_Id_Low_Bound     .. Scheduler_Id_High_Bound;

   subtype Interrupt_Id             is Oak_Agent_Id
     range Interrupt_Id_Low_Bound     .. Interrupt_Id_High_Bound;

   subtype Task_Id                  is Oak_Agent_Id
     range Task_Id_Low_Bound          .. Task_Id_High_Bound;

   subtype Protected_Id             is Oak_Agent_Id
   range Protected_Id_Low_Bound     .. Protected_Id_High_Bound;

   --  Subtype defintions for Agent Ids that derive from Oak Agent that also
   --  include the No_Agent id. The advantage of this approach is twofold:
   --  First there is a slight machine level optimisation with the ability to
   --  quickly test for No_Agent when it is zero (say on Power where r0 reads
   --  zero). Secondly, it allows subprograms to clearly specifiy in their
   --  specification whether or not they will accept No_Agent as a valid
   --  parameter.

   subtype Kernel_Id_With_No is Oak_Agent_Id
     with Static_Predicate => Kernel_Id_With_No in No_Agent | Kernel_Id;

   subtype Scheduler_Id_With_No is Oak_Agent_Id
     with Static_Predicate => Scheduler_Id_With_No in No_Agent | Scheduler_Id;

   subtype Interrupt_Id_With_No is Oak_Agent_Id
     with Static_Predicate => Interrupt_Id_With_No in No_Agent | Interrupt_Id;

   subtype Task_Id_With_No is Oak_Agent_Id
     with Static_Predicate => Task_Id_With_No in No_Agent | Task_Id;

   subtype Protected_Id_With_No is Oak_Agent_Id
     with Static_Predicate => Protected_Id_With_No in No_Agent | Protected_Id;

   --  Derivatives of Agent list which only have a head pointer. Used to
   --  indicate a list is being used.

   subtype Task_List is Task_Id_With_No;

   subtype Charge_List_Head is Oak_Agent_Id;

   Agent_Pool_Capacity_Error : exception;
   --  An error raised when there is no more room in an Agent Pool.

end Oak.Agent;
