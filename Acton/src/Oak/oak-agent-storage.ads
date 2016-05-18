------------------------------------------------------------------------------
--                                                                          --
--                              OAK COMPONENTS                              --
--                                                                          --
--                            OAK.AGENT.STORAGE                             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                 Copyright (C) 2013-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

--  This package provides the storage used to store Oak's agents and the
--  operations on that storage. Each instantiation creates a new storage pool
--  (or table) to store the specific kind of agents in. The size of the pool
--  is given by the id type of that kind of agent. Clients have direct access
--  to the array making up the pool, with access to its elements through the
--  provided Agent_Id_Type.

--  This version also does not allow elements to be deleted. This is used
--  when Oak does not allow agents to be deleted and allows for the
--  implementation to be significantly simplified (by making essentially not
--  a linked list anymore). Exists to be compatible with the version that
--  does allow elements to be deleted for systems where Oak is allowed to
--  to remove agents.

generic
   type Agent_Record_Type is private;
   type Agent_Id_Type     is mod <>;

--  package Oak.Agent.Storage with Preelaborate is
package Oak.Agent.Storage is
   pragma Preelaborate;

   type Pool_Type is array (Agent_Id_Type) of Agent_Record_Type;

   Agent_Pool : Pool_Type;
   --  Storage used to store Agents.

   procedure Allocate_An_Agent (Agent : out Agent_Id_Type)
     with Pre => Has_Space and Is_Storage_Ready, Inline;
   --  Allocates space in the pool for a new Agent and returns the Agent Id.
   --  Callers should ensure that there is space free in the pool before
   --  calling otherwise Agent_Pool_Capacity_Error will be raised.

   procedure Allocate_An_Agent_With_Id (Id : in Agent_Id_Type)
     with Pre => Is_Storage_Ready, Inline;
   --  Like above, but this time the subprogram allocates the node associated
   --  with the provided Id. No check is made to see if this will clobber
   --  another agent. Used so that id's in this pool match those of the parent
   --  pools.

   procedure Deallocate_Agent (Id : in Agent_Id_Type) with Inline;
   --  Dellocates the storage associated with the Id.

   function Has_Agent (Agent_Id : Agent_Id_Type) return Boolean
     with Pre => Is_Storage_Ready;
   --  Returns true if the Agent_Id has been allocate.

   function Has_Space return Boolean
     with Pre => Is_Storage_Ready;
   --  Returns true if there is room in the pool.

   function Is_Storage_Ready return Boolean
     with Ghost;
   --  Returns true if the agent pool has been setup.

   procedure Setup_Storage
     with Post => Is_Storage_Ready;
   --  Sets up the agent pool.

private

   Free_Cell  : Agent_Id_Type'Base := Agent_Id_Type'First;
   --  The first free cell avaliable.

   Pool_Full  : Boolean := False;
   --  Flag to indicate if the pool is full. Set by the Allocate_Agent
   --  procedure if Free_Cell = Agent_Id_Type'Last.

   Storage_Ready : Boolean := False;
   --  Signals that the storage has been setup and is ready to allocate new
   --  agents.

   function Has_Agent (Agent_Id : Agent_Id_Type) return Boolean
     is (Pool_Full or else Agent_Id < Free_Cell);
   --  If the pool is full then the Id has definately been allocated, otherwise
   --  the Agent has been assigned if the Free_Cell pointer has gone pass the
   --  id in question.

   function Has_Space return Boolean is (not Pool_Full);

   function Is_Storage_Ready return Boolean is (Storage_Ready);

end Oak.Agent.Storage;
