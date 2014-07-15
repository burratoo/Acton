------------------------------------------------------------------------------
--                                                                          --
--                              OAK COMPONENTS                              --
--                                                                          --
--                         OAK.STORAGE.GENERIC_POOL                         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                 Copyright (C) 2014-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

--  This package provides a generic storage pool built around an array. Each
--  instantiation creates a new storage pool (or table) to store the specific
--  kind of item in it. The size of the pool is given by the id type of that
--  kind of Item. Clients have direct access to the array making up the pool,
--  with access to its elements through the provided Item_Id_Type.

generic
   type Item_Type is private;
   type Item_Id_Type     is mod <>;

package Oak.Storage.Generic_Pool with Preelaborate is

   type Pool_Type is array (Item_Id_Type) of Item_Type;

   Item_Pool : Pool_Type;
   --  Storage used to store Items.

   procedure Allocate_An_Item (Item : out Item_Id_Type)
     with Pre => Has_Space and Is_Storage_Ready;
   --  Allocates space in the pool for a new item and returns the Item Id.
   --  Callers should ensure that there is space free in the pool before
   --  calling otherwise Item_Pool_Capacity_Error will be raised.

   procedure Deallocate_Item (Id : in Item_Id_Type);
   --  Dellocates the storage associated with the Id.

   function Has_Space return Boolean
     with Pre => Is_Storage_Ready;
   --  Returns true if there is room in the pool.

   function Is_Storage_Ready return Boolean
     with Convention => Ghost;
   --  Returns true if the Item pool has been setup.

   procedure Setup_Storage
     with Post => Is_Storage_Ready;
   --  Sets up the Item pool.

private

   type Id_List is array (Item_Id_Type) of Item_Id_Type;

   Storage_Track : Id_List;

   No_Item  : constant Item_Id_Type'Base := Item_Id_Type'First;

   Free_List : Item_Id_Type := No_Item;
   --  The first node in the free list. This list consists of free nodes
   --  that are not part of the bulk free store of the array, since they
   --  have been allocated and subsquently deallocated.
   --
   --  No_Node means the list is empty.

   Bulk_Free : Item_Id_Type := No_Item + 1;
   --  The first node in the bulk free store. This is the free area on the
   --  right side of the array that has not been allocate. The bulk store
   --  saves from having to populate the free list in the first place, which
   --  may be an expensive operation (it is an O (n) operation).
   --
   --  No_Node means bulk free store is empty.

   Storage_Ready : Boolean := False;
   --  Signals that the storage has been setup and is ready to allocate new
   --  Items.

   function Has_Space return Boolean is
     (Free_List = No_Item and Bulk_Free = No_Item);

   function Is_Storage_Ready return Boolean is (Storage_Ready);

end Oak.Storage.Generic_Pool;
