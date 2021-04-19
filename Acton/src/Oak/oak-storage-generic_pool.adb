------------------------------------------------------------------------------------------
--                                                                                      --
--                                    OAK COMPONENTS                                    --
--                                                                                      --
--                                OAK.STORAGE.GENERIC_POOL                              --
--                                                                                      --
--                       Copyright (C) 2014-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

package body Oak.Storage.Generic_Pool is

   -----------------------
   -- Allocate_An_Item --
   -----------------------

   procedure Allocate_An_Item (Item : out Item_Id_Type) is
   begin
      --  Unallocated nodes live in one of two places. The first is in the bulk
      --  free store which is the group of unallocated nodes at the right end
      --  of the storage array (assuming that node are allocated from the array
      --  left to right. When a pool is first initialised all the nodes but the
      --  No_Node node are part of the bulk free store.
      --
      --  The second group of unallocated nodes live on the free list. Nodes
      --  are added to the free list when they are unallocated and they are not
      --  next to the bulk free store, where in this case the node joins that
      --  store. This dual store approach prevents the need to have to
      --  initialise the free list during setup, which would be an O(n)
      --  operation. The free list is a linked list using the nodes' right
      --  link.
      --
      --  The array looks like this (x marks an allocated node):
      --
      --  ---------------------------------------------------------------------
      --  | x |   | x | x |   |   | x |   |   | x |   |   |   |   |   |   |   |
      --  ---------------------------------------------------------------------
      --        ^-----------^---^-------^---^     | ^ --- Bulk Free Store ----|
      --        |                                   |
      --       Free_list                            Bulk_Free
      --

      if Free_List = No_Item then
         --  The free list is empty so we allocate a node from the bulk free
         --  store.

         if Bulk_Free = No_Item then
            --  No more room in the pool!
            raise Pool_Capacity_Error with "No room in the pool!";
         end if;

         Item      := Bulk_Free;
         Bulk_Free := Bulk_Free + 1;

      else
         --  Extract a node from the free list.

         Item := Free_List;
         Free_List := Storage_Track (Free_List);
      end if;

   end Allocate_An_Item;

   ----------------------
   -- Deallocate_Item --
   ----------------------

   procedure Deallocate_Item (Id : in Item_Id_Type) is
   begin
      --  See the Allocate_Node procedure above for a description of how free
      --  nodes are stored.

      if Id + 1 = Bulk_Free then
         --  The next node is adjacent to the bulk free store, so we move the
         --  node inside it.

         Bulk_Free := Id;

      else
         --  Add the node to the free list.

         Storage_Track (Id) := Free_List;
         Free_List := Id;

      end if;
   end Deallocate_Item;

   -------------------
   -- Setup_Storage --
   -------------------

   procedure Setup_Storage is
   begin
      Storage_Ready := True;
   end Setup_Storage;

end Oak.Storage.Generic_Pool;
