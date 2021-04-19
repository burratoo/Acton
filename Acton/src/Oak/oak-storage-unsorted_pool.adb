------------------------------------------------------------------------------------------
--                                                                                      --
--                                    OAK COMPONENTS                                    --
--                                                                                      --
--                               OAK.STORAGE.UNSORTED_POOL                              --
--                                                                                      --
--                       Copyright (C) 2013-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

package body Oak.Storage.Unsorted_Pool is
--     pragma Suppress (All_Checks);

   --  ???? TODO: Need to write some code to verify this actually works. Spark
   --  should help.

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Allocate_Node
     (Pool     : in out Pool_Type;
      New_Node : out Node_Location);
   --  Allocate storage for a node.

   procedure Deallocate_Node
     (Pool    : in out Pool_Type;
      Node_Id : in Node_Location);
   --  Deallocate a node from the storage pool. The node to be deallocated must
   --  already be removed from the tree. TODO: add function to search tree for
   --  node as a precondition.

   -------------------
   -- Allocate_Node --
   -------------------

   procedure Allocate_Node
     (Pool     : in out Pool_Type;
      New_Node : out Node_Location) is
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

      if Pool.Free_List = No_Node then
         --  The free list is empty so we allocate a node from the bulk free
         --  store.

         if Pool.Bulk_Free = No_Node then
            --  No more room in the pool!
            raise Pool_Capacity_Error with "No room in the pool!";
         end if;

         New_Node       := Pool.Bulk_Free;
         Pool.Bulk_Free := Pool.Bulk_Free + 1;

      else
         --  Extract a node from the free list.

         New_Node := Pool.Free_List;
         Pool.Free_List := Pool.Nodes (Pool.Free_List).Next_Free_Node;
      end if;

   end Allocate_Node;

   ---------------------
   -- Deallocate_Node --
   ---------------------

   procedure Deallocate_Node
     (Pool    : in out Pool_Type;
      Node_Id : in Node_Location) is
   begin
      --  See the Allocate_Node procedure above for a description of how free
      --  nodes are stored.

      if Node_Id + 1 = Pool.Bulk_Free then
         --  The next node is adjacent to the bulk free store, so we move the
         --  node inside it.

         Pool.Nodes (Node_Id).Has_Element := False;
         Pool.Bulk_Free := Node_Id;

      else
         --  Add the node to the free list.

         Pool.Nodes (Node_Id).Next_Free_Node := Pool.Free_List;
         Pool.Free_List := Node_Id;

      end if;
   end Deallocate_Node;

   -----------------
   -- Delete_Item --
   -----------------

   procedure Delete_Item
     (Pool    : in out Pool_Type;
      Item_Id : in Node_Location) is
   begin
      if In_Tree (Pool, Item_Id) then
         Remove_Node (Pool, Item_Id);
      end if;

      Deallocate_Node (Pool, Item_Id);
   end Delete_Item;

   -----------------------
   -- Find_Earlest_Item --
   -----------------------

   function Find_Earliest_Item
     (In_Pool        : in Pool_Type;
      Above_Priority : in Priority_Type := Priority_Type'First)
      return Node_Location
   is
      Nodes : Node_Array renames In_Pool.Nodes;

      Selected_Node : Node_Location := No_Node;

   begin
      for N in 1 .. Nodes'Last loop
         if Nodes (N).In_Tree
           and then Priority (Nodes (N).Element) > Above_Priority
         then
            if Selected_Node = No_Node_Value
              or else Nodes (N).Element < Nodes (Selected_Node).Element
            then
               Selected_Node := N;
            end if;
         end if;
      end loop;

      return Selected_Node;
   end Find_Earliest_Item;

   -------------------------
   -- Generic_Update_Time --
   -------------------------

   procedure Generic_Update_Time
     (Pool    : in out Pool_Type;
      Item_Id : in Node_Location;
      Time    : in Oak_Time.Time)
   is
      Element : Element_Type renames Pool.Nodes (Item_Id).Element;
   begin
      Set_Time (Element, Time);
   end Generic_Update_Time;

   -----------------
   -- Insert_Node --
   -----------------

   procedure Insert_Node (Pool : in out Pool_Type; Node : in Node_Location) is
      N : Node_Array renames Pool.Nodes;
   begin
      N (Node).In_Tree := True;
   end Insert_Node;

   --------------
   -- New_Item --
   --------------

   procedure New_Item
     (Pool        : in out Pool_Type;
      Item        : in Element_Type;
      Item_Id     : out Node_Location;
      Add_To_Tree : in Boolean := True) is
   begin
      if Is_Pool_Full (Pool) then
         --  No more room in the pool!
         raise Pool_Capacity_Error with "No room in the pool!";
      end if;

      Allocate_Node
        (Pool     => Pool,
         New_Node => Item_Id);

      Pool.Nodes (Item_Id) := (Has_Element    => True,
                               In_Tree        => False,
                               Next_Free_Node => No_Node,
                               Element        => Item);

      if Add_To_Tree then
         Insert_Node
           (Pool => Pool,
            Node => Item_Id);
      end if;
   end New_Item;

   -----------------
   -- Remove_Node --
   -----------------

   procedure Remove_Node (Pool : in out Pool_Type; Node : in Node_Location)
   is
      N : Node_Array renames Pool.Nodes;

   begin
      N (Node).In_Tree := False;
   end Remove_Node;

   ------------------
   -- Replace_Item --
   ------------------

   procedure Replace_Item
     (Pool     : in out Pool_Type;
      Item_Id  : in Node_Location;
      Contents : in Element_Type)
   is
      Node    : Node_Type renames Pool.Nodes (Item_Id);
   begin
      Node.Element := Contents;
   end Replace_Item;

end Oak.Storage.Unsorted_Pool;
