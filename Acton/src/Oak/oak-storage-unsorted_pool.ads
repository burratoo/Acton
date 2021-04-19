------------------------------------------------------------------------------------------
--                                                                                      --
--                                    OAK COMPONENTS                                    --
--                                                                                      --
--                               OAK.STORAGE.UNSORTED_POOL                              --
--                                                                                      --
--                       Copyright (C) 2013-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

with Oak.Oak_Time;

generic
   type Element_Type is private;
   type Key_Type is private;
   type Node_Location is mod <>;
   --  Node_Location is mod so it can pack the most elements in the smallest
   --  base type.

   type Priority_Type is range <>;

   pragma Preelaborable_Initialization (Element_Type);

   with function "<" (Left, Right : Element_Type) return Boolean is <>;

   with function Key      (Element : in Element_Type) return Key_Type
     with Unreferenced;
   with function Priority (Element : in Element_Type) return Priority_Type;

package Oak.Storage.Unsorted_Pool with Pure is
   No_Node_Value    : constant := 0;
   First_Node_Value : constant := No_Node_Value + 1;
   --  The constant that represents the No_Node. Needed to satisfy the
   --  preelaborable initialization constraints on the types below. ??? Should
   --  check to see if there is a bug in the compiler preventing the direct use
   --  of No_Node.

   No_Node          : constant Node_Location := No_Node_Value;
   --  The Node_Location that represents the null node, or that no node exists
   --  at all.

   type Pool_Type is private with Preelaborable_Initialization;

   procedure Delete_Item
     (Pool    : in out Pool_Type;
      Item_Id : in Node_Location);
   --  Removes an item from the pool and deallocates its storage.

   function Element (Pool : in Pool_Type; Item_Id : in Node_Location)
                     return Element_Type
     with Pre => Has_Element (Pool, Item_Id);
   --  Return the element associated with the Item_Id.

   function Find_Earliest_Item
     (In_Pool        : in Pool_Type;
      Above_Priority : in Priority_Type := Priority_Type'First)
      return Node_Location;
   --  Finds the earliest/smallest item in the pool above the given priority.
   --  Returns No_Node if there is no item found.

   generic
      with procedure Set_Time
        (Element  : in out Element_Type;
         Set_Time : in     Oak_Time.Time);
   procedure Generic_Update_Time
     (Pool    : in out Pool_Type;
      Item_Id : in Node_Location;
      Time    : in Oak_Time.Time);
   --  Updates the time associated with the item. Provides a quicker way to
   --  just update the time which is the most common operation on this pool.
   --  May cause the item to relocate in the tree.

   function Has_Element (Pool : in Pool_Type; Item_Id : in Node_Location)
                         return Boolean;

   function In_Tree (Pool : in Pool_Type; Item_Id : in Node_Location)
                     return Boolean
     with Inline;
   --  Is the Item_Id in the pool tree.

   function Is_Pool_Full (Pool : in Pool_Type) return Boolean;

   procedure Insert_Node (Pool : in out Pool_Type; Node : in Node_Location);
   --  Insert a node in to the storage pool tree.

   procedure New_Item
     (Pool        : in out Pool_Type;
      Item        : in Element_Type;
      Item_Id     : out Node_Location;
      Add_To_Tree : in Boolean := True);
   --  Allocates storage for the new item in the pool and initialises its
   --  contents to New_Item. Returns the id associated with item.

   procedure Replace_Item
     (Pool     : in out Pool_Type;
      Item_Id  : in Node_Location;
      Contents : in Element_Type);
   --  Replace the contents of the item at Item_Id with the provided contents.

   procedure Remove_Node (Pool : in out Pool_Type; Node : in Node_Location);
   --  Removes the node from the pool tree. Does not deallocate the node.

private

   type Node_Type is record
   --  This type represents a Node in the tree.

      Has_Element    : Boolean       := False;
      In_Tree        : Boolean       := False;
      Next_Free_Node : Node_Location := No_Node_Value;
      Element        : Element_Type;
   end record;

   type Node_Array is array (Node_Location) of Node_Type;

   type Pool_Type is record
   --  This type represents the set which is actually a red-black tree.

      Free_List : Node_Location := No_Node_Value;
      --  The first node in the free list. This list consists of free nodes
      --  that are not part of the bulk free store of the array, since they
      --  have been allocated and subsquently deallocated.
      --
      --  No_Node_Value means the list is empty.

      Bulk_Free : Node_Location := First_Node_Value;
      --  The first node in the bulk free store. This is the free area on the
      --  right side of the array that has not been allocate. The bulk store
      --  saves from having to populate the free list in the first place, which
      --  may be an expensive operation (it is an O (n) operation).
      --
      --  No_Node means bulk free store is empty.

      Nodes     : Node_Array;
      --  The node pool. The first node is the No_Node which needs to be
      --  initialised here.
   end record;

   --------------------------
   -- Function Expressions --
   --------------------------

   function Has_Element (Pool : in Pool_Type; Item_Id : in Node_Location)
                         return Boolean is
     (Pool.Nodes (Item_Id).Has_Element);

   function Is_Pool_Full (Pool : in Pool_Type) return Boolean is
     (Pool.Free_List = No_Node and Pool.Bulk_Free = No_Node);

   function Element (Pool : in Pool_Type; Item_Id : in Node_Location)
                     return Element_Type is (Pool.Nodes (Item_Id).Element);

   function In_Tree (Pool : in Pool_Type; Item_Id : in Node_Location)
                     return Boolean is
     (Pool.Nodes (Item_Id).In_Tree);

end Oak.Storage.Unsorted_Pool;
