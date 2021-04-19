------------------------------------------------------------------------------------------
--                                                                                      --
--                                    OAK COMPONENTS                                    --
--                                                                                      --
--                            OAK.STORAGE.TIME_PRIORITY_POOL                            --
--                                                                                      --
--                       Copyright (C) 2013-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

--  This package implements Oak's Time-Priority Pool. It is actually a set that
--  in the first instance stores elements by increasing time (though the set is
--  more general than that). When it comes to finding the eariliest (smallest)
--  node in the set though, an optional priority can be provided that will
--  cause the search function to pick the eariliest item with a priority above
--  the provided priority.
--
--  The purpose of the pool is to allow users to easily populate it with timers
--  or sleeping tasks and then quickly find in O (log n) time the element they
--  have to react to first without being bothered by elements that are below
--  their interested priority. As a set the functionalty is quite limited.

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

   with function Key      (Element : in Element_Type) return Key_Type;
   with function Priority (Element : in Element_Type) return Priority_Type;

package Oak.Storage.Time_Priority_Pool with Pure is

   No_Node_Value : constant := 0;
   First_Node_Value : constant := No_Node_Value + 1;
   --  The constant that represents the No_Node. Needed to satisfy the
   --  preelaborable initialization constraints on the types below. ??? Should
   --  check to see if there is a bug in the compiler preventing the direct use
   --  of No_Node.

   No_Node : constant Node_Location := No_Node_Value;
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

   First_Priority : constant Priority_Type := Priority_Type'First;
   --  Constant for the first priority.

   type Node_Colour is (Red, Black);
   --  Colour of a node in the red-black tree.

   type Node_Type is record
   --  This type represents a Node in the tree. Each node has a link to its
   --  parent to allow a bottom-up transversal of the tree without using
   --  recursion. Its presence does not impact of the effective size of the
   --  node if the Node_Location and priority types' base type is a byte. On a
   --  32 bit aligned system should be able to pack the infrustructure
   --  components into just two registers, or two 32 bit words in memory, with
   --  two bytes free.

      Colour         : Node_Colour   := Black;
      Has_Element    : Boolean       := False;
      In_Tree        : Boolean       := False;
      Parent         : Node_Location := No_Node_Value;
      Left, Right    : Node_Location := No_Node_Value;
      Left_Priority  : Priority_Type := Priority_Type'First;
      Right_Priority : Priority_Type := Priority_Type'First;
      --  Left and right subtree priorities are store seperately so we do not
      --  have to access the child nodes just so we can determine which path to
      --  take.

      Element        : Element_Type;
   end record;

   type Node_Array is array (Node_Location) of Node_Type;

   type Pool_Type is record
   --  This type represents the set which is actually a red-black tree.

      Root      : Node_Location := No_Node_Value;
      --  The node pointing to the root of the tree.

      Free_List : Node_Location := No_Node_Value;
      --  The first node in the free list. This list consists of free nodes
      --  that are not part of the bulk free store of the array, since they
      --  have been allocated and subsquently deallocated.
      --
      --  No_Node means the list is empty.

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

end Oak.Storage.Time_Priority_Pool;
