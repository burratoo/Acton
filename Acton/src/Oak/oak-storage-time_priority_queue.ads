------------------------------------------------------------------------------
--                                                                          --
--                              OAK COMPONENTS                              --
--                                                                          --
--                      OAK.STORAGE.TIME_PRIORITY_QUEUE                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                 Copyright (C) 2013-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

generic
   type Item_Type is private;
   pragma Preelaborable_Initialization (Item_Type);

   type Index_Type is mod <>;
   --  Index_Type is mod so it can pack the most elements in the smallest
   --  base type.

   type Priority_Type is range <>;

   No_Item : Item_Type;

   with function "<" (Left, Right : Item_Type) return Boolean is <>;
   with function Priority (Element : in Item_Type) return Priority_Type;

package Oak.Storage.Time_Priority_Queue with Pure is

   Queue_Capacity_Error : exception;

   type Queue_Type is private with Preelaborable_Initialization;

   procedure Enqueue_Item (To_Queue    : in out Queue_Type;
                           Item        : in Item_Type);
   --  Adds an item to the priority queue

   function Find_Earliest_Item
     (In_Queue       : in Queue_Type;
      Above_Priority : in Priority_Type := Priority_Type'First)
      return Item_Type;
   --  Finds the earliest/smallest item in the pool above the given priority.
   --  Returns No_Item if there is no item found.

   function Head_Of_Queue (From_Queue : in out Queue_Type) return Item_Type;

   procedure Remove_Queue_Head (From_Queue : in out Queue_Type;
                                Item       : out Item_Type);

   --  Removes the first item from the priority queue. Returns an empty element
   --  if the queue is empty.

   function Is_Queue_Full (Queue : in Queue_Type) return Boolean;

private

   No_Node_Value    : constant := 0;
   First_Node_Value : constant := No_Node_Value + 1;
   --  The constant that represents the No_Node. Needed to satisfy the
   --  preelaborable initialization constraints on the types below. ??? Should
   --  check to see if there is a bug in the compiler preventing the direct use
   --  of No_Node.

   No_Node          : constant Index_Type := No_Node_Value;
   --  The Index_Type that represents the null node, or that no node exists
   --  at all.

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
      Parent         : Index_Type    := No_Node_Value;
      Left, Right    : Index_Type    := No_Node_Value;
      Left_Priority  : Priority_Type := Priority_Type'First;
      Right_Priority : Priority_Type := Priority_Type'First;
      --  Left and right subtree priorities are store seperately so we do not
      --  have to access the child nodes just so we can determine which path to
      --  take.

      Item           : Item_Type;
   end record;

   type Node_Array is array (Index_Type) of Node_Type;

   type Queue_Type is record
   --  This type represents the set which is actually a red-black tree.

      Root      : Index_Type := No_Node_Value;
      --  The node pointing to the root of the tree.

      Free_List : Index_Type := No_Node_Value;
      --  The first node in the free list. This list consists of free nodes
      --  that are not part of the bulk free store of the array, since they
      --  have been allocated and subsquently deallocated.
      --
      --  No_Node means the list is empty.

      Bulk_Free : Index_Type := First_Node_Value;
      --  The first node in the bulk free store. This is the free area on the
      --  right side of the array that has not been allocate. The bulk store
      --  saves from having to populate the free list in the first place, which
      --  may be an expensive operation (it is an O (n) operation).
      --
      --  No_Node means bulk free store is empty.

      First_Node       : Index_Type := No_Node_Value;
      First_Node_Valid : Boolean := False;
      --  Caches the first node in the priority queue.

      Nodes     : Node_Array;
      --  The node pool. The first node is the No_Node which needs to be
      --  initialised here.
   end record;

   --------------------------
   -- Function Expressions --
   --------------------------

   function Is_Queue_Full (Queue : in Queue_Type) return Boolean is
      (Queue.Free_List = No_Node and Queue.Bulk_Free = No_Node);

end Oak.Storage.Time_Priority_Queue;
