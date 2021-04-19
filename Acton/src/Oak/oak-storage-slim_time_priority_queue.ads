------------------------------------------------------------------------------------------
--                                                                                      --
--                                    OAK COMPONENTS                                    --
--                                                                                      --
--                         OAK.STORAGE.SLIM_TIME_PRIORITY_QUEUE                         --
--                                                                                      --
--                       Copyright (C) 2014-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

--  This is a slim priority queue since it doesn't store anything. Instead
--  the Item_Type is used to point not only to the node inside the queue, but
--  also to the item refered to outside the queue.

generic
   type Item_Type is mod <>;

   type Priority_Type is range <>;

   No_Item : Item_Type;

   with function "<" (Left, Right : Item_Type) return Boolean is <>;
   with function Priority (Element : in Item_Type) return Priority_Type;

--  package Oak.Storage.Slim_Time_Priority_Queue with Pure is
package Oak.Storage.Slim_Time_Priority_Queue is
   pragma Pure;

   Queue_Capacity_Error : exception;

   type Queue_Type is private with Preelaborable_Initialization;

   procedure Enqueue_Item (To_Queue : in out Queue_Type;
                           Item     : in Item_Type);
   --  Adds an item to the priority queue

   function Find_Earliest_Item
     (In_Queue       : in Queue_Type;
      Above_Priority : in Priority_Type := Priority_Type'First)
      return Item_Type;
   --  Finds the earliest/smallest item in the pool above the given priority.
   --  Returns No_Item if there is no item found.

   function In_Queue (Queue : in Queue_Type; Item_Id : in Item_Type)
                      return Boolean;

   procedure Remove_Item (Queue : in out Queue_Type; Item : in Item_Type);
   --  Removes the node from the Queue tree. Does not deallocate the node.

private

   No_Node_Value    : constant := 0;
   First_Node_Value : constant := No_Node_Value + 1;
   --  The constant that represents the No_Node. Needed to satisfy the
   --  preelaborable initialization constraints on the types below. ??? Should
   --  check to see if there is a bug in the compiler preventing the direct use
   --  of No_Node.

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
      In_Tree        : Boolean       := False;
      Parent         : Item_Type     := No_Node_Value;
      Left, Right    : Item_Type     := No_Node_Value;
      Node_Priority  : Priority_Type := Priority_Type'First;
      Left_Priority  : Priority_Type := Priority_Type'First;
      Right_Priority : Priority_Type := Priority_Type'First;
      --  Left and right subtree priorities are store seperately so we do not
      --  have to access the child nodes just so we can determine which path to
      --  take.
   end record;

   type Node_Array is array (Item_Type) of Node_Type;

   type Queue_Type is record
   --  This type represents the set which is actually a red-black tree.

      Root : Item_Type := No_Node_Value;
      --  The node pointing to the root of the tree.

      First_Node       : Item_Type := No_Node_Value;
      First_Node_Valid : Boolean := False;
      --  Caches the first node in the priority queue.

      Nodes : Node_Array;
      --  The node pool. The first node is the No_Node which needs to be
      --  initialised here.
   end record;

   --------------------------
   -- Function Expressions --
   --------------------------

   function In_Queue (Queue : in Queue_Type; Item_Id : in Item_Type)
                      return Boolean is
      (Queue.Nodes (Item_Id).In_Tree);

end Oak.Storage.Slim_Time_Priority_Queue;
