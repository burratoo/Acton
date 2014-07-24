------------------------------------------------------------------------------
--                                                                          --
--                              OAK COMPONENTS                              --
--                                                                          --
--                      OAK.STORAGE.SLIM_PRIORITY_QUEUE                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                 Copyright (C) 2014-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

--  This is a slim priority queue since it doesn't store anything. Instead
--  the Item_Type is used to point not only to the node inside the queue, but
--  also to the item refered to outside the queue.

generic
   type Item_Type is mod <>;

   No_Item : Item_Type;

   with function ">" (Left, Right : Item_Type) return Boolean is <>;
   with function ">=" (Left, Right : Item_Type) return Boolean is <>;
package Oak.Storage.Slim_Priority_Queue with Pure is

   Queue_Capacity_Error : exception;

   type Queue_Type is private with Preelaborable_Initialization;

   procedure Enqueue_Item (To_Queue    : in out Queue_Type;
                           Item        : in Item_Type;
                           Add_To_Head : in Boolean := False);
   --  Adds an item to the priority queue

   function Head_Of_Queue (From_Queue : in out Queue_Type) return Item_Type;

   procedure Remove_Queue_Head (From_Queue : in out Queue_Type;
                                Item       : out Item_Type);

   --  Removes the first item from the priority queue. Returns an empty element
   --  if the queue is empty.

   procedure Remove_Item (Queue : in out Queue_Type; Node : in Item_Type);
   --  Removes the node from the Queue tree. Does not deallocate the node.

private

   No_Node_Value    : constant := 0;
   First_Node_Value : constant := No_Node_Value + 1;
   --  The constant that represents the No_Node. Needed to satisfy the
   --  preelaborable initialization constraints on the types below. ??? Should
   --  check to see if there is a bug in the compiler preventing the direct use
   --  of No_Node.

   No_Node          : constant Item_Type := No_Node_Value;
   --  The Item_Type that represents the null node, or that no node exists
   --  at all.

   type Node_Colour is (Red, Black);
   --  Colour of a node in the red-black tree.

   type Node_Type is record
   --  This type represents a Node in the tree. Each node has a link to its
   --  parent to allow a bottom-up transversal of the tree without using
   --  recursion. Its presence does not impact of the effective size of the
   --  node if the Item_Type and priority types' base type is a byte. On a
   --  32 bit aligned system should be able to pack the infrustructure
   --  components into just two registers, or two 32 bit words in memory, with
   --  two bytes free.

      Colour         : Node_Colour := Black;
      Parent         : Item_Type   := No_Node_Value;
      Left, Right    : Item_Type   := No_Node_Value;
   end record;

   type Node_Array is array (Item_Type) of Node_Type;

   type Queue_Type is record
   --  This type represents the set which is actually a red-black tree.

      Root : Item_Type := No_Node_Value;
      --  The node pointing to the root of the tree.

      First_Node       : Item_Type := No_Node_Value;
      First_Node_Valid : Boolean   := False;
      --  Caches the first node in the priority queue.

      Nodes : Node_Array;
      --  The node pool. The first node is the No_Node which needs to be
      --  initialised here.
   end record;

end Oak.Storage.Slim_Priority_Queue;
