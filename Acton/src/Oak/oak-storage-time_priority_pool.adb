------------------------------------------------------------------------------
--                                                                          --
--                              OAK COMPONENTS                              --
--                                                                          --
--                      OAK.STORAGE.TIME_PRIORITY_POOL                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                 Copyright (C) 2013-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

package body Oak.Storage.Time_Priority_Pool is

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

   function Max_Priority_For_Subtree
     (Pool : in Pool_Type; Subtree : in Node_Location)
      return Priority_Type;
   --  Calculates the maximum priority that a node in the subtree possesses.

   procedure Node_Has_Updated
     (Pool    : in out Pool_Type;
      Node_Id : in Node_Location;
      Old_Key : in Key_Type);
   --  Checks to see if the node is in the right position in the tree after
   --  the time associated with the node has been updated.

   procedure Rotate_Left  (Pool    : in out Pool_Type;
                           Node_Id : in Node_Location);
   procedure Rotate_Right (Pool    : in out Pool_Type;
                           Node_Id : in Node_Location);
   --  Red-Black Tree procedures. Rotates the subtree head defined by the
   --  Node_Id left (right) such that it becomes the left (right) child of its
   --  right (left) child which itself becomes the new head node for the
   --  subtree.

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
         Pool.Free_List := Pool.Nodes (Pool.Free_List).Right;
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

         Pool.Nodes (Node_Id).Right := Pool.Free_List;
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
      N                : Node_Array renames In_Pool.Nodes;

      Current_Node     : Node_Location := In_Pool.Root;
      Selected_Node    : Node_Location;

   begin
      while Current_Node /= No_Node loop
         --  The current node has been selected as having a priority above the
         --  requested priority.

         Selected_Node := Current_Node;

         --  Try to find an earlier node that satisfies the requested priority.

         if N (Current_Node).Left_Priority > Above_Priority then
            --  There is an eariler node that statisfies the priority.

            Current_Node     := N (Current_Node).Left;

         elsif Priority (N (Current_Node).Element) > Above_Priority then
            --  This node is the earliest node that satisfies the priority
            --  requirement.

            Current_Node := No_Node;

         elsif N (Current_Node).Right_Priority > Above_Priority then
            --  The eariliest node that satisfies the priority requiement
            --  exists on the right side of the node (i.e. the earliest node
            --  that satisfies the priority requirement occurs after this
            --  node).

            Current_Node     := N (Current_Node).Right;

         else
            --  This means we are at a node where neither it or its children
            --  can satisfy the priority requirement. The only time this should
            --  occur is the root node.

            pragma Assert (Current_Node = In_Pool.Root);

            Current_Node  := No_Node;
            Selected_Node := No_Node;

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
      Old_Key : constant Key_Type := Key (Element => Element);
   begin
      Set_Time (Element, Time);
      if In_Tree (Pool, Item_Id) then
         Node_Has_Updated (Pool, Item_Id, Old_Key);
      end if;

   end Generic_Update_Time;

   -----------------
   -- Insert_Node --
   -----------------

   procedure Insert_Node (Pool : in out Pool_Type; Node : in Node_Location) is
      N                : Node_Array renames Pool.Nodes;
      Current_Node     : Node_Location := Pool.Root;
      Parent_Node      : Node_Location := No_Node;
      Grandparent_Node : Node_Location;
      Uncle_Node       : Node_Location;

      Node_Priority    : constant Priority_Type := Priority (N (Node).Element);

   begin
      --  Insert the node as a leaf in the appropriate spot.

      while Current_Node /= No_Node loop
         Parent_Node := Current_Node;

         if N (Node).Element < N (Current_Node).Element then
            if Node_Priority > N (Current_Node).Left_Priority then
               --  Update priority references on the way down.
               N (Current_Node).Left_Priority := Node_Priority;
            end if;

            Current_Node := N (Current_Node).Left;

         else
            if Node_Priority > N (Current_Node).Right_Priority then
               --  Update priority references on the way down.
               N (Current_Node).Right_Priority := Node_Priority;
            end if;

            Current_Node := N (Current_Node).Right;
         end if;
      end loop;

      N (Node).Parent := Parent_Node;

      if Parent_Node = No_Node then
         Pool.Root := Node;
      elsif N (Node).Element < N (Parent_Node).Element then
         N (Parent_Node).Left := Node;
      else
         N (Parent_Node).Right := Node;
      end if;

      --  Set Node properties

      N (Node).Left   := No_Node;
      N (Node).Right  := No_Node;
      N (Node).Colour := Red;

      N (Node).Left_Priority  := First_Priority;
      N (Node).Right_Priority := First_Priority;

      --  Repair tree due to insertion

      Current_Node := Node;

      while N (N (Current_Node).Parent).Colour = Red loop
         Parent_Node      := N (Current_Node).Parent;
         Grandparent_Node := N (Parent_Node).Parent;

         if Parent_Node = N (Grandparent_Node).Left then
            Uncle_Node := N (Grandparent_Node).Right;

            if N (Uncle_Node).Colour = Red then
               N (Parent_Node).Colour      := Black;
               N (Uncle_Node).Colour       := Black;
               N (Grandparent_Node).Colour := Red;

               Current_Node := Grandparent_Node;

            else
               if Current_Node = N (Parent_Node).Right then
                  Current_Node := Parent_Node;
                  Rotate_Left (Pool, Current_Node);
               end if;

               N (Parent_Node).Colour      := Black;
               N (Grandparent_Node).Colour := Red;
               Rotate_Right (Pool, Grandparent_Node);

            end if;

         else
            pragma Assert (Parent_Node = N (Grandparent_Node).Right);

            Uncle_Node := N (Grandparent_Node).Left;

            if N (Uncle_Node).Colour = Red then
               N (Parent_Node).Colour      := Black;
               N (Uncle_Node).Colour       := Black;
               N (Grandparent_Node).Colour := Red;

               Current_Node := Grandparent_Node;

            else
               if Current_Node = N (Parent_Node).Left then
                  Current_Node := Parent_Node;
                  Rotate_Right (Pool, Current_Node);
               end if;

               N (Parent_Node).Colour      := Black;
               N (Grandparent_Node).Colour := Red;
               Rotate_Left (Pool, Grandparent_Node);

            end if;
         end if;
      end loop;

      N (Pool.Root).Colour := Black;
      N (Node).In_Tree     := True;
   end Insert_Node;

   ------------------------------
   -- Max_Priority_For_Subtree --
   ------------------------------

   function Max_Priority_For_Subtree
     (Pool : in Pool_Type; Subtree : in Node_Location) return Priority_Type
   is
      N            : Node_Array renames Pool.Nodes;
      Max_Priority : Priority_Type;
   begin
      Max_Priority :=  Priority_Type'Max
          (Priority (N (Subtree).Element), N (Subtree).Left_Priority);
      Max_Priority :=
        Priority_Type'Max
          (Max_Priority, N (Subtree).Right_Priority);
      return Max_Priority;
   end Max_Priority_For_Subtree;

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

      Pool.Nodes (Item_Id) := (Colour         => Black,
                               Has_Element    => True,
                               In_Tree        => False,
                               Parent         => No_Node,
                               Left           => No_Node,
                               Right          => No_Node,
                               Left_Priority  => First_Priority,
                               Right_Priority => First_Priority,
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

      procedure Fixup        (X                : in out Node_Location;
                              Repair_Structure : in     Boolean);
      function  Minimum_Node (X    : in     Node_Location)
                              return Node_Location;
      procedure Transplant   (X, Y : in     Node_Location);

      -----------
      -- Fixup --
      -----------

      procedure Fixup (X : in out Node_Location; Repair_Structure : Boolean) is
         Sibling             : Node_Location;
         Structure_Repaired  : Boolean := not Repair_Structure;
      begin
         --  While there can exist a point where we no longer need to repair
         --  the tree, the same cannot be said for the priority references,
         --  particularly when a transplant has occured. Thus we still need
         --  to work our way to the root node.

         while X /= Pool.Root loop
            if X = N (N (X).Parent).Left then
               --  X is a left child

               --  Fix up priority references

               declare
                  Parent : Node_Type renames N (N (X).Parent);
               begin
                  if X = No_Node then
                     Parent.Left_Priority := First_Priority;
                  else
                     Parent.Left_Priority :=
                       Max_Priority_For_Subtree (Pool, X);
                  end if;
               end;

               --  Fix up tree structure

               if not Structure_Repaired then
                  Sibling := N (N (X).Parent).Right;

                  if N (Sibling).Colour = Red then
                     N (Sibling).Colour      := Black;
                     N (N (X).Parent).Colour := Red;
                     Rotate_Left (Pool, N (X).Parent);
                     Sibling := N (N (X).Parent).Right;
                  end if;

                  if N (N (Sibling).Left).Colour = Black
                    and then N (N (Sibling).Right).Colour = Black
                  then
                     N (Sibling).Colour := Red;

                  else
                     if N (N (Sibling).Right).Colour = Black then
                        N (N (Sibling).Left).Colour := Black;
                        N (Sibling).Colour          := Red;
                        Rotate_Right (Pool, Sibling);
                        Sibling := N (N (X).Parent).Right;
                     end if;

                     N (Sibling).Colour           := N (N (X).Parent).Colour;
                     N (N (X).Parent).Colour      := Black;
                     N (N (Sibling).Right).Colour := Black;

                     Rotate_Left (Pool, N (X).Parent);

                     Structure_Repaired := True;
                  end if;
               end if;

            else
               --  X is a left child

               pragma Assert (X = N (N (X).Parent).Right);

               --  Fix up priority references

               declare
                  Parent : Node_Type renames N (N (X).Parent);
               begin
                  if X = No_Node then
                     Parent.Right_Priority := First_Priority;
                  else
                     Parent.Right_Priority :=
                       Max_Priority_For_Subtree (Pool, X);
                  end if;
               end;

               --  Fix up tree structure

               if not Structure_Repaired then

                  Sibling := N (N (X).Parent).Left;

                  if N (Sibling).Colour = Red then
                     N (Sibling).Colour      := Black;
                     N (N (X).Parent).Colour := Red;
                     Rotate_Right (Pool, N (X).Parent);
                     Sibling := N (N (X).Parent).Left;
                  end if;

                  if N (N (Sibling).Right).Colour = Black
                    and then N (N (Sibling).Left).Colour = Black
                  then
                     N (Sibling).Colour := Red;

                  else
                     if N (N (Sibling).Left).Colour = Black then
                        N (N (Sibling).Right).Colour := Black;
                        N (Sibling).Colour          := Red;
                        Rotate_Left (Pool, Sibling);
                        Sibling := N (N (X).Parent).Left;
                     end if;

                     N (Sibling).Colour          := N (N (X).Parent).Colour;
                     N (N (X).Parent).Colour     := Black;
                     N (N (Sibling).Left).Colour := Black;

                     Rotate_Right (Pool, N (X).Parent);

                     Structure_Repaired := True;
                  end if;
               end if;
            end if;

            X := N (X).Parent;

            --  If the colour of the new node is black, the tree structure
            --  volations have been resolved. This is normally at the top of
            --  CLRS's algorithm as part of the while condition.

            if N (X).Colour = Black then
               Structure_Repaired := True;
            end if;

         end loop;

         --  The root node is always black.

         N (X).Colour := Black;

      end Fixup;

      ------------------
      -- Minimum_Node --
      ------------------

      function Minimum_Node (X : in Node_Location) return Node_Location is
         Min : Node_Location := X;
      begin
         while N (Min).Left /= No_Node loop
            Min := N (Min).Left;
         end loop;

         return Min;
      end Minimum_Node;

      ----------------
      -- Transplant --
      ----------------

      procedure Transplant (X, Y : in Node_Location) is
      begin
         if N (X).Parent = No_Node then
            Pool.Root := Y;
         elsif X = N (N (X).Parent).Left then
            N (N (X).Parent).Left := Y;
         else
            N (N (X).Parent).Right := Y;
         end if;

         N (Y).Parent := N (X).Parent;
      end Transplant;

      Replacement_Node : Node_Location;
      --  Node that replaces Node when it is removed from the tree.

      Fill_Node        : Node_Location;
      --  The node that fills the spot of the replacement node or the deleted
      --  node.

      Replacement_Original_Colour : Node_Colour;

   begin
      Replacement_Node            := Node;
      Replacement_Original_Colour := N (Replacement_Node).Colour;

      if N (Node).Left = No_Node then
         Fill_Node := N (Node).Right;
         Transplant (Node, Fill_Node);

      elsif N (Node).Right = No_Node then
         Fill_Node := N (Node).Left;
         Transplant (Node, Fill_Node);

      else
         Replacement_Node            := Minimum_Node (N (Node).Right);
         Replacement_Original_Colour := N (Replacement_Node).Colour;
         Fill_Node                   := N (Replacement_Node).Right;

         if N (Replacement_Node).Parent = Node then
            N (Fill_Node).Parent := Replacement_Node;

         else
            Transplant (Replacement_Node, N (Replacement_Node).Right);
            N (Replacement_Node).Right            := N (Node).Right;
            N (Replacement_Node).Right_Priority   := N (Node).Right_Priority;
            N (N (Replacement_Node).Right).Parent := Replacement_Node;
         end if;

         Transplant (Node, Replacement_Node);
         N (Replacement_Node).Left            := N (Node).Left;
         N (Replacement_Node).Left_Priority   := N (Node).Left_Priority;
         N (N (Replacement_Node).Left).Parent := Replacement_Node;
         N (Replacement_Node).Colour          := N (Node).Colour;
      end if;

      if Replacement_Original_Colour = Black then
         Fixup (Fill_Node, Repair_Structure => True);
      else
         Fixup (Fill_Node, Repair_Structure => False);
      end if;

      N (Node).In_Tree := False;
      N (Node).Parent  := No_Node;
      N (Node).Left    := No_Node;
      N (Node).Right   := No_Node;

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
      Old_Key : constant Key_Type := Key (Node.Element);
   begin
      Node.Element := Contents;
      if In_Tree (Pool, Item_Id) then
         Node_Has_Updated (Pool, Item_Id, Old_Key);
      end if;
   end Replace_Item;

   ----------------------
   -- Node_Has_Updated --
   ----------------------

   procedure Node_Has_Updated
     (Pool    : in out Pool_Type;
      Node_Id : in Node_Location;
      Old_Key : in Key_Type)
   is
      Node : Node_Type renames Pool.Nodes (Node_Id);
   begin
      if Key (Node.Element) = Old_Key then
         return;
      else
         Remove_Node (Pool, Node_Id);
         Insert_Node (Pool, Node_Id);
      end if;
   end Node_Has_Updated;

   -----------------
   -- Rotate_Left --
   -----------------

   procedure Rotate_Left
     (Pool    : in out Pool_Type;
      Node_Id : in Node_Location)
   is
      N        : Node_Array renames Pool.Nodes;
      Old_Head : constant Node_Location := Node_Id;
      New_Head : constant Node_Location := N (Old_Head).Right;

   begin
      --  The procedure takes this tree headed by Old_Head:
      --
      --                        \
      --                     Old_Head
      --                     /      \
      --                    A     New_Head
      --                          /      \
      --                         B        C
      --
      --  and transforms it into the tree headed by New_Head:
      --
      --                        \
      --                     New_Head
      --                     /      \
      --                Old_Head     C
      --                /       \
      --               A         B
      --

      N (Old_Head).Right := N (New_Head).Left;

      if N (New_Head).Left /= No_Node then
         N (N (New_Head).Left).Parent := Old_Head;
      end if;

      N (New_Head).Parent := N (Old_Head).Parent;

      if N (Old_Head).Parent = No_Node then
         Pool.Root := New_Head;

      elsif Old_Head = N (N (Old_Head).Parent).Left then
         N (N (Old_Head).Parent).Left := New_Head;

      else
         pragma Assert (Old_Head = N (N (Old_Head).Parent).Right);
         N (N (Old_Head).Parent).Right := New_Head;
      end if;

      N (New_Head).Left   := Old_Head;
      N (Old_Head).Parent := New_Head;

      --  Update priority branches

      N (Old_Head).Right_Priority := N (New_Head).Left_Priority;

      --  New_Head's left priority is the maximum of Old_Head's priority and
      --  Old_Head's left and right priorities.

      N (New_Head).Left_Priority := Max_Priority_For_Subtree (Pool, Old_Head);
   end Rotate_Left;

   ------------------
   -- Rotate_Right --
   -------------------

   --  The procedure takes this tree headed by Old_Head:
   --
   --                        \
   --                     Old_Head
   --                     /      \
   --                 New_Head    C
   --                /       \
   --               A         B
   --
   --  and transforms it into the tree headed by New_Head:
   --
   --                        \
   --                     New_Head
   --                     /      \
   --                    A     Old_Head
   --                          /      \
   --                         B        C
   --

   procedure Rotate_Right
     (Pool    : in out Pool_Type;
      Node_Id : in Node_Location)
   is
      N        : Node_Array renames Pool.Nodes;
      Old_Head : constant Node_Location := Node_Id;
      New_Head : constant Node_Location := N (Old_Head).Left;

   begin

      N (Old_Head).Left := N (New_Head).Right;

      if N (New_Head).Right /= No_Node then
         N (N (New_Head).Right).Parent := Old_Head;
      end if;

      N (New_Head).Parent := N (Old_Head).Parent;

      if N (Old_Head).Parent = No_Node then
         Pool.Root := New_Head;

      elsif Old_Head = N (N (Old_Head).Parent).Left then
         N (N (Old_Head).Parent).Left := New_Head;

      else
         pragma Assert (Old_Head = N (N (Old_Head).Parent).Right);
         N (N (Old_Head).Parent).Right := New_Head;
      end if;

      N (New_Head).Right   := Old_Head;
      N (Old_Head).Parent := New_Head;

      --  Update priority branches

      N (Old_Head).Left_Priority := N (New_Head).Right_Priority;

      --  New_Head's right priority is the maximum of Old_Head's priority and
      --  Old_Head's left and right priorities.

      N (New_Head).Right_Priority := Max_Priority_For_Subtree (Pool, Old_Head);
   end Rotate_Right;

end Oak.Storage.Time_Priority_Pool;
