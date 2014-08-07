------------------------------------------------------------------------------
--                                                                          --
--                              OAK COMPONENTS                              --
--                                                                          --
--                      OAK.STORAGE.SLIM_PRIORITY_QUEUE                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                 Copyright (C) 2014-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

package body Oak.Storage.Slim_Priority_Queue is
   pragma Suppress (All_Checks);

   --  ???? TODO: Need to write some code to verify this actually works. Spark
   --  should help.

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Insert_Node (Queue       : in out Queue_Type;
                          Node        : in Item_Type;
                          Add_To_Head : in Boolean);
   --  Insert a node in to the storage Queue tree.

   procedure Remove_Node (Queue : in out Queue_Type; Node : in Item_Type);
   --  Removes the node from the Queue tree. Does not deallocate the node.

   procedure Rotate_Left  (Queue   : in out Queue_Type;
                           Node_Id : in Item_Type);
   procedure Rotate_Right (Queue   : in out Queue_Type;
                           Node_Id : in Item_Type);
   --  Red-Black Tree procedures. Rotates the subtree head defined by the
   --  Node_Id left (right) such that it becomes the left (right) child of its
   --  right (left) child which itself becomes the new head node for the
   --  subtree.

   ------------------
   -- Enqueue_Item --
   ------------------

   procedure Enqueue_Item (To_Queue    : in out Queue_Type;
                           Item        : in Item_Type;
                           Add_To_Head : in Boolean := False)
   is
      Node : constant Item_Type := Item;
   begin
      Insert_Node (Queue       => To_Queue,
                   Node        => Node,
                   Add_To_Head => Add_To_Head);
   end Enqueue_Item;

   -------------------
   -- Head_Of_Queue --
   -------------------

   function Head_Of_Queue (From_Queue : in out Queue_Type) return Item_Type is
      Node : Item_Type := From_Queue.Root;
      N    : Node_Array renames From_Queue.Nodes;
   begin
      if not From_Queue.First_Node_Valid then
         while N (Node).Left /= No_Node loop
            Node := N (Node).Left;
         end loop;

         From_Queue.First_Node := Node;
         From_Queue.First_Node_Valid := True;
      end if;

      return From_Queue.First_Node;
   end Head_Of_Queue;

   -----------------
   -- Insert_Node --
   -----------------

   procedure Insert_Node (Queue       : in out Queue_Type;
                          Node        : in Item_Type;
                          Add_To_Head : in Boolean)
   is
      N                : Node_Array renames Queue.Nodes;
      Current_Node     : Item_Type := Queue.Root;
      Parent_Node      : Item_Type := No_Node;
      Grandparent_Node : Item_Type;
      Uncle_Node       : Item_Type;

   begin
      --  Insert the node as a leaf in the appropriate spot.

      if Add_To_Head then
         while Current_Node /= No_Node loop
            Parent_Node := Current_Node;

            if Node >= Current_Node then
               Current_Node := N (Current_Node).Left;
            else
               Current_Node := N (Current_Node).Right;
            end if;
         end loop;

         N (Node).Parent := Parent_Node;

         if Parent_Node = No_Node then
            Queue.Root := Node;
         elsif Node >= Parent_Node then
            N (Parent_Node).Left := Node;
         else
            N (Parent_Node).Right := Node;
         end if;
      else
         while Current_Node /= No_Node loop
            Parent_Node := Current_Node;

            if Node > Current_Node then
               Current_Node := N (Current_Node).Left;
            else
               Current_Node := N (Current_Node).Right;
            end if;
         end loop;

         N (Node).Parent := Parent_Node;

         if Parent_Node = No_Node then
            Queue.Root := Node;
         elsif Node > Parent_Node then
            N (Parent_Node).Left := Node;
         else
            N (Parent_Node).Right := Node;
         end if;
      end if;
      --  Set Node properties

      N (Node).Left   := No_Node;
      N (Node).Right  := No_Node;
      N (Node).Colour := Red;

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
                  Rotate_Left (Queue, Current_Node);
               end if;

               Parent_Node      := N (Current_Node).Parent;
               Grandparent_Node := N (Parent_Node).Parent;

               N (Parent_Node).Colour      := Black;
               N (Grandparent_Node).Colour := Red;
               Rotate_Right (Queue, Grandparent_Node);

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
                  Rotate_Right (Queue, Current_Node);
               end if;

               Parent_Node      := N (Current_Node).Parent;
               Grandparent_Node := N (Parent_Node).Parent;

               N (Parent_Node).Colour      := Black;
               N (Grandparent_Node).Colour := Red;
               Rotate_Left (Queue, Grandparent_Node);

            end if;
         end if;
      end loop;

      N (Queue.Root).Colour := Black;

      N (No_Node).Parent := No_Node;
      N (No_Node).Left   := No_Node;
      N (No_Node).Right  := No_Node;

      Queue.First_Node_Valid := False;
   end Insert_Node;

   -----------------
   -- Remove_Item --
   -----------------

   procedure Remove_Item (Queue : in out Queue_Type; Node : in Item_Type)
     renames Remove_Node;

   -----------------
   -- Remove_Node --
   -----------------

   procedure Remove_Node (Queue : in out Queue_Type; Node : in Item_Type)
   is
      N : Node_Array renames Queue.Nodes;

      procedure Fixup        (X                : in out Item_Type;
                              Repair_Structure : in     Boolean);
      function  Minimum_Node (X    : in     Item_Type)
                              return Item_Type;
      procedure Transplant   (X, Y : in     Item_Type);

      -----------
      -- Fixup --
      -----------

      procedure Fixup (X : in out Item_Type; Repair_Structure : Boolean) is
         Sibling             : Item_Type;
         Structure_Repaired  : Boolean := not Repair_Structure;
      begin
         while X /= Queue.Root and then N (X).Colour = Black loop
            if X = N (N (X).Parent).Left then
               --  X is a left child

               --  Fix up tree structure

               if not Structure_Repaired then
                  Sibling := N (N (X).Parent).Right;

                  if N (Sibling).Colour = Red then
                     N (Sibling).Colour      := Black;
                     N (N (X).Parent).Colour := Red;
                     Rotate_Left (Queue, N (X).Parent);
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
                        Rotate_Right (Queue, Sibling);
                        Sibling := N (N (X).Parent).Right;
                     end if;

                     N (Sibling).Colour           := N (N (X).Parent).Colour;
                     N (N (X).Parent).Colour      := Black;
                     N (N (Sibling).Right).Colour := Black;

                     Rotate_Left (Queue, N (X).Parent);

                     Structure_Repaired := True;
                  end if;
               end if;

            else
               --  X is a left child

               pragma Assert (X = N (N (X).Parent).Right);

               --  Fix up tree structure

               if not Structure_Repaired then

                  Sibling := N (N (X).Parent).Left;

                  if N (Sibling).Colour = Red then
                     N (Sibling).Colour      := Black;
                     N (N (X).Parent).Colour := Red;
                     Rotate_Right (Queue, N (X).Parent);
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
                        Rotate_Left (Queue, Sibling);
                        Sibling := N (N (X).Parent).Left;
                     end if;

                     N (Sibling).Colour          := N (N (X).Parent).Colour;
                     N (N (X).Parent).Colour     := Black;
                     N (N (Sibling).Left).Colour := Black;

                     Rotate_Right (Queue, N (X).Parent);

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

      function Minimum_Node (X : in Item_Type) return Item_Type is
         Min : Item_Type := X;
      begin
         while N (Min).Left /= No_Node loop
            Min := N (Min).Left;
         end loop;

         return Min;
      end Minimum_Node;

      ----------------
      -- Transplant --
      ----------------

      procedure Transplant (X, Y : in Item_Type) is
      begin
         if N (X).Parent = No_Node then
            Queue.Root := Y;
         elsif X = N (N (X).Parent).Left then
            N (N (X).Parent).Left := Y;
         else
            N (N (X).Parent).Right := Y;
         end if;

         N (Y).Parent := N (X).Parent;
      end Transplant;

      Replacement_Node : Item_Type;
      --  Node that replaces Node when it is removed from the tree.

      Fill_Node        : Item_Type;
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
            N (N (Replacement_Node).Right).Parent := Replacement_Node;
         end if;

         Transplant (Node, Replacement_Node);
         N (Replacement_Node).Left            := N (Node).Left;
         N (N (Replacement_Node).Left).Parent := Replacement_Node;
         N (Replacement_Node).Colour          := N (Node).Colour;
      end if;

      if Replacement_Original_Colour = Black then
         Fixup (Fill_Node, Repair_Structure => True);
      else
         Fixup (Fill_Node, Repair_Structure => False);
      end if;

      N (Node).Parent := No_Node;
      N (Node).Left   := No_Node;
      N (Node).Right  := No_Node;

      Queue.First_Node_Valid := False;
   end Remove_Node;

   -----------------------
   -- Remove_Queue_Head --
   -----------------------

   procedure Remove_Queue_Head (From_Queue : in out Queue_Type;
                                Item       : out Item_Type) is
   begin
      Item := Head_Of_Queue (From_Queue);

      if Item /= No_Item then
         Remove_Node (From_Queue, Item);
      end if;
   end Remove_Queue_Head;

   -----------------
   -- Rotate_Left --
   -----------------

   procedure Rotate_Left
     (Queue   : in out Queue_Type;
      Node_Id : in Item_Type)
   is
      N        : Node_Array renames Queue.Nodes;
      Old_Head : constant Item_Type := Node_Id;
      New_Head : constant Item_Type := N (Old_Head).Right;

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
         Queue.Root := New_Head;

      elsif Old_Head = N (N (Old_Head).Parent).Left then
         N (N (Old_Head).Parent).Left := New_Head;

      else
         pragma Assert (Old_Head = N (N (Old_Head).Parent).Right);
         N (N (Old_Head).Parent).Right := New_Head;
      end if;

      N (New_Head).Left   := Old_Head;
      N (Old_Head).Parent := New_Head;
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
     (Queue    : in out Queue_Type;
      Node_Id  : in Item_Type)
   is
      N        : Node_Array renames Queue.Nodes;
      Old_Head : constant Item_Type := Node_Id;
      New_Head : constant Item_Type := N (Old_Head).Left;

   begin

      N (Old_Head).Left := N (New_Head).Right;

      if N (New_Head).Right /= No_Node then
         N (N (New_Head).Right).Parent := Old_Head;
      end if;

      N (New_Head).Parent := N (Old_Head).Parent;

      if N (Old_Head).Parent = No_Node then
         Queue.Root := New_Head;

      elsif Old_Head = N (N (Old_Head).Parent).Left then
         N (N (Old_Head).Parent).Left := New_Head;

      else
         pragma Assert (Old_Head = N (N (Old_Head).Parent).Right);
         N (N (Old_Head).Parent).Right := New_Head;
      end if;

      N (New_Head).Right  := Old_Head;
      N (Old_Head).Parent := New_Head;
   end Rotate_Right;

end Oak.Storage.Slim_Priority_Queue;
