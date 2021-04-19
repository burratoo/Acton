------------------------------------------------------------------------------------------
--                                                                                      --
--                                    OAK COMPONENTS                                    --
--                                                                                      --
--                                OAK.STORAGE.BINARY_HEAP                               --
--                                                                                      --
--                       Copyright (C) 2014-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

package body Oak.Storage.Binary_Heap is

   procedure Add_Item (To_Heap : in out Heap_Type;
                       Item    : in Item_Type)
   is
      J : Index_Type;
   begin
      if Is_Heap_Full (To_Heap) then
         raise Heap_Capacity_Error;
      end if;

      To_Heap.Size := To_Heap.Size + 1;
      J := To_Heap.Size;

      while J / 2 /= 0 and then To_Heap.Items (J / 2) > Item loop
         To_Heap.Items (J) := To_Heap.Items (J / 2);
         J := J / 2;
      end loop;

      To_Heap.Items (J) := Item;
   end Add_Item;

   procedure Remove_Top (From_Heap : in out Heap_Type;
                         Item      : out Item_Type)
   is
      J         : Index_Type := 1;
      Child     : Index_Type;
      Heap_Size : Index_Type renames From_Heap.Size;
      Items     : Heap_Array renames From_Heap.Items;
      Last_Item : constant Index_Type := Heap_Size;
   begin
      if Heap_Size = 0 then
         Item := No_Item;
         return;
      end if;

      Item := Items (Items'First);
      Heap_Size := Heap_Size - 1;

      loop
         --  Find the smaller child if it exists

         Child := J * 2;
         exit when Child > Heap_Size;

         if Child /= Heap_Size and then Items (Child) > Items (Child + 1) then
            Child := Child + 1;
         end if;

         --  Push the current item down one level of the tree if needed

         exit when not (Items (Last_Item) > Items (Child));
         Items (J) := Items (Child);
         J := Child;
      end loop;

      --  Move the last item to its new home.
      Items (J) := Items (Last_Item);
   end Remove_Top;

end Oak.Storage.Binary_Heap;
