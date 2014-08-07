generic
   type Item_Type is private;
   pragma Preelaborable_Initialization (Item_Type);

   No_Item : Item_Type;

   Size : Positive;

   with function ">" (Left, Right : Item_Type) return Boolean is <>;

--  package Oak.Storage.Binary_Heap with Pure is
package Oak.Storage.Binary_Heap is
   pragma Pure;

   Heap_Capacity_Error : exception;

   type Heap_Type is private with Preelaborable_Initialization;

   procedure Add_Item (To_Heap : in out Heap_Type;
                       Item    : in Item_Type);
   --  Adds an item to the priority queue

   procedure Remove_Top (From_Heap : in out Heap_Type;
                         Item      : out Item_Type);
   --  Removes the first item from the priority queue. Returns an empty element
   --  if the queue is empty.

   function First_Item (From_Heap : in out Heap_Type) return Item_Type;

   function Is_Heap_Full (Heap : in Heap_Type) return Boolean;

private

   subtype Index_Type is Natural;

   type Heap_Array is array (Index_Type range 1 .. Size) of Item_Type;
   type Heap_Type is record
      Size  : Index_Type := 0;
      Items : Heap_Array;
   end record;

   function Is_Heap_Full (Heap : in Heap_Type) return Boolean is
     (Heap.Size = Heap.Items'Last);

   function First_Item (From_Heap : in out Heap_Type) return Item_Type is
     (if From_Heap.Size > 0 then From_Heap.Items (From_Heap.Items'First)
                            else No_Item);

end Oak.Storage.Binary_Heap;
