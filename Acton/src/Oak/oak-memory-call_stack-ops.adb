package body Oak.Memory.Call_Stack.Ops is

   --------------------
   -- New_Call_Stack --
   --------------------

   procedure Allocate_Call_Stack
     (Stack            : out Call_Stack_Handler;
      Size_In_Elements : Storage_Elements.Storage_Count :=
        PSP_Stack.Call_Stack_Size)
   is
      Size : Storage_Elements.Storage_Count := Size_In_Elements;
   begin
      if (Size mod PSP_Stack.Call_Stack_Allignment) /= 0 then
         Size := (Size / PSP_Stack.Call_Stack_Allignment + 1) *
                 PSP_Stack.Call_Stack_Allignment;
      end if;
      Stack.Top := Stack_Pool_Bottom;
      Stack.Pointer := Stack_Pool_Bottom;
      Stack_Pool_Bottom := Stack_Pool_Bottom - Size;
      Stack.Bottom := Stack_Pool_Bottom;
   end Allocate_Call_Stack;

end Oak.Memory.Call_Stack.Ops;
