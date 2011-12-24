package body Oak.Oak_Task.Queue is
   procedure Add_Task_Before (Queue  : in out Oak_Task_Handler;
                       T      : in Oak_Task_Handler;
                       Before : in Oak_Task_Handler) is
   begin
      if Queue = null then
         Queue := T;
         T.Queue_Link := (Next => T,
                          Previous => T);
      else
         if Queue = Before then
            Queue := T;
         end if;
         declare
            Next_Task : constant Oak_Task_Handler := Before;
            Prev_Task : constant Oak_Task_Handler
              := Before.Queue_Link.Previous;
         begin
            T.Queue_Link := (Next => Next_Task,
                             Previous => Prev_Task);
            Next_Task.Queue_Link.Previous := Prev_Task;
            Prev_Task.Queue_Link.Next := Next_Task;
         end;
      end if;
   end Add_Task_Before;

   procedure Add_Task_After (Queue : in out Oak_Task_Handler;
                       T     : in Oak_Task_Handler;
                       After : in Oak_Task_Handler)
   is
   begin
      if Queue = null then
         Queue := T;
         T.Queue_Link := (Next => T,
                          Previous => T);
      else
         declare
            Next_Task : constant Oak_Task_Handler := After.Queue_Link.Next;
            Prev_Task : constant Oak_Task_Handler := After;
         begin
            T.Queue_Link := (Next => Next_Task,
                             Previous => Prev_Task);
            Next_Task.Queue_Link.Previous := Prev_Task;
            Prev_Task.Queue_Link.Next := Next_Task;
         end;
      end if;
   end Add_Task_After;

   procedure Add_Task_To_Head (Queue : in out Oak_Task_Handler;
                               T     : in Oak_Task_Handler) is
   begin
      Add_Task_Before (Queue => Queue,
                       T     => T,
                       Before => Queue);
   end Add_Task_To_Head;

   procedure Add_Task_To_Tail (Queue : in out Oak_Task_Handler;
                               T     : in Oak_Task_Handler) is
   begin
      if Queue = null then
         Add_Task_After (Queue => Queue,
                         T     => T,
                   After => null);
      else
         Add_Task_After (Queue => Queue,
                   T     => T,
                   After => Queue.Queue_Link.Previous);
      end if;
   end Add_Task_To_Tail;

   procedure Remove_Task (Queue : in out Oak_Task_Handler;
                          T     : in Oak_Task_Handler) is
   begin
      if T = T.Queue_Link.Next then
         Queue := null;
      else
         if Queue = T then
            Queue := Queue.Queue_Link.Next;
         end if;
         declare
            Next_Task : constant Oak_Task_Handler := Queue.Queue_Link.Next;
            Prev_Task : constant Oak_Task_Handler := Queue.Queue_Link.Previous;
         begin
            Next_Task.Queue_Link.Previous := Prev_Task;
            Prev_Task.Queue_Link.Next := Next_Task;
         end;
      end if;
      T.Queue_Link := Blank_Link;
   end Remove_Task;

   procedure Remove_Task_From_Head (Queue : in out Oak_Task_Handler) is
   begin
      Remove_Task (Queue => Queue, T => Queue);
   end Remove_Task_From_Head;

   procedure Remove_Task_From_Tail (Queue : in out Oak_Task_Handler) is
   begin
      Remove_Task (Queue => Queue, T => Queue.Queue_Link.Previous);
   end Remove_Task_From_Tail;

   function Get_Next_Task
     (T    : Oak_Task_Handler)
      return Oak_Task_Handler
   is
   begin
      return T.Queue_Link.Next;
   end Get_Next_Task;

   function Get_Prev_Task
     (T    : Oak_Task_Handler)
      return Oak_Task_Handler
   is
   begin
      return T.Queue_Link.Previous;
   end Get_Prev_Task;

   procedure Set_Next_Task (T, Next : in Oak_Task_Handler) is
   begin
      T.Queue_Link.Next := Next;
   end Set_Next_Task;

   procedure Set_Prev_Task (T, Prev : in Oak_Task_Handler) is
   begin
      T.Queue_Link.Previous := Prev;
   end Set_Prev_Task;

   procedure Set_Queue_Link (T, Prev, Next : in Oak_Task_Handler) is
   begin
      T.Queue_Link.Previous := Prev;
      T.Queue_Link.Next     := Next;
   end Set_Queue_Link;
end Oak.Oak_Task.Queue;
