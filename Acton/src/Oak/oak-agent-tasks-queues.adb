package body Oak.Agent.Tasks.Queues is
   procedure Set_Blank_Link
     (T : access Task_Agent'Class) is
   begin
      T.Queue_Link.Next := null;
      T.Queue_Link.Previous := null;
   end Set_Blank_Link;

   procedure Set_Next_Task
     (T, Next : access Task_Agent'Class) is
   begin
      T.Queue_Link.Next := Next;
   end Set_Next_Task;

   procedure Set_Prev_Task
     (T, Prev : access Task_Agent'Class) is
   begin
      T.Queue_Link.Previous := Prev;
   end Set_Prev_Task;

   procedure Set_Queue_Link
     (T, Prev, Next : access Task_Agent'Class) is
   begin
      T.Queue_Link.Next := Next;
      T.Queue_Link.Previous := Prev;
   end Set_Queue_Link;

end Oak.Agent.Tasks.Queues;
