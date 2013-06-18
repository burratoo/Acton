package body Oak.Agent.Tasks.Queues is
   procedure Set_Blank_Task_Link
     (T : access Task_Agent'Class) is
   begin
      T.Queue_Link.Next := null;
      T.Queue_Link.Previous := null;
   end Set_Blank_Task_Link;

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

   procedure Set_Task_Queue_Link
     (T, Prev, Next : access Task_Agent'Class) is
   begin
      T.Queue_Link.Next := Next;
      T.Queue_Link.Previous := Prev;
   end Set_Task_Queue_Link;

   procedure Set_Blank_Deadline_Link
     (T : access Task_Agent'Class) is
   begin
      T.Deadline_List.Next := null;
      T.Deadline_List.Previous := null;
   end Set_Blank_Deadline_Link;

   procedure Set_Next_Deadline
     (T, Next : access Task_Agent'Class) is
   begin
      T.Deadline_List.Next := Next;
   end Set_Next_Deadline;

   procedure Set_Prev_Deadline
     (T, Prev : access Task_Agent'Class) is
   begin
      T.Deadline_List.Previous := Prev;
   end Set_Prev_Deadline;

   procedure Set_Deadline_Queue_Link
     (T, Prev, Next : access Task_Agent'Class) is
   begin
      T.Deadline_List.Next := Next;
      T.Deadline_List.Previous := Prev;
   end Set_Deadline_Queue_Link;
end Oak.Agent.Tasks.Queues;
