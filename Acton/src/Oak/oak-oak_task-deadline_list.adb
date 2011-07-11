package body Oak.Oak_Task.Deadline_List is

   procedure Insert_Task_Forward
     (Start       : in Oak_Task_Handler;
      List_Head   : in out Oak_Task_Handler;
      Task_To_Add : in Oak_Task_Handler);

   procedure Insert_Task_Backwards
     (Start       : in Oak_Task_Handler;
      List_Head   : in out Oak_Task_Handler;
      Task_To_Add : in Oak_Task_Handler);

   -----------------------------------
   -- Insert_Task_Into_Dealine_List --
   --  Complexity O(n)              --
   -----------------------------------

   procedure Insert_Task
     (List_Head   : in out Oak_Task_Handler;
      Task_To_Add : Oak_Task_Handler)
   is
   begin
      Insert_Task_Forward
        (Start       => List_Head,
         List_Head   => List_Head,
         Task_To_Add => Task_To_Add);
   end Insert_Task;

   ------------------------------------
   -- Remove_Task_From_Deadline_List --
   ------------------------------------

   procedure Remove_Task
     (List_Head      : in out Oak_Task_Handler;
      Task_To_Remove : Oak_Task_Handler)
   is
      Next_Task     : constant Oak_Task_Handler :=
        Task_To_Remove.Deadline_List.Next;
      Previous_Task : constant Oak_Task_Handler :=
        Task_To_Remove.Deadline_List.Previous;
   begin
      if Next_Task /= null then
         Next_Task.Deadline_List.Previous := Previous_Task;
      end if;

      if Previous_Task = null then
         List_Head := Next_Task;
      else
         Previous_Task.Deadline_List.Next := Next_Task;
      end if;
   end Remove_Task;

   ---------------------------
   -- Task_Deadline_Updated --
   ---------------------------

   procedure Task_Deadline_Updated
     (List_Head    : in out Oak_Task_Handler;
      Updated_Task : Oak_Task_Handler)
   is
   begin

      if Updated_Task.Deadline_List.Next /= null
        and then Updated_Task.Deadline >
                 Updated_Task.Deadline_List.Next.Deadline
      then
         Remove_Task (List_Head => List_Head, Task_To_Remove => Updated_Task);
         Insert_Task_Forward
           (Start       => Updated_Task.Deadline_List.Next,
            List_Head   => List_Head,
            Task_To_Add => Updated_Task);
      elsif Updated_Task.Deadline_List.Previous /= null
        and then Updated_Task.Deadline <
                 Updated_Task.Deadline_List.Previous.Deadline
      then
         Remove_Task (List_Head => List_Head, Task_To_Remove => Updated_Task);
         Insert_Task_Backwards
           (Start       => Updated_Task.Deadline_List.Previous,
            List_Head   => List_Head,
            Task_To_Add => Updated_Task);
      end if;

   end Task_Deadline_Updated;

   ---------------------------
   -- Get_Earliest_Deadline --
   ---------------------------

   function Get_Earliest_Deadline
     (List_Head : Oak_Task_Handler)
      return      Time
   is
   begin
      if List_Head = null then
         return Time_Last;
      else
         return List_Head.Next_Deadline;
      end if;
   end Get_Earliest_Deadline;

   -------------------------
   -- Insert_Task_Forward --
   -------------------------

   procedure Insert_Task_Forward
     (Start       : in Oak_Task_Handler;
      List_Head   : in out Oak_Task_Handler;
      Task_To_Add : in Oak_Task_Handler)
   is
      Current_Task  : Oak_Task_Handler := Start;
      Previous_Task : Oak_Task_Handler;
   begin

      while Current_Task /= null
        and then Current_Task.Deadline <= Task_To_Add.Deadline
      loop
         Previous_Task := Current_Task;
         Current_Task := Current_Task.Deadline_List.Next;
      end loop;

      Task_To_Add.Deadline_List.Previous := Previous_Task;
      Task_To_Add.Deadline_List.Next     := Current_Task;

      if Current_Task /= null then
         Current_Task.Deadline_List.Previous := Task_To_Add;
      end if;

      if Previous_Task = null then
         List_Head := Task_To_Add;
      else
         Previous_Task.Deadline_List.Next := Task_To_Add;
      end if;

   end Insert_Task_Forward;

   procedure Insert_Task_Backwards
     (Start       : in Oak_Task_Handler;
      List_Head   : in out Oak_Task_Handler;
      Task_To_Add : in Oak_Task_Handler)
   is
      Current_Task : Oak_Task_Handler := Start;
      Next_Task    : Oak_Task_Handler;
   begin
      while Current_Task /= null
        and then Current_Task.Deadline > Task_To_Add.Deadline
      loop
         Next_Task := Current_Task;
         Current_Task := Current_Task.Deadline_List.Previous;
      end loop;

      Task_To_Add.Deadline_List.Previous := Current_Task;
      Task_To_Add.Deadline_List.Next     := Next_Task;

      if Next_Task /= null then
         Next_Task.Deadline_List.Previous := Task_To_Add;
      end if;

      if Current_Task = null then
         List_Head := Task_To_Add;
      else
         Current_Task.Deadline_List.Next := Task_To_Add;
      end if;
   end Insert_Task_Backwards;

end Oak.Oak_Task.Deadline_List;
