with Oak.Oak_Task.Internal;
with Oak.Oak_Task.Queue;
with Oak.Core;
with Oak.Scheduler;

package body Oak.Oak_Task.Protected_Object is
   procedure Initialise_Protected_Object
     (PO                    : in Oak_Task_Handler;
      Name                  : in String;
      Ceiling_Priority      : in Integer;
      Barriers_Function     : in Entry_Barrier_Function_Handler;
      Has_Count_Attribute   : in Boolean;
      Object_Record_Address : in System.Address) is
   begin
      PO.Name_Length               :=
        Natural'Min (Task_Name'Length, Name'Length);
      PO.Name (1 .. PO.Name_Length) :=
        Name (Name'First .. Name'First + PO.Name_Length - 1);

      PO.all :=
        (Kind                   => Regular,
         Num_Entries            => PO.Num_Entries,
         Id                     => Internal.New_Task_Id,
         Name                   => PO.Name,
         Name_Length            => PO.Name_Length,
         State                  => Inactive,
         Shared_State           => No_Shared_State,
         Normal_Priority        => PO.Normal_Priority,
         Deadline               => Time_Span_Last,
         Cycle_Period           => Time_Span_Zero,
         Phase                  => Time_Span_Zero,
         Next_Deadline          => Time_First,
         Next_Run_Cycle         => Time_First,
         Wake_Time              => Time_First,
         Run_Loop               => Null_Address,
         Call_Stack             => (others => Null_Address),
         Scheduler_Agent        => null,
         Queue_Link             => Blank_Link,
         Deadline_List          => Blank_Link,
         Memory_List            => null,
         Activation_List        => null,
         Elaborated             => null,
         Message_Location       => null,
         Is_Protected_Object    => True,
         Has_Count_Attribute    => Has_Count_Attribute,
         Tasks_Within           => null,
         Active_Subprogram_Kind => Protected_Function,
         Entry_Barriers         => Barriers_Function,
         Entry_Queues           => (others => null),
         Object_Record          => Object_Record_Address,
         Controlling_Shared_State => Waiting);

      if Ceiling_Priority >= Any_Priority'First and
        Ceiling_Priority <= Any_Priority'Last
      then
         PO.Normal_Priority := System.Any_Priority (Ceiling_Priority);
      elsif Ceiling_Priority = Unspecified_Priority then
         PO.Normal_Priority := Any_Priority'Last;
      else
         raise Program_Error with "Priority out of range";
      end if;

      Oak.Scheduler.Add_New_Task_To_Inactive_List
        (Scheduler_Info => Core.Get_Scheduler_Info (Core.Get_Oak_Instance).all,
         T              => PO);
   end Initialise_Protected_Object;

   procedure Add_Task_To_Protected_Object
     (T  : in Oak_Task_Handler;
      PO : in Oak_Task_Handler) is
   begin
      Queue.Add_Task_To_Head (Queue => PO.Tasks_Within,
                              T     => T);
   end Add_Task_To_Protected_Object;

   procedure Remove_Task_From_Protected_Object
     (T  : in Oak_Task_Handler;
      PO : in Oak_Task_Handler) is
   begin
      Queue.Remove_Task (Queue => PO.Tasks_Within, T => T);
   end Remove_Task_From_Protected_Object;

   function Is_Task_Inside_Protect_Object (T  : in Oak_Task_Handler;
                                           PO : in Oak_Task_Handler)
                                           return Boolean is
      Current_Task : Oak_Task_Handler := PO.Tasks_Within;
   begin
      if Current_Task = null then
         return False;
      end if;

      Current_Task := Queue.Get_Next_Task (Current_Task);
      while Current_Task /= PO.Tasks_Within and Current_Task /= T loop
         Current_Task := Queue.Get_Next_Task (Current_Task);
      end loop;
      return Current_Task = T;
   end Is_Task_Inside_Protect_Object;

   function Get_Task_Within
     (PO : in Oak_Task_Handler) return Oak_Task_Handler is
   begin
      return PO.Tasks_Within;
   end Get_Task_Within;

   procedure Add_Task_To_Entry_Queue
     (PO       : in Oak_Task_Handler;
      T        : in Oak_Task_Handler;
      Entry_Id : Entry_Index) is
   begin
      Queue.Add_Task_To_Tail (Queue => PO.Entry_Queues (Entry_Id),
                                 T     => T);
   end Add_Task_To_Entry_Queue;

   procedure Remove_Task_From_Entry_Queue
     (PO       : in Oak_Task_Handler;
      T        : in Oak_Task_Handler;
      Entry_Id : Entry_Index) is
   begin
      Queue.Remove_Task (Queue => PO.Entry_Queues (Entry_Id),
                                 T     => T);
   end Remove_Task_From_Entry_Queue;

   procedure Get_And_Remove_Next_Task_From_Entry_Queues
     (PO : in Oak_Task_Handler;
      Next_Task  : out Oak_Task_Handler) is
   begin
      Next_Task := null;
      for Entry_Id in PO.Entry_Queues'Range loop
         Next_Task := PO.Entry_Queues (Entry_Id);
         if Next_Task /= null and then
           Is_Barrier_Open (PO, Entry_Id) then
            Queue.Remove_Task (Queue => PO.Entry_Queues (Entry_Id),
                               T     => Next_Task);
            exit;
         else
            Next_Task := null;
         end if;
      end loop;
   exception
      when Program_Error =>
         Next_Task := null;
   end Get_And_Remove_Next_Task_From_Entry_Queues;

   function Get_Acquiring_Tasks_State
     (For_Protected_Object : in Oak_Task_Handler)
      return Task_State is
   begin
      return For_Protected_Object.Controlling_Shared_State;
   end Get_Acquiring_Tasks_State;

   function Get_Reference_To_Acquiring_Tasks_State
     (For_Protected_Object : in Oak_Task_Handler)
      return Shared_Task_State is
   begin
      return For_Protected_Object.Controlling_Shared_State'Access;
   end Get_Reference_To_Acquiring_Tasks_State;

   procedure Set_Acquiring_Tasks_State
     (For_Protected_Object : in Oak_Task_Handler;
      To_State             : Task_State) is
   begin
      For_Protected_Object.Controlling_Shared_State := To_State;
   end Set_Acquiring_Tasks_State;

   function Entry_Queue_Length
     (PO : in Oak_Task_Handler;
      Entry_Id         : in Entry_Index) return Natural
   is
      Head_Task : constant Oak_Task_Handler
        := PO.Entry_Queues (Entry_Id);
      Current_Task : Oak_Task_Handler := Head_Task;
      Length       : Natural := 0;
   begin
      if Current_Task /= null then
         Length := Length + 1;
         while Current_Task.Queue_Link.Next /= Head_Task loop
            Length := Length + 1;
            Current_Task := Current_Task.Queue_Link.Next;
         end loop;
      end if;
      return Length;
   end Entry_Queue_Length;

   function Get_Active_Subprogram_Kind
     (PO : in Oak_Task_Handler) return Protected_Subprogram_Type is
   begin
      return PO.Active_Subprogram_Kind;
   end Get_Active_Subprogram_Kind;

   function Is_Barrier_Open
     (PO       : in Oak_Task_Handler;
      Entry_Id : in Entry_Index) return Boolean is
   begin
      return PO.Entry_Barriers (PO.Object_Record, Entry_Id);
   exception
      when others =>
         Purge_Entry_Queues (PO             => PO,
                             New_Task_State => Enter_PO_Refused);
         raise Program_Error;
   end Is_Barrier_Open;

   function Has_Count_Attribute (PO : in Oak_Task_Handler) return Boolean is
   begin
      return PO.Has_Count_Attribute;
   end Has_Count_Attribute;

   function Is_Protected_Object
     (PO : in Oak_Task_Handler) return Boolean is
   begin
      return PO.Is_Protected_Object;
   end Is_Protected_Object;

   function Is_Entry_Id_Valid
     (PO : in Oak_Task_Handler;
      Entry_Id         : in Entry_Index) return Boolean is
   begin
      return Entry_Id in PO.Entry_Queues'Range;
   end Is_Entry_Id_Valid;

   procedure Purge_Entry_Queues
     (PO                   : in Oak_Task_Handler;
      New_Task_State : in Task_State)
   is
      Current_Task : Oak_Task_Handler := null;
   begin
      for Entry_Id in PO.Entry_Queues'Range loop
         Current_Task := PO.Entry_Queues (Entry_Id);
         while Current_Task /= null loop
            Queue.Remove_Task (Queue => PO.Entry_Queues (Entry_Id),
                               T     => Current_Task);
            Current_Task.State := New_Task_State;
            Current_Task.Shared_State := No_Shared_State;
            Current_Task := PO.Entry_Queues (Entry_Id);
         end loop;
      end loop;
   end Purge_Entry_Queues;

end Oak.Oak_Task.Protected_Object;
