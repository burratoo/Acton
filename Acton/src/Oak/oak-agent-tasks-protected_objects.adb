with Oak.Agent.Tasks.Queues;
with Oak.Core;
with Oak.Scheduler;

with System; use System;

package body Oak.Agent.Tasks.Protected_Objects is

   package Queue renames Oak.Agent.Tasks.Queues.General;

   procedure Initialise_Protected_Agent
     (Agent                 : in out Protected_Agent'Class;
      Name                  : in String;
      Ceiling_Priority      : in Integer;
      Barriers_Function     : in Entry_Barrier_Function_Handler;
      Object_Record_Address : in System.Address) is
      No_Chain : Activation_Chain := (Head => null);
   begin

      Oak.Agent.Tasks.Initialise_Task_Agent
        (Agent             => Agent,
         Stack_Address     => Null_Address,
         Stack_Size        => 0,
         Name              => Name,
         Normal_Priority   => Ceiling_Priority,
         Relative_Deadline => Time_Span_Last,
         Cycle_Period      => Time_Span_Zero,
         Phase             => Time_Span_Zero,
         Run_Loop          => Null_Address,
         Task_Value_Record => Null_Address,
         Chain             => No_Chain,
         Elaborated        => null);

      Agent.Entry_Barriers           := Barriers_Function;
      Agent.Object_Record            := Object_Record_Address;
      Agent.Controlling_Shared_State := Waiting;

      Oak.Scheduler.Add_New_Task_To_Inactive_List
        (Scheduler_Info => Core.Scheduler_Info (Core.Oak_Instance).all,
         T              => Agent'Access);
   end Initialise_Protected_Agent;

   procedure Add_Task_To_Entry_Queue
     (PO       : not null access Protected_Agent'Class;
      T        : access Task_Agent'Class;
      Entry_Id : Entry_Index) is
   begin
      Queues.General.Add_Agent_To_Tail
        (Queue => Queue.Agent_Handler (PO.Entry_Queues (Entry_Id)),
         Agent => T);
   end Add_Task_To_Entry_Queue;

   procedure Add_Task_To_Protected_Object
     (PO : not null access Protected_Agent'Class;
      T  : access Task_Agent'Class) is
   begin
      Queues.General.Add_Agent_To_Head
        (Queue => Queue.Agent_Handler (PO.Tasks_Within),
         Agent => T);
   end Add_Task_To_Protected_Object;

   function Entry_Queue_Length
     (PO       : not null access Protected_Agent'Class;
      Entry_Id : in Entry_Index) return Natural
   is
      Head_Task    : constant access Task_Agent'Class :=
                       PO.Entry_Queues (Entry_Id);
      Current_Task : access Task_Agent'Class := Head_Task;
      Length       : Natural := 0;
   begin
      if Current_Task /= null then
         Length := Length + 1;
         while Current_Task.Queue_Link.Next /= Head_Task loop
            Length := Length + 1;
            Current_Task := Queues.Next_Task (Current_Task);
         end loop;
      end if;
      return Length;
   end Entry_Queue_Length;

   procedure Get_And_Remove_Next_Task_From_Entry_Queues
     (PO         : not null access Protected_Agent'Class;
      Next_Task  : out Task_Handler) is
   begin
      Next_Task := null;
      for Entry_Id in PO.Entry_Queues'Range loop
         Next_Task := Task_Handler (PO.Entry_Queues (Entry_Id));
         if Next_Task /= null and then
           Is_Barrier_Open (PO, Entry_Id) then
            Queues.General.Remove_Agent
              (Queue => Queue.Agent_Handler (PO.Entry_Queues (Entry_Id)),
               Agent => Next_Task);
            exit;
         else
            Next_Task := null;
         end if;
      end loop;
   exception
      when Program_Error =>
         Next_Task := null;
   end Get_And_Remove_Next_Task_From_Entry_Queues;

   function Is_Barrier_Open
     (PO       : not null access Protected_Agent'Class;
      Entry_Id : in Entry_Index) return Boolean is
   begin
      return PO.Entry_Barriers (PO.Object_Record, Entry_Id);
   exception
      when others =>
         Purge_Entry_Queues
           (PO             => PO,
            New_Task_State => Enter_PO_Refused);
         raise Program_Error;
   end Is_Barrier_Open;

   function Is_Task_Inside_Protect_Object
     (PO : not null access Protected_Agent'Class;
      T  : access Task_Agent'Class)
      return Boolean is
      Current_Task : access Task_Agent'Class := PO.Tasks_Within;
   begin
      if Current_Task = null then
         return False;
      end if;

      Current_Task := Queues.Next_Task (Current_Task);
      while Current_Task /= PO.Tasks_Within and Current_Task /= T loop
         Current_Task := Queues.Next_Task (Current_Task);
      end loop;
      return Current_Task = T;
   end Is_Task_Inside_Protect_Object;

   procedure Purge_Entry_Queues
     (PO             : not null access Protected_Agent'Class;
      New_Task_State : in Task_State)
   is
      Current_Task : access Task_Agent'Class := null;
   begin
      for Queue_Head of PO.Entry_Queues loop
         Current_Task := Queue_Head;
         while Current_Task /= null loop
            Queues.General.Remove_Agent
              (Queue => Queue.Agent_Handler (Queue_Head),
               Agent => Current_Task);
            Current_Task.State := New_Task_State;
            Current_Task.Shared_State := No_Shared_State;
            Current_Task := Queue_Head;
         end loop;
      end loop;
   end Purge_Entry_Queues;

   procedure Remove_Task_From_Entry_Queue
     (PO       : not null access Protected_Agent'Class;
      T        : access Task_Agent'Class;
      Entry_Id : Entry_Index) is
   begin
      Queues.General.Remove_Agent
        (Queue => Queue.Agent_Handler (PO.Entry_Queues (Entry_Id)),
         Agent     => T);
   end Remove_Task_From_Entry_Queue;

   procedure Remove_Task_From_Protected_Object
     (PO : not null access Protected_Agent'Class;
      T  : access Task_Agent'Class) is
   begin
      Queues.General.Remove_Agent
        (Queue => Queue.Agent_Handler (PO.Tasks_Within),
         Agent => T);
   end Remove_Task_From_Protected_Object;

   procedure Set_Acquiring_Tasks_State
     (For_Protected_Object : not null access Protected_Agent'Class;
      To_State             : in Task_State) is
   begin
      For_Protected_Object.Controlling_Shared_State := To_State;
   end Set_Acquiring_Tasks_State;

end Oak.Agent.Tasks.Protected_Objects;
