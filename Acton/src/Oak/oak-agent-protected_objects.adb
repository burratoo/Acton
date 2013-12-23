with Ada.Unchecked_Conversion;
with Oak.Agent.Queue;
with Oak.Scheduler;

with System; use System;
with Oak.Oak_Time; use Oak.Oak_Time;

package body Oak.Agent.Protected_Objects is

   procedure Initialise_Protected_Agent
     (Agent                 : not null access Protected_Agent'Class;
      Name                  : in String;
      Ceiling_Priority      : in Integer;
      Barriers_Function     : in Entry_Barrier_Function_Handler;
      Object_Record_Address : in System.Address) is
   begin

      Oak.Agent.Initialise_Agent
        (Agent                => Agent,
         Name                 => Name,
         Call_Stack_Address   => Null_Address,
         Call_Stack_Size      => 0,
         Run_Loop             => Null_Address,
         Run_Loop_Parameter   => Null_Address,
         Normal_Priority      => Ceiling_Priority,
         Initial_State        => Inactive,
         Wake_Time            => Time_First);

      Agent.Scheduler_Agent :=
        Scheduler.Find_Scheduler_For_System_Priority (Ceiling_Priority, 1);

      Agent.Entry_Barriers           := Barriers_Function;
      Agent.Object_Record            := Object_Record_Address;

      Agent.Entry_Queues             := (others => null);
      Agent.Active_Subprogram_Kind   := Protected_Procedure;
      Agent.Tasks_Within             := null;
      Agent.Contending_Tasks         := null;
   end Initialise_Protected_Agent;

   procedure Add_Contending_Task
     (PO : in out Protected_Agent'Class;
      T  : access Oak_Agent'Class) is
   begin
      Queue.Add_Agent_To_Tail
        (Queue => PO.Contending_Tasks,
         Agent => T);
   end Add_Contending_Task;

   procedure Add_Task_To_Entry_Queue
     (PO       : in out Protected_Agent'Class;
      T        : access Oak_Agent'Class;
      Entry_Id : Entry_Index) is
   begin
      Queue.Add_Agent_To_Tail
        (Queue => PO.Entry_Queues (Entry_Id),
         Agent => T);
   end Add_Task_To_Entry_Queue;

   procedure Add_Task_To_Protected_Object
     (PO : in out Protected_Agent'Class;
      T  : not null access Oak_Agent'Class) is
   begin
      Queue.Add_Agent_To_Head
        (Queue => PO.Tasks_Within,
         Agent => T);
   end Add_Task_To_Protected_Object;

   function Entry_Queue_Length
     (PO       : in Protected_Agent'Class;
      Entry_Id : in Entry_Index) return Natural
   is
      Head_Task    : constant access Oak_Agent'Class :=
                       PO.Entry_Queues (Entry_Id);
      Current_Task : access Oak_Agent'Class := Head_Task;
      Length       : Natural := 0;
   begin
      if Current_Task /= null then
         Length := Length + 1;
         while Queue.Next_Agent (Current_Task) /= Head_Task loop
            Length := Length + 1;
            Current_Task := Queue.Next_Agent (Current_Task);
         end loop;
      end if;
      return Length;
   end Entry_Queue_Length;

   procedure Get_And_Remove_Next_Contending_Task
     (PO        : in out Protected_Agent'Class;
      Next_Task : out Agent_Handler) is
   begin
      Next_Task := PO.Contending_Tasks;
      if Next_Task /= null then
         Queue.Remove_Agent_From_Head (Queue => PO.Contending_Tasks);
      end if;
   end Get_And_Remove_Next_Contending_Task;

   procedure Get_And_Remove_Next_Task_From_Entry_Queues
     (PO         : in out Protected_Agent'Class;
      Next_Task  : out Agent_Handler) is
   begin
      Next_Task := null;
      for Entry_Id in PO.Entry_Queues'Range loop
         Next_Task := PO.Entry_Queues (Entry_Id);
         if Next_Task /= null and then
           Is_Barrier_Open (PO, Entry_Id) then
            Queue.Remove_Agent
              (Queue => PO.Entry_Queues (Entry_Id),
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

   --  This function will need to be run in the protected object's virtual
   --  memory space. This should be enough to protected the kernel from
   --  any bad things.

   function Is_Barrier_Open
     (PO       : in out Protected_Agent'Class;
      Entry_Id : in Entry_Index)
      return Boolean is
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
     (PO : in Protected_Agent'Class;
      T  : not null access Oak_Agent'Class)
      return Boolean is
      Current_Task : access Oak_Agent'Class := PO.Tasks_Within;
   begin
      if Current_Task = null then
         return False;
      end if;

      Current_Task := Queue.Next_Agent (Current_Task);
      while Current_Task /= PO.Tasks_Within and Current_Task /= T loop
         Current_Task := Queue.Next_Agent (Current_Task);
      end loop;
      return Current_Task = T;
   end Is_Task_Inside_Protect_Object;

   procedure Purge_Entry_Queues
     (PO             : in out Protected_Agent'Class;
      New_Task_State : in     Agent_State)
   is
      Current_Task : access Oak_Agent'Class := null;
   begin
      for Queue_Head of PO.Entry_Queues loop
         Current_Task := Queue_Head;
         while Current_Task /= null loop
            Queue.Remove_Agent
              (Queue => Queue_Head,
               Agent => Current_Task);
            Current_Task.State := New_Task_State;
            Current_Task := Queue_Head;
         end loop;
      end loop;
   end Purge_Entry_Queues;

   procedure Remove_Task_From_Entry_Queue
     (PO       : in out Protected_Agent'Class;
      T        : access Oak_Agent'Class;
      Entry_Id : Entry_Index) is
   begin
      Queue.Remove_Agent
        (Queue => PO.Entry_Queues (Entry_Id),
         Agent => T);
   end Remove_Task_From_Entry_Queue;

   procedure Remove_Task_From_Protected_Object
     (PO : in out Protected_Agent'Class;
      T  : access Oak_Agent'Class) is
   begin
      Queue.Remove_Agent
        (Queue => PO.Tasks_Within,
         Agent => T);
   end Remove_Task_From_Protected_Object;

   type Protected_Record is record
      Agent : access Protected_Agent'Class;
   end record;

   type Protected_Subprogram_Components is record
      Object : access Protected_Record;
      Handler_Address : System.Address;
   end record;

   function Protected_Object_From_Access
     (Handler : Parameterless_Access)
      return access Protected_Agent'Class
   is
      function To_Protected_Subprogram_Components is
        new Ada.Unchecked_Conversion
          (Parameterless_Access, Protected_Subprogram_Components);
   begin
      return To_Protected_Subprogram_Components (Handler).Object.Agent;
   end Protected_Object_From_Access;

   function Protected_Object_From_Access
     (Handler : Ada.Cyclic_Tasks.Response_Handler)
      return access Protected_Agent'Class
   is
      function To_Protected_Subprogram_Components is
        new Ada.Unchecked_Conversion
          (Ada.Cyclic_Tasks.Response_Handler, Protected_Subprogram_Components);
   begin
      return To_Protected_Subprogram_Components (Handler).Object.Agent;
   end Protected_Object_From_Access;

end Oak.Agent.Protected_Objects;
