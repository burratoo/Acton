------------------------------------------------------------------------------
--                                                                          --
--                              OAK COMPONENTS                              --
--                                                                          --
--                        OAK.AGENT.PROTECTED_OBJECTS                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                 Copyright (C) 2011-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;

with Oak.Agent.Oak_Agent;         use Oak.Agent.Oak_Agent;
with Oak.Agent.Protected_Objects; use Oak.Agent.Protected_Objects;
with Oak.Agent.Tasks;             use Oak.Agent.Tasks;
with Oak.Oak_Time;                use Oak.Oak_Time;
with Oak.Scheduler;               use Oak.Scheduler;

with Oak.Processor_Support_Package; use Oak.Processor_Support_Package;
with Oak.Core_Support_Package.Task_Support;
use  Oak.Core_Support_Package.Task_Support;

package body Oak.Agent.Protected_Objects is

   procedure Task_Action
     (Protected_Object : in  Protected_Id;
      Open_Entry       : out Entry_Index;
      Exception_Raised : out Boolean;
      Preference       : in  Entry_Index := No_Entry)
     with Export, Convention => Ada;

   -------------------------
   -- Add_Contending_Task --
   -------------------------

   procedure Add_Contending_Task
     (PO : in Protected_Id;
      T  : in Task_Id)
   is
      P : Protected_Agent_Record renames Agent_Pool (PO);
   begin
      if P.Contending_Tasks.Head = No_Agent then
         P.Contending_Tasks.Head := T;

      else
         pragma Assert (P.Contending_Tasks.Tail /= No_Agent);

         Set_Next_Agent (For_Agent  => P.Contending_Tasks.Tail,
                         Next_Agent => T);
      end if;

      P.Contending_Tasks.Tail := T;

   end Add_Contending_Task;

   -----------------------------
   -- Add_Task_To_Entry_Queue --
   -----------------------------

   procedure Add_Task_To_Entry_Queue
     (PO       : in Protected_Id;
      T        : in Task_Id;
      Entry_Id : Entry_Index)
   is
      P : Protected_Agent_Record renames Agent_Pool (PO);

      Q, Prev_Q      : Task_Id_With_No;
   begin
      Set_Next_Queue  (T, No_Agent);
      Set_Next_Agent  (T, No_Agent);

      Q := P.Entry_Queues;

      if Q = No_Agent then
         P.Entry_Queues := T;
      else
         Prev_Q := No_Agent;

         while Q /= No_Agent and then Id_Of_Entry (Q) /= Entry_Id loop
            Prev_Q := Q;
            Q      := Next_Queue (For_Task => Q);
         end loop;

         if Q = No_Agent then
            --  No Agent representing that queue has been found. Append the
            --  Task Agent to the end of the queue list

            Set_Next_Queue (For_Task   => Prev_Q,
                            Next_Queue => T);

         elsif Id_Of_Entry (Q) /= No_Entry then
            --  There is a Task Agent representing the queue. Find the end of
            --  this queue.

            while Q /= No_Agent loop
               Prev_Q := Q;
               Q      := Next_Agent (Q);
            end loop;

            Set_Next_Agent (For_Agent => Prev_Q, Next_Agent => T);

         else
            --  There should not be a case where we found an Agent queue head
            --  but no Entry Id attached.

            pragma Assert (False);

         end if;

      end if;
   end Add_Task_To_Entry_Queue;

   ----------------------------------
   -- Add_Task_To_Protected_Object --
   ----------------------------------

   procedure Add_Task_To_Protected_Object
     (PO : in Protected_Id;
      T  : in Task_Id)
   is
      P : Protected_Agent_Record renames Agent_Pool (PO);
   begin
      if P.Tasks_Within.Head = No_Agent then
         P.Tasks_Within.Head := T;

      else
         pragma Assert (P.Tasks_Within.Tail /= No_Agent);

         Set_Next_Agent (For_Agent  => P.Tasks_Within.Tail,
                         Next_Agent => T);
      end if;

      P.Tasks_Within.Tail := T;
   end Add_Task_To_Protected_Object;

   ------------------------
   -- Entry_Queue_Length --
   ------------------------

   function Entry_Queue_Length
     (PO       : in Protected_Id;
      Entry_Id : in Entry_Index)
      return Natural
   is
      P : Protected_Agent_Record renames Agent_Pool (PO);

      Length         : Natural := 0;
      T              : Task_Id_With_No;
      Q              : Task_Id_With_No := P.Entry_Queues;
      Queue_Entry_Id : Entry_Index;

   begin
      --  If there is no queues at all, then the queue length is 0.

      if Q = No_Agent then
         return Length;
      end if;

      --  Find the queue

      Queue_Entry_Id := Id_Of_Entry (For_Task => Q);

      while Q /= No_Agent and then Queue_Entry_Id /= Entry_Id loop
         Q := Next_Queue (For_Task => Q);
         if Q /= No_Agent then
            Queue_Entry_Id := Id_Of_Entry (For_Task => Q);
         end if;
      end loop;

      --  Count the number of tasks on the queue.

      if Q = No_Agent then
         return Length;
      else
         T := Q;
         while T /= No_Agent loop
            Length := Length + 1;
            T      := Next_Agent (T);
         end loop;

         return Length;
      end if;
   end Entry_Queue_Length;

   ---------------------
   -- Find_Open_Entry --
   ---------------------

   procedure Find_Open_Entry
     (Protected_Object : in  Protected_Id;
      Open_Entry       : out Entry_Index;
      Exception_Raised : out Boolean;
      Preference       : in  Entry_Index := No_Entry)
   is

   begin

      --  This procedure operates in the context of the current agent.
      --  Switching across requires the callee registers to be saved since we
      --  cannot trust the agent not to damage them.

      Context_Switch_Will_Switch_In_Place;
      Context_Switch_Save_Callee_Registers;

      Task_Action
        (Protected_Object => Protected_Object,
         Open_Entry       => Open_Entry,
         Exception_Raised => Exception_Raised,
         Preference       => Preference);

      Context_Switch;
   end Find_Open_Entry;

   -----------------------------------------
   -- Get_And_Remove_Next_Contending_Task --
   -----------------------------------------

   procedure Get_And_Remove_Next_Contending_Task
     (PO        : in Protected_Id;
      Next_Task : out Task_Id_With_No)
   is
      P : Protected_Agent_Record renames Agent_Pool (PO);

   begin
      Next_Task := P.Contending_Tasks.Head;

      if P.Contending_Tasks.Head = P.Contending_Tasks.Tail then
         --  This task was the only task on the list.
         P.Contending_Tasks := Empty_List;

      elsif Next_Agent (Next_Task) /= No_Agent then
         --  Still more tasks on the list
         P.Contending_Tasks.Head := Next_Agent (Next_Task);

      else
         --  Do nothing if the list is empty (Next_Task will be set to No_Agent
         --  already.
         null;
      end if;

   end Get_And_Remove_Next_Contending_Task;

   -----------------------------------------------
   -- Get_And_Remove_Next_Task_From_Entry_Queue --
   -----------------------------------------------

   procedure Get_And_Remove_Next_Task_From_Entry_Queue
     (PO        : in Protected_Id;
      Entry_Id  : in Entry_Index;
      Next_Task : out Task_Id)
   is
      P : Protected_Agent_Record renames Agent_Pool (PO);

      Prev_Q         : Task_Id_With_No := No_Agent;
      Q              : Task_Id_With_No := P.Entry_Queues;
      New_Q          : Task_Id_With_No := P.Entry_Queues;

   begin
      --  Find the queue.
      --  TODO : Need some mechanism to handle the problem if a malicious task
      --  has sent an index that is either not valid or points to a queue with
      --  no one on it.

      while Q /= No_Agent and then Id_Of_Entry (For_Task => Q) /= Entry_Id loop
         Prev_Q := Q;
         Q      := Next_Queue (Q);
      end loop;

      if Q = No_Agent then
         raise Program_Error;
      end if;

      --  Pull the first task off the queue

      --  Prev_Q needs updating to point to the next queue head. This will be
      --  the next task in the entry queue that Q was in or the next queue
      --  if the entry queue that Q belong to is now empty.

      New_Q := Next_Agent (Q);

      if New_Q = No_Agent then
         --  The queue that Q belong to was empty, pick next queue. It does not
         --  matter if that queue is empty.

         New_Q := Next_Queue (Q);
      else
         --  The new queue head points to the same task as the old queue head
         --  did.
         Set_Next_Queue (For_Task   => New_Q,
                         Next_Queue => Next_Queue (Q));
      end if;

      --  Fix previous node's next queue pointer

      if P.Entry_Queues = Q then
         --  If Q was the first entry in the queue list, fix head reference.
         P.Entry_Queues := New_Q;
      else
         --  Otherwise fix the previous Q item reference.
         Set_Next_Queue (For_Task => Prev_Q, Next_Queue => New_Q);
      end if;

      --  Next_Task is simply Q.

      Next_Task := Q;
      Set_Next_Agent (For_Agent => Q, Next_Agent => No_Agent);
      Set_Next_Queue (For_Task  => Q, Next_Queue => No_Agent);
   end Get_And_Remove_Next_Task_From_Entry_Queue;

   -----------------------------------
   -- Is_Task_Inside_Protect_Object --
   -----------------------------------

   function Is_Task_Inside_Protect_Object
     (PO : in Protected_Id;
      T  : in Task_Id)
      return Boolean
   is
      P : Protected_Agent_Record renames Agent_Pool (PO);

      Current_Task : Task_Id_With_No := P.Tasks_Within.Head;
   begin
      while Current_Task /= No_Agent and then Current_Task /= T loop
         Current_Task := Next_Agent (Current_Task);
      end loop;

      return Current_Task /= No_Agent;
   end Is_Task_Inside_Protect_Object;

   -------------------------
   -- New_Protected_Agent --
   -------------------------

   procedure New_Protected_Agent
     (Agent                 : out Protected_Id;
      Name                  : in String;
      Ceiling_Priority      : in Integer;
      Barriers_Function     : in Address;
      Object_Record_Address : in Address) is
   begin
      Allocate_An_Agent (Agent);

      Setup_Oak_Agent : declare
         SA : Scheduler_Id_With_No;
         P  : Any_Priority;
      begin
         if Ceiling_Priority in Any_Priority then
            P := System.Any_Priority (Ceiling_Priority);
         elsif Ceiling_Priority = Unspecified_Priority then
            P := Interrupt_Priority'First;
         else
            raise Program_Error with "Priority out of range";
         end if;

         SA := Scheduler.Find_Scheduler_For_System_Priority (P, 1);

         New_Agent
           (Agent              => Agent,
            Name               => Name,
            Call_Stack_Address => Null_Address,
            Call_Stack_Size    => 0,
            Run_Loop           => Null_Address,
            Run_Loop_Parameter => Null_Address,
            Normal_Priority    => P,
            Initial_State      => Inactive,
            Scheduler_Agent    => SA,
            Wake_Time          => Time_First);
      end Setup_Oak_Agent;

      Setup_Protected_Agent : declare
         P : Protected_Agent_Record renames Agent_Pool (Agent);
      begin
         P.Object_Record          := Object_Record_Address;
         P.Entry_Barriers         := Barriers_Function;
         P.Entry_Queues           := No_Agent;
         P.Active_Subprogram_Kind := Protected_Procedure;
         P.Tasks_Within           := Empty_List;
         P.Contending_Tasks       := Empty_List;
      end Setup_Protected_Agent;
   end New_Protected_Agent;

   ------------------------
   -- Purge_Entry_Queues --
   ------------------------

   procedure Purge_Entry_Queues
     (PO             : in Protected_Id;
      New_Task_State : in Agent_State)
   is
      P : Protected_Agent_Record renames Agent_Pool (PO);

      Q         : Task_Id_With_No := P.Entry_Queues;
      Next_Q    : Task_Id_With_No;
      T, Next_T : Task_Id_With_No;
   begin
      while Q /= No_Agent loop
         Next_Q := Next_Queue (Q);
         T      := Q;

         while T /= No_Agent loop
            Next_T := Next_Agent (T);

            Set_Next_Agent (For_Agent => T, Next_Agent => No_Agent);
            Set_Next_Queue (For_Task  => T, Next_Queue => No_Agent);
            Set_Id_Of_Entry (For_Task => T, Entry_Id => No_Entry);
            Set_State (For_Agent => T, State => New_Task_State);
            Add_Agent_To_Scheduler (Agent => T);
            T := Next_T;
         end loop;

         Q := Next_Q;
      end loop;

      P.Entry_Queues := No_Agent;
   end Purge_Entry_Queues;

   procedure Remove_Task_From_Entry_Queue
     (PO       : in Protected_Id;
      T        : in Task_Id)
   is
      P : Protected_Agent_Record renames Agent_Pool (PO);

      Entry_Id       : constant Entry_Index := Id_Of_Entry (T);
      Prev_Q         : Task_Id_With_No      := No_Agent;
      Q              : Task_Id_With_No      := P.Entry_Queues;
      New_Q          : Task_Id_With_No      := P.Entry_Queues;
      Queue_Entry_Id : Entry_Index;

   begin
      --  Find the queue.
      --  TODO : Need some mechanism to handle the problem if a malicious task
      --  has sent an index that is either not valid or points to a queue with
      --  no one on it.

      Queue_Entry_Id := Id_Of_Entry (For_Task => Q);

      while Q /= No_Agent and then Queue_Entry_Id /= Entry_Id loop
         Prev_Q := Q;
         Q      := Next_Queue (Q);
      end loop;

      if Q = T then
         --  The task to remove is the head of the queue.

         --  Prev_Q needs updating to point to the next queue head. This will
         --  be the next task in the entry queue that Q was in or the next
         --  queue if the entry queue that Q belong to is now empty.

         New_Q := Next_Agent (Q);

         if New_Q /= No_Agent then
            --  The queue that Q belong to was empty, pick next queue. It does
            --  not matter if that queue is empty.

            New_Q := Next_Queue (Q);
         end if;

         --  The new queue head points to the same task as the old queue head
         --  did.

         Set_Next_Queue (For_Task   => New_Q,
                         Next_Queue => Next_Queue (Q));

         if P.Entry_Queues = Q then
            --  If Q was the first entry in the queue list, fix head reference.
            P.Entry_Queues := New_Q;
         else
            --  Otherwise fix the previous Q item reference.
            Agent_Pool (Prev_Q).Entry_Queues := New_Q;
         end if;

      else
         --  The task is a child of the queue. Find it.

         while Q /= T and then Q /= No_Agent loop
            --  Note that we should more likely hit T before No_Agent.

            Prev_Q := Q;
            Q      := Next_Agent (Q);
         end loop;

         --  Have T and the link before it now (Confusingly known here as
         --  Prev_Q and Q).

         Set_Next_Agent (For_Agent => Prev_Q, Next_Agent => Next_Agent (Q));
      end if;

      Set_Next_Agent  (For_Agent => T, Next_Agent => No_Agent);
      Set_Next_Queue  (For_Task  => T, Next_Queue => No_Agent);
      Set_Id_Of_Entry (For_Task  => T, Entry_Id  => No_Entry);

   end Remove_Task_From_Entry_Queue;

   procedure Remove_Task_From_Within_Protected_Object
     (PO : in Protected_Id;
      T  : in Task_Id)
   is
      P : Protected_Agent_Record renames Agent_Pool (PO);

      Curr_T : Task_Id_With_No := P.Tasks_Within.Head;
      Prev_T : Task_Id_With_No := No_Agent;

   begin
      --  Check to see if the task is the only one in the protected object.

      if P.Tasks_Within.Head = P.Tasks_Within.Tail then
         --  Check to see if the only task is our task
         if P.Tasks_Within.Head = T then
            P.Tasks_Within := Empty_List;
         end if;

         --  Nothing more to do here, return
         return;
      end if;

      --  The following code is pointless when we only have one processor,
      --  because there can only be one task inside the protected object at a
      --  time due to the Priority Ceiling Protocol. This should only include
      --  the code if we have more than one processor.

      pragma Warnings (Off, "condition is always*");
      if Number_Of_Processors > 1 then
         pragma Warnings (On, "condition is always*");
         --  Search for the task. At this point there is more than one task in
         --  the list.

         while Curr_T /= T and then Curr_T /= No_Agent loop
            --  More likely to hit T than No_Agent.

            Prev_T := Curr_T;
            Curr_T := Next_Agent (Curr_T);
         end loop;

         --  If Curr_T is No_Agent then T was never in the Protected Object
         --  which should not happen as this procedure should in theory only be
         --  called with tasks that are inside the PO. It is harmless though so
         --  we let it slip for now.

         if Curr_T = No_Agent then
            return;
         end if;

         if Prev_T = No_Agent then
            --  Task is list head.

            P.Tasks_Within.Head := Next_Agent (T);

         else
            Set_Next_Agent (For_Agent => Prev_T, Next_Agent => Next_Agent (T));
         end if;

         if T = P.Tasks_Within.Tail then
            P.Tasks_Within.Tail := Prev_T;
         end if;
      end if;
   end Remove_Task_From_Within_Protected_Object;

   -----------------
   -- Task_Action --
   -----------------

   procedure Task_Action
     (Protected_Object : in  Protected_Id;
      Open_Entry       : out Entry_Index;
      Exception_Raised : out Boolean;
      Preference       : in  Entry_Index := No_Entry)
   is
      P : Protected_Agent_Record renames Agent_Pool (Protected_Object);

      type Barrier_Eval_Function is access function
        (O : Address; E : Entry_Index) return Boolean;

      function To_Barrier_Eval_Function is
        new Ada.Unchecked_Conversion
          (Address, Barrier_Eval_Function);

      Is_Barrier_Open : constant Barrier_Eval_Function :=
                          To_Barrier_Eval_Function (P.Entry_Barriers);

   begin
      if Preference /= No_Entry then
         if Is_Barrier_Open (P.Object_Record, Preference) then
            Open_Entry := Preference;
            Exception_Raised := False;
            return;
         end if;
      end if;

      --  Search queues and check to see if they are open

      Search_For_Open_Queue : declare
         Queue : Task_Id_With_No := P.Entry_Queues;
      begin
         while Queue /= No_Agent loop
            if Is_Barrier_Open (P.Object_Record, Id_Of_Entry (Queue)) then
               Open_Entry := Id_Of_Entry (Queue);
               Exception_Raised := False;
               return;
            end if;
            Queue := Next_Queue (Queue);
         end loop;
      end Search_For_Open_Queue;

      Open_Entry := No_Index;
      Exception_Raised := False;

   exception
      when others =>
         Exception_Raised := False;
         raise Program_Error;
   end Task_Action;

   ----------------------------------------------------------------
   -- Access to the protected agent from a protected access type --
   ----------------------------------------------------------------

   type Protected_Record is record
      Agent : Protected_Id;
   end record;
   --  The first component of the record that contains the protected object's
   --  data is the reference to the protected object's agent. The subsquent
   --  components are not relavent for the purposes of extracting the Agent
   --  from a protected access type and are protected object dependent anyway.

   type Protected_Access_Components is record
      Object          : access Protected_Record;
      Handler_Address : System.Address;
   end record;
   --  A protected access type consists of a pointer to the record holding
   --  the protected object's data and a pointer to the procedure the access
   --  is pointing to.

   ----------------------------------
   -- Protected_Object_From_Access --
   ----------------------------------

   function Protected_Object_From_Access
     (Handler : Parameterless_Access)
      return Protected_Id
   is
      function To_Protected_Subprogram_Components is
        new Ada.Unchecked_Conversion
          (Parameterless_Access, Protected_Access_Components);
   begin
      return To_Protected_Subprogram_Components (Handler).Object.Agent;
   end Protected_Object_From_Access;

   function Protected_Object_From_Access
     (Handler : Ada.Cyclic_Tasks.Response_Handler)
      return Protected_Id
   is
      function To_Protected_Subprogram_Components is
        new Ada.Unchecked_Conversion
          (Ada.Cyclic_Tasks.Response_Handler, Protected_Access_Components);
   begin
      return To_Protected_Subprogram_Components (Handler).Object.Agent;
   end Protected_Object_From_Access;

end Oak.Agent.Protected_Objects;
