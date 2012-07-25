with Oak.Agent.Tasks.Protected_Objects; use Oak.Agent.Tasks.Protected_Objects;
with Oak.Agent.Tasks.Queues; use Oak.Agent.Tasks.Queues;
with Oak.Scheduler;          use Oak.Scheduler;

package body Oak.Atomic_Actions is

   package Queue renames Oak.Agent.Tasks.Queues.General;

   procedure Add_Protected_Object
     (AO : not null access Atomic_Object;
      PO : not null access Agent.Tasks.Protected_Objects.Protected_Agent'Class)
   is
      PO_List : access Task_Agent'Class renames AO.Protected_Objects;
   begin
      while PO_List.Activation_List /= null loop
         PO_List := PO_List.Activation_List;
      end loop;
      PO_List.Set_Activation_List (PO);
   end Add_Protected_Object;

   procedure Enter_Action
     (AO          : not null access Atomic_Object;
      T           : not null access Task_Agent'Class;
      Action_Id   : in Action_Index;
      Chosen_Task : in out Task_Handler);

   procedure Enter_Action
     (AO          : not null access Atomic_Object;
      T           : not null access Task_Agent'Class;
      Action_Id   : in Action_Index;
      Chosen_Task : in out Task_Handler) is
   begin
      AO.Actions (Action_Id).Current_Task := T;
      T.Set_Current_Atomic_Action (AO);

      if AO.Barrier_Start then
         declare
            Not_All_Present : Boolean := False;
         begin
            for A of AO.Actions loop
               if A.Current_Task = null then
                  Not_All_Present := True;
                  exit;
               end if;
            end loop;

            --  While we use the task agent's shared state to act as the
            --  barrier here, we could have used the activation chain link
            --  as well.

            if Not_All_Present then
               AO.Controlling_State := Waiting;
               T.Set_State (Shared_State);
               T.Set_Shared_State (AO.Controlling_State'Access);
            else
               AO.Controlling_State := Runnable;
               T.Set_State (Runnable);
               for A of AO.Actions loop
                  A.Current_Task.Set_State (Runnable);
               end loop;

               --  Ensure that the atomic object's Exception_Raised field
               --  is false when leaving the barrier as no action has started
               --  running at this point.

               AO.Exception_Raised := False;

            end if;

            --  Set Chosen_Task to null as it is not our responsibility to
            --  identify the task with the highest priority.

            Chosen_Task := null;
         end;
      else
         T.Set_State (Runnable);
         Chosen_Task := Task_Handler (T);
      end if;
   end Enter_Action;

   procedure Exit_Barrier
     (AO               : not null access Atomic_Object;
      T                : not null access Task_Agent'Class;
      Action_Id        : in Action_Index;
      Exception_Raised : in Boolean;
      Chosen_Task      : out Task_Handler)
   is
      Not_All_Present : Boolean := False;
   begin
      if T /= AO.Actions (Action_Id).Current_Task then
         T.Set_State (Exit_Atomic_Action_Error);
         return;
      end if;

      --  Register with the atomic object that we have reached the end barrier.

      AO.Actions (Action_Id).End_Barrier := T;

      for A of AO.Actions loop
         if A.End_Barrier = null and then
           (AO.Participating = All_Actions
            or A.Current_Task /= null) then
            Not_All_Present := True;
            exit;
         end if;
      end loop;

      if Exception_Raised then
         AO.Exception_Raised := True;
      end if;

      --  While we use the task agent's shared state to act as the
      --  barrier here, we could have used the activation chain link
      --  as well.

      if Not_All_Present then
         AO.Controlling_State := Waiting;
         T.Set_State (Shared_State);
         T.Set_Shared_State (AO.Controlling_State'Access);
      else
         declare
            New_State : constant Task_State :=
                          (if AO.Exception_Raised
                           then Atomic_Action_Error else Runnable);
         begin
            T.Set_State (New_State);
            for A of AO.Actions loop
               A.Current_Task.Set_State (New_State);
               A.End_Barrier := null;
            end loop;

            --  On leaving the exit barrier we negate the atomic object's
            --  Exception_Raised field as the atomic exception will have
            --  propagated to all participating tasks by now.

            AO.Exception_Raised := False;

         end;
      end if;

      Chosen_Task := null;

   end Exit_Barrier;

   procedure Initialise_Atomic_Object
     (AO            : not null access Atomic_Object;
      Parent        : access Atomic_Object;
      End_Barrier   : in Boolean;
      Start_Barrier : in Boolean;
      Participating : in Participating_Actions)
   is
   begin
      AO.Parent           := Parent;
      AO.Barrier_Start    := Start_Barrier;
      AO.Barrier_End      := End_Barrier;
      AO.Participating    := Participating;
      AO.Exception_Raised := False;
   end Initialise_Atomic_Object;

   procedure Process_Enter_Request
     (AO             : not null access Atomic_Object;
      T              : not null access Task_Agent'Class;
      Scheduler_Info : in out Scheduler.Oak_Scheduler_Info;
      Action_Id      : in Action_Index;
      Chosen_Task    : out Task_Handler)
   is
   begin
      Chosen_Task := null;

      if Action_Id not in AO.Actions'Range or
        T.Current_Atomic_Action /= AO.Parent then
         T.Set_State (Enter_Atomic_Action_Refused);
         Chosen_Task := Task_Handler (T);
         return;
      end if;

      if AO.Actions (Action_Id).Current_Task /= null then

         Scheduler.Remove_Task_From_Scheduler (T);
         T.Set_State (Waiting);
         Queue.Add_Agent_To_Tail
           (Queue =>
              Queue.Agent_Handler (AO.Actions (Action_Id).Queue),
            Agent => T);

      else
         Enter_Action (AO, T, Action_Id, Chosen_Task);
      end if;

      if Chosen_Task = null then
         Scheduler.Check_With_Scheduler_Agents_On_Which_Task_To_Run_Next
           (Scheduler_Info => Scheduler_Info,
            Chosen_Task    => Chosen_Task);
      end if;
   end Process_Enter_Request;

   procedure Process_Exit_Request
     (AO               : not null access Atomic_Object;
      T                : not null access Task_Agent'Class;
      Scheduler_Info   : in out Scheduler.Oak_Scheduler_Info;
      Action_Id        : in Action_Index;
      Exception_Raised : in Boolean;
      Chosen_Task      : out Task_Handler)
   is
      Protected_Object : access Task_Agent'Class renames
                           AO.Protected_Objects;
   begin
      if T /= AO.Actions (Action_Id).Current_Task then
         T.Set_State (Exit_Atomic_Action_Error);
         Chosen_Task := Task_Handler (T);
         return;
      end if;

      Chosen_Task := null;

      if Exception_Raised or AO.Exception_Raised then
         T.Set_State (Atomic_Action_Error);

         --  If we have a start barrier then set atomic action's
         --  Exception_Raised field. This will allow an exception to propagate
         --  to tailing tasks if there is no end barrier.  We can only do this
         --  if a start barrier is present as we would not no when a new
         --  action starts otherwise.

         if AO.Barrier_Start then
            AO.Exception_Raised := True;
         end if;
      else
         T.Set_State (Runnable);
      end if;

      T.Set_Current_Atomic_Action (AO.Parent);
      AO.Actions (Action_Id).Current_Task := null;

      --  For the last task to exit, need to release any PO's held and unqueue
      --  any tasks queued on the atomic unit's actions.

      if AO.Protected_Objects /= null then
         Release : declare
            Last_Task : Boolean := True;
            QT        : access Task_Agent'Class;
         begin
            for A of AO.Actions loop
               if A.Current_Task /= null then
                  Last_Task := False;
                  exit;
               end if;
            end loop;

            if Last_Task then
               for Id in AO.Actions'Range loop
                  if AO.Actions (Id).Queue /= null then
                     QT := AO.Actions (Id).Queue;
                     Queue.Remove_Agent_From_Head
                       (Queue.Agent_Handler
                          (AO.Actions (Id).Queue));
                     QT.Set_State (Runnable);
                     Scheduler.Add_Task_To_Scheduler
                       (Scheduler_Info => Scheduler_Info,
                        T              => QT);
                     Enter_Action (AO => AO,
                                   T             => QT,
                                   Action_Id     => Id,
                                   Chosen_Task   => Chosen_Task);
                  end if;
               end loop;
               while Protected_Object /= null loop
                  Protected_Object.Set_Current_Atomic_Action
                    (AO.Parent);
                  Protected_Object := Protected_Object.Activation_List;
               end loop;
            end if;
         end Release;
      end if;

      Scheduler.Check_With_Scheduler_Agents_On_Which_Task_To_Run_Next
       (Scheduler_Info => Scheduler_Info,
        Chosen_Task    => Chosen_Task);

   end Process_Exit_Request;

end Oak.Atomic_Actions;
