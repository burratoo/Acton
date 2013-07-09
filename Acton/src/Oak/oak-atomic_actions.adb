with Oak.Agent.Tasks.Protected_Objects; use Oak.Agent.Tasks.Protected_Objects;
with Oak.Agent.Queue; use Oak.Agent.Queue;
with Oak.Scheduler;          use Oak.Scheduler;

package body Oak.Atomic_Actions is

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
     (AO                : not null access Atomic_Object;
      T                 : not null access Task_Agent'Class;
      Action_Id         : in Action_Index;
      Next_Task_To_Run : out Agent_Handler);

   procedure Enter_Action
     (AO                : not null access Atomic_Object;
      T                 : not null access Task_Agent'Class;
      Action_Id         : in Action_Index;
      Next_Task_To_Run : out Agent_Handler) is
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

            --  TODO: Need to figure out how to implement barriers without
            --  using the shared state variable

            if Not_All_Present then
               --  Holding tasks at the barrier.

               AO.Controlling_State := Waiting_For_Protected_Object;
--                 T.Set_State (Shared_State);
--                 T.Set_Shared_State (AO.Controlling_State'Access);

               --  All tasks are now present, release all of them.

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

            Next_Task_To_Run := null;
         end;
      else
         T.Set_State (Runnable);
         Next_Task_To_Run := Agent_Handler (T);
      end if;
   end Enter_Action;

   procedure Exit_Barrier
     (AO                : not null access Atomic_Object;
      T                 : not null access Task_Agent'Class;
      Scheduler_Info    : in out Scheduler.Oak_Scheduler_Info;
      Action_Id         : in Action_Index;
      Exception_Raised  : in Boolean;
      Next_Agent_To_Run : out Agent_Handler)
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
      --  TODO: see the above TODO to figure out what to do here.

      if Not_All_Present then
         AO.Controlling_State := Waiting_For_Protected_Object;
--           T.Set_State (Shared_State);
--           T.Set_Shared_State (AO.Controlling_State'Access);
      else
         declare
            New_State : constant Agent_State :=
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

      Scheduler.Check_Sechduler_Agents_For_Next_Task_To_Run
        (Scheduler_Info   => Scheduler_Info,
         Next_Task_To_Run => Next_Agent_To_Run);

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
     (AO                : not null access Atomic_Object;
      T                 : not null access Task_Agent'Class;
      Scheduler_Info    : in out Scheduler.Oak_Scheduler_Info;
      Action_Id         : in Action_Index;
      Next_Agent_To_Run : out Agent_Handler)
   is
   begin
      Next_Agent_To_Run := null;

      if Action_Id not in AO.Actions'Range or
        T.Current_Atomic_Action /= AO.Parent then
         T.Set_State (Enter_Atomic_Action_Refused);
         Next_Agent_To_Run := Agent_Handler (T);
         return;
      end if;

      if AO.Actions (Action_Id).Current_Task /= null then

         Scheduler.Remove_Agent_From_Scheduler (T);
         T.Set_State (Waiting_For_Protected_Object);
         Add_Agent_To_Tail
           (Queue => Agent_Handler (AO.Actions (Action_Id).Queue),
            Agent => T);

      else
         Enter_Action (AO, T, Action_Id, Next_Agent_To_Run);
      end if;

      if Next_Agent_To_Run = null then
         Scheduler.Check_Sechduler_Agents_For_Next_Task_To_Run
           (Scheduler_Info   => Scheduler_Info,
            Next_Task_To_Run => Next_Agent_To_Run);
      end if;
   end Process_Enter_Request;

   procedure Process_Exit_Request
     (AO               : not null access Atomic_Object;
      T                : not null access Task_Agent'Class;
      Scheduler_Info   : in out Scheduler.Oak_Scheduler_Info;
      Action_Id        : in Action_Index;
      Exception_Raised : in Boolean;
      Next_Agent_To_Run : out Agent_Handler)
   is
      Protected_Object : access Task_Agent'Class renames
                           AO.Protected_Objects;
   begin
      if T /= AO.Actions (Action_Id).Current_Task then
         T.Set_State (Exit_Atomic_Action_Error);
         Next_Agent_To_Run := Agent_Handler (T);
         return;
      end if;

      Next_Agent_To_Run := null;

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
                     Remove_Agent_From_Head
                       (Agent_Handler (AO.Actions (Id).Queue));
                     QT.Set_State (Runnable);
                     Scheduler.Add_Agent_To_Scheduler (QT);
                     Enter_Action (AO => AO,
                                   T                 => QT,
                                   Action_Id         => Id,
                                   Next_Task_To_Run => Next_Agent_To_Run);
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

      Scheduler.Check_Sechduler_Agents_For_Next_Task_To_Run
       (Scheduler_Info   => Scheduler_Info,
        Next_Task_To_Run => Next_Agent_To_Run);

   end Process_Exit_Request;

end Oak.Atomic_Actions;
