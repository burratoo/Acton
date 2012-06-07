with Oak.Agent.Tasks.Queues; use Oak.Agent.Tasks.Queues;
with Oak.Scheduler;          use Oak.Scheduler;

package body Oak.Atomic_Actions is

   package Queue renames Oak.Agent.Tasks.Queues.General;

   procedure Enter_Action
     (Atomic_Action  : not null access Atomic_Action_State;
      T              : not null access Task_Agent'Class;
      Action_Id      : in Action_Index;
      Chosen_Task    : in out Task_Handler);

   procedure Enter_Action
     (Atomic_Action  : not null access Atomic_Action_State;
      T              : not null access Task_Agent'Class;
      Action_Id      : in Action_Index;
      Chosen_Task    : in out Task_Handler) is
   begin
      Atomic_Action.Actions (Action_Id).Current_Task := T;
      T.Set_Current_Atomic_Action (Atomic_Action);

      if not Atomic_Action.Active then
         Atomic_Action.Active := True;
      end if;

      if Atomic_Action.Barrier_Start then
         declare
            Not_All_Present : Boolean := False;
         begin
            for A of Atomic_Action.Actions loop
               if A.Current_Task = null then
                  Not_All_Present := True;
                  exit;
               end if;
            end loop;

            --  While we use the task agent's shared state to act as the
            --  barrier here, we could have used the activation chain link
            --  as well.
            if Not_All_Present then
               Atomic_Action.Controlling_State := Waiting;
               T.Set_State (Shared_State);
               T.Set_Shared_State (Atomic_Action.Controlling_State'Access);
            else
               Atomic_Action.Controlling_State := Runnable;
               T.Set_State (Runnable);
               for A of Atomic_Action.Actions loop
                  A.Current_Task.Set_State (Runnable);
               end loop;
            end if;
         end;
      else
         T.Set_State (Runnable);
         Chosen_Task := Task_Handler (T);
      end if;
   end Enter_Action;

   procedure Exit_Barrier
     (Atomic_Action    : not null access Atomic_Action_State;
      T                : not null access Task_Agent'Class;
      Action_Id        : in Action_Index;
      Exception_Raised : in Boolean;
      Chosen_Task      : out Task_Handler)
   is
      Not_All_Present : Boolean := False;
   begin
      Chosen_Task := Task_Handler (T);

      if T /= Atomic_Action.Actions (Action_Id).Current_Task then
         T.Set_State (Exit_Atomic_Action_Error);
         return;
      end if;

      for A of Atomic_Action.Actions loop
         if A.End_Barrier = null and then
           (Atomic_Action.Require_All_Tasks or A.Current_Task /= null) then
            Not_All_Present := True;
            exit;
         end if;
      end loop;

      --  While we use the task agent's shared state to act as the
      --  barrier here, we could have used the activation chain link
      --  as well.
      if Not_All_Present then
         Atomic_Action.Controlling_State := Waiting;
         T.Set_State (Shared_State);
         T.Set_Shared_State (Atomic_Action.Controlling_State'Access);
         Atomic_Action.Actions (Action_Id).End_Barrier := T;
      else
         declare
            New_State : constant Task_State := (if Exception_Raised
                                              then Atomic_Error else Runnable);
         begin
            T.Set_State (New_State);
            for A of Atomic_Action.Actions loop
               A.Current_Task.Set_State (New_State);
               A.End_Barrier := null;
            end loop;
         end;
      end if;

   end Exit_Barrier;

   procedure Process_Enter_Request
     (Atomic_Action  : not null access Atomic_Action_State;
      T              : not null access Task_Agent'Class;
      Scheduler_Info : in out Scheduler.Oak_Scheduler_Info;
      Action_Id      : in Action_Index;
      Chosen_Task    : out Task_Handler)
   is
   begin
      Chosen_Task := null;

      if Action_Id not in Atomic_Action.Actions'Range or
        T.Current_Atomic_Action /= Atomic_Action.Parent then
         T.Set_State (Enter_Atomic_Action_Refused);
         Chosen_Task := Task_Handler (T);
         return;
      end if;

      if Atomic_Action.Actions (Action_Id).Current_Task /= null then

         Scheduler.Remove_Task_From_Scheduler (T);
         T.Set_State (Waiting);
         Queue.Add_Agent_To_Tail
           (Queue =>
              Queue.Agent_Handler (Atomic_Action.Actions (Action_Id).Queue),
            Agent => T);

      else
         Enter_Action (Atomic_Action, T, Action_Id, Chosen_Task);
      end if;

      if Chosen_Task = null then
         Scheduler.Check_With_Scheduler_Agents_On_Which_Task_To_Run_Next
           (Scheduler_Info => Scheduler_Info,
            Chosen_Task    => Chosen_Task);
      end if;
   end Process_Enter_Request;

   procedure Process_Exit_Request
     (Atomic_Action  : not null access Atomic_Action_State;
      T              : not null access Agent.Tasks.Task_Agent'Class;
      Scheduler_Info : in out Scheduler.Oak_Scheduler_Info;
      Action_Id      : in Action_Index;
      Chosen_Task    : out Agent.Tasks.Task_Handler)
   is
   begin
      if T /= Atomic_Action.Actions (Action_Id).Current_Task then
         T.Set_State (Exit_Atomic_Action_Error);
         Chosen_Task := Task_Handler (T);
         return;
      end if;

      Chosen_Task := null;

      T.Set_State (Runnable);
      T.Set_Current_Atomic_Action (null);
      Atomic_Action.Actions (Action_Id).Current_Task := null;

      --  For the last task to exit, need to release any PO's held and unqueue
      --  any tasks queued on the atomic unit's actions.
      Release :
      declare
         Last_Task : Boolean := True;
         QT        : access Task_Agent'Class;
      begin
         for A of Atomic_Action.Actions loop
            if A.Current_Task /= null then
               Last_Task := False;
               exit;
            end if;
         end loop;

         if Last_Task then
            for Id in Atomic_Action.Actions'Range loop
               if Atomic_Action.Actions (Id).Queue /= null then
                  QT := Atomic_Action.Actions (Id).Queue;
                  Queue.Remove_Agent_From_Head
                    (Queue.Agent_Handler (Atomic_Action.Actions (Id).Queue));
                  QT.Set_State (Runnable);
                  Scheduler.Add_Task_To_Scheduler
                    (Scheduler_Info => Scheduler_Info,
                     T              => QT);
                  Enter_Action (Atomic_Action => Atomic_Action,
                                T             => QT,
                                Action_Id     => Id,
                                Chosen_Task   => Chosen_Task);
               end if;
            end loop;
            --  insert fancy release PO code here.
         end if;
      end Release;

      Scheduler.Check_With_Scheduler_Agents_On_Which_Task_To_Run_Next
       (Scheduler_Info => Scheduler_Info,
        Chosen_Task    => Chosen_Task);

   end Process_Exit_Request;

end Oak.Atomic_Actions;
