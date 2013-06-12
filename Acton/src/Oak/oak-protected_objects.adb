with Oak.Indices;

with Oak.Agent.Tasks;                   use Oak.Agent.Tasks;
with Oak.Agent.Tasks.Protected_Objects; use Oak.Agent.Tasks.Protected_Objects;
with Oak.Atomic_Actions; use Oak.Atomic_Actions;
with Oak.Scheduler;                     use Oak.Scheduler;
with Oak.Processor_Support_Package.Interrupts;
use Oak.Processor_Support_Package.Interrupts;

package body Oak.Protected_Objects is

   procedure Process_Enter_Request
     (Scheduler_Info  : in out Oak_Scheduler_Info;
      T               : not null access Task_Agent'Class;
      PO              : not null access Protected_Agent'Class;
      Subprogram_Kind : in Protected_Subprogram_Type;
      Entry_Id        : in Indices.Entry_Index;
      Chosen_Task     : out Task_Handler) is
   begin
      if PO.all not in Protected_Agent'Class or
        (Subprogram_Kind = Protected_Entry and then
           not PO.Is_Entry_Id_Valid (Entry_Id))
      then
         T.Set_State (Enter_PO_Refused);
         Chosen_Task := Task_Handler (T);
         return;
      end if;

      if T.Normal_Priority > PO.Normal_Priority then
         T.Set_State (Enter_PO_Refused);
         Chosen_Task := Task_Handler (T);
         return;
      end if;

      --  Need to inform the interrupt system that we are acquiring the object.
      --  The resource is released in three places when:
      --     1. We encounter an error while evaluating the barrier states when
      --        a task calls an entry.
      --     2. There are no tasks able to run in the protected object.
      --     3. Once the protected action is completed in Process_Exit_Request.
      Get_Resource (PO);

      if T.Current_Atomic_Action /= PO.Current_Atomic_Action and then
        Parent (T.Current_Atomic_Action) /= PO.Current_Atomic_Action then
         T.Set_State (Shared_State);
         T.Set_Shared_State (PO.Reference_To_Acquiring_Tasks_State);
         Chosen_Task := null;
      else
         if T.Current_Atomic_Action /= PO.Current_Atomic_Action then
            PO.Set_Current_Atomic_Action (T.Current_Atomic_Action);
            Atomic_Actions.Add_Protected_Object
              (AO => T.Current_Atomic_Action,
               PO => PO);
         end if;

         if PO.State = Inactive then
            Scheduler.Remove_Task_From_Scheduler (T);
            Chosen_Task := null;

            declare
            begin
               if Subprogram_Kind = Protected_Entry and then
                 not PO.Is_Barrier_Open (Entry_Id => Entry_Id) then
                  T.Set_State (Waiting_For_Protected_Object);
                  PO.Add_Task_To_Entry_Queue
                    (T        => T,
                     Entry_Id => Entry_Id);

                  --  We need to check the queues here in case a barrier has
                  --  changed as a result of it using the queue attribute.
                  --  We are not able to conditional this to only protected
                  --  objects that have barriers that use Count as the front
                  --  end does lend itself to achieve this.
                  PO.Get_And_Remove_Next_Task_From_Entry_Queues
                    (Next_Task => Chosen_Task);

               else
                  Chosen_Task := Task_Handler (T);
               end if;
            exception
               when Program_Error =>
                  Scheduler.Add_Task_To_Scheduler
                    (Scheduler_Info => Scheduler_Info,
                     T              => T);
                  --  Add call to check if we need to decativate the PO.
                  --  Add call to see if we need to remove the task from the
                  --  PO.
                  T.Set_State (Enter_PO_Refused);
                  Chosen_Task := Task_Handler (T);
                  --  Object release point 1.
                  Release_Resource (PO);
                  return;
            end;

            if Chosen_Task /= null then
               PO.Add_Task_To_Protected_Object (Chosen_Task);
               T.Set_State (State => Runnable);
               PO.Set_Acquiring_Tasks_State (Waiting_For_Protected_Object);
               Scheduler.Activate_Task
                 (Scheduler_Info => Scheduler_Info,
                  T              => PO);
               Chosen_Task := Task_Handler (PO);
            end if;

         elsif Subprogram_Kind = Protected_Function and
                  PO.Active_Subprogram_Kind = Protected_Function then
            Scheduler.Remove_Task_From_Scheduler (T);
            T.Set_State (Runnable);
            PO.Add_Task_To_Protected_Object (T);
            Chosen_Task := Task_Handler (PO);
         else
            T.Set_State (Shared_State);
            T.Set_Shared_State (PO.Reference_To_Acquiring_Tasks_State);
            Chosen_Task := null;
         end if;
      end if;

      if Chosen_Task = null then
         --  Object release point 2.
         --  Inform the interrupt subsystem that we are releasing the object
         --  since no task inside the protected object is able to run.
         Release_Resource (PO);
         Scheduler.Check_With_Scheduler_Agents_On_Which_Task_To_Run_Next
           (Scheduler_Info => Scheduler_Info,
            Chosen_Task    => Chosen_Task);
      end if;

   end Process_Enter_Request;

   procedure Process_Exit_Request
     (Scheduler_Info : in out Scheduler.Oak_Scheduler_Info;
      T              : not null access Agent.Tasks.Task_Agent'Class;
      PO             : not null access
        Agent.Tasks.Protected_Objects.Protected_Agent'Class;
      Chosen_Task    : out Agent.Tasks.Task_Handler) is
   begin
      if not PO.Is_Task_Inside_Protect_Object (T) then
         T.Set_State (Exit_PO_Error);
         Chosen_Task := Task_Handler (T);
         return;
      end if;

      T.Set_State (Runnable);
      PO.Remove_Task_From_Protected_Object (T => T);
      Scheduler.Add_Task_To_Scheduler
        (Scheduler_Info => Scheduler_Info,
         T              => T);

      PO.Get_And_Remove_Next_Task_From_Entry_Queues
        (Next_Task => Chosen_Task);

      if Chosen_Task = null then
         --  Protected action ends.
         PO.Set_Acquiring_Tasks_State (Entering_PO);
         Scheduler.Deactivate_Task
           (Scheduler_Info => Scheduler_Info,
            T              => PO);
         --  Object release pont 3.
         Release_Resource (PO);
         Scheduler.Check_With_Scheduler_Agents_On_Which_Task_To_Run_Next
           (Scheduler_Info => Scheduler_Info,
            Chosen_Task    => Chosen_Task);
      else
         --  Protected action continues.
         Chosen_Task.Set_State (Runnable);
         PO.Add_Task_To_Protected_Object (Chosen_Task);
      end if;
   end Process_Exit_Request;

end Oak.Protected_Objects;
