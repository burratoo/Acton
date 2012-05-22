with Oak.Entries;

with Oak.Agent.Tasks; use Oak.Agent.Tasks;
with Oak.Agent.Tasks.Protected_Object; use Oak.Agent.Tasks.Protected_Object;
with Oak.Scheduler; use Oak.Scheduler;
with Oak.Processor_Support_Package.Interrupts;
use Oak.Processor_Support_Package.Interrupts;

package body Oak.Protected_Object is

   procedure Process_Enter_Request
     (Scheduler_Info  : in out Oak_Scheduler_Info;
      T               : not null access Task_Agent'Class;
      PO              : not null access Protected_Agent'Class;
      Subprogram_Kind : in Protected_Subprogram_Type;
      Entry_Id        : in Entries.Entry_Index;
      Chosen_Task     : out Task_Handler) is
   begin
      if PO.all not in Protected_Agent'Class or
        (Subprogram_Kind = Protected_Entry and then
           not Is_Entry_Id_Valid (PO => PO, Entry_Id => Entry_Id))
      then
         Set_State (T => T, State => Enter_PO_Refused);
         Chosen_Task := Task_Handler (T);
         return;
      end if;

      if Normal_Priority (T => T) > Normal_Priority (T => PO) then
         Set_State (T => T, State => Enter_PO_Refused);
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

      if State (PO) = Inactive then
         Scheduler.Remove_Task_From_Scheduler (T);
         Chosen_Task := null;

         declare
         begin
            if Subprogram_Kind = Protected_Entry and then
              not Is_Barrier_Open (PO => PO, Entry_Id => Entry_Id) then
               Set_State (T => T, State => Waiting);
               Add_Task_To_Entry_Queue
                 (PO       => PO,
                  T        => T,
                  Entry_Id => Entry_Id);

               --  We need to check the queues here in case a barrier has
               --  changed as a result of it using the queue attribute.
               --  We are not able to conditional this to only protected
               --  objects that have barriers that use Count as the front
               --  end does lend itself to achieve this.
               Get_And_Remove_Next_Task_From_Entry_Queues
                 (PO => PO, Next_Task => Chosen_Task);

            else
               Chosen_Task := Task_Handler (T);
            end if;
         exception
            when Program_Error =>
               Scheduler.Add_Task_To_Scheduler
                 (Scheduler_Info => Scheduler_Info,
                  T              => T);
               --  Add call to check if we need to decativate the PO.
               --  Add call to see if we need to remove the task from the PO.
               Set_State (T     => T,
                          State => Enter_PO_Refused);
               Chosen_Task := Task_Handler (T);
               --  Object release point 1.
               Release_Resource;
               return;
         end;

         if Chosen_Task /= null then
            Add_Task_To_Protected_Object (T  => Chosen_Task, PO => PO);
            Set_State (T => Chosen_Task, State => Runnable);
            Set_Acquiring_Tasks_State
              (For_Protected_Object => PO,
               To_State             => Waiting);
            Scheduler.Activate_Task
              (Scheduler_Info => Scheduler_Info,
               T              => PO);
            Chosen_Task := Task_Handler (PO);
         end if;

      elsif Subprogram_Kind = Protected_Function and
               Active_Subprogram_Kind (PO) = Protected_Function then
         Scheduler.Remove_Task_From_Scheduler (T);
         Set_State (T => T, State => Runnable);
         Add_Task_To_Protected_Object (PO => PO, T  => T);
         Chosen_Task := Task_Handler (PO);
      else
         Set_State (T => T, State => Shared_State);
         Set_Shared_State
           (For_Task           => T,
            With_State_Pointer => Reference_To_Acquiring_Tasks_State
                                     (For_Protected_Object => PO));
         Chosen_Task := null;
      end if;

      if Chosen_Task = null then
         --  Object release point 2.
         --  Inform the interrupt subsystem that we are releasing the object
         --  since no task inside the protected object is able to run.
         Release_Resource;
         Scheduler.Check_With_Scheduler_Agents_On_Which_Task_To_Run_Next
           (Scheduler_Info => Scheduler_Info,
            Chosen_Task    => Chosen_Task);
      end if;

   end Process_Enter_Request;

   procedure Process_Exit_Request
     (Scheduler_Info : in out Scheduler.Oak_Scheduler_Info;
      T              : not null access Agent.Tasks.Task_Agent'Class;
      PO             : not null access
        Agent.Tasks.Protected_Object.Protected_Agent'Class;
      Chosen_Task    : out Agent.Tasks.Task_Handler) is
   begin
      if not Is_Task_Inside_Protect_Object (PO  => PO, T => T) then
         Set_State (T => T, State => Exit_PO_Error);
         return;
      end if;

      Set_State (T => T, State => Runnable);
      Remove_Task_From_Protected_Object (PO => PO, T => T);
      Scheduler.Add_Task_To_Scheduler
        (Scheduler_Info => Scheduler_Info,
         T              => T);

      Get_And_Remove_Next_Task_From_Entry_Queues
        (PO => PO, Next_Task => Chosen_Task);

      if Chosen_Task = null then
         --  Protected action ends.
         Set_Acquiring_Tasks_State
           (For_Protected_Object => PO,
            To_State             => Entering_PO);
         Scheduler.Deactivate_Task
           (Scheduler_Info => Scheduler_Info,
            T              => PO);
         --  Object release pont 3.
         Release_Resource;
         Scheduler.Check_With_Scheduler_Agents_On_Which_Task_To_Run_Next
           (Scheduler_Info => Scheduler_Info,
            Chosen_Task    => Chosen_Task);
      else
         --  Protected action continues.
         Set_State (T => Chosen_Task, State => Runnable);
         Add_Task_To_Protected_Object (PO => PO, T  => Chosen_Task);
      end if;
   end Process_Exit_Request;

end Oak.Protected_Object;
