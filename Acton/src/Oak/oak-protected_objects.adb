with Oak.States;                        use Oak.States;
with Oak.Processor_Support_Package.Interrupts;
use Oak.Processor_Support_Package.Interrupts;

package body Oak.Protected_Objects is

   procedure Process_Enter_Request
     (Scheduler_Info   : in out Oak_Scheduler_Info;
      Entering_Agent   : in Agent_Handler;
      PO               : not null access Protected_Agent'Class;
      Subprogram_Kind  : in Protected_Subprogram_Type;
      Entry_Id         : in Entry_Index;
      Next_Agent_To_Run : out Agent_Handler) is
   begin
      if PO.all not in Protected_Agent'Class or
        (Subprogram_Kind = Protected_Entry and then
           not PO.Is_Entry_Id_Valid (Entry_Id))
      then
         Entering_Agent.Set_State (Enter_PO_Refused);
         Next_Agent_To_Run := Entering_Agent;
         return;
      end if;

      if Entering_Agent.Normal_Priority > PO.Normal_Priority then
         Entering_Agent.Set_State (Enter_PO_Refused);
         Next_Agent_To_Run := Entering_Agent;
         return;
      end if;

      --  Need to inform the interrupt system that we are acquiring the object.
      --  The resource is released in three places when:
      --     1. We encounter an error while evaluating the barrier states when
      --        a task calls an entry.
      --     2. There are no tasks able to run in the protected object.
      --     3. Once the protected action is completed in Process_Exit_Request.
      Get_Resource (PO);

      Scheduler.Remove_Agent_From_Scheduler (Entering_Agent);

      if PO.State = Inactive then
         Next_Agent_To_Run := null;

         declare begin
            if Subprogram_Kind = Protected_Entry and then
              not PO.Is_Barrier_Open (Entry_Id => Entry_Id) then
               Entering_Agent.Set_State (Waiting_For_Protected_Object);
               PO.Add_Task_To_Entry_Queue
                 (T        => Entering_Agent,
                  Entry_Id => Entry_Id);

               --  We need to check the queues here in case a barrier has
               --  changed as a result of it using the queue attribute.
               --  We are not able to conditional this to only protected
               --  objects that have barriers that use Count as the front
               --  end does lend itself to achieve this.
               PO.Get_And_Remove_Next_Task_From_Entry_Queues
                 (Next_Task => Next_Agent_To_Run);

            else
               Next_Agent_To_Run := Entering_Agent;
            end if;
         exception
            when Program_Error =>
               Scheduler.Add_Agent_To_Scheduler (Entering_Agent);
               --  Add call to check if we need to decativate the PO.
               --  Add call to see if we need to remove the task from the
               --  PO.
               Entering_Agent.Set_State (Enter_PO_Refused);
               Next_Agent_To_Run := Entering_Agent;
               --  Object release point 1.
               Release_Resource (PO);

               --  TODO Need to unset the atomic action stuff here.
               return;
         end;

         if Next_Agent_To_Run /= null then
            PO.Add_Task_To_Protected_Object (Next_Agent_To_Run);
            Next_Agent_To_Run.Set_State (State => Runnable);

            --  Run protected agent
            PO.Set_State (State => Runnable);
            Scheduler.Add_Agent_To_Scheduler (PO);
            Next_Agent_To_Run := Agent_Handler (PO);
         end if;

      elsif Subprogram_Kind = Protected_Function and
               PO.Active_Subprogram_Kind = Protected_Function then
         Entering_Agent.Set_State (Runnable);
         PO.Add_Task_To_Protected_Object (Entering_Agent);
         Next_Agent_To_Run := Agent_Handler (PO);
      else
         PO.Add_Contending_Task (Entering_Agent);
         Next_Agent_To_Run := null;
      end if;

      if Next_Agent_To_Run = null then
         --  Object release point 2.
         --  Inform the interrupt subsystem that we are releasing the object
         --  since no task inside the protected object is able to run.
         Release_Resource (PO);
         Check_Sechduler_Agents_For_Next_Task_To_Run
           (Scheduler_Info   => Scheduler_Info,
            Next_Task_To_Run => Next_Agent_To_Run);
      end if;

   end Process_Enter_Request;

   procedure Process_Exit_Request
     (Scheduler_Info    : in out Oak_Scheduler_Info;
      Exiting_Agent     : not null access Oak_Agent'Class;
      PO                : not null access Protected_Agent'Class;
      Next_Agent_To_Run : out Agent_Handler) is
   begin
      if not PO.Is_Task_Inside_Protect_Object (Exiting_Agent) then
         Exiting_Agent.Set_State (Exit_PO_Error);
         Next_Agent_To_Run := Agent_Handler (Exiting_Agent);
         return;
      end if;

      Exiting_Agent.Set_State (Runnable);
      PO.Remove_Task_From_Protected_Object (Exiting_Agent);
      Scheduler.Add_Agent_To_Scheduler (Exiting_Agent);

      PO.Get_And_Remove_Next_Task_From_Entry_Queues
        (Next_Task => Next_Agent_To_Run);

      while Next_Agent_To_Run = null loop
         --  Admit new contending taks.
         PO.Get_And_Remove_Next_Contending_Task (Next_Agent_To_Run);

         exit when Next_Agent_To_Run = null;

         Process_Enter_Request
           (Scheduler_Info   => Scheduler_Info,
            Entering_Agent   => Next_Agent_To_Run,
            PO               => PO,
            Subprogram_Kind  => Exiting_Agent.Agent_Message.Subprogram_Kind,
            Entry_Id         => Exiting_Agent.Agent_Message.Entry_Id_Enter,
            Next_Agent_To_Run => Next_Agent_To_Run);
         return;
      end loop;

      if Next_Agent_To_Run = null then
         --  Protected action ends. Remove protected agentfrom scheduler.
         Scheduler.Remove_Agent_From_Scheduler (PO);
         PO.Set_State (Inactive);

         --  Object release point 3.
         Release_Resource (PO);
         Check_Sechduler_Agents_For_Next_Task_To_Run
           (Scheduler_Info   => Scheduler_Info,
            Next_Task_To_Run => Next_Agent_To_Run);
      else
         --  Protected action continues.
         Next_Agent_To_Run.Set_State (Runnable);
         PO.Add_Task_To_Protected_Object (Next_Agent_To_Run);
      end if;
   end Process_Exit_Request;

   procedure Acquire_Protected_Object_For_Interrupt
     (PO : not null access
        Agent.Protected_Objects.Protected_Agent'Class) is
   begin
      PO.Set_State (Handling_Interrupt);
      Get_Resource (PO);
   end Acquire_Protected_Object_For_Interrupt;

   procedure Release_Protected_Object_For_Interrupt
     (PO : not null access
        Agent.Protected_Objects.Protected_Agent'Class) is
   begin
      PO.Set_State (Inactive);
      Release_Resource (PO);
   end Release_Protected_Object_For_Interrupt;

end Oak.Protected_Objects;
