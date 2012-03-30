with Oak.Oak_Task.Protected_Object; use Oak.Oak_Task.Protected_Object;
with Oak.Oak_Task.Data_Access;

package body Oak.Protected_Object is

   procedure Process_Enter_Request
     (Scheduler_Info  : in out Scheduler.Oak_Scheduler_Info;
      T               : in Oak_Task_Handler;
      PO              : in Oak_Task_Handler;
      Subprogram_Kind : in Protected_Subprogram_Type;
      Entry_Id        : in Entry_Index;
      Chosen_Task     : out Oak_Task_Handler) is
   begin
      --  Check that the request is valid
      if PO = null then
         raise Program_Error;
      end if;

      if not Is_Protected_Object (PO) or
        (Subprogram_Kind = Protected_Entry and then
           not Is_Entry_Id_Valid (PO => PO, Entry_Id => Entry_Id))
      then
         Oak_Task.Data_Access.Set_State (T => T, State => Enter_PO_Refused);
         Chosen_Task := T;
         return;
      end if;

      if Oak_Task.Data_Access.Get_Normal_Priority (T => T) >
        Oak_Task.Data_Access.Get_Normal_Priority (T => PO)
      then
         Oak_Task.Data_Access.Set_State (T     => T,
                                         State => Enter_PO_Refused);
         Chosen_Task := T;
         return;
      end if;

      if Oak_Task.Data_Access.Get_State (PO) = Inactive then
         Scheduler.Remove_Task_From_Scheduler (T);
         declare
         begin
            if Subprogram_Kind = Protected_Entry and then
               not Is_Barrier_Open (PO => PO, Entry_Id => Entry_Id) then
               Oak_Task.Data_Access.Set_State (T => T, State => Waiting);
               Add_Task_To_Entry_Queue (PO       => PO,
                                        T        => T,
                                        Entry_Id => Entry_Id);
               if Has_Count_Attribute (PO) then
                  Get_And_Remove_Next_Task_From_Entry_Queues
                    (PO => PO, Next_Task => Chosen_Task);
               end if;
            else
               Add_Task_To_Protected_Object (T  => T, PO => PO);
               Oak_Task.Data_Access.Set_State (T => T, State => Runnable);
               Set_Acquiring_Tasks_State (For_Protected_Object => PO,
                                          To_State             => Waiting);
               Scheduler.Activate_Task (Scheduler_Info => Scheduler_Info,
                                        T              => PO);
               Chosen_Task := PO;
            end if;
         exception
            when Program_Error =>
               Scheduler.Add_Task_To_Scheduler
                 (Scheduler_Info => Scheduler_Info,
                  T              => T);
               --  Add call to check if we need to decativate the PO.
               --  Add call to see if we need to remove the task from the PO.
               Oak_Task.Data_Access.Set_State (T     => T,
                                               State => Enter_PO_Refused);
               Chosen_Task := T;
               return;
         end;
      elsif Subprogram_Kind = Protected_Function and
               Get_Active_Subprogram_Kind (PO) = Protected_Function then
         Scheduler.Remove_Task_From_Scheduler (T);
         Oak_Task.Data_Access.Set_State (T => T, State => Runnable);
         Add_Task_To_Protected_Object (T  => T, PO => PO);
         Chosen_Task := PO;
      else
         Oak_Task.Data_Access.Set_State (T => T, State => Shared_State);
         Oak_Task.Data_Access.Set_Shared_State
           (For_Task           => T,
            With_State_Pointer => Get_Reference_To_Acquiring_Tasks_State
                                              (For_Protected_Object => PO));
         Chosen_Task := null;
      end if;

      if Chosen_Task = null then
         Scheduler.Check_With_Scheduler_Agents_On_Which_Task_To_Run_Next
           (Scheduler_Info => Scheduler_Info,
            Chosen_Task    => Chosen_Task);
      end if;

   end Process_Enter_Request;

   procedure Process_Exit_Request
     (Scheduler_Info    : in out Scheduler.Oak_Scheduler_Info;
      T                 : in Oak_Task_Handler;
      PO                : in Oak_Task_Handler;
      Chosen_Task       : out Oak_Task_Handler) is
   begin
      if not Is_Task_Inside_Protect_Object (T => T, PO  => PO) then
         Oak_Task.Data_Access.Set_State (T => T, State => Exit_PO_Error);
         return;
      end if;

      Oak_Task.Data_Access.Set_State (T => T, State => Runnable);
      Remove_Task_From_Protected_Object (T => T, PO => PO);
      Scheduler.Add_Task_To_Scheduler (Scheduler_Info => Scheduler_Info,
                                       T              => T);

      Get_And_Remove_Next_Task_From_Entry_Queues
        (PO => PO, Next_Task => Chosen_Task);

      if Chosen_Task = null then
         Set_Acquiring_Tasks_State (For_Protected_Object => PO,
                                    To_State             => Entering_PO);
         Scheduler.Deactivate_Task (Scheduler_Info => Scheduler_Info,
                                    T              => PO);
         Scheduler.Check_With_Scheduler_Agents_On_Which_Task_To_Run_Next
           (Scheduler_Info => Scheduler_Info,
            Chosen_Task    => Chosen_Task);
      else
         Oak_Task.Data_Access.Set_State (T     => Chosen_Task,
                                         State => Runnable);
         Add_Task_To_Protected_Object (T  => Chosen_Task, PO => PO);
      end if;
   end Process_Exit_Request;

end Oak.Protected_Object;
