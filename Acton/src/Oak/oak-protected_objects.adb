------------------------------------------------------------------------------
--                                                                          --
--                              OAK COMPONENTS                              --
--                                                                          --
--                           OAK.PROTECTED_OBJECTS                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                 Copyright (C) 2011-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

with Oak.Agent.Oak_Agent;         use Oak.Agent.Oak_Agent;
with Oak.Agent.Protected_Objects; use Oak.Agent.Protected_Objects;
with Oak.Agent.Tasks;             use Oak.Agent.Tasks;

with Oak.Scheduler; use Oak.Scheduler;
with Oak.States;    use Oak.States;

package body Oak.Protected_Objects is

   --------------------------------------------
   -- Acquire_Protected_Object_For_Interrupt --
   --------------------------------------------

   procedure Acquire_Protected_Object_For_Interrupt (PO : in Protected_Id) is
   begin
      --  Need a Lock around this.
      Set_State (For_Agent => PO, State => Handling_Interrupt);
   end Acquire_Protected_Object_For_Interrupt;

   ---------------------------
   -- Process_Enter_Request --
   ---------------------------

   procedure Process_Enter_Request
     (Entering_Agent    : in  Task_Id;
      PO                : in  Protected_Id;
      Subprogram_Kind   : in  Protected_Subprogram_Type;
      Entry_Id          : in  Entry_Index;
      Next_Agent_To_Run : out Oak_Agent_Id;
      Resubmitted       : in  Boolean := False)
   is
   begin
      --  Sanity check to make sure the entry id is valid.

      if Subprogram_Kind = Protected_Entry and then
           not Is_Entry_Id_Valid (PO, Entry_Id)
      then
         Set_State (For_Agent => Entering_Agent, State => Enter_PO_Refused);
         Next_Agent_To_Run := Entering_Agent;
         return;
      end if;

      --  Check for ceiling protocol volation.

      if Normal_Priority (Entering_Agent) > Normal_Priority (PO) then
         Set_State (For_Agent => Entering_Agent, State => Enter_PO_Refused);
         Next_Agent_To_Run := Entering_Agent;
         return;
      end if;

      --  Need to lock the protected object from other processors here since we
      --  are making modifications to the object:
      --     1. We encounter an error while evaluating the barrier states when
      --        a task calls an entry.
      --     2. There are no tasks able to run in the protected object.
      --     3. Once the protected action is completed in Process_Exit_Request.
      --  Acquire_Lock  (PO);

      if not Resubmitted then
         Remove_Agent_From_Scheduler (Agent => Entering_Agent);
         Set_Id_Of_Entry (Entering_Agent, Entry_Id);
      end if;

      if Task_Within (PO) = No_Agent then
         Next_Agent_To_Run := No_Agent;

         if Subprogram_Kind = Protected_Entry then
            --  Handle protected entry. Note that it only selects the next
            --  task to run inside the protected object and the actual
            --  placement inside the protected object occurs after this block.

            Handle_Entry : declare
               Open_Entry       : Entry_Index;
               Exception_Raised : Boolean;
            begin
               --  Add the entering task to its entry queue. Need to do this
               --  before the queue check as its presence may effect the state
               --  of the entry queue's barrier.

               Set_State (Entering_Agent, Waiting_For_Protected_Object);
               Add_Task_To_Entry_Queue
                 (PO       => PO,
                  T        => Entering_Agent,
                  Entry_Id => Entry_Id);

               Find_Open_Entry
                 (Protected_Object => PO,
                  Open_Entry       => Open_Entry,
                  Exception_Raised => Exception_Raised,
                  Preference       => Entry_Id);

               if not Exception_Raised then
                  if Open_Entry /= No_Entry then
                     Get_And_Remove_Next_Task_From_Entry_Queue
                       (PO        => PO,
                        Entry_Id  => Open_Entry,
                        Next_Task => Next_Agent_To_Run);
                  else
                     Next_Agent_To_Run := No_Agent;
                  end if;

               else
                  --  Exception has been raised, the queues are purged.
                  Purge_Entry_Queues
                    (PO, New_Task_State => Enter_PO_Refused);

                  Next_Agent_To_Run := Entering_Agent;
                  return;

               end if;
            end Handle_Entry;
         else
            --  Handle protected procedure or function. Only need to select
            --  the next agent to run here since the code to place the agent
            --  inside the protected object occurs below (because we need to
            --  cover the situation where an entry becomes queued).

            Next_Agent_To_Run := Entering_Agent;

         end if;

         if Next_Agent_To_Run /= No_Agent then
            Add_Task_To_Protected_Object (PO, T => Next_Agent_To_Run);
            Set_State (Entering_Agent, Runnable);

            if State (PO) = Inactive then
               --  Run protected agent
               Set_State (For_Agent => PO, State => Runnable);
               Add_Agent_To_Scheduler (PO);
            end if;

            Next_Agent_To_Run := PO;
         end if;

      elsif Subprogram_Kind = Protected_Function and then
        Active_Subprogram_Kind (PO) = Protected_Function then
         --  Another task is operating inside a protected function, so this
         --  task is able to join as well.

         Set_State (Entering_Agent, Runnable);
         Add_Task_To_Protected_Object (PO, Entering_Agent);
         Next_Agent_To_Run := PO;
      else
         --  Protected object is currently occupied by someone else.

         Add_Contending_Task (PO, Entering_Agent);
         Next_Agent_To_Run := No_Agent;
      end if;

      if Next_Agent_To_Run = No_Agent then
         --  No active task inside the protected object so need to find a new
         --  Agent to run.
         --  Object release point 2.
         --  Release Lock(PO);
         Check_Sechduler_Agents_For_Next_Agent_To_Run
           (Next_Agent_To_Run => Next_Agent_To_Run);
      end if;

   end Process_Enter_Request;

   --------------------------
   -- Process_Exit_Request --
   --------------------------

   procedure Process_Exit_Request
     (Exiting_Agent     : in Task_Id;
      PO                : in  Protected_Id;
      Next_Agent_To_Run : out Oak_Agent_Id)
   is
   begin

      --  Make sure that the exiting task is actually inside the object.

      if not Is_Task_Inside_Protect_Object
        (PO => PO, T => Exiting_Agent)
      then
         Set_State (Exiting_Agent, Exit_PO_Error);
         Next_Agent_To_Run := Exiting_Agent;
         return;
      end if;

      Set_State (Exiting_Agent, Runnable);
      Remove_Task_From_Within_Protected_Object (PO, Exiting_Agent);
      Add_Agent_To_Scheduler (Exiting_Agent);

      if Has_Entries (PO) then
         --  Service entries.

         declare
            E          : Boolean;
            Next_Entry : Entry_Index;
         begin
            Find_Open_Entry
              (Protected_Object => PO,
               Open_Entry       => Next_Entry,
               Exception_Raised => E);

            if E then
               Purge_Entry_Queues
                 (PO, New_Task_State => Enter_PO_Refused);
               Next_Entry := No_Entry;
            end if;

            if Next_Entry /= No_Entry then
               Get_And_Remove_Next_Task_From_Entry_Queue
                 (PO        => PO,
                  Entry_Id  => Next_Entry,
                  Next_Task => Next_Agent_To_Run);

               --  If there is a queued task to service, allow it to execute
               --  inside the protected object.

               if Next_Agent_To_Run /= No_Agent then
                  Set_State
                    (For_Agent => Next_Agent_To_Run, State => Runnable);
                  Add_Task_To_Protected_Object
                    (PO => PO, T => Next_Agent_To_Run);
               end if;
            end if;
         end;
      end if;

      while Next_Agent_To_Run = No_Agent loop
         --  Admit new contending taks. Need to loop to cover the case where a
         --  task may end up on an entry queue.

         Get_And_Remove_Next_Contending_Task (PO, Next_Agent_To_Run);

         --  Exit here when there are no more tasks in the contending queue.
         exit when Next_Agent_To_Run = No_Agent;

         Process_Enter_Request
           (Entering_Agent    => Next_Agent_To_Run,
            PO                => Protected_Agent_To_Access (Next_Agent_To_Run),
            Subprogram_Kind   => Protected_Subprogram_Kind (Next_Agent_To_Run),
            Entry_Id          => Id_Of_Entry (Next_Agent_To_Run),
            Next_Agent_To_Run => Next_Agent_To_Run,
            Resubmitted       => True);
      end loop;

      --  If there is no agents to run inside the protected object, the
      --  protected object is made inactive.

      if Next_Agent_To_Run = No_Agent then
         Remove_Agent_From_Scheduler (PO);
         Set_State (PO, Inactive);

         --  Object release point 3.
         --  Release Agent (PO);

         Check_Sechduler_Agents_For_Next_Agent_To_Run
           (Next_Agent_To_Run => Next_Agent_To_Run);
      end if;
   end Process_Exit_Request;

   ----------------------------
   -- Process_Interrupt_Exit --
   ----------------------------

   procedure Process_Interrupt_Exit
     (PO                : in  Protected_Id;
      Next_Agent_To_Run : out Oak_Agent_Id) is
   begin
      Next_Agent_To_Run := No_Agent;

      if Has_Entries (PO) then
         --  Service entries.
         declare
            E          : Boolean;
            Next_Entry : Entry_Index;
         begin
            Find_Open_Entry
              (Protected_Object => PO,
               Open_Entry       => Next_Entry,
               Exception_Raised => E);

            if E then
               Purge_Entry_Queues
                 (PO, New_Task_State => Enter_PO_Refused);
               Next_Entry := No_Entry;
            end if;

            if Next_Entry /= No_Entry then
               Get_And_Remove_Next_Task_From_Entry_Queue
                 (PO        => PO,
                  Entry_Id  => Next_Entry,
                  Next_Task => Next_Agent_To_Run);
            end if;
         end;
      end if;

      if Next_Agent_To_Run /= No_Agent then
         Set_State
           (For_Agent => Next_Agent_To_Run, State => Runnable);
         Add_Task_To_Protected_Object
           (PO => PO, T => Next_Agent_To_Run);

         --  The protected agent has serviced an interrupt handler
         --  then it will not be presence in a runnable queue.

         --  Run protected agent
         Set_State (For_Agent => PO, State => Runnable);
         Add_Agent_To_Scheduler (PO);
      else
         Set_State (PO, Inactive);
      end if;

      --  If there is no agents to run inside the protected object, the
      --  protected object is made inactive.

      if Next_Agent_To_Run = No_Agent then
         Check_Sechduler_Agents_For_Next_Agent_To_Run
           (Next_Agent_To_Run => Next_Agent_To_Run);
      end if;
   end Process_Interrupt_Exit;

   --------------------------------------------
   -- Release_Protected_Object_For_Interrupt --
   --------------------------------------------

   procedure Release_Protected_Object_For_Interrupt (PO : in Protected_Id) is
   begin
      --  Need a lock around this.
      Set_State (PO, Inactive);
   end Release_Protected_Object_For_Interrupt;

end Oak.Protected_Objects;
