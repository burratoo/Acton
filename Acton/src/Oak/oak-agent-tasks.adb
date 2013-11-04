with Oak.Agent.Schedulers;
with Oak.Agent.Tasks.Cycle;
with System;                            use System;
with System.Storage_Elements;           use System.Storage_Elements;
with Oak.Agent.Protected_Objects; use Oak.Agent.Protected_Objects;
with Oak.Scheduler;

package body Oak.Agent.Tasks is

   -----------------
   -- Set_Up_Task --
   ------------------

   procedure Initialise_Task_Agent
     (Agent             : not null access Task_Agent'Class;
      Stack_Address     : in System.Address;
      Stack_Size        : in System.Storage_Elements.Storage_Count;
      Name              : in String;
      Run_Loop          : in System.Address;
      Task_Value_Record : in System.Address;
      Normal_Priority   : in Integer;
      Cycle_Behaviour   : in Ada.Cyclic_Tasks.Behaviour;
      Cycle_Period      : in Oak_Time.Time_Span;
      Phase             : in Oak_Time.Time_Span;
      Execution_Budget  : in Oak_Time.Time_Span;
      Budget_Action     : in Ada.Cyclic_Tasks.Event_Response;
      Budget_Handler    : in Ada.Cyclic_Tasks.Response_Handler;
      Relative_Deadline : in Oak_Time.Time_Span;
      Deadline_Action   : in Ada.Cyclic_Tasks.Event_Response;
      Deadline_Handler  : in Ada.Cyclic_Tasks.Response_Handler;
      Scheduler_Agent   : access Schedulers.Scheduler_Agent'Class;
      Chain             : in out Activation_Chain;
      Elaborated        : in Boolean_Access)
   is
   begin
      Oak.Agent.Initialise_Agent
        (Agent              => Agent,
         Name               => Name,
         Call_Stack_Address => Stack_Address,
         Call_Stack_Size    => Stack_Size,
         Run_Loop           => Run_Loop,
         Run_Loop_Parameter => Task_Value_Record,
         Normal_Priority    => Normal_Priority,
         Initial_State      => Activation_Pending,
         Wake_Time          => Time_Last);

      Agent.Cycle_Behaviour   := Cycle_Behaviour;
      Agent.Cycle_Period      := Cycle_Period;
      Agent.Phase             := Phase;

      Agent.Execution_Budget  := Execution_Budget;
      Agent.Relative_Deadline := Relative_Deadline;

      if Deadline_Action in Ada.Cyclic_Tasks.Handler then
         Agent.Deadline_Timer.Set_Timer
           (Priority        =>
              Protected_Object_From_Access (Deadline_Handler).Normal_Priority,
            Timer_Action    => Deadline_Action,
            Handler         => Deadline_Handler,
            Agent_To_Handle => Agent);
      else
         Agent.Deadline_Timer.Set_Timer
           (Priority        => Normal_Priority + 1,
            Timer_Action    => Deadline_Action,
            Handler         => Deadline_Handler,
            Agent_To_Handle => Agent);
      end if;

      if Budget_Action in Ada.Cyclic_Tasks.Handler then
         Agent.Execution_Timer.Set_Timer
           (Priority        =>
              Protected_Object_From_Access (Budget_Handler).Normal_Priority,
            Timer_Action    => Budget_Action,
            Handler         => Budget_Handler,
            Agent_To_Handle => Agent);
      else
         Agent.Execution_Timer.Set_Timer
           (Priority        => Oak_Priority'Last,
            Timer_Action    => Budget_Action,
            Handler         => Budget_Handler,
            Agent_To_Handle => Agent);
      end if;

      Agent.Elaborated   := Elaborated;

      if Normal_Priority in Any_Priority then
         Agent.Normal_Priority := System.Any_Priority (Normal_Priority);
      elsif Normal_Priority = Unspecified_Priority then
         Agent.Normal_Priority := Default_Priority;
      else
         raise Program_Error with "Priority out of range";
      end if;

      if Scheduler_Agent = null then
         Agent.Scheduler_Agent :=
           Scheduler.Find_Scheduler_For_System_Priority (Normal_Priority, 1);
      else
         Agent.Scheduler_Agent  := Scheduler_Agent;
      end if;

      Agent.Next_Run_Cycle   := Oak_Time.Time_Last;
      Agent.Remaining_Budget := Oak_Time.Time_Span_Last;
      Agent.Event_Raised     := False;

      Agent.Activation_List := Chain.Head;
      Chain.Head            := Agent;

   end Initialise_Task_Agent;

   overriding function Destination_On_Wake_Up (Agent : in out Task_Agent)
                                    return Wake_Destination is
   begin
      case Agent.State is
         when Sleeping =>
            Agent.State := Runnable;
            return Run_Queue;

         when Sleeping_And_Waiting =>
            if Agent.Event_Raised then
               Agent.Event_Raised := False;
               Oak.Agent.Tasks.Cycle.Task_Released (Agent'Access);
               return Run_Queue;
            else
               Agent.State := Waiting_For_Event;
               return Remove;
            end if;

         when others =>
            --  Should raise an exception here
            return Run_Queue;
      end case;
   end Destination_On_Wake_Up;

   procedure Set_Activation_List
     (T   : in out Task_Agent'Class;
      Add : access Task_Agent'Class) is
   begin
      T.Activation_List := Add;
   end Set_Activation_List;

   procedure Set_Activation_List
     (T     : in out Task_Agent'Class;
      Chain : in Activation_Chain_Access)
   is
      TP : access Task_Agent'Class := Chain.Head;
   begin
      --  Only set the task's activation list if all tasks's in the activation
      --  chain have been activated. Raise Program_Error otherwise.

      while TP /= null and then TP.Elaborated.all loop
         TP := TP.Activation_List;
      end loop;

      if TP /= null and then TP.Elaborated.all = False then
         raise Program_Error with "task bodies not elaborated";
      end if;

      T.Activation_List := Chain.Head;
   end Set_Activation_List;

   procedure Set_Cycle_Period
     (T  : in out Task_Agent'Class;
      CP : in Oak_Time.Time_Span) is
   begin
      T.Cycle_Period := CP;
   end Set_Cycle_Period;

   procedure Set_Next_Deadline_For_Task
     (T     : in out Task_Agent'Class;
      Using : in Deadline_Base) is

   begin
      if T.Relative_Deadline = Oak_Time.Time_Span_Last then
         T.Deadline_Timer.Remove_Timer;

      else
         case Using is
            when Wake_Up_Time =>
               T.Absolute_Deadline := T.Wake_Time + T.Relative_Deadline;
               T.Deadline_Timer.Update_Timer (New_Time => T.Absolute_Deadline);
            when Clock_Time =>
               T.Absolute_Deadline := Clock + T.Relative_Deadline;
               T.Deadline_Timer.Update_Timer (New_Time => T.Absolute_Deadline);
         end case;

         if not T.Deadline_Timer.Is_Armed then
            Oak.Timers.Add_Timer_To_Current_Processor
              (T.Deadline_Timer'Unchecked_Access);
         end if;
      end if;
   end Set_Next_Deadline_For_Task;

   procedure Set_Relative_Deadline
     (T  : in out Task_Agent'Class;
      RD : in Oak_Time.Time_Span) is
   begin
      T.Relative_Deadline := RD;
   end Set_Relative_Deadline;

end Oak.Agent.Tasks;
