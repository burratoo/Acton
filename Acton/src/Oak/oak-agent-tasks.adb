with Oak.Agent.Schedulers;
with Oak.Agent.Tasks.Cycle;
with Oak.Atomic_Actions;
with Oak.Core_Support_Package.Call_Stack;
with Oak.Memory.Call_Stack.Ops; use Oak.Memory.Call_Stack.Ops;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with Oak.Agent.Tasks.Protected_Objects; use Oak.Agent.Tasks.Protected_Objects;

package body Oak.Agent.Tasks is

   -----------------
   -- Set_Up_Task --
   ------------------

   procedure Initialise_Task_Agent
     (Agent             : access Task_Agent'Class;
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
      Budget_Action     : in Ada.Cyclic_Tasks.Event_Action;
      Budget_Handler    : in Ada.Cyclic_Tasks.Action_Handler;
      Relative_Deadline : in Oak_Time.Time_Span;
      Deadline_Action   : in Ada.Cyclic_Tasks.Event_Action;
      Deadline_Handler  : in Ada.Cyclic_Tasks.Action_Handler;
      Execution_Server  : access Ada.Execution_Server.Execution_Server;
      Chain             : in out Activation_Chain;
      Elaborated        : in Boolean_Access)
   is
      Call_Stack : Call_Stack_Handler;
   begin
      Oak.Agent.Initialise_Agent
        (Agent      => Agent,
         Name       => Name,
         Call_Stack => Call_Stack);

      Agent.State             := Activation_Pending;
      Agent.Shared_State      := No_Shared_State;

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
           (Priority        => Oak_Priority'Last,
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

      Agent.Execution_Server  := Execution_Server;

      Agent.Elaborated   := Elaborated;

      if Stack_Address = Null_Address and Stack_Size > 0 then
         Allocate_Call_Stack
           (Stack            => Agent.Call_Stack,
            Size_In_Elements => Stack_Size);

         Initialise_Call_Stack
           (Stack             => Agent.Call_Stack,
            Start_Instruction => Run_Loop,
            Task_Value_Record => Task_Value_Record,
            Message_Location  => Agent.Message_Location);
      elsif Stack_Address /= Null_Address then
         Initialise_Call_Stack
           (Stack             => Agent.Call_Stack,
            Start_Instruction => Run_Loop,
            Task_Value_Record => Task_Value_Record,
            Stack_Address     => Stack_Address,
            Stack_Size        => Stack_Size,
            Message_Location  => Agent.Message_Location);
      end if;

      if Normal_Priority in Any_Priority then
         Agent.Normal_Priority := System.Any_Priority (Normal_Priority);
      elsif Normal_Priority = Unspecified_Priority then
         Agent.Normal_Priority := Default_Priority;
      else
         raise Program_Error with "Priority out of range";
      end if;

      Agent.Activation_List := Chain.Head;
      Chain.Head            := Agent;

   end Initialise_Task_Agent;

   procedure Initialise_Sleep_Agent
     (Agent    : access Task_Agent'Class;
      Run_Loop : in System.Address)
   is
      C : Activation_Chain;
   begin
      Initialise_Task_Agent
        (Agent             => Agent,
         Stack_Address     => Null_Address,
         Stack_Size        => Core_Support_Package.Call_Stack.Sleep_Stack_Size,
         Name              => "Sleep",
         Run_Loop          => Run_Loop,
         Task_Value_Record => Null_Address,
         Normal_Priority   => Priority'First,
         Cycle_Behaviour   => Ada.Cyclic_Tasks.Normal,
         Cycle_Period      => Oak_Time.Time_Span_Last,
         Phase             => Oak_Time.Time_Span_Zero,
         Execution_Budget  => Oak_Time.Time_Span_Last,
         Budget_Action     => Ada.Cyclic_Tasks.No_Action,
         Budget_Handler    => null,
         Relative_Deadline => Oak_Time.Time_Span_Last,
         Deadline_Action   => Ada.Cyclic_Tasks.No_Action,
         Deadline_Handler  => null,
         Execution_Server  => null,
         Chain             => C,
         Elaborated        => null);

      Agent.State := Runnable;
   end Initialise_Sleep_Agent;

   overriding procedure Charge_Execution_Time
     (To_Agent  : in out Task_Agent;
      Exec_Time : in Oak_Time.Time_Span) is
   begin
      To_Agent.Remaining_Budget := To_Agent.Remaining_Budget - Exec_Time;
      Oak.Agent.Charge_Execution_Time (Oak_Agent (To_Agent), Exec_Time);
   end Charge_Execution_Time;

   function Destination_On_Wake_Up (T : in out Task_Agent'Class)
     return Destination is
   begin
      case T.State is
         when Sleeping =>
            T.State := Runnable;
            return Run_Queue;

         when Sleeping_And_Waiting =>
            if T.Event_Raised then
               T.Event_Raised := False;
               Oak.Agent.Tasks.Cycle.Task_Released (T'Access);
               return Run_Queue;
            else
               T.State := Waiting_For_Event;
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

   procedure Set_Current_Atomic_Action
     (T  : in out Task_Agent'Class;
      AA : access Atomic_Actions.Atomic_Object) is
   begin
      T.In_Atomic_Action := AA;
   end Set_Current_Atomic_Action;

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
               T.Deadline_Timer.Update_Timer
                 (New_Time => T.Wake_Time + T.Relative_Deadline);
            when Clock_Time =>
               T.Deadline_Timer.Update_Timer
                 (New_Time =>  Clock + T.Relative_Deadline);
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

   procedure Set_Scheduler_Agent
     (T     : in out Task_Agent'Class;
      Agent : access Schedulers.Scheduler_Agent'Class)
   is
   begin
      T.Scheduler_Agent := Agent;
   end Set_Scheduler_Agent;

   procedure Set_Scheduler_Agent_For_Task
     (T     : in out Task_Agent'Class;
      Agent : access Schedulers.Scheduler_Agent'Class) is
   begin
      T.Scheduler_Agent := Agent;
   end Set_Scheduler_Agent_For_Task;

   procedure Set_Shared_State
     (For_Task : in out Task_Agent'Class;
      With_State_Pointer : in Shared_Task_State) is
   begin
      For_Task.Shared_State := With_State_Pointer;
   end Set_Shared_State;

   procedure Set_State
     (T     : in out Task_Agent'Class;
      State : in Task_State) is
   begin
      T.State := State;
   end Set_State;

   procedure Store_Task_Yield_Status
     (For_Task : in out Task_Agent'Class;
      Yielded  : in Yielded_State)
   is
   begin
      For_Task.Message_Location.Yield_Status := Yielded;
   end Store_Task_Yield_Status;

   procedure Set_Wake_Time
     (T  : in out Task_Agent'Class;
      WT : in Oak_Time.Time) is
   begin
      T.Wake_Time := WT;
   end Set_Wake_Time;

   procedure Store_Oak_Task_Message
     (For_Task : in out Task_Agent'Class;
      Message  : in Oak_Task_Message) is
   begin
      For_Task.Message_Location.Message := Message;
   end Store_Oak_Task_Message;

end Oak.Agent.Tasks;
