with Oak.Agent.Schedulers;
with Oak.Atomic_Actions;
with Oak.Memory.Call_Stack.Ops; use Oak.Memory.Call_Stack.Ops;
with System; use System;

with System.Storage_Elements; use System.Storage_Elements;

package body Oak.Agent.Tasks is

   -----------------
   -- Set_Up_Task --
   ------------------

   procedure Initialise_Task_Agent
     (Agent             : access Task_Agent'Class;
      Stack_Address     : in System.Address;
      Stack_Size        : in System.Storage_Elements.Storage_Count;
      Name              : in String;
      Normal_Priority   : in Integer;
      Relative_Deadline : in Oak_Time.Time_Span;
      Cycle_Period      : in Oak_Time.Time_Span;
      Phase             : in Oak_Time.Time_Span;
      Run_Loop          : in System.Address;
      Task_Value_Record : in System.Address;
      Chain             : in out Activation_Chain;
      Elaborated        : in Boolean_Access)
   is
      Call_Stack : Call_Stack_Handler;
   begin
      Oak.Agent.Initialise_Agent
        (Agent      => Agent,
         Name       => Name,
         Call_Stack => Call_Stack);

      Agent.State        := Activation_Pending;
      Agent.Shared_State := No_Shared_State;
      Agent.Deadline     := Relative_Deadline;
      Agent.Cycle_Period := Cycle_Period;
      Agent.Phase        := Phase;
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

   procedure Next_Run_Cycle (T : in out Task_Agent'Class) is
   begin
      T.Wake_Time      := T.Next_Run_Cycle;
      T.Next_Run_Cycle := T.Next_Run_Cycle + T.Cycle_Period;
   end Next_Run_Cycle;

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
     (T : in out Task_Agent'Class;
      Atomic_Action : access Atomic_Actions.Atomic_Action_State) is
   begin
      T.Atomic_Action := Atomic_Action;
   end Set_Current_Atomic_Action;

   procedure Set_Cycle_Period
     (T  : in out Task_Agent'Class;
      CP : in Oak_Time.Time_Span) is
   begin
      T.Cycle_Period := CP;
   end Set_Cycle_Period;

   procedure Set_Relative_Deadline
     (T  : in out Task_Agent'Class;
      RD : in Oak_Time.Time_Span) is
   begin
      T.Deadline := RD;
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
