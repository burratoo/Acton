with Oak.Core;
with Oak.Scheduler;             use Oak.Scheduler;
with Oak.Memory.Call_Stack.Ops; use Oak.Memory.Call_Stack.Ops;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;

package body Oak.Agent.Tasks is

   -----------------
   -- Set_Up_Task --
   ------------------
   procedure Initialise_Agent
     (Agent             : in out Task_Agent'Class;
      Stack_Address     : in System.Address;
      Stack_Size        : in System.Storage_Elements.Storage_Count;
      Name              : in String;
      Normal_Priority   : in Integer;
      Relative_Deadline : in Time_Span;
      Cycle_Period      : in Time_Span;
      Phase             : in Time_Span;
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
      else
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

      if Chain.Head /= null then
         Agent.Activation_List := Chain.Head;
         Chain.Head            := Agent'Unrestricted_Access;
      end if;

   end Initialise_Agent;

   procedure Initialise_Main_Task
     (Stack_Size      : System.Storage_Elements.Storage_Count;
      Name            : String;
      Normal_Priority : Integer;
      Run_Loop        : Address)
   is
      Agent : constant access Task_Agent  := Oak.Core.Main_Task;
      OI : constant access Oak.Core.Oak_Data := Oak.Core.Oak_Instance;

      Scheduler    : constant access Oak_Scheduler_Info :=
                       Oak.Core.Scheduler_Info (OI);
      Current_Time : constant Time                      := Clock;
      No_Chain : Activation_Chain := (Head => null);
   begin
      Initialise_Agent
        (Agent             => Agent.all,
         Stack_Address     => Null_Address,
         Stack_Size        => Stack_Size,
         Name              => Name,
         Normal_Priority   => Normal_Priority,
         Relative_Deadline => Time_Span_Zero,
         Cycle_Period      => Time_Span_Zero,
         Phase             => Time_Span_Zero,
         Run_Loop          => Run_Loop,
         Task_Value_Record => Null_Address,
         Chain             => No_Chain,
         Elaborated        => null);

      Agent.State           := Sleeping;
      Agent.Next_Deadline   := Time_Last;
      Agent.Next_Run_Cycle  := Current_Time;
      Agent.Wake_Time       := Current_Time;

      Add_Task_To_Scheduler (Scheduler_Info => Scheduler.all, T => Agent);
   end Initialise_Main_Task;

   procedure Set_Activation_List
     (T     : not null access Task_Agent'Class;
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

   procedure Store_Oak_Task_Message
     (For_Task : not null access Task_Agent'Class;
      Message  : in Oak_Task_Message) is
   begin
      For_Task.Message_Location.Message := Message;
   end Store_Oak_Task_Message;

   procedure Set_Scheduler_Agent
     (T               : not null access Task_Agent'Class;
      Scheduler_Agent : access Scheduler.Scheduler_Agent'Class)
   is
   begin
      T.Scheduler_Agent := Scheduler_Agent;
   end Set_Scheduler_Agent;

   procedure Set_Shared_State
     (For_Task : not null access Task_Agent'Class;
      With_State_Pointer : in Shared_Task_State) is
   begin
      For_Task.Shared_State := With_State_Pointer;
   end Set_Shared_State;

   procedure Set_State
     (T     : not null access Task_Agent'Class;
      State : in Task_State) is
   begin
      T.State := State;
   end Set_State;

   procedure Set_Wake_Time
     (T  : not null access Task_Agent'Class;
      WT : in Time) is
   begin
      T.Wake_Time := WT;
   end Set_Wake_Time;

end Oak.Agent.Tasks;
