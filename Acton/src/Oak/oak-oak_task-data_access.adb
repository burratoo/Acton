with Oak.Oak_Task.Internal;
with Oak.Core;
with Oak.Scheduler;             use Oak.Scheduler;
with Oak.Memory.Call_Stack.Ops; use Oak.Memory.Call_Stack.Ops;

package body Oak.Oak_Task.Data_Access is

   -----------------
   -- Set_Up_Task --
   ------------------
   procedure Initialise_Task
     (T                 : in Oak_Task_Handler;
      Stack_Address     : in System.Address;
      Stack_Size        : in System.Storage_Elements.Storage_Count;
      Name              : in String;
      Normal_Priority   : in Integer;
      Relative_Deadline : in Time_Span;
      Cycle_Period      : in Time_Span;
      Phase             : in Time_Span;
      Run_Loop          : in Address;
      Task_Value_Record : in System.Address;
      Chain             : in out Activation_Chain;
      Elaborated        : in Boolean_Access)
   is
   begin
      T.Name_Length               :=
        Natural'Min (Task_Name'Length, Name'Length);
      T.Name (1 .. T.Name_Length) :=
        Name (Name'First .. Name'First + T.Name_Length - 1);

      T.all :=
        (Kind                   => Regular,
         Num_Entries            => No_Entry,
         Id                     => Internal.New_Task_Id,
         Name                   => T.Name,
         Name_Length            => T.Name_Length,
         State                  => Activation_Pending,
         Shared_State           => No_Shared_State,
         Normal_Priority        => T.Normal_Priority,
         Deadline               => Relative_Deadline,
         Cycle_Period           => Cycle_Period,
         Phase                  => Phase,
         Next_Deadline          => Time_Last,
         Next_Run_Cycle         => Time_Last,
         Wake_Time              => Time_Last,
         Run_Loop               => Run_Loop,
         Call_Stack             => T.Call_Stack,
         Scheduler_Agent        => null,
         Queue_Link             => Blank_Link,
         Deadline_List          => (null, null),
         Memory_List            => null,
         Activation_List        => null,
         Elaborated             => Elaborated,
         Message_Location       => null,
         Is_Protected_Object    => False,
         Tasks_Within           => null,
         Active_Subprogram_Kind => Protected_Function,
         Entry_Queues           => (others => null),
         Entry_Barriers         => null,
         Object_Record          => Null_Address,
         Controlling_Shared_State => Waiting);

      if Stack_Address = Null_Address then
         Allocate_Call_Stack
           (Stack            => T.Call_Stack,
            Size_In_Elements => Stack_Size);

         Initialise_Call_Stack
           (Stack             => T.Call_Stack,
            Start_Instruction => Run_Loop,
            Task_Value_Record => Task_Value_Record,
            Message_Location  => T.Message_Location);
      else
         Initialise_Call_Stack
           (Stack             => T.Call_Stack,
            Start_Instruction => Run_Loop,
            Task_Value_Record => Task_Value_Record,
            Stack_Address     => Stack_Address,
            Stack_Size        => Stack_Size,
            Message_Location  => T.Message_Location);
      end if;

      if Normal_Priority >= Any_Priority'First and
        Normal_Priority <= Any_Priority'Last
      then
         T.Normal_Priority := System.Any_Priority (Normal_Priority);
      elsif Normal_Priority = Unspecified_Priority then
         T.Normal_Priority := Default_Priority;
      else
         raise Program_Error with "Priority out of range";
      end if;

      T.Activation_List := Chain.Head;
      Chain.Head        := T;

   end Initialise_Task;

   procedure Initialise_Main_Task
     (Stack_Size      : System.Storage_Elements.Storage_Count;
      Name            : String;
      Normal_Priority : Integer;
      Run_Loop        : Address)
   is
      T  : constant Oak_Task_Handler         := Oak.Core.Get_Main_Task;
      OI : constant access Oak.Core.Oak_Data := Oak.Core.Get_Oak_Instance;

      Scheduler    : constant access Oak_Scheduler_Info :=
                       Oak.Core.Get_Scheduler_Info (OI);
      Current_Time : constant Time                      := Clock;
   begin
      T.Name_Length               :=
        Natural'Min (Task_Name'Length, Name'Length);
      T.Name (1 .. T.Name_Length) :=
        Name (Name'First .. Name'First + T.Name_Length - 1);

      T.all :=
        (Kind                   => Regular,
         Num_Entries            => No_Entry,
         Id                     => Internal.New_Task_Id,
         Name                   => T.Name,
         Name_Length            => T.Name_Length,
         State                  => Sleeping,
         Shared_State           => No_Shared_State,
         Normal_Priority        => T.Normal_Priority,
         Deadline               => Time_Span_Zero,
         Cycle_Period           => Time_Span_Zero,
         Phase                  => Time_Span_Zero,
         Next_Deadline          => Time_Last,
         Next_Run_Cycle         => Current_Time,
         Wake_Time              => Current_Time,
         Run_Loop               => Run_Loop,
         Call_Stack             => T.Call_Stack,
         Scheduler_Agent        => null,
         Queue_Link             => Blank_Link,
         Deadline_List          => (null, null),
         Memory_List            => null,
         Activation_List        => null,
         Elaborated             => null,
         Message_Location       => null,
         Is_Protected_Object    => False,
         Tasks_Within           => null,
         Active_Subprogram_Kind => Protected_Function,
         Entry_Queues           => (others => null),
         Entry_Barriers         => null,
         Object_Record            => Null_Address,
         Controlling_Shared_State => Waiting);

      Allocate_Call_Stack
        (Stack            => T.Call_Stack,
         Size_In_Elements => Stack_Size);

      Initialise_Call_Stack
        (Stack             => T.Call_Stack,
         Start_Instruction => Run_Loop,
         Message_Location => T.Message_Location);

      if Normal_Priority >= Priority'First and
        Normal_Priority <= Priority'Last
      then
         T.Normal_Priority := System.Any_Priority (Normal_Priority);
      elsif Normal_Priority = Unspecified_Priority then
         T.Normal_Priority := Default_Priority;
      else
         raise Program_Error with "Priority out of range";
      end if;

      Add_Task_To_Scheduler (Scheduler_Info => Scheduler.all, T => T);
   end Initialise_Main_Task;

   -------------------------
   -- Get_Normal_Priority --
   -------------------------

   procedure Set_Scheduler_Agent
     (T               : access Oak_Task;
      Scheduler_Agent : in Oak_Task_Handler)
   is
   begin
      T.Scheduler_Agent := Scheduler_Agent;
   end Set_Scheduler_Agent;

   ------------
   -- Get_Id --
   ------------

   function Get_Id (T : access Oak_Task) return Task_Id is
   begin
      return T.Id;
   end Get_Id;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (T : access Oak_Task) return Task_Name is
   begin
      return T.Name;
   end Get_Name;

   ---------------
   -- Get_State --
   ---------------

   function Get_State (T : access Oak_Task) return Task_State is
   begin
      return T.State;
   end Get_State;

   procedure Set_State (T : access Oak_Task; State : Task_State) is
   begin
      T.State := State;
   end Set_State;

   function Get_Shared_State (For_Task : access Oak_Task)
                              return Task_State is
   begin
      return For_Task.Shared_State.all;
   end Get_Shared_State;

   procedure Set_Shared_State
     (For_Task : access Oak_Task;
      With_State_Pointer : in Shared_Task_State) is
   begin
      For_Task.Shared_State := With_State_Pointer;
   end Set_Shared_State;

   function Get_Oak_Task_Message (For_Task : in Oak_Task_Handler)
                              return Oak_Task_Message is
   begin
      return For_Task.Message_Location.Message;
   end Get_Oak_Task_Message;

   procedure Store_Oak_Task_Message (For_Task : in Oak_Task_Handler;
                                     Message : in Oak_Task_Message) is
   begin
      For_Task.Message_Location.Message := Message;
   end Store_Oak_Task_Message;
   -------------------------
   -- Get_Normal_Priority --
   -------------------------

   function Get_Normal_Priority (T : access Oak_Task) return Any_Priority is
   begin
      return T.Normal_Priority;
   end Get_Normal_Priority;

   ------------------
   -- Get_Deadline --
   ------------------

   function Get_Deadline (T : access Oak_Task) return Time_Span is
   begin
      return T.Deadline;
   end Get_Deadline;

   ----------------------
   -- Get_Cycle_Period --
   ----------------------

   function Get_Cycle_Period (T : access Oak_Task) return Time_Span is
   begin
      return T.Cycle_Period;
   end Get_Cycle_Period;

   ---------------------
   -- Get_Start_Delay --
   ---------------------

   function Get_Phase (T : access Oak_Task) return Time_Span is
   begin
      return T.Phase;
   end Get_Phase;

   function Get_Next_Run_Time (T : access Oak_Task) return Time is
   begin
      return T.Next_Run_Cycle;
   end Get_Next_Run_Time;

   function Get_Wake_Time (T : access Oak_Task) return Time is
   begin
      return T.Wake_Time;
   end Get_Wake_Time;

   procedure Set_Wake_Time (T : access Oak_Task; WT : Time) is
   begin
      T.Wake_Time := WT;
   end Set_Wake_Time;

   function Is_Elaborated (T : access Oak_Task) return Boolean is
   begin
      return T.Elaborated.all;
   end Is_Elaborated;

   procedure Set_Activation_List
     (T     : access Oak_Task;
      Chain : in Activation_Chain_Access)
   is
      TP : access Oak_Task := Chain.Head;
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

   function Get_Activation_List
     (T    : access Oak_Task)
      return access Oak_Task is
   begin
      return T.Activation_List;
   end Get_Activation_List;
end Oak.Oak_Task.Data_Access;
