with Oak.Oak_Time; use Oak.Oak_Time;
with Oak.Memory.Call_Stack.Ops; use Oak.Memory.Call_Stack.Ops;

package body Oak.Agent is

   Global_Task_Id : Task_Id := 1;

   function New_Task_Id return Task_Id;

   procedure Initialise_Agent
     (Agent      : not null access Oak_Agent'Class;
      Name       : in String) is
   begin
      Agent.Name_Length                   :=
         Natural'Min (Task_Name'Length, Name'Length);
      Agent.Name (1 .. Agent.Name_Length) :=
        Name (Name'First .. Name'First + Agent.Name_Length - 1);

      Agent.Id         := New_Task_Id;

      Agent.Total_Execution_Time   := Oak_Time.Time_Span_Zero;
      Agent.Max_Execution_Time     := Oak_Time.Time_Span_Zero;
      Agent.Current_Execution_Time := Oak_Time.Time_Span_Zero;
      Agent.Execution_Cycles       := 0;

      Agent.Call_Stack := No_Call_Stack;

   end Initialise_Agent;

   procedure Initialise_Agent
     (Agent      : not null access Oak_Agent'Class;
      Name       : in String;
      Call_Stack : in Call_Stack_Handler) is
   begin
      Initialise_Agent (Agent, Name);
      Agent.Call_Stack := Call_Stack;
   end Initialise_Agent;

   procedure Initialise_Agent
     (Agent           : not null access Oak_Agent'Class;
      Name            : in String;
      Call_Stack_Size : in System.Storage_Elements.Storage_Count) is
   begin
      Initialise_Agent (Agent, Name);
      Oak.Memory.Call_Stack.Ops.Allocate_Call_Stack
        (Stack            => Agent.Call_Stack,
         Size_In_Elements => Call_Stack_Size);
   end Initialise_Agent;

   procedure Initialise_Agent
     (Agent              : not null access Oak_Agent'Class;
      Name               : in String;
      Call_Stack_Address : in Address;
      Call_Stack_Size    : in Storage_Count;
      Run_Loop           : in Address;
      Run_Loop_Parameter : in Address;
      Normal_Priority    : in Integer;
      Initial_State      : in Agent_State) is
   begin
      Initialise_Agent (Agent, Name);

      if Call_Stack_Address = Null_Address and Call_Stack_Size > 0 then
         Allocate_Call_Stack
           (Stack            => Agent.Call_Stack,
            Size_In_Elements => Call_Stack_Size);

         Initialise_Call_Stack
           (Stack             => Agent.Call_Stack,
            Start_Instruction => Run_Loop,
            Task_Value_Record => Run_Loop_Parameter,
            Message_Location  => Agent.Message_Store);
      elsif Call_Stack_Address /= Null_Address then
         Initialise_Call_Stack
           (Stack             => Agent.Call_Stack,
            Start_Instruction => Run_Loop,
            Task_Value_Record => Run_Loop_Parameter,
            Stack_Address     => Call_Stack_Address,
            Stack_Size        => Call_Stack_Size,
            Message_Location  => Agent.Message_Store);
      else
         Agent.Message_Store := null;
      end if;

      Agent.Normal_Priority   := Normal_Priority;
      Agent.State             := Initial_State;
      Agent.Wake_Time         := Oak_Time.Time_Last;
      Agent.Absolute_Deadline := Oak_Time.Time_Last;
   end Initialise_Agent;

   procedure Charge_Execution_Time
     (To_Agent  : in out Oak_Agent;
      Exec_Time : in Oak_Time.Time_Span) is
   begin
      To_Agent.Current_Execution_Time :=
        To_Agent.Current_Execution_Time + Exec_Time;
      To_Agent.Total_Execution_Time :=
        To_Agent.Total_Execution_Time + Exec_Time;
   end Charge_Execution_Time;

   function Destination_On_Wake_Up (Agent : in out Oak_Agent)
                                    return Wake_Destination is
      pragma Unreferenced (Agent);
   begin
      return Run_Queue;
   end Destination_On_Wake_Up;

   procedure New_Execution_Cycle (Agent : in out Oak_Agent'Class) is
   begin
      Agent.Execution_Cycles := Agent.Execution_Cycles + 1;
      if Agent.Current_Execution_Time > Agent.Max_Execution_Time then
         Agent.Max_Execution_Time := Agent.Current_Execution_Time;
      end if;
      Agent.Current_Execution_Time := Oak_Time.Time_Span_Zero;
   end New_Execution_Cycle;

   function New_Task_Id return Task_Id is
      Chosen_Id : constant Task_Id := Global_Task_Id;
   begin
      Global_Task_Id := Global_Task_Id + 1;
      return Chosen_Id;
   end New_Task_Id;

   procedure Set_Agent_Message
     (For_Agent : in out Oak_Agent'Class;
      Message   : in     Oak_Message) is
   begin
      For_Agent.Message_Store.Message := Message;
   end Set_Agent_Message;

   procedure Set_Agent_Yield_Status
     (For_Agent : in out Oak_Agent'Class;
      Yielded   : in     Yielded_State)
   is
   begin
      For_Agent.Message_Store.Yield_Status := Yielded;
   end Set_Agent_Yield_Status;

   procedure Set_Scheduler_Agent
     (Agent     : in out Oak_Agent'Class;
      Scheduler : access Schedulers.Scheduler_Agent'Class) is
   begin
      Agent.Scheduler_Agent := Scheduler;
   end Set_Scheduler_Agent;

   procedure Set_Stack_Pointer
     (Agent         : in out Oak_Agent'Class;
      Stack_Pointer : in System.Address) is
   begin
      Agent.Call_Stack.Pointer := Stack_Pointer;
   end Set_Stack_Pointer;

   procedure Set_State
     (A     : in out Oak_Agent'Class;
      State : in     Agent_State) is
   begin
      A.State := State;
   end Set_State;

   procedure Set_Wake_Time
     (Agent : in out Oak_Agent'Class;
      WT    : in Oak_Time.Time) is
   begin
      Agent.Wake_Time := WT;
   end Set_Wake_Time;

end Oak.Agent;
