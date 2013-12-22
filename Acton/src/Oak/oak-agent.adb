with Oak.Oak_Time; use Oak.Oak_Time;
with Oak.Memory.Call_Stack.Ops; use Oak.Memory.Call_Stack.Ops;

package body Oak.Agent is

   procedure Initialise_Agent
     (Agent      : not null access Oak_Agent'Class;
      Name       : in String) is
   begin
      Agent.Name_Length                   :=
         Natural'Min (Task_Name'Length, Name'Length);
      Agent.Name (1 .. Agent.Name_Length) :=
        Name (Name'First .. Name'First + Agent.Name_Length - 1);

      Agent.Total_Execution_Time   := Time_Span_Zero;
      Agent.Max_Execution_Time     := Time_Span_Zero;
      Agent.Current_Execution_Time := Time_Span_Zero;
      Agent.Remaining_Budget       := Time_Span_Last;
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
     (Agent                : not null access Oak_Agent'Class;
      Name                 : in String;
      Call_Stack_Address   : in Address;
      Call_Stack_Size      : in Storage_Count;
      Run_Loop             : in Address;
      Run_Loop_Parameter   : in Address;
      Normal_Priority      : in Integer;
      Initial_State        : in Agent_State;
      Wake_Time            : in Oak_Time.Time;
      When_To_Charge_Agent : in Charge_Occurrence := All_Priorities) is
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
      Agent.Wake_Time         := Wake_Time;
      Agent.Absolute_Deadline := Oak_Time.Time_Last;
      Agent.When_To_Charge    := When_To_Charge_Agent;
   end Initialise_Agent;

   function Destination_On_Wake_Up (Agent : in out Oak_Agent)
                                    return Wake_Destination is
      pragma Unreferenced (Agent);
   begin
      return Run_Queue;
   end Destination_On_Wake_Up;

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

   procedure Add_Agent_To_Exec_Charge_List
     (Agent : not null access Oak_Agent'Class;
      List  : in out Agent_Handler)
   is
      Head : Agent_Handler renames List;
   begin
      if List = null then
         List := Agent_Handler (Agent);
         Agent.Previous_Charge_Item := null;
         Agent.Next_Charge_Item     := null;
      else
         Head.Previous_Charge_Item  := Agent;
         Agent.Next_Charge_Item     := Head;
         Agent.Previous_Charge_Item := null;

         List := Agent_Handler (Agent);
      end if;
   end Add_Agent_To_Exec_Charge_List;

   procedure Clear_Exec_Charge_List
     (List : in out Agent_Handler)
   is
      Agent : access Oak_Agent'Class;
   begin
      while List /= null loop
         Agent                      := List;
         List                       := Agent.Next_Charge_Item;
         Agent.Next_Charge_Item     := null;
         Agent.Previous_Charge_Item := null;
      end loop;
   end Clear_Exec_Charge_List;

   procedure Charge_Execution_Time
     (To_Agent  : in out Oak_Agent'Class;
      Exec_Time : in Oak_Time.Time_Span) is
   begin
      To_Agent.Current_Execution_Time :=
        To_Agent.Current_Execution_Time + Exec_Time;
      To_Agent.Total_Execution_Time :=
        To_Agent.Total_Execution_Time + Exec_Time;

      --  Only decrement remaining budget only if it does not contain
      --  Time_Span_Last since that signifies that the Remaining_Budget
      --  variable is not begining used.

      if To_Agent.Remaining_Budget /= Time_Span_Last then
         To_Agent.Remaining_Budget :=
           To_Agent.Remaining_Budget - Exec_Time;
      end if;
   end Charge_Execution_Time;

   procedure Charge_Execution_Time_To_List
     (List      : not null access Oak_Agent'Class;
      Exec_Time : in Oak_Time.Time_Span;
      Current_Priority : in Oak_Priority)
   is
      Agent : access Oak_Agent'Class := List;
   begin
      while Agent /= null loop
         case Agent.When_To_Charge is
            when Do_Not_Charge =>
               null;

            when Same_Priority =>
               if Current_Priority = Agent.Normal_Priority then
                  Agent.Charge_Execution_Time (Exec_Time);
               end if;

            when All_Priorities =>
               Agent.Charge_Execution_Time (Exec_Time);

            when Below_Priority =>
               if Current_Priority <= Agent.Normal_Priority then
                  Agent.Charge_Execution_Time (Exec_Time);
               end if;
         end case;
         Agent := Agent.Next_Charge_Item;
      end loop;
   end Charge_Execution_Time_To_List;

   --  fix this, the problem is that it can return an agent who may not
   --  be affected by the next agent.
   function Earliest_Expiring_Budget
     (Charge_List : not null access Oak_Agent'Class)
      return access Oak_Agent'Class
   is
      Selected_Agent : not null access Oak_Agent'Class := Charge_List;
      Agent          : access Oak_Agent'Class := Charge_List.Next_Charge_Item;
   begin
      while Agent /= null loop
         if Agent.Remaining_Budget < Selected_Agent.Remaining_Budget then
            Selected_Agent := Agent;
         end if;

         Agent := Agent.Next_Charge_Item;
      end loop;

      if Selected_Agent.Remaining_Budget = Time_Span_Last then
         return null;
      else
         return Selected_Agent;
      end if;
   end Earliest_Expiring_Budget;

   procedure Replenish_Execution_Budget
     (Agent     : in out Oak_Agent'Class;
      By_Amount : in Oak_Time.Time_Span) is
   begin
      Agent.Remaining_Budget := Agent.Remaining_Budget + By_Amount;
   end Replenish_Execution_Budget;

   procedure Set_Remaining_Budget
     (Agent     : in out Oak_Agent'Class;
      To_Amount : in Oak_Time.Time_Span) is
   begin
      Agent.Remaining_Budget := To_Amount;
   end Set_Remaining_Budget;

   procedure Remove_Agent_From_Exec_Charge_List
     (Agent : not null access Oak_Agent'Class;
      List  : in out Agent_Handler)
   is
      Head  : Agent_Handler renames List;
   begin
      if Agent.Previous_Charge_Item /= null then
         Agent.Previous_Charge_Item.Next_Charge_Item :=
           Agent.Next_Charge_Item;
      end if;

      if Agent.Next_Charge_Item /= null then
         Agent.Next_Charge_Item.Previous_Charge_Item :=
           Agent.Previous_Charge_Item;
      end if;

      if Agent = Head then
         Head := Agent.Next_Charge_Item;
      end if;

      Agent.Next_Charge_Item     := null;
      Agent.Previous_Charge_Item := null;
   end Remove_Agent_From_Exec_Charge_List;
end Oak.Agent;
