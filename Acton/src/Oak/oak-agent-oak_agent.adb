------------------------------------------------------------------------------
--                                                                          --
--                              OAK COMPONENTS                              --
--                                                                          --
--                           OAK.AGENT.OAK_AGENT                            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                 Copyright (C) 2013-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

with Oak.Oak_Time;              use Oak.Oak_Time;
with Oak.Memory.Call_Stack.Ops; use Oak.Memory.Call_Stack.Ops;

package body Oak.Agent.Oak_Agent is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Set_Name
     (Agent_Id : in Oak_Agent_Id;
      Name     : in String);
   --  Sets the name of the Agent.

   ---------------------------
   -- Charge_Execution_Time --
   ---------------------------

   procedure Charge_Execution_Time
     (To_Agent  : in Oak_Agent_Id;
      Exec_Time : in Oak_Time.Time_Span)
   is
      Agent : Oak_Agent_Record renames Agent_Pool (To_Agent);
   begin
      Agent.Current_Execution_Time := Agent.Current_Execution_Time + Exec_Time;
      Agent.Total_Execution_Time   := Agent.Total_Execution_Time + Exec_Time;

      --  Only decrement remaining budget only if it does not contain
      --  Time_Span_Last since that signifies that the Remaining_Budget
      --  variable is not begining used.

      if Agent.Remaining_Budget < Time_Span_Last then
         Agent.Remaining_Budget := Agent.Remaining_Budget - Exec_Time;
      end if;
   end Charge_Execution_Time;

   -----------------------------------
   -- Charge_Execution_Time_To_List --
   -----------------------------------

   procedure Charge_Execution_Time_To_List
     (List             : in Charge_List_Head;
      Exec_Time        : in Oak_Time.Time_Span;
      Current_Agent    : in Oak_Agent_Id;
      Current_Priority : in Oak_Priority)
   is
      Agent   : Oak_Agent_Id := List;
   begin
      --  The first node on the charge list can be the No_Node (since it is
      --  also the sleep node), so the check for the terminating No_Node is
      --  done at the bottom.

      loop
         case Agent_Pool (Agent).When_To_Charge is
            when Only_While_Running =>
               if Agent = Current_Agent then
                  Charge_Execution_Time (Agent, Exec_Time);
               end if;

            when Same_Priority =>
               if Current_Priority = Agent_Pool (Agent).Normal_Priority then
                  Charge_Execution_Time (Agent, Exec_Time);
               end if;

            when All_Priorities =>
               Charge_Execution_Time (Agent, Exec_Time);

            when Below_Priority =>
               if Current_Priority <= Agent_Pool (Agent).Normal_Priority then
                  Charge_Execution_Time (Agent, Exec_Time);
               end if;
         end case;
         Agent := Agent_Pool (Agent).Next_Charge_Agent;
         exit when Agent = No_Agent;
      end loop;
   end Charge_Execution_Time_To_List;

   ------------------
   -- Delete_Agent --
   ------------------

   procedure Delete_Agent (Agent : Oak_Agent_Id)
   is
   begin
      Deallocate_Agent (Agent);
   end Delete_Agent;

   ------------------------------
   -- Earliest_Expiring_Budget --
   ------------------------------

   function Earliest_Expiring_Budget
     (Charge_List      : in Charge_List_Head;
      Current_Priority : in Any_Priority)
      return Oak_Agent_Id
   is
      Selected_Agent : Oak_Agent_Id := No_Agent;
      Agent          : Oak_Agent_Id := Charge_List;
   begin
      while Agent /= No_Agent loop
         declare
            A : Oak_Agent_Record renames Agent_Pool (Agent);
         begin
            if (A.When_To_Charge = Only_While_Running
                or else A.When_To_Charge = All_Priorities
                or else (A.When_To_Charge = Same_Priority
                         and then Current_Priority = A.Normal_Priority)
                or else (A.When_To_Charge = Below_Priority
                         and then Current_Priority <= A.Normal_Priority))
              and then A.Remaining_Budget
                < Agent_Pool (Selected_Agent).Remaining_Budget
            then
               Selected_Agent := Agent;
            end if;
         end;

         Agent := Agent_Pool (Agent).Next_Charge_Agent;
      end loop;

      return
        (if Agent_Pool (Selected_Agent).Remaining_Budget = Time_Span_Last then
              No_Agent else Selected_Agent);

   end Earliest_Expiring_Budget;

   -------------------------------------
   -- Increment_Execution_Cycle_Count --
   -------------------------------------

   procedure Increment_Execution_Cycle_Count
     (For_Agent : in Oak_Agent_Id;
      By        : in Natural)
   is
      A : Oak_Agent_Record renames Agent_Pool (For_Agent);
   begin
      A.Execution_Cycles := A.Execution_Cycles + By;

      if A.Current_Execution_Time > A.Max_Execution_Time then
         A.Max_Execution_Time := A.Current_Execution_Time;
      end if;

      A.Current_Execution_Time := Time_Span_Zero;
   end Increment_Execution_Cycle_Count;

   ---------------
   -- New_Agent --
   ---------------

   procedure New_Agent
     (Agent                : in Oak_Agent_Id;
      Name                 : in String;
      Call_Stack_Address   : in Address;
      Call_Stack_Size      : in Storage_Count;
      Run_Loop             : in Address;
      Run_Loop_Parameter   : in Address;
      Normal_Priority      : in Any_Priority;
      Initial_State        : in Agent_State;
      Scheduler_Agent      : in Scheduler_Id_With_No := No_Agent;
      Wake_Time            : in Oak_Time.Time        := Oak_Time.Time_Last;
      When_To_Charge_Agent : in Charge_Occurrence    := All_Priorities)
   is
      A : Oak_Agent_Record renames Agent_Pool (Agent);
   begin
      Allocate_An_Agent_With_Id (Agent);

      Set_Name (Agent, Name);

      if Call_Stack_Address = Null_Address and Call_Stack_Size > 0 then
         --  Allocate a call stack if needed.

         Allocate_Call_Stack
           (Stack            => A.Call_Stack,
            Size_In_Elements => Call_Stack_Size);

         if Agent in Task_Id then
            --  Scheduler agent only need their initial instruction set
            Initialise_Call_Stack
              (Stack             => A.Call_Stack,
               Start_Instruction => Run_Loop,
               Task_Value_Record => Run_Loop_Parameter);
         else
            Initialise_Call_Stack
              (Stack             => A.Call_Stack,
               Start_Instruction => Run_Loop);
         end if;

      elsif Call_Stack_Address /= Null_Address then
         --  Otherwise just assigned the passed stack

         Initialise_Call_Stack
           (Stack             => A.Call_Stack,
            Start_Instruction => Run_Loop,
            Task_Value_Record => Run_Loop_Parameter,
            Stack_Address     => Call_Stack_Address,
            Stack_Size        => Call_Stack_Size);
      end if;

      A.Next_Agent             := No_Agent;
      A.Next_Charge_Agent      := No_Agent;
      A.State                  := Initial_State;
      A.Normal_Priority        := Normal_Priority;
      A.Scheduler_Agent        := Scheduler_Agent;
      A.Wake_Time              := Wake_Time;
      A.Absolute_Deadline      := Oak_Time.Time_Last;
      A.Total_Execution_Time   := Time_Span_Zero;
      A.Max_Execution_Time     := Time_Span_Zero;
      A.Current_Execution_Time := Time_Span_Zero;
      A.Remaining_Budget       := Time_Span_Last;
      A.Execution_Cycles       := Natural'First;
      A.Scheduler_Agent        := Scheduler_Agent;
      A.When_To_Charge         := When_To_Charge_Agent;
      if Agent in Task_Id then
         A.Agent_Interrupted := True;
      else
         A.Agent_Interrupted := False;
      end if;

   end New_Agent;

   ---------------------------
   -- Set_Agent_Interrupted --
   ---------------------------

   procedure Set_Agent_Interrupted
     (For_Agent : in Oak_Agent_Id;
      Value     : Boolean := True) is
   begin
      Agent_Pool (For_Agent).Agent_Interrupted := Value;
   end Set_Agent_Interrupted;

   ---------------------------
   -- Set_Absolute_Deadline --
   ---------------------------

   procedure Set_Absolute_Deadline
     (For_Agent : in Oak_Agent_Id;
      Deadline  : in Oak_Time.Time) is
   begin
      Agent_Pool (For_Agent).Absolute_Deadline := Deadline;
   end Set_Absolute_Deadline;

   --------------------------------
   -- Set_Current_Execution_Time --
   --------------------------------

   procedure Set_Current_Execution_Time
     (For_Agent : in Oak_Agent_Id;
      To        : in Oak_Time.Time_Span) is
   begin
      Agent_Pool (For_Agent).Current_Execution_Time := To;
   end Set_Current_Execution_Time;

   ----------------------------
   -- Set_Max_Execution_Time --
   ----------------------------

   procedure Set_Max_Execution_Time
     (For_Agent : in Oak_Agent_Id;
      To        : in Oak_Time.Time_Span) is
   begin
      Agent_Pool (For_Agent).Max_Execution_Time := To;
   end Set_Max_Execution_Time;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name
     (Agent_Id : in Oak_Agent_Id;
      Name     : in String)
   is
      Agent : Oak_Agent_Record renames Agent_Pool (Agent_Id);
   begin
      Agent.Name_Length                   :=
        Natural'Min (Name'Length, Name'Length);
      Agent.Name (1 .. Agent.Name_Length) :=
        Name (Name'First .. Name'First + Agent.Name_Length - 1);
   end Set_Name;

   --------------------
   -- Set_Next_Agent --
   --------------------

   procedure Set_Next_Agent
     (For_Agent  : in Oak_Agent_Id;
      Next_Agent : in Oak_Agent_Id) is
   begin
      Agent_Pool (For_Agent).Next_Agent := Next_Agent;
   end Set_Next_Agent;

   ---------------------------
   -- Set_Next_Charge_Agent --
   ---------------------------

   procedure Set_Next_Charge_Agent
     (For_Agent  : in Oak_Agent_Id;
      Next_Agent : in Oak_Agent_Id) is
   begin
      Agent_Pool (For_Agent).Next_Charge_Agent := Next_Agent;
   end Set_Next_Charge_Agent;

   --------------------------
   -- Set_Remaining_Budget --
   --------------------------

   procedure Set_Remaining_Budget
     (For_Agent : in Oak_Agent_Id;
      To_Amount : in Oak_Time.Time_Span) is
   begin
      Agent_Pool (For_Agent).Remaining_Budget := To_Amount;
   end Set_Remaining_Budget;

   -------------------------
   -- Set_Scheduler_Agent --
   -------------------------

   procedure Set_Scheduler_Agent
     (For_Agent : in Oak_Agent_Id;
      Scheduler : in Scheduler_Id_With_No) is
   begin
      Agent_Pool (For_Agent).Scheduler_Agent := Scheduler;
   end Set_Scheduler_Agent;

   -----------------------
   -- Set_Stack_Pointer --
   -----------------------

   procedure Set_Stack_Pointer
     (For_Agent     : in Oak_Agent_Id;
      Stack_Pointer : in System.Address) is
   begin
      Agent_Pool (For_Agent).Call_Stack.Pointer := Stack_Pointer;
   end Set_Stack_Pointer;

   ---------------
   -- Set_State --
   ---------------

   procedure Set_State
     (For_Agent : in Oak_Agent_Id;
      State     : in Agent_State) is
   begin
      Agent_Pool (For_Agent).State := State;
   end Set_State;

   -------------------
   -- Set_Wake_Time --
   -------------------

   procedure Set_Wake_Time
     (For_Agent : in Oak_Agent_Id;
      Wake_Time : in Oak_Time.Time) is
   begin
      Agent_Pool (For_Agent).Wake_Time := Wake_Time;
   end Set_Wake_Time;

   -------------------
   -- Setup_Storage --
   -------------------

   procedure Setup_Storage is
   begin
      Oak_Agent_Pool.Setup_Storage;
   end Setup_Storage;

   --------------------------------
   -- Replenish_Execution_Budget --
   --------------------------------

   procedure Replenish_Execution_Budget
     (For_Agent : in Oak_Agent_Id;
      By_Amount : in Oak_Time.Time_Span)
   is
      Agent : Oak_Agent_Record renames Agent_Pool (For_Agent);
   begin
      Agent.Remaining_Budget := Agent.Remaining_Budget + By_Amount;
   end Replenish_Execution_Budget;
end Oak.Agent.Oak_Agent;
