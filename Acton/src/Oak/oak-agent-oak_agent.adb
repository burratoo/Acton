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

with Oak.Core_Support_Package.Task_Support;
with Oak.Core_Support_Package.Call_Stack;
use  Oak.Core_Support_Package.Call_Stack;

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

      if Agent.Remaining_Budget /= Time_Span_Last then
         Agent.Remaining_Budget := Agent.Remaining_Budget - Exec_Time;
      end if;
   end Charge_Execution_Time;

   -----------------------------------
   -- Charge_Execution_Time_To_List --
   -----------------------------------

   procedure Charge_Execution_Time_To_List
     (List             : in Agent_List;
      Exec_Time        : in Oak_Time.Time_Span;
      Current_Priority : in Oak_Priority)
   is
      Agent : Oak_Agent_Id := List;
   begin
      while Agent /= No_Agent loop
         case Agent_Pool (Agent).When_To_Charge is
            when Do_Not_Charge =>
               null;

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
         Agent := Agent_Pool (Agent).Next_Agent;
      end loop;
   end Charge_Execution_Time_To_List;

   ------------------------------
   -- Earliest_Expiring_Budget --
   ------------------------------

   function Earliest_Expiring_Budget
     (Charge_List : in Agent_List) return Oak_Agent_Id
   is
      Selected_Agent : Oak_Agent_Id := Charge_List;
      Agent          : Oak_Agent_Id :=
                         Agent_Pool (Charge_List).Next_Agent;
   begin
      while Agent /= No_Agent loop
         if Agent_Pool (Agent).Remaining_Budget
           < Agent_Pool (Selected_Agent).Remaining_Budget then
            Selected_Agent := Agent;
         end if;

         Agent := Agent_Pool (Agent).Next_Agent;
      end loop;

      return
        (if Agent_Pool (Selected_Agent).Remaining_Budget = Time_Span_Last then
              No_Agent else Selected_Agent);

   end Earliest_Expiring_Budget;

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
      Normal_Priority      : in Integer;
      Initial_State        : in Agent_State;
      Wake_Time            : in Oak_Time.Time;
      When_To_Charge_Agent : in Charge_Occurrence := All_Priorities)
   is
      A : Oak_Agent_Record renames Agent_Pool (Agent);

   begin
      Set_Name (Agent, Name);

      if Call_Stack_Address = Null_Address and Call_Stack_Size > 0 then
         --  Allocate a call stack if needed.

         Allocate_Call_Stack
           (Stack            => A.Call_Stack,
            Size_In_Elements => Call_Stack_Size);

         Initialise_Call_Stack
           (Stack             => A.Call_Stack,
            Start_Instruction => Run_Loop,
            Task_Value_Record => Run_Loop_Parameter);

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
      A.State                  := Initial_State;
      A.Normal_Priority        := Normal_Priority;
      A.Scheduler_Agent        := No_Agent;
      A.Wake_Time              := Wake_Time;
      A.Absolute_Deadline      := Oak_Time.Time_Last;
      A.Total_Execution_Time   := Time_Span_Zero;
      A.Max_Execution_Time     := Time_Span_Zero;
      A.Current_Execution_Time := Time_Span_Zero;
      A.Remaining_Budget       := Time_Span_Last;
      A.Execution_Cycles       := Natural'First;
      A.When_To_Charge         := When_To_Charge_Agent;
   end New_Agent;

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
      SAgent : Oak_Agent_Record renames Agent_Pool (Sleep_Agent);
   begin
      Oak_Agent_Pool.Setup_Storage;

      --  Set up the Sleep Agent

      Allocate_Call_Stack
          (Stack            => SAgent.Call_Stack,
           Size_In_Elements => Sleep_Stack_Size);

      Initialise_Call_Stack
        (Stack             => SAgent.Call_Stack,
         Start_Instruction =>
           Core_Support_Package.Task_Support.Sleep_Agent'Address,
         Task_Value_Record => Null_Address);

      Set_Name (Sleep_Agent, "Sleep");
      SAgent.Normal_Priority := Priority'First;
      SAgent.State           := Runnable;
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
