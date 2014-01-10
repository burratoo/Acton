------------------------------------------------------------------------------
--                                                                          --
--                              OAK COMPONENTS                              --
--                                                                          --
--                             OAK.AGENT.KERNEL                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                 Copyright (C) 2014-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

with Oak.Agent.Interrupts; use Oak.Agent.Interrupts;
with Oak.Agent.Oak_Agent;  use Oak.Agent.Oak_Agent;

with Oak.Core_Support_Package.Call_Stack;
use Oak.Core_Support_Package.Call_Stack;

with Oak.States; use Oak.States;

package body Oak.Agent.Kernel is

   ------------------------------
   -- Add_Agent_To_Charge_List --
   ------------------------------

   procedure Add_Agent_To_Charge_List
     (Oak_Kernel : in Kernel_Id;
      Agent      : in Oak_Agent_Id)
   is
      K : Oak_Kernel_Record renames Agent_Pool (Oak_Kernel);
   begin
      Set_Next_Agent (For_Agent => Agent, Next_Agent => K.Budgets_To_Charge);
      K.Budgets_To_Charge := Agent;
   end Add_Agent_To_Charge_List;

   -------------------------------
   -- Find_Top_Active_Interrupt --
   -------------------------------

   function Find_Top_Active_Interrupt
     (Oak_Kernel : in Kernel_Id)
      return Interrupt_Id_With_No
   is
      K : Oak_Kernel_Record renames Agent_Pool (Oak_Kernel);
   begin
      --  Scan interrupt state vector to find top active agent.

      for P in reverse K.Interrupt_States'Range loop
         if K.Interrupt_States (P) = Handling then
            return K.Interrupt_Agents (P);
         end if;
      end loop;

      --  If we go here it means no interrupt agents are active.

      return No_Agent;
   end Find_Top_Active_Interrupt;

   ----------------------
   -- New_Kernel_Agent --
   ----------------------

   procedure New_Kernel_Agent
     (Agent  : out Kernel_Id)
   is
   begin
      Allocate_An_Agent (Agent);

      New_Agent
        (Agent                => Agent,
         Name                 => "Kernel",
         Call_Stack_Address   => Null_Address,
         Call_Stack_Size      => Oak_Call_Stack_Size,
         Run_Loop             => Null_Address,
         Run_Loop_Parameter   => Null_Address,
         Normal_Priority      => Any_Priority'Last,
         Initial_State        => Running);

      Setup_Kernel_Agent : declare
         K : Oak_Kernel_Record renames Agent_Pool (Agent);
      begin
         K.Schedulers        := No_Agent;
         K.Reason_For_Run    := First_Run;
         K.Current_Agent     := No_Agent;
         K.Entry_Exit_Stamp  := Clock;
         K.Interrupt_States  := (others => Inactive);
         K.Budgets_To_Charge := No_Agent;

         for P in K.Interrupt_Agents'Range loop
            New_Interrupt_Agent
              (Agent    => K.Interrupt_Agents (P),
               Priority => P);
         end loop;
      end Setup_Kernel_Agent;
   end New_Kernel_Agent;

   -----------------------------------
   -- Remove_Agent_From_Charge_List --
   -----------------------------------

   procedure Remove_Agent_From_Charge_List
     (Oak_Kernel : in Kernel_Id;
      Agent      : in Oak_Agent_Id)
   is
      K : Oak_Kernel_Record renames Agent_Pool (Oak_Kernel);
   begin
      if K.Budgets_To_Charge = Agent then
         --  The agent is at the head of the charge list.

         K.Budgets_To_Charge := Next_Agent (Agent);

      else
         --  The agent is further down the charge list.
         Find_And_Remove_Agent : declare
            Prev_A : Oak_Agent_Id := K.Budgets_To_Charge;
            A      : Oak_Agent_Id := Next_Agent (K.Budgets_To_Charge);
         begin
            while A /= Agent and then A /= No_Agent loop
               Prev_A := A;
               A      := Next_Agent (A);
            end loop;

            if A /= No_Agent then
               --  We have found the agent
               Set_Next_Agent (Prev_A, Next_Agent (A));
            end if;
         end Find_And_Remove_Agent;
      end if;

      --  Clear the next agent link for the removed agent so we know that it
      --  is possibly not in a list.

      Set_Next_Agent (For_Agent => Agent, Next_Agent => No_Agent);

   end Remove_Agent_From_Charge_List;

   -----------------------
   -- Set_Current_Agent --
   -----------------------

   procedure Set_Current_Agent
     (Oak_Kernel : in Kernel_Id;
      Agent      : in Oak_Agent_Id)
   is
   begin
      Agent_Pool (Oak_Kernel).Current_Agent := Agent;
   end Set_Current_Agent;

   ------------------------
   -- Set_Reason_For_Run --
   ------------------------

   procedure Set_Reason_For_Run
     (Oak_Kernel : in Kernel_Id;
      Reason     : in Run_Reason)
   is
   begin
      Agent_Pool (Oak_Kernel).Reason_For_Run := Reason;
   end Set_Reason_For_Run;

end Oak.Agent.Kernel;
