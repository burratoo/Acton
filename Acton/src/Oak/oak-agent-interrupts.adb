------------------------------------------------------------------------------
--                                                                          --
--                              OAK COMPONENTS                              --
--                                                                          --
--                           OAK.AGENT.INTERRUPTS                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                 Copyright (C) 2013-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;
with Ada.Task_Identification;

with Oak.Agent.Oak_Agent; use Oak.Agent.Oak_Agent;

with Oak.Core;
with Oak.Core_Support_Package.Task_Support;
with Oak.Core_Support_Package.Call_Stack;
use Oak.Core_Support_Package.Call_Stack;

with Oak.States; use Oak.States;
with Oak.Timers; use Oak.Timers;

package body Oak.Agent.Interrupts is

   ----------------
   -- To_Task_Id --
   ----------------

   --  Converts to an Ada.Task_Identification Task_Id from an Oak Task_Id.
   --  ??? Should this really be in a subpackage of Ada.Task_Identification?

   function To_Task_Id is
     new Ada.Unchecked_Conversion
       (Agent_Handler, Ada.Task_Identification.Task_Id);

   ------------------------
   -- Interrupt_Run_Loop --
   ------------------------

   procedure Interrupt_Run_Loop (Self : Interrupt_Id) is
      I : Interrupt_Agent_Record renames Agent_Pool (Self);

      Exit_Message : constant Oak_Message := (Message_Type => Interrupt_Done);
   begin
      loop
         case I.Interrupt_Kind is
            when External =>
               External_Interrupt_Handler (I.External_Id);
            when Timer_Action =>
               Handler (I.Timer_To_Handle).all
                 (To_Task_Id (Agent_To_Handle (I.Timer_To_Handle)));
         end case;

         Core.Current_Agent.Set_Agent_Message (Exit_Message);
         Core_Support_Package.Task_Support.Yield_Processor_To_Kernel;
      end loop;
   end Interrupt_Run_Loop;

   ------------------------
   -- New_Interupt_Agent --
   ------------------------

   procedure New_Interrupt_Agent
     (Agent    : out Interrupt_Id;
      Priority : in Oak_Priority)
   is
   begin
      New_Agent
        (Agent                => Agent,
         Name                 => "Interrupt_Agent",
         Call_Stack_Address   => Null_Address,
         Call_Stack_Size      => Interrupt_Stack_Size,
         Run_Loop             => Interrupt_Run_Loop'Address,
         Run_Loop_Parameter   => Agent,
         Normal_Priority      => Priority,
         Initial_State        => Interrupt_Done);
   end New_Interrupt_Agent;

   ---------------------
   -- Set_External_Id --
   ---------------------

   procedure Set_External_Id
     (Agent : in Interrupt_Id;
      Id    : in Oak_Interrupt_Id) is
   begin
      Agent_Pool (Agent).External_Id := Id;
   end Set_External_Id;

   ------------------------
   -- Set_Interrupt_Kind --
   ------------------------

   procedure Set_Interrupt_Kind
     (Agent : in Interrupt_Id;
      Kind  : in Interrupt_Type) is
   begin
      Agent_Pool (Agent).Interrupt_Kind := Kind;
   end Set_Interrupt_Kind;

   -------------------------
   -- Set_Timer_To_Handle --
   -------------------------

   procedure Set_Timer_To_Handle
     (Agent : in Interrupt_Id;
      Timer : in Oak_Timer_Id) is
   begin
      Agent_Pool (Agent).Timer_To_Handle := Timer;
   end Set_Timer_To_Handle;

end Oak.Agent.Interrupts;
