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

with Oak.Agent.Kernel;            use Oak.Agent.Kernel;
with Oak.Agent.Oak_Agent;         use Oak.Agent.Oak_Agent;
with Oak.Agent.Protected_Objects; use Oak.Agent.Protected_Objects;
with Oak.Protected_Objects;       use Oak.Protected_Objects;

with Oak.Core_Support_Package.Call_Stack;
use Oak.Core_Support_Package.Call_Stack;

with Oak.Core;    use Oak.Core;
with Oak.Message; use Oak.Message;
with Oak.States;  use Oak.States;

package body Oak.Agent.Interrupts is

   ----------------
   -- To_Task_Id --
   ----------------

   --  Converts to an Ada.Task_Identification Task_Id from an Oak Task_Id.
   --  ??? Should this really be in a subpackage of Ada.Task_Identification?

   function To_Task_Id is
     new Ada.Unchecked_Conversion
       (Oak_Agent_Id, Ada.Task_Identification.Task_Id);

   ------------------------
   -- Interrupt_Run_Loop --
   ------------------------

   procedure Interrupt_Run_Loop is
      Agent : Interrupt_Id;

      Exit_Message : Oak_Message :=
                       (Message_Type => Interrupt_Done);
   begin
      loop
         Agent := Current_Agent (This_Oak_Kernel);
         case Agent_Pool (Agent).Interrupt_Kind is
            when External =>
               External_Interrupt_Handler (Agent_Pool (Agent).External_Id);
            when Timer_Action =>
               Handler (Agent_Pool (Agent).Timer_To_Handle).all
                 (To_Task_Id
                    (Agent_To_Handle (Agent_Pool (Agent).Timer_To_Handle)));
         end case;

         Request_Oak_Service
           (Reason_For_Run => Agent_Request, Message => Exit_Message);
      end loop;
   end Interrupt_Run_Loop;

   --------------------
   -- Interrupt_Done --
   --------------------

   procedure Interrupt_Done
     (Kernel            : in  Kernel_Id;
      Current_Agent     : in  Interrupt_Id;
      Next_Agent_To_Run : out Oak_Agent_Id)
   is
      pragma Unreferenced (Next_Agent_To_Run);
   begin
      Deactivate_Interrupt_Agent
        (Oak_Kernel => Kernel,
         Interrupt  => Current_Agent);

      --  Service any open entries

      case Interrupt_Kind (Current_Agent) is
         when Timer_Action =>
            Process_Interrupt_Exit
              (PO                =>
                  Protected_Object_From_Access (
                 Handler (Timer_To_Handle (Current_Agent))),
               Next_Agent_To_Run => Next_Agent_To_Run);

         when External =>
            Process_Interrupt_Exit
              (PO                =>
                  Handler_Protected_Object
                 (Agent_Pool (Current_Agent).External_Id),
               Next_Agent_To_Run => Next_Agent_To_Run);
      end case;
   end Interrupt_Done;

   ------------------------
   -- New_Interupt_Agent --
   ------------------------

   procedure New_Interrupt_Agent
     (Agent    : out Interrupt_Id;
      Priority : in Oak_Priority)
   is
   begin
      Allocate_An_Agent (Agent);

      New_Agent
        (Agent                => Agent,
         Name                 => "Interrupt_Agent",
         Call_Stack_Address   => Null_Address,
         Call_Stack_Size      => Interrupt_Stack_Size,
         Run_Loop             => Interrupt_Run_Loop'Address,
         Run_Loop_Parameter   => Null_Address,
         Normal_Priority      => Priority,
         Initial_State        => Interrupt_Done);
   end New_Interrupt_Agent;

   ---------------------
   -- Set_External_Id --
   ---------------------

   procedure Set_External_Id
     (For_Agent : in Interrupt_Id;
      Id        : in External_Interrupt_Id) is
   begin
      Agent_Pool (For_Agent).External_Id := Id;
   end Set_External_Id;

   ------------------------
   -- Set_Interrupt_Kind --
   ------------------------

   procedure Set_Interrupt_Kind
     (For_Agent : in Interrupt_Id;
      Kind      : in Interrupt_Type) is
   begin
      Agent_Pool (For_Agent).Interrupt_Kind := Kind;
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
