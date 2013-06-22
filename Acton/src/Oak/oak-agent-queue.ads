generic
   type Agent_Type is tagged limited private;
   type Agent_Handler is access all Agent_Type'Class;

   with function Get_Next_Agent
     (Agent : access Agent_Type'Class)
      return access Agent_Type'Class;
   with function Get_Prev_Agent
     (Agent : access Agent_Type'Class)
      return access Agent_Type'Class;
   with procedure Set_Blank_Link (Agent : access Agent_Type'Class);
   with procedure Set_Next_Agent (Agent, Next : access Agent_Type'Class);
   with procedure Set_Prev_Agent (Agent, Prev : access Agent_Type'Class);
   with procedure Set_Queue_Link (Agent, Prev, Next : access Agent_Type'Class);

package Oak.Agent.Queue is

   pragma Preelaborate;

   type Queue_End_Point is (Head, Tail);

   procedure Add_Agent_Before
     (Queue     : in out Agent_Handler;
      Agent     : access Agent_Type'Class;
      Before    : access Agent_Type'Class;
      Queue_End : in Queue_End_Point := Head);

   procedure Add_Agent_After
     (Queue : in out Agent_Handler;
      Agent : access Agent_Type'Class;
      After : access Agent_Type'Class);

   procedure Add_Agent_To_Head
     (Queue : in out Agent_Handler;
      Agent : access Agent_Type'Class);

   procedure Add_Agent_To_Tail
     (Queue : in out Agent_Handler;
      Agent : access Agent_Type'Class);

   function Next_Agent
     (Agent : access Agent_Type'Class)
      return access Agent_Type'Class renames Get_Next_Agent;

   function Prev_Agent
     (Agent : access Agent_Type'Class)
      return access Agent_Type'Class renames Get_Prev_Agent;

   procedure Remove_Agent
     (Queue : in out Agent_Handler;
      Agent : access Agent_Type'Class);

   procedure Remove_Agent_From_Head (Queue : in out Agent_Handler);
   procedure Remove_Agent_From_Tail (Queue : in out Agent_Handler);

   procedure Move_Head_To_Tail (Queue : in out Agent_Handler);

   function Is_In_Queue (Agent : access Agent_Type'Class) return Boolean;

private
   function Is_In_Queue (Agent : access Agent_Type'Class) return Boolean
     is (Next_Agent (Agent) /= null);
end Oak.Agent.Queue;
