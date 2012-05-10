generic
   type Agent_Type is tagged private;

   with function Next_Agent
     (Agent : access Agent_Type'Class)
      return access Agent_Type'Class;
   with function Prev_Agent
     (Agent : access Agent_Type'Class)
      return access Agent_Type'Class;
   with procedure Set_Blank_Link (Agent : access Agent_Type'Class);
   with procedure Set_Next_Agent (Agent, Next : access Agent_Type'Class);
   with procedure Set_Prev_Agent (Agent, Prev : access Agent_Type'Class);
   with procedure Set_Queue_Link (Agent, Prev, Next : access Agent_Type'Class);

package Oak.Agent.Queue is

   pragma Pure;

   type Agent_Handler is access all Agent_Type'Class;

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

   procedure Remove_Agent
     (Queue : in out Agent_Handler;
      Agent : access Agent_Type'Class);

   procedure Remove_Agent_From_Head (Queue : in out Agent_Handler);
   procedure Remove_Agent_From_Tail (Queue : in out Agent_Handler);

end Oak.Agent.Queue;
