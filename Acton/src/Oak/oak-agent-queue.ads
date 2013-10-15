package Oak.Agent.Queue with Preelaborate is

   type Queue_End_Point is (Head, Tail);

   procedure Add_Agent_Before
     (Queue     : in out Agent_Handler;
      Agent     : not null access Oak_Agent'Class;
      Before    : not null access Oak_Agent'Class;
      Queue_End : in Queue_End_Point := Head);

   procedure Add_Agent_After
     (Queue : in out Agent_Handler;
      Agent : not null access Oak_Agent'Class;
      After : not null access Oak_Agent'Class);

   procedure Add_Agent_To_Head
     (Queue : in out Agent_Handler;
      Agent : not null access Oak_Agent'Class);

   procedure Add_Agent_To_Tail
     (Queue : in out Agent_Handler;
      Agent : not null access Oak_Agent'Class);

   function End_Of_Queue
     (Queue : not null access Oak_Agent'Class) return access Oak_Agent'Class;

   procedure Remove_Agent
     (Queue : in out Agent_Handler;
      Agent : not null access Oak_Agent'Class);

   function Next_Agent
     (Agent : not null access Oak_Agent'Class) return access Oak_Agent'Class;

   function Previous_Agent
     (Agent : not null access Oak_Agent'Class) return access Oak_Agent'Class;

   procedure Set_Blank_Agent_Link
     (A : not null access Oak_Agent'Class);

   procedure Remove_Agent_From_Head (Queue : in out Agent_Handler);
   procedure Remove_Agent_From_Tail (Queue : in out Agent_Handler);

   procedure Move_Head_To_Tail (Queue : in out Agent_Handler);

   function Is_In_Queue (Agent : not null access Oak_Agent'Class)
     return Boolean;

private
   function End_Of_Queue
     (Queue : not null access Oak_Agent'Class) return access Oak_Agent'Class is
     (Queue.Previous_Agent);

   function Is_In_Queue (Agent : not null access Oak_Agent'Class)
     return Boolean is (Agent.Next_Agent /= null);

   function Next_Agent
     (Agent : not null access Oak_Agent'Class) return access Oak_Agent'Class
      is (Agent.Next_Agent);

   function Previous_Agent
     (Agent : not null access Oak_Agent'Class) return access Oak_Agent'Class
      is (Agent.Previous_Agent);
end Oak.Agent.Queue;
