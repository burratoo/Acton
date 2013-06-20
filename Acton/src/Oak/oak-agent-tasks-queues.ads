with Oak.Agent.Queue;

package Oak.Agent.Tasks.Queues with Preelaborate is
   function Next_Task
     (T : access Task_Agent'Class)
      return access Task_Agent'Class with Inline;

   function Prev_Task
     (T : access Task_Agent'Class)
      return access Task_Agent'Class with Inline;

   procedure Set_Blank_Task_Link
     (T : access Task_Agent'Class) with Inline;

   procedure Set_Next_Task
     (T, Next : access Task_Agent'Class) with Inline;

   procedure Set_Prev_Task
     (T, Prev : access Task_Agent'Class) with Inline;

   procedure Set_Task_Queue_Link
     (T, Prev, Next : access Task_Agent'Class) with Inline;

   package Task_Queues is new Oak.Agent.Queue
     (Agent_Type     => Task_Agent,
      Agent_Handler  => Task_Handler,
      Get_Next_Agent => Next_Task,
      Get_Prev_Agent => Prev_Task,
      Set_Blank_Link => Set_Blank_Task_Link,
      Set_Next_Agent => Set_Next_Task,
      Set_Prev_Agent => Set_Prev_Task,
      Set_Queue_Link => Set_Task_Queue_Link);

private
   function Next_Task
     (T : access Task_Agent'Class)
      return access Task_Agent'Class is (T.Queue_Link.Next);

   function Prev_Task
     (T : access Task_Agent'Class)
      return access Task_Agent'Class is (T.Queue_Link.Previous);
end Oak.Agent.Tasks.Queues;
