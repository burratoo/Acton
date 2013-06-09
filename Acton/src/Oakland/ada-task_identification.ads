with Oak.Agent.Tasks;
with System;

package Ada.Task_Identification with Preelaborate is

   type Task_Id is private;
   pragma Preelaborable_Initialization (Task_Id);

   Null_Task_Id : constant Task_Id;

--     function "=" (Left, Right : Task_Id) return Boolean;
--     pragma Inline ("=");
--
--     function Image (T : Task_Id) return String;
--
--     function Current_Task return Task_Id;
--     pragma Inline (Current_Task);
--
--     procedure Abort_Task (T : Task_Id);
--     pragma Inline (Abort_Task);
--     --  Note: parameter is mode IN, not IN OUT, per AI-00101
--
--     function Is_Terminated (T : Task_Id) return Boolean;
--     pragma Inline (Is_Terminated);
--
--     function Is_Callable (T : Task_Id) return Boolean;
--     pragma Inline (Is_Callable);

private

   type Task_Id is access Oak.Agent.Tasks.Task_Agent;

   Null_Task_Id : constant Task_Id := null;

end Ada.Task_Identification;
