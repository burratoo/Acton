------------------------------------------------------------------------------------------
--                                                                                      --
--                                  OAKLAND COMPONENTS                                  --
--                                                                                      --
--                                ADA.TASK_IDENTIFICATION                               --
--                                                                                      --
--                       Copyright (C) 2014-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

with Oak.Agent;
--  with System;

package Ada.Task_Identification with Preelaborate is

   type Task_Id is private;
   pragma Preelaborable_Initialization (Task_Id);

   Null_Task_Id : constant Task_Id;

   function "=" (Left, Right : Task_Id) return Boolean;
   pragma Inline ("=");

   function Image (T : Task_Id) return String;

   function Current_Task return Task_Id with Inline;

   procedure Abort_Task (T : Task_Id) with Inline;
   --  Note: parameter is mode IN, not IN OUT, per AI-00101

   function Is_Terminated (T : Task_Id) return Boolean with Inline;

   function Is_Callable (T : Task_Id) return Boolean with Inline;

private

   type Task_Id is new Oak.Agent.Task_Id_With_No;

   Null_Task_Id : constant Task_Id := Task_Id (Oak.Agent.No_Agent);
   --  We can do the type conversion because we are awesome. Plus Task_Id is
   --  simply a new instance Oak.Agent.Task_Id_With_No, off which No_Agent is
   --  a valid memeber.

end Ada.Task_Identification;
