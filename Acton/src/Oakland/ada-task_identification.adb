with Oak.Agent;           use Oak.Agent;
with Oak.Agent.Kernel;    use Oak.Agent.Kernel;
with Oak.Agent.Oak_Agent; use Oak.Agent.Oak_Agent;

with Oak.Core; use Oak.Core;

package body Ada.Task_Identification is
   function "=" (Left, Right : Task_Id) return Boolean is
   begin
      return Oak_Agent_Id (Left) = Oak_Agent_Id (Right);
   end "=";

   function Image (T : Task_Id) return String is
   begin
      return Name (Oak_Agent_Id (T));
   end Image;

   function Current_Task return Task_Id is
   begin
      return Task_Id (Current_Agent (This_Oak_Kernel));
   end Current_Task;

   procedure Abort_Task (T : Task_Id) is
   begin
      raise Program_Error;
   end Abort_Task;

   function Is_Terminated (T : Task_Id) return Boolean is
      pragma Unreferenced (T);
   begin
      return False;
   end Is_Terminated;

   function Is_Callable (T : Task_Id) return Boolean is
      pragma Unreferenced (T);
   begin
      return False;
   end Is_Callable;

end Ada.Task_Identification;
