with Oak.Indices; use Oak.Indices;

with Oak.Agent.Tasks; use Oak.Agent.Tasks;

limited with Oak.Agent.Tasks.Protected_Objects;
limited with Oak.Scheduler;

package Oak.Atomic_Actions with Preelaborate is

   type Atomic_Action_State (Num_Actions : Action_Index) is private;

   procedure Process_Enter_Request
     (Atomic_Action  : not null access Atomic_Action_State;
      T              : not null access Task_Agent'Class;
      Scheduler_Info : in out Scheduler.Oak_Scheduler_Info;
      Action_Id      : in Action_Index;
      Chosen_Task    : out Task_Handler);

   procedure Exit_Barrier
     (Atomic_Action    : not null access Atomic_Action_State;
      T                : not null access Task_Agent'Class;
      Action_Id        : in Action_Index;
      Exception_Raised : in Boolean;
      Chosen_Task      : out Task_Handler);

   procedure Process_Exit_Request
     (Atomic_Action  : not null access Atomic_Action_State;
      T              : not null access Task_Agent'Class;
      Scheduler_Info : in out Scheduler.Oak_Scheduler_Info;
      Action_Id      : in Action_Index;
      Chosen_Task    : out Task_Handler);

   procedure Add_Protected_Object
     (Atomic_Action : not null access Atomic_Action_State;
      PO            : not null access
        Agent.Tasks.Protected_Objects.Protected_Agent'Class);

   function Parent
     (Atomic_Action : not null access Atomic_Action_State)
     return access Atomic_Action_State;

private

   type Action_State is record
      Current_Task : access Task_Agent'Class := null;
      End_Barrier  : access Task_Agent'Class := null;
      Queue        : access Task_Agent'Class := null;
   end record;

   type Action_State_Array is array (Action_Index range <>)
     of access Action_State;

   type Atomic_Action_State (Num_Actions : Action_Index) is record
      Barrier_Start     : Boolean;
      Barried_End       : Boolean;
      Require_All_Tasks : Boolean;
      Exception_Raised  : Boolean;

      Controlling_State : aliased Task_State := No_State;

      Parent            : access Atomic_Action_State := null;
      Actions           : Action_State_Array (1 .. Num_Actions);
      Protected_Objects : access Task_Agent'Class := null;
   end record;

   function Parent
     (Atomic_Action : not null access Atomic_Action_State)
     return access Atomic_Action_State is (Atomic_Action.Parent);

end Oak.Atomic_Actions;
