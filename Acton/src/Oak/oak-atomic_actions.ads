with Ada.Atomic_Actions; use Ada.Atomic_Actions;
with Oak.Indices;        use Oak.Indices;
with Oak.Agent.Tasks;    use Oak.Agent.Tasks;
with Oak.States; use Oak.States;

limited with Oak.Agent.Tasks.Protected_Objects;
limited with Oak.Scheduler;
with Oak.Agent; use Oak.Agent;

package Oak.Atomic_Actions with Preelaborate is

   type Atomic_Object (Num_Actions : Action_Index) is private;

   procedure Add_Protected_Object
     (AO : not null access Atomic_Object;
      PO : not null access
        Agent.Tasks.Protected_Objects.Protected_Agent'Class);

   procedure Exit_Barrier
     (AO                : not null access Atomic_Object;
      T                 : not null access Task_Agent'Class;
      Scheduler_Info    : in out Scheduler.Oak_Scheduler_Info;
      Action_Id         : in Action_Index;
      Exception_Raised  : in Boolean;
      Next_Agent_To_Run : out Agent_Handler);

   procedure Initialise_Atomic_Object
     (AO            : not null access Atomic_Object;
      Parent        : access Atomic_Object;
      End_Barrier   : in Boolean;
      Start_Barrier : in Boolean;
      Participating : in Participating_Actions);

   function Parent
     (AO : not null access Atomic_Object)
      return access Atomic_Object;

   procedure Process_Enter_Request
     (AO                : not null access Atomic_Object;
      T                 : not null access Task_Agent'Class;
      Scheduler_Info    : in out Scheduler.Oak_Scheduler_Info;
      Action_Id         : in Action_Index;
      Next_Agent_To_Run : out Agent_Handler);

   procedure Process_Exit_Request
     (AO               : not null access Atomic_Object;
      T                : not null access Task_Agent'Class;
      Scheduler_Info   : in out Scheduler.Oak_Scheduler_Info;
      Action_Id        : in Action_Index;
      Exception_Raised : in Boolean;
      Next_Agent_To_Run : out Agent_Handler);

private

   type Action_State is record
      Current_Task : access Task_Agent'Class := null;
      End_Barrier  : access Task_Agent'Class := null;
      Queue        : access Task_Agent'Class := null;
   end record;

   type Action_State_Array is array (Action_Index range <>)
     of Action_State;

   type Atomic_Object (Num_Actions : Action_Index) is record
      Barrier_Start     : Boolean;
      Barrier_End       : Boolean;
      Exception_Raised  : Boolean;
      Participating     : Participating_Actions;

      Controlling_State : aliased Agent_State := No_State;

      Parent            : access Atomic_Object := null;
      Actions           : Action_State_Array (1 .. Num_Actions);
      Protected_Objects : access Task_Agent'Class := null;
   end record;

   function Parent
     (AO : not null access Atomic_Object)
     return access Atomic_Object is (AO.Parent);

end Oak.Atomic_Actions;
