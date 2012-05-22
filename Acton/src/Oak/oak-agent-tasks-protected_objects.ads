with System;

with Oak.Entries;           use Oak.Entries;
with Oak.Protected_Objects; use Oak.Protected_Objects;

package Oak.Agent.Tasks.Protected_Objects with Preelaborate is

   type Protected_Agent (Num_Entries : Entry_Index)
     is new Task_Agent with private;

   type Entry_Barrier_Function_Handler is private;

   procedure Initialise_Protected_Agent
     (Agent                 : in out Protected_Agent'Class;
      Name                  : in String;
      Ceiling_Priority      : in Integer;
      Barriers_Function     : in Entry_Barrier_Function_Handler;
      Object_Record_Address : in System.Address);

   function Active_Subprogram_Kind
     (PO : not null access Protected_Agent'Class)
      return Protected_Subprogram_Type;

   function Acquiring_Tasks_State
     (For_Protected_Object : not null access Protected_Agent'Class)
      return Task_State;

   function Entry_Queue_Length
     (PO       : not null access Protected_Agent'Class;
      Entry_Id : in Entry_Index)
      return Natural;

   function Is_Barrier_Open
     (PO       : not null access Protected_Agent'Class;
      Entry_Id : in Entry_Index)
      return Boolean;
   --  Evaluates the entry's barrier function to see if it is open. Note that
   --  this function is not side-effect free: If an exception occurs while
   --  evaluating the barrier, the entries queues are all purged and the
   --  reponse PO_Entry_Refused propergated to all tasks that were queued.
   --  While the exception and purging could be handled by the caller, since
   --  the response is common to callers, it make sense to place it here.

   function Is_Entry_Id_Valid
     (PO       : not null access Protected_Agent'Class;
      Entry_Id : in Entry_Index)
      return Boolean;

   function Is_Task_Inside_Protect_Object
     (PO : not null access Protected_Agent'Class;
      T  : access Task_Agent'Class)
      return Boolean;

   function Reference_To_Acquiring_Tasks_State
     (For_Protected_Object : access Protected_Agent'Class)
      return Shared_Task_State;

   function Has_Entries
     (PO : not null access Protected_Agent'Class)
      return Boolean;

   function Task_Within
     (PO : not null access Protected_Agent'Class)
      return access Task_Agent'Class;

   procedure Add_Task_To_Entry_Queue
     (PO       : not null access Protected_Agent'Class;
      T        : access Task_Agent'Class;
      Entry_Id : Entry_Index);

   procedure Add_Task_To_Protected_Object
     (PO : not null access Protected_Agent'Class;
      T  : access Task_Agent'Class);

   procedure Get_And_Remove_Next_Task_From_Entry_Queues
     (PO         : not null access Protected_Agent'Class;
      Next_Task  : out Task_Handler);

   procedure Purge_Entry_Queues
     (PO             : not null access Protected_Agent'Class;
      New_Task_State : in Task_State);

   procedure Remove_Task_From_Entry_Queue
     (PO       : not null access Protected_Agent'Class;
      T        : access Task_Agent'Class;
      Entry_Id : Entry_Index);

   procedure Remove_Task_From_Protected_Object
     (PO : not null access Protected_Agent'Class;
      T  : access Task_Agent'Class);

   procedure Set_Acquiring_Tasks_State
     (For_Protected_Object : not null access Protected_Agent'Class;
      To_State             : in Task_State);

private

   type Entry_Barrier_Function_Handler is
     access function (PO : System.Address;
                      E  : Protected_Entry_Index) return Boolean;

   type Entry_Queue_Array is array (Entry_Index range <>)
     of access Task_Agent'Class;

   type Protected_Agent (Num_Entries : Entry_Index)
     is new Task_Agent with record
      Object_Record  : System.Address;

      Entry_Barriers : Entry_Barrier_Function_Handler;
      Entry_Queues   : Entry_Queue_Array (1 .. Num_Entries) :=
                         (others => null);

      Controlling_Shared_State : aliased Task_State :=  Waiting;
      Active_Subprogram_Kind   : Protected_Subprogram_Type
        := Protected_Function;
      Tasks_Within             : access Task_Agent'Class := null;
   end record;

   function Active_Subprogram_Kind
     (PO : not null access Protected_Agent'Class)
      return Protected_Subprogram_Type is (PO.Active_Subprogram_Kind);

   function Acquiring_Tasks_State
     (For_Protected_Object : not null access Protected_Agent'Class)
      return Task_State is (For_Protected_Object.Controlling_Shared_State);

   function Is_Entry_Id_Valid
     (PO       : not null access Protected_Agent'Class;
      Entry_Id : in Entry_Index)
      return Boolean is (Entry_Id in PO.Entry_Queues'Range);

   function Reference_To_Acquiring_Tasks_State
     (For_Protected_Object : access Protected_Agent'Class)
      return Shared_Task_State
        is (For_Protected_Object.Controlling_Shared_State'Access);

   function Has_Entries
     (PO : not null access Protected_Agent'Class)
      return Boolean is (if PO.Entry_Barriers = null then False else True);

   function Task_Within
     (PO : not null access Protected_Agent'Class)
      return access Task_Agent'Class is (PO.Tasks_Within);

end Oak.Agent.Tasks.Protected_Objects;
