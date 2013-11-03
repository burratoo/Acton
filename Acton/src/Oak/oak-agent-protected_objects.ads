with System;

with Ada.Cyclic_Tasks;
with Oak.Indices;     use Oak.Indices;

package Oak.Agent.Protected_Objects with Preelaborate is

   type Protected_Agent (Num_Entries : Entry_Index)
     is new Oak_Agent with private
     with Preelaborable_Initialization;

   type Entry_Barrier_Function_Handler is
     access function (PO : System.Address;
                      E  : Protected_Entry_Index) return Boolean;

   procedure Initialise_Protected_Agent
     (Agent                 : not null access Protected_Agent'Class;
      Name                  : in String;
      Ceiling_Priority      : in Integer;
      Barriers_Function     : in Entry_Barrier_Function_Handler;
      Object_Record_Address : in System.Address);

   function Active_Subprogram_Kind
     (PO : in Protected_Agent'Class)
      return Protected_Subprogram_Type;

   function Entry_Queue_Length
     (PO       : in Protected_Agent'Class;
      Entry_Id : in Entry_Index)
      return Natural;

   function Is_Barrier_Open
     (PO       : in out Protected_Agent'Class;
      Entry_Id : in     Entry_Index)
      return Boolean;
   --  Evaluates the entry's barrier function to see if it is open. Note that
   --  this function is not side-effect free: If an exception occurs while
   --  evaluating the barrier, the entries queues are all purged and the
   --  reponse PO_Entry_Refused propergated to all tasks that were queued.
   --  While the exception and purging could be handled by the caller, since
   --  the response is common to callers, it make sense to place it here.

   function Is_Entry_Id_Valid
     (PO       : in Protected_Agent'Class;
      Entry_Id : in Entry_Index)
      return Boolean;

   function Is_Task_Inside_Protect_Object
     (PO : in Protected_Agent'Class;
      T  : not null access Oak_Agent'Class)
      return Boolean;

   function Has_Entries
     (PO : in Protected_Agent'Class)
      return Boolean;

   function Task_Within
     (PO : in Protected_Agent'Class)
      return access Oak_Agent'Class;

   procedure Add_Contending_Task
     (PO : in out Protected_Agent'Class;
      T  : access Oak_Agent'Class);

   procedure Add_Task_To_Entry_Queue
     (PO       : in out Protected_Agent'Class;
      T        : access Oak_Agent'Class;
      Entry_Id : Entry_Index);

   procedure Add_Task_To_Protected_Object
     (PO : in out Protected_Agent'Class;
      T  : not null access Oak_Agent'Class);

   procedure Get_And_Remove_Next_Contending_Task
     (PO        : in out Protected_Agent'Class;
      Next_Task : out Agent_Handler);

   procedure Get_And_Remove_Next_Task_From_Entry_Queues
     (PO         : in out Protected_Agent'Class;
      Next_Task  : out Agent_Handler);

   procedure Purge_Entry_Queues
     (PO             : in out Protected_Agent'Class;
      New_Task_State : in     Agent_State);

   procedure Remove_Task_From_Entry_Queue
     (PO       : in out Protected_Agent'Class;
      T        : access Oak_Agent'Class;
      Entry_Id : Entry_Index);

   procedure Remove_Task_From_Protected_Object
     (PO : in out Protected_Agent'Class;
      T  : access Oak_Agent'Class);

   type Parameterless_Access is access protected procedure;

   function Protected_Object_From_Access
     (Handler : Parameterless_Access)
      return access Protected_Agent'Class;

   function Protected_Object_From_Access
     (Handler : Ada.Cyclic_Tasks.Response_Handler)
      return access Protected_Agent'Class;

private

   type Entry_Queue_Array is array (Entry_Index range <>)
     of access Oak_Agent'Class;

   type Protected_Agent (Num_Entries : Entry_Index)
     is new Oak_Agent with record
      Object_Record  : System.Address;

      Entry_Barriers : Entry_Barrier_Function_Handler;
      Entry_Queues   : Entry_Queue_Array (1 .. Num_Entries);

      Active_Subprogram_Kind   : Protected_Subprogram_Type;
      Tasks_Within             : access Oak_Agent'Class;
      Contending_Tasks         : access Oak_Agent'Class;
   end record;

   function Active_Subprogram_Kind
     (PO : in Protected_Agent'Class)
      return Protected_Subprogram_Type is (PO.Active_Subprogram_Kind);

   function Is_Entry_Id_Valid
     (PO       : in Protected_Agent'Class;
      Entry_Id : in Entry_Index)
      return Boolean is (Entry_Id in PO.Entry_Queues'Range);

   function Has_Entries
     (PO : in Protected_Agent'Class)
      return Boolean is (if PO.Entry_Barriers = null then False else True);

   function Task_Within
     (PO : in Protected_Agent'Class)
      return access Oak_Agent'Class is (PO.Tasks_Within);

end Oak.Agent.Protected_Objects;
