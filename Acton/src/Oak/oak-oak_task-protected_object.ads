package Oak.Oak_Task.Protected_Object is

   pragma Preelaborate;

   procedure Initialise_Protected_Object
     (PO                    : in Oak_Task_Handler;
      Name                  : in String;
      Ceiling_Priority      : in Integer;
      Barriers_Function     : in Entry_Barrier_Function_Handler;
      Object_Record_Address : in System.Address);

   procedure Add_Task_To_Protected_Object
     (T  : in Oak_Task_Handler;
      PO : in Oak_Task_Handler);

   procedure Remove_Task_From_Protected_Object
     (T  : in Oak_Task_Handler;
      PO : in Oak_Task_Handler);

   function Is_Task_Inside_Protect_Object (T  : in Oak_Task_Handler;
                                           PO : in Oak_Task_Handler)
                                      return Boolean;

   function Get_Task_Within
     (PO : in Oak_Task_Handler) return Oak_Task_Handler;

   procedure Add_Task_To_Entry_Queue
     (PO       : in Oak_Task_Handler;
      T        : in Oak_Task_Handler;
      Entry_Id : Entry_Index);

   procedure Remove_Task_From_Entry_Queue
     (PO : in Oak_Task_Handler;
      T        : in Oak_Task_Handler;
      Entry_Id : Entry_Index);

   procedure Get_And_Remove_Next_Task_From_Entry_Queues
     (PO : in Oak_Task_Handler;
      Next_Task  : out Oak_Task_Handler);

   function Get_Acquiring_Tasks_State
     (For_Protected_Object : in Oak_Task_Handler)
      return Task_State;

   function Get_Reference_To_Acquiring_Tasks_State
     (For_Protected_Object : in Oak_Task_Handler)
      return Shared_Task_State;

   procedure Set_Acquiring_Tasks_State
     (For_Protected_Object : in Oak_Task_Handler;
      To_State : Task_State);

   function Entry_Queue_Length
     (PO               : in Oak_Task_Handler;
      Entry_Id         : in Entry_Index) return Natural;

   function Get_Active_Subprogram_Kind
     (PO : in Oak_Task_Handler) return Protected_Subprogram_Type;

   function Is_Barrier_Open
     (PO       : in Oak_Task_Handler;
      Entry_Id : in Entry_Index) return Boolean;

   function Is_Protected_Object
     (PO : in Oak_Task_Handler) return Boolean;

   function Is_Entry_Id_Valid
     (PO               : in Oak_Task_Handler;
      Entry_Id         : in Entry_Index) return Boolean;

   procedure Purge_Entry_Queues
     (PO             : in Oak_Task_Handler;
      New_Task_State : in Task_State);

   function Has_Entries (PO : in Oak_Task_Handler) return Boolean;

end Oak.Oak_Task.Protected_Object;
