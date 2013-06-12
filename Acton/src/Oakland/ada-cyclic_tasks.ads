limited with Ada.Task_Identification;

package Ada.Cyclic_Tasks with Preelaborate is
   type Behaviour is (Normal, Aperiodic, Sporadic, Periodic);

   type Event_Action is (No_Action, Handler, Abort_Action,
                         Abort_And_Raise_Exception);
   type Action_Handler is
     access protected procedure (T : in Ada.Task_Identification.Task_Id);

   procedure Release_Task (T : in Ada.Task_Identification.Task_Id);
end Ada.Cyclic_Tasks;
