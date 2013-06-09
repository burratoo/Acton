limited with Ada.Task_Identification;

package Ada.Cyclic_Tasks with Pure is
   type Behaviour is (Normal, Periodic, Sporadic, Aperiodic);

   type Event_Action is (No_Action, Handler, Abort_Action,
                         Abort_And_Raise_Exception);
   type Action_Handler is
     access protected procedure (T : in Ada.Task_Identification.Task_Id);

end Ada.Cyclic_Tasks;
