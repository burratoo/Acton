limited with Ada.Task_Identification;
with System;

package Ada.Cyclic_Tasks with Preelaborate is
   type Behaviour is (Normal, Aperiodic, Sporadic, Periodic);

   type Event_Action is (No_Action, Handler, Abort_Action,
                         Abort_And_Raise_Exception);
   type Action_Handler is
     access protected procedure (T : in Ada.Task_Identification.Task_Id);

   Min_Handler_Ceiling : constant System.Any_Priority :=
                           System.Interrupt_Priority'First;
   --  The ceiling range for the protected object containing the handler is
   --  set to the interrupt priority range. This allows Acton to use its
   --  interrupt handling facilities to make the protected procedure call.
   --  It also allows the user if they need to prevent the handler from
   --  running in a time critical section of code by running that code inside
   --  a protected object with a higher ceiling. Do we really want to allow
   --  the last bit???

   --  Well that was the old comment. As it happens there is currently no
   --  easy way of figuring out what the ceiling priority of the handler's
   --  protected object is before calling it. That is because the access
   --  type contains the reference to the protect object record which contains
   --  the protected agent which we need to pull out the priority data. However
   --  due to finalization reasons, the protected agent is palced after
   --  the private variables, thus Oak has no idea where the protected agent
   --  is inside the record. Hence, we play it safe and force the user
   --  to use the highest priority possible since the handler will most
   --  likely be shorter than the to and fro-ing between the handler and Oak
   --  that would occur if we tried to use the existing PO entry request
   --  mechanism. Sigh, changed mind again.
   --

   procedure Release_Task (T : in Ada.Task_Identification.Task_Id);
end Ada.Cyclic_Tasks;
