------------------------------------------------------------------------------
--                                                                          --
--                            OAKLAND COMPONENTS                            --
--                                                                          --
--                              OAKLAND.TASKS                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                 Copyright (C) 2011-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

with Oak.Agent.Kernel;    use Oak.Agent.Kernel;
with Oak.Agent.Oak_Agent; use Oak.Agent.Oak_Agent;

with Oak.Core;     use Oak.Core;
with Oak.States;   use Oak.States;
with Oak.Oak_Time; use Oak.Oak_Time;

package body Oakland.Tasks is

   --------------------
   -- Activate_Tasks --
   --------------------

   --  Called by the Activator

   procedure Activate_Tasks (Activation_Chain : in Task_List) is
      Self : constant Oak_Agent_Id := Current_Agent (This_Oak_Kernel);

      Activation_Pending_Message  : Oak_Message :=
                                (Message_Type    => Activation_Pending,
                                 Activation_List => Activation_Chain);
      Activation_Complete_Message : Oak_Message :=
                                (Message_Type => Activation_Complete);
   begin
      --  TODO:  The check and transfer of tasks from chain to Activation list
      --  should occur in Oak not here.

      --  Need to check elaboration here.

      Yield_Processor_To_Kernel
        (With_Message => Activation_Pending_Message);

      if State (Self) = Activation_Successful then
         Yield_Processor_To_Kernel
           (With_Message => Activation_Complete_Message);
      else
         raise Tasking_Error with "Failure during activation";
      end if;

   end Activate_Tasks;

   procedure Begin_Cycles_Stage is
      Message : Oak_Message := (Message_Type => Setup_Cycles);
   begin
      Yield_Processor_To_Kernel (With_Message => Message);
   end Begin_Cycles_Stage;

   -----------------------------
   -- Complete_Activation     --
   -----------------------------

   --  Called by the Activatee

   procedure Complete_Activation is
      Self : constant Oak_Agent_Id := Current_Agent (This_Oak_Kernel);
      Activation_Successful_Message : Oak_Message :=
        (Message_Type => Activation_Successful);
   begin
      --  Why do we send the message twice?? And we never send a failure
      --  message.

      Yield_Processor_To_Kernel
        (With_Message => Activation_Successful_Message);
      if State (Self) /= Runnable then
         --  Need to include a cleanup routine here, though for a Ravenscar
         --  Profile system that isn't required as no tasks will be running on
         --  the system.
         raise Tasking_Error with "Failure during activation";
      end if;
   end Complete_Activation;

   --  Trival complete task.
   procedure Complete_Task is
      Message : Oak_Message :=
        (Message_Type            => Sleeping,
         Wake_Up_At              => Time_Last,
         Remove_From_Charge_List => True);
   begin
      Yield_Processor_To_Kernel (With_Message => Message);
   end Complete_Task;

   procedure Change_Cycle_Period (New_Period : in Time_Span) is
      Message : Oak_Message :=
        (Message_Type       => Update_Task_Property,
         Update_Task        => Current_Agent (This_Oak_Kernel),
         Property_To_Update =>
           (Property     => Cycle_Period,
            Cycle_Period => New_Period));
   begin
      Yield_Processor_To_Kernel (With_Message => Message);
   end Change_Cycle_Period;

   procedure Change_Relative_Deadline (New_Deadline : in Time_Span)
   is
      Message : Oak_Message :=
        (Message_Type       => Update_Task_Property,
         Update_Task        => Current_Agent (This_Oak_Kernel),
         Property_To_Update =>
           (Property     => Relative_Deadline,
            Deadline_Span => New_Deadline));
   begin
      Yield_Processor_To_Kernel (With_Message => Message);
   end Change_Relative_Deadline;

   procedure New_Cycle is
      Message : Oak_Message := (Message_Type => New_Cycle);
   begin
      Yield_Processor_To_Kernel (With_Message => Message);
   end New_Cycle;

   procedure Yield_Processor_To_Kernel
     (With_Message : in out Oak_Message)
   is
   begin
      Request_Oak_Service
        (Reason_For_Run => Agent_Request,
         Message        => With_Message);
   end Yield_Processor_To_Kernel;

end Oakland.Tasks;
