------------------------------------------------------------------------------
--                                                                          --
--                            OAKLAND COMPONENTS                            --
--                                                                          --
--                              OAKLAND.TASKS                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                 Copyright (C) 2011-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

with Oak.Agent;    use Oak.Agent;
with Oak.Message;  use Oak.Message;
with Oak.Oak_Time;

package Oakland.Tasks with Preelaborate is

   procedure Activate_Tasks (Activation_Chain : in Task_List);
   procedure Complete_Activation;
   procedure Complete_Task;

   procedure Change_Cycle_Period (New_Period : in Oak.Oak_Time.Time_Span);

   procedure Change_Relative_Deadline
     (New_Deadline : in Oak.Oak_Time.Time_Span);

   procedure Begin_Cycles_Stage;

   procedure Yield_Processor_To_Kernel
     (With_Message : in out Oak_Message) with Inline => False;

   procedure New_Cycle;

   type Elaboration_Boolean is private;

private
   type Elaboration_Boolean is access Boolean;

end Oakland.Tasks;
