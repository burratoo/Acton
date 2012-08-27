with System; use System;

with Oak.Agent.Tasks.Protected_Objects; use Oak.Agent.Tasks.Protected_Objects;

package Oak.Processor_Support_Package.Interrupts with Preelaborate is

   type Oak_Interrupt_Id is range 1 .. 35;
   Default_Interrupt_Priority : constant Interrupt_Priority :=
                                  Interrupt_Priority'Last;

   type Parameterless_Handler is access protected procedure;

   procedure Initialise_Interrupts;
   procedure Complete_Interrupt_Initialisation;

   procedure Attach_Handler (Interrupt : Oak_Interrupt_Id;
                             Handler   : Parameterless_Handler;
                             Priority  : Interrupt_Priority);
   procedure Get_Resource
     (PO : access Agent.Tasks.Protected_Objects.Protected_Agent'Class);
   procedure Release_Resource
        (PO : access Agent.Tasks.Protected_Objects.Protected_Agent'Class);

private

   procedure Interrupt_Handler (Id : Oak_Interrupt_Id);

   Interrupt_Vector_Table   : array (Oak_Interrupt_Id)
     of Parameterless_Handler :=
    (null, null, null, null, null, null, null, null, null, null, null, null,
     null, null, null, null, null, null, null, null, null, null, null, null,
     null, null, null, null, null, null, null, null, null, null, null);

   procedure Vector_1
     with Export, Convention => Ada, External_Name => "__PSP_Vector_1";
   procedure Vector_2
     with Export, Convention => Ada, External_Name => "__PSP_Vector_2";
   procedure Vector_3
     with Export, Convention => Ada, External_Name => "__PSP_Vector_3";
   procedure Vector_4
     with Export, Convention => Ada, External_Name => "__PSP_Vector_4";
   procedure Vector_5
     with Export, Convention => Ada, External_Name => "__PSP_Vector_5";
   procedure Vector_6
     with Export, Convention => Ada, External_Name => "__PSP_Vector_6";
   procedure Vector_7
     with Export, Convention => Ada, External_Name => "__PSP_Vector_7";
   procedure Vector_8
     with Export, Convention => Ada, External_Name => "__PSP_Vector_8";
   procedure Vector_9
     with Export, Convention => Ada, External_Name => "__PSP_Vector_9";
   procedure Vector_10
     with Export, Convention => Ada, External_Name => "__PSP_Vector_10";
   procedure Vector_11
     with Export, Convention => Ada, External_Name => "__PSP_Vector_11";
   procedure Vector_12
     with Export, Convention => Ada, External_Name => "__PSP_Vector_12";
   procedure Vector_13
     with Export, Convention => Ada, External_Name => "__PSP_Vector_13";
   procedure Vector_14
     with Export, Convention => Ada, External_Name => "__PSP_Vector_14";
   procedure Vector_15
     with Export, Convention => Ada, External_Name => "__PSP_Vector_15";
   procedure Vector_16
     with Export, Convention => Ada, External_Name => "__PSP_Vector_16";
   procedure Vector_17
     with Export, Convention => Ada, External_Name => "__PSP_Vector_17";
   procedure Vector_18
     with Export, Convention => Ada, External_Name => "__PSP_Vector_18";
   procedure Vector_19
     with Export, Convention => Ada, External_Name => "__PSP_Vector_19";
   procedure Vector_20
     with Export, Convention => Ada, External_Name => "__PSP_Vector_20";
   procedure Vector_21
     with Export, Convention => Ada, External_Name => "__PSP_Vector_21";
   procedure Vector_22
     with Export, Convention => Ada, External_Name => "__PSP_Vector_22";
   procedure Vector_23
     with Export, Convention => Ada, External_Name => "__PSP_Vector_23";
   procedure Vector_24
     with Export, Convention => Ada, External_Name => "__PSP_Vector_24";
   procedure Vector_25
     with Export, Convention => Ada, External_Name => "__PSP_Vector_25";
   procedure Vector_26
     with Export, Convention => Ada, External_Name => "__PSP_Vector_26";
   procedure Vector_27
     with Export, Convention => Ada, External_Name => "__PSP_Vector_27";
   procedure Vector_28
     with Export, Convention => Ada, External_Name => "__PSP_Vector_28";
   procedure Vector_29
     with Export, Convention => Ada, External_Name => "__PSP_Vector_29";
   procedure Vector_30
     with Export, Convention => Ada, External_Name => "__PSP_Vector_30";
   procedure Vector_31
     with Export, Convention => Ada, External_Name => "__PSP_Vector_31";
   procedure Vector_32
     with Export, Convention => Ada, External_Name => "__PSP_Vector_32";
   procedure Vector_33
     with Export, Convention => Ada, External_Name => "__PSP_Vector_33";
   procedure Vector_34
     with Export, Convention => Ada, External_Name => "__PSP_Vector_34";
   procedure Vector_35
     with Export, Convention => Ada, External_Name => "__PSP_Vector_35";

end Oak.Processor_Support_Package.Interrupts;
