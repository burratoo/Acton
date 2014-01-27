package Oak.Project_Support_Package with Pure is

   Max_Kernel_Agents     : constant := 1;
   --  The number of Oak Kernel you need should be equal to the number of
   --  processors in the system. Not linked to the constant
   --  Oak.Processor_Support_Package.Number_Of_Processors because in theory
   --  there is nothing to stop an instance being scheduled by a scheduler
   --  agent.

   Max_Scheduler_Agents  : constant := 2;
   --  A limit on the number of Scheduler Agents in the system.

   Max_Interrupt_Agents  : constant := 5;
   --  A limit on the number of interrupt priorities used by the system. Could
   --  be less than the number of interrupt prioities available on the hardware
   --  if not all the priorities are used. This allows for some space saving.

   Max_Task_Agents       : constant := 7;
   --  Limit on the number of tasks on the system.

   Max_Protected_Agents  : constant := 1;
   --  Limit on the number of protected objects use in the system.

   Max_Sleep_Agents      : constant := 1;
   --  The maximum number of sleep agents is always one. Only defined to help
   --  explain the Max_Oak_Agents constant.

   Max_Oak_Agents        : constant := Max_Kernel_Agents +
                             Max_Scheduler_Agents + Max_Interrupt_Agents +
                               Max_Task_Agents + Max_Protected_Agents +
                                 Max_Sleep_Agents;
   --  The maximum number of Oak Agents is defined by the sum of the previous
   --  agents. Each of these agents contains an Oak Agent as their general
   --  data structure.

   Max_Timers            : constant := Max_Kernel_Agents
                             + Max_Task_Agents + Max_Scheduler_Agents;
   --  The maximum number of Oak Timers used in the system. At the bare minimum
   --  each oak instance, scheduler agent and task agent need at least on timer
   --  each.

   Max_Task_Name_Length  : constant := 80;
   --  The length of a task name. Set to an appropriate value for the system.
   --  Can be zero if not needed.

   Max_Protected_Entries : constant := 10;
   Max_Task_Entries      : constant := 0;
   --  Sets the maximum number of entries on the system. Oak does not directly
   --  apply a limit on the number of entries a protect object or task can
   --  have: instead this is used to determine the appropriate size of the
   --  variables used to hold entry ids. Can also be used to limit the number
   --  of entries when the application requires for certification or
   --  provability reasons for the number of entries to be restricted, for
   --  example under the Ravenscar Profile.

end Oak.Project_Support_Package;
