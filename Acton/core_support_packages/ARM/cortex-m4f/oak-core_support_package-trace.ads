------------------------------------------------------------------------------------------
--                                                                                      --
--                               OAK CORE SUPPORT PACKAGE                               --
--                                    ARM CORTEX M4F                                    --
--                                                                                      --
--                            OAK.CORE_SUPPORT_PACKAGE.TRACE                            --
--                                                                                      --
--                       Copyright (C) 2014-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

with Oak.Agent;   use Oak.Agent;
with Oak.Core;    use Oak.Core;
with Oak.States;  use Oak.States;

package Oak.Core_Support_Package.Trace with Preelaborate is

   ---------------------------------
   -- INTERNAL PACKAGE COMPONENTS --
   ---------------------------------

   type Kernel_Entry_Tracing is record
      Reason  : Run_Reason;
      Request : Agent_State;
   end record with Size => 16;

   type Kernel_Exit_Tracing is record
      To_Agent : Oak_Agent_Id;
   end record with Size => 8;

   for Kernel_Entry_Tracing use record
      Reason  at 0 range 0 .. 7;
      Request at 0 range 8 .. 15;
   end record;

   for Kernel_Exit_Tracing use record
      To_Agent at 0 range 0 .. 7;
   end record;

end Oak.Core_Support_Package.Trace;
