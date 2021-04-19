------------------------------------------------------------------------------------------
--                                                                                      --
--                               OAK CORE SUPPORT PACKAGE                               --
--                                    ARM CORTEX M4F                                    --
--                                                                                      --
--                             ISA.ARM.CORTEX_M4.EXCEPTIONS                             --
--                                                                                      --
--                       Copyright (C) 2014-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

with System.Machine_Code; use System.Machine_Code;

package body ISA.ARM.Cortex_M4.Exceptions is

   function Current_Exception return Exception_Id is
      Id : Exception_Id;
   begin
      Asm ("mrs %0, ipsr",
           Outputs => Exception_Id'Asm_Output ("=r", Id),
           Volatile => True);
      return Id;
   end Current_Exception;

end ISA.ARM.Cortex_M4.Exceptions;
