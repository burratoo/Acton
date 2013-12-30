package Oak.Processor_Support_Package with Pure is
   Number_Of_Processors : constant := 1;
   type Oak_Instance_Id is range 1 .. Number_Of_Processors;
   --  Defines the number of processors used by the processor package
   --  (yes, overloading the term processors here).

end Oak.Processor_Support_Package;
