package Oakland.Memory with Pure is
   procedure Malloc
     with Export, Convention => C, External_Name => "__gnat_malloc";
end Oakland.Memory;
