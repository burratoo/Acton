package Oak.Interrupt is
   --  Actually, an interrupt goes of the top of almost everything and
   --  interacts directly with its respective protected object. Doesn't need to
   --  interact with Acton's scheduler system except if a task happens to be in
   --  the protected objec that it is trying to access. In this case, when a
   --  task is inside a protected object we should disable interrupts that are
   --  at that priority level and below it.
end Oak.Interrupt;
