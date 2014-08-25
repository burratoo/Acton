with Interfaces;

package Oakland.Maths.Arithmetic_64 with Pure is

   function Integer_64_Divide
     (A, B : Interfaces.Integer_64)
      return Interfaces.Integer_64;

   function Unsigned_64_Divide
     (A, B      : Interfaces.Unsigned_64;
      Remainder : access Interfaces.Unsigned_64)
      return Interfaces.Unsigned_64;
   --  Divides two unsigned 64 bit numbers on a 32 bit platform. If R /= 0 then
   --  R returns the remainder of X / Y.

   pragma Export (C, Integer_64_Divide, "__divdi3");
   pragma Export (C, Unsigned_64_Divide, "__udivmoddi4");
end Oakland.Maths.Arithmetic_64;
