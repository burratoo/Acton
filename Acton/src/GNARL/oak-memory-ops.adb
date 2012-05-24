with Ada.Unchecked_Conversion;
with System.Storage_Elements; use System.Storage_Elements;

package body Oak.Memory.Ops is

   function Mem_Compare (dest, src : Address;
                         n         : size_t) return unsigned_char is
      subtype mem is char_array (size_t);
      type memptr is access mem;
      function to_memptr is
         new Ada.Unchecked_Conversion (Address, memptr);
      function To_Unsigned_Char is
         new Ada.Unchecked_Conversion (char, unsigned_char);
      dest_p : constant memptr := to_memptr (dest);
      src_p  : constant memptr := to_memptr (src);

   begin
      if n > 0 then
         --  need to guard against n=0 since size_t is a modular type
         for J in 0 .. n - 1 loop
            if dest_p (J) /= src_p (J) then
               return To_Unsigned_Char (dest_p (J)) -
                 To_Unsigned_Char (src_p (J));
            end if;
         end loop;
      end if;

      return 0;

   end Mem_Compare;

   function Mem_Copy (dest, src : Address;
                      n         : size_t) return Address is
      subtype mem is char_array (size_t);
      type memptr is access mem;
      function to_memptr is
         new Ada.Unchecked_Conversion (Address, memptr);
      dest_p : constant memptr := to_memptr (dest);
      src_p  : constant memptr := to_memptr (src);

   begin
      if n > 0 then
         --  need to guard against n=0 since size_t is a modular type
         for J in 0 .. n - 1 loop
            dest_p (J) := src_p (J);
         end loop;
      end if;

      return dest;
   end Mem_Copy;

   function Mem_Move (dest, src : Address;
                      n         : size_t) return Address is
      subtype mem is char_array (size_t);
      type memptr is access mem;
      function to_memptr is
         new Ada.Unchecked_Conversion (Address, memptr);
      dest_p : constant memptr := to_memptr (dest);
      src_p  : constant memptr := to_memptr (src);

   begin
      if n > 0 then
         --  need to guard against n=0 since size_t is a modular type

         if (src < dest) and (dest < src + Storage_Offset (n)) then
            --  Copy backwards
            for J in reverse 0 .. n - 1 loop
               dest_p (J) := src_p (J);
            end loop;
         else
            for J in 0 .. n - 1 loop
               dest_p (J) := src_p (J);
            end loop;
         end if;
      end if;

      return dest;
   end Mem_Move;

end Oak.Memory.Ops;
