------------------------------------------------------------------------------------------
--                                                                                      --
--                                      OAK VIEWER                                      --
--                                                                                      --
--                                  ISA.ARM.CORTEX_M4                                   --
--                                                                                      --
--                       Copyright (C) 2014-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

package ISA.ARM.Cortex_M4.Exceptions with Pure is

   type Exception_Id is mod 2 ** 9 with Size => 9;
   subtype System_Exception_Id is Exception_Id range 4 .. 15;
   --  Actually only the system exceptions which can have their priorities set.

   type Exception_Priority is mod 2 ** 8 with Size => 8;

   function To_Exception (IRQ : Exception_Id) return Exception_Id;
   function To_IRQ (E : Exception_Id) return Exception_Id;

   Thread_Mode   : constant Exception_Id := 0;
   Stack_Address : constant Exception_Id := 0;
   Reset         : constant Exception_Id := 1;
   NMI           : constant Exception_Id := 2;
   Hard_Fault    : constant Exception_Id := 3;
   Mem_Manage    : constant Exception_Id := 4;
   Bus_Fault     : constant Exception_Id := 5;
   Usage_Fault   : constant Exception_Id := 6;
   Reserved2     : constant Exception_Id := 7;
   Reserved3     : constant Exception_Id := 8;
   Reserved4     : constant Exception_Id := 9;
   Reserved5     : constant Exception_Id := 10;
   SVCall        : constant Exception_Id := 11;
   Reserved6     : constant Exception_Id := 12;
   Reserved7     : constant Exception_Id := 13;
   PendSV        : constant Exception_Id := 14;
   SysTick       : constant Exception_Id := 15;
   IRQ0          : constant Exception_Id := 16;
   IRQ1          : constant Exception_Id := 17;
   IRQ2          : constant Exception_Id := 18;
   IRQ3          : constant Exception_Id := 19;
   IRQ4          : constant Exception_Id := 20;
   IRQ5          : constant Exception_Id := 21;
   IRQ6          : constant Exception_Id := 22;
   IRQ7          : constant Exception_Id := 23;
   IRQ8          : constant Exception_Id := 24;
   IRQ9          : constant Exception_Id := 25;
   IRQ10         : constant Exception_Id := 26;
   IRQ11         : constant Exception_Id := 27;
   IRQ12         : constant Exception_Id := 28;
   IRQ13         : constant Exception_Id := 29;
   IRQ14         : constant Exception_Id := 30;
   IRQ15         : constant Exception_Id := 31;
   IRQ16         : constant Exception_Id := 32;
   IRQ17         : constant Exception_Id := 33;
   IRQ18         : constant Exception_Id := 34;
   IRQ19         : constant Exception_Id := 35;
   IRQ20         : constant Exception_Id := 36;
   IRQ21         : constant Exception_Id := 37;
   IRQ22         : constant Exception_Id := 38;
   IRQ23         : constant Exception_Id := 39;
   IRQ24         : constant Exception_Id := 40;
   IRQ25         : constant Exception_Id := 41;
   IRQ26         : constant Exception_Id := 42;
   IRQ27         : constant Exception_Id := 43;
   IRQ28         : constant Exception_Id := 44;
   IRQ29         : constant Exception_Id := 45;
   IRQ30         : constant Exception_Id := 46;
   IRQ31         : constant Exception_Id := 47;
   IRQ32         : constant Exception_Id := 48;
   IRQ33         : constant Exception_Id := 49;
   IRQ34         : constant Exception_Id := 50;
   IRQ35         : constant Exception_Id := 51;
   IRQ36         : constant Exception_Id := 52;
   IRQ37         : constant Exception_Id := 53;
   IRQ38         : constant Exception_Id := 54;
   IRQ39         : constant Exception_Id := 55;
   IRQ40         : constant Exception_Id := 56;
   IRQ41         : constant Exception_Id := 57;
   IRQ42         : constant Exception_Id := 58;
   IRQ43         : constant Exception_Id := 59;
   IRQ44         : constant Exception_Id := 60;
   IRQ45         : constant Exception_Id := 61;
   IRQ46         : constant Exception_Id := 62;
   IRQ47         : constant Exception_Id := 63;
   IRQ48         : constant Exception_Id := 64;
   IRQ49         : constant Exception_Id := 65;
   IRQ50         : constant Exception_Id := 66;
   IRQ51         : constant Exception_Id := 67;
   IRQ52         : constant Exception_Id := 68;
   IRQ53         : constant Exception_Id := 69;
   IRQ54         : constant Exception_Id := 70;
   IRQ55         : constant Exception_Id := 71;
   IRQ56         : constant Exception_Id := 72;
   IRQ57         : constant Exception_Id := 73;
   IRQ58         : constant Exception_Id := 74;
   IRQ59         : constant Exception_Id := 75;
   IRQ60         : constant Exception_Id := 76;
   IRQ61         : constant Exception_Id := 77;
   IRQ62         : constant Exception_Id := 78;
   IRQ63         : constant Exception_Id := 79;
   IRQ64         : constant Exception_Id := 80;
   IRQ65         : constant Exception_Id := 81;
   IRQ66         : constant Exception_Id := 82;
   IRQ67         : constant Exception_Id := 83;
   IRQ68         : constant Exception_Id := 84;
   IRQ69         : constant Exception_Id := 85;
   IRQ70         : constant Exception_Id := 86;
   IRQ71         : constant Exception_Id := 87;
   IRQ72         : constant Exception_Id := 88;
   IRQ73         : constant Exception_Id := 89;
   IRQ74         : constant Exception_Id := 90;
   IRQ75         : constant Exception_Id := 91;
   IRQ76         : constant Exception_Id := 92;
   IRQ77         : constant Exception_Id := 93;
   IRQ78         : constant Exception_Id := 94;
   IRQ79         : constant Exception_Id := 95;
   IRQ80         : constant Exception_Id := 96;
   IRQ81         : constant Exception_Id := 97;

private
   function To_Exception (IRQ : Exception_Id) return Exception_Id is
     (IRQ + IRQ0);

   function To_IRQ (E : Exception_Id) return Exception_Id is
      (E - IRQ0);
end ISA.ARM.Cortex_M4.Exceptions;
