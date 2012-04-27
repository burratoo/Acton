with Interfaces; use Interfaces;
with MPC5554.Flash; use MPC5554.Flash;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;

package MPC5554.H7F_Driver is

   type Return_Code is (Ok,
                        Info_Read_Write_Error_For_Previous_Reads,
                        Info_ECC_Error_For_Previous_Reads,
                        Info_Program_Erase_For_All_Blocks_Disabled,
                        Info_Program_Erase_For_Boot_Block_Disabled,
                        Error_Driver_Cannot_Operate_On_This_Part,
                        Error_Flash_In_STOP_Mode,
                        Error_Alignment,
                        Error_Address_Range,
                        Error_Busy,
                        Error_Programming_Unsuccessful,
                        Error_Erase_Unsuccessful,
                        Error_Memory_Not_Blanked,
                        Error_Verify_Failed,
                        Error_Invalid_Block_Lock_Indicator,
                        Error_Read_Write_Error_For_Previous_Reads,
                        Error_Bad_Password);

   type Lock_Indicator_Type is (Shadow_Primary,
                                Shadow_Secondary,
                                Low_Primary,
                                Low_Secondary,
                                Mid_Primary,
                                Mid_Secondary,
                                High);

   type H7F_Page_Size is (H7FA_Page_Size, H7FB_Page_Size);

   type SSD_Config is record
      H7F_Control_Register_Base : Address;
      Main_Array_Base           : Address;
      Main_Array_Size           : Unsigned_32;
      Shadow_Row_Base           : Address;
      Shadow_Row_Size           : Unsigned_32;
      Low_Block_Number          : Unsigned_32;
      Mid_Block_Number          : Unsigned_32;
      High_Block_Number         : Unsigned_32;
      Page_Size                 : H7F_Page_Size;
      Debug_Mode_Selection      : Unsigned_32;
   end record;

   Null_Callback : constant Address := System'To_Address (16#FFFF_FFFF#);

   function Flash_Init (Config : access SSD_Config) return Return_Code;
   function Flash_Program (Config : access SSD_Config;
                           Destination : Address;
                           Size        : Unsigned_32;
                           Source      : Address;
                           Call_Back   : Address)
                           return Return_Code;
   function Get_Lock (Config : access SSD_Config;
                      Lock_Indicator : Lock_Indicator_Type;
                      Lock_Enable    : access Lock_Enable_Type;
                      Lock_State     : access Unsigned_32)
                      return Return_Code;
   function Set_Lock (Config : access SSD_Config;
                      Lock_Indicator : Lock_Indicator_Type;
                      Lock_State     : Unsigned_32;
                      Password       : Unsigned_32)
                      return Return_Code;

private

   for Return_Code use (Ok                                         => 16#0000#,
                        Info_Read_Write_Error_For_Previous_Reads   => 16#0001#,
                        Info_ECC_Error_For_Previous_Reads          => 16#0002#,
                        Info_Program_Erase_For_All_Blocks_Disabled => 16#0004#,
                        Info_Program_Erase_For_Boot_Block_Disabled => 16#0008#,
                        Error_Driver_Cannot_Operate_On_This_Part   => 16#0010#,
                        Error_Flash_In_STOP_Mode                   => 16#0020#,
                        Error_Alignment                            => 16#0100#,
                        Error_Address_Range                        => 16#0200#,
                        Error_Busy                                 => 16#0300#,
                        Error_Programming_Unsuccessful             => 16#0400#,
                        Error_Erase_Unsuccessful                   => 16#0500#,
                        Error_Memory_Not_Blanked                   => 16#0600#,
                        Error_Verify_Failed                        => 16#0700#,
                        Error_Invalid_Block_Lock_Indicator         => 16#0800#,
                        Error_Read_Write_Error_For_Previous_Reads  => 16#0900#,
                        Error_Bad_Password                         => 16#0B00#
                       );

   for Lock_Indicator_Type use (Shadow_Primary   => 0,
                                Shadow_Secondary => 1,
                                Low_Primary      => 2,
                                Low_Secondary    => 3,
                                Mid_Primary      => 4,
                                Mid_Secondary    => 5,
                                High             => 6);

   for H7F_Page_Size use (H7FA_Page_Size => 0, H7FB_Page_Size => 1);

   pragma Import (C, Flash_Init, "mpc5554__h4f_driver__flash_int");
   pragma Import (C, Flash_Program, "mpc5554__h4f_Driver__flash_program");
   pragma Import (C, Get_Lock, "mpc5554__h4f_driver__get_lock");
   pragma Import (C, Set_Lock, "mpc5554__h4f_driver__set_lock");

   type Driver_Program_Space is array (Integer_Address range <>) of
     Unsigned_32;

   Flash_Init_Store : constant Driver_Program_Space :=
     (16#9421FFF0#, 16#93E1000C#, 16#83E30000#, 16#80BF0000#, 16#54AA0673#,
      16#4182000C#, 16#3BE00020#, 16#480000E8#, 16#54BF97BF#, 16#4182000C#,
      16#8183000C#, 16#818C00E0#, 16#38C00000#, 16#90C30018#, 16#90C3001C#,
      16#54A6677E#, 16#28060004#, 16#7CAC28F8#, 16#558CB73A#, 16#7FFF6378#,
      16#40800010#, 16#39800002#, 16#7D863030#, 16#48000030#, 16#2C060004#,
      16#4082000C#, 16#38C0000A#, 16#48000020#, 16#2C060005#, 16#4082000C#,
      16#38C0000C#, 16#48000010#, 16#2C060006#, 16#4082000C#, 16#38C00006#,
      16#90C30014#, 16#54AC00C7#, 16#41820034#, 16#54AC010F#, 16#41820018#,
      16#3D800005#, 16#91830008#, 16#39600004#, 16#91630018#, 16#48000054#,
      16#3D800002#, 16#91830008#, 16#39600004#, 16#91630014#, 16#48000040#,
      16#54ABD29A#, 16#3CCB0004#, 16#3C800004#, 16#7C062040#, 16#90C30008#,
      16#40810010#, 16#54AC8FBC#, 16#398C0002#, 16#91830018#, 16#3D800008#,
      16#7C066040#, 16#40810010#, 16#3D86FFF8#, 16#558C7C7E#, 16#9183001C#,
      16#81830024#, 16#387F0000#, 16#83E1000C#, 16#38210010#, 16#2C0C0000#,
      16#41820008#, 16#00000000#, 16#4E800020#,
      16#4D504348#, 16#37467846#, 16#49333233#);

   pragma Export (Assembly, Flash_Init_Store,
                  "mpc5554__h4f_driver__flash_int");

   Flash_Program_Store : constant Driver_Program_Space :=
     (16#7C892B78#, 16#7C0802A6#, 16#9421FFD0#, 16#5529077F#, 16#BEC10008#,
      16#83E30000#, 16#90010034#, 16#3B430000#, 16#3BC40000#, 16#3BA50000#,
      16#3AE60000#, 16#3B270000#, 16#3B800000#, 16#3AC00010#, 16#4082000C#,
      16#54CC07BF#, 16#4182000C#, 16#3B800100#, 16#480001C4#, 16#831A000C#,
      16#80630010#, 16#7C18F040#, 16#809A0004#, 16#80FA0008#, 16#7F781A14#,
      16#7CC43A14#, 16#7CBE2A14#, 16#41810024#, 16#7C1ED840#, 16#4080001C#,
      16#7C03E840#, 16#41800014#, 16#7C05D840#, 16#4181000C#, 16#38E00001#,
      16#48000034#, 16#7C04F040#, 16#41810024#, 16#7C1E3040#, 16#4080001C#,
      16#7C07E840#, 16#41800014#, 16#7C053040#, 16#4181000C#, 16#38E00002#,
      16#4800000C#, 16#3B800200#, 16#48000150#, 16#2C1D0000#, 16#41820148#,
      16#837F0000#, 16#736B0014#, 16#41820020#, 16#736C0817#, 16#2C0C0006#,
      16#4082000C#, 16#2C070002#, 16#4182000C#, 16#3B800300#, 16#48000120#,
      16#576C0423#, 16#41820008#, 16#819800E0#, 16#817F0000#, 16#3F00FFFF#,
      16#63183FFF#, 16#616B0010#, 16#7D6BC038#, 16#917F0000#, 16#815A0020#,
      16#57BBE8FE#, 16#2C0A0000#, 16#40820008#, 16#3AC00020#, 16#2C1B0000#,
      16#3BA00001#, 16#41820098#, 16#81770000#, 16#81970004#, 16#917E0000#,
      16#919E0004#, 16#3BDE0008#, 16#7C1EB396#, 16#3AF70008#, 16#7C00B1D6#,
      16#7D00F051#, 16#4182000C#, 16#7C1DD840#, 16#4082005C#, 16#819F0000#,
      16#618C0001#, 16#7D8CC038#, 16#919F0000#, 16#48000014#, 16#2C19FFFF#,
      16#4182000C#, 16#7F2903A6#, 16#4E800421#, 16#819F0000#, 16#558C056B#,
      16#4182FFE8#, 16#819F0000#, 16#3D60FFFF#, 16#558C05AD#, 16#819F0000#,
      16#616B3FFE#, 16#7D6C6038#, 16#919F0000#, 16#4082000C#, 16#3B800400#,
      16#48000010#, 16#3BBD0001#, 16#7C1DD840#, 16#4081FF70#, 16#819F0000#,
      16#3D60FFFF#, 16#616B3FEF#, 16#7D6C6038#, 16#919F0000#, 16#83DA0000#,
      16#83FE001C#, 16#819E001C#, 16#558C003C#, 16#919E001C#, 16#815A0000#,
      16#816A001C#, 16#3BCA001C#, 16#616B0001#, 16#916A001C#, 16#819A0000#,
      16#93EC001C#, 16#819A0024#, 16#80010034#, 16#387C0000#, 16#7C0803A6#,
      16#BAC10008#, 16#38210030#, 16#2C0C0000#, 16#41820008#, 16#00000000#,
      16#4E800020#, 16#4D504348#, 16#37467846#, 16#50333232#);

   pragma Export (Assembly, Flash_Program_Store,
                  "mpc5554__h4f_driver__flash_program");

   Get_Lock_Store : constant Driver_Program_Space :=
     (16#2C040006#, 16#9421FFF0#, 16#39600000#, 16#93E1000C#, 16#3BE00000#,
      16#99650000#, 16#4081000C#, 16#3BE00800#, 16#48000094#, 16#2C040002#,
      16#38E00004#, 16#3D200010#, 16#39000014#, 16#4082000C#, 16#3D200000#,
      16#4800004C#, 16#2C040004#, 16#40820010#, 16#3D20000F#, 16#39000010#,
      16#48000040#, 16#2C040000#, 16#41820038#, 16#2C040003#, 16#38E0000C#,
      16#40820008#, 16#4BFFFFD0#, 16#2C040005#, 16#40820008#, 16#4BFFFFD4#,
      16#2C040006#, 16#40820014#, 16#38E00008#, 16#3D200FFF#, 16#6129FFFF#,
      16#390B0000#, 16#81830000#, 16#7C87602E#, 16#548B0001#, 16#4182000C#,
      16#39800001#, 16#99850000#, 16#7C8C4838#, 16#7D8C4430#, 16#91860000#,
      16#81830024#, 16#387F0000#, 16#83E1000C#, 16#38210010#, 16#2C0C0000#,
      16#41820008#, 16#00000000#, 16#4E800020#,
      16#4D504348#, 16#37467847#, 16#4C333230#);

   pragma Export (Assembly, Get_Lock_Store,
                  "mpc5554__h4f_driver__get_lock");

   Set_Lock_Store : constant Driver_Program_Space :=
     (16#2C040006#, 16#9421FFF0#, 16#93E1000C#, 16#4081000C#, 16#3BE00800#,
      16#480000C0#, 16#39200004#, 16#2C040002#, 16#3BE00000#, 16#3D000010#,
      16#38E00014#, 16#4082000C#, 16#3D000000#, 16#4800004C#, 16#2C040004#,
      16#40820010#, 16#3D00000F#, 16#38E00010#, 16#48000040#, 16#2C040000#,
      16#41820038#, 16#2C040003#, 16#3920000C#, 16#40820008#, 16#4BFFFFD0#,
      16#2C040005#, 16#40820008#, 16#4BFFFFD4#, 16#2C040006#, 16#40820014#,
      16#39200008#, 16#3D000FFF#, 16#6108FFFF#, 16#38E00000#, 16#81830000#,
      16#7D296214#, 16#81690000#, 16#556B0001#, 16#4082001C#, 16#90C90000#,
      16#81890000#, 16#558C0001#, 16#4082000C#, 16#3BE00B00#, 16#48000024#,
      16#81890000#, 16#7CAA3830#, 16#7D8C4078#, 16#91890000#, 16#81690000#,
      16#7D0A5038#, 16#7D6B5378#, 16#91690000#, 16#81830024#, 16#387F0000#,
      16#83E1000C#, 16#38210010#, 16#2C0C0000#, 16#41820008#, 16#00000000#,
      16#4E800020#,  16#4D504348#, 16#37467853#, 16#4C333230#);

   pragma Export (Assembly, Set_Lock_Store,
                    "mpc5554__h4f_driver__Set_lock");

end MPC5554.H7F_Driver;
