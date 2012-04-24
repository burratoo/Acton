ENTRY(__start)
/*
 *****************************************************************
 * PE_MPC5554_rom.ld
 * GNU powerpc-eabispe Linker Script for the MPC5554
 * By default, this application runs in internal flash, SRAM, and cache
 * c. 2005, P&E Microcomputer Systems, Inc.
 * REV      AUTHOR       DATE       DESCRIPTION OF CHANGE
 * ---   -----------   ----------   ---------------------
 * 0.1   C.Baker FSL   19/Jul/06    Changed memory layout, stack
 *                                  variables, and filename.
 * 0.2   C.Baker FSL   21/Sep/06    Changed stack in cache address.
 * 0.3   C.Baker FSL   22/Jan/07    Aligned heap on 16byte boundary.
 *****************************************************************
 */


MEMORY
{
/* 2M External Flash (unused) */
/* 512K External SRAM (unused) */
        ext_flash : org = 0x20000000, len = 0x00200000
        ext_ram   : org = 0x3ff80000, len = 0x80000

/* Internal Flash RCW */
/* MPC5554 2M Internal Flash, but subtract one 128K block for use by BAM. */
/* MPC5554 64K Internal SRAM */
        flash_rcw        : org = 0x00000000, len = 0x00010000
        interrupts_flash : org = 0x00010000, len = 0x00020000
        int_flash        : org = 0x00020000, len = 0x001E0000
        int_sram         : org = 0x40000000, len = 0x10000
}

/* MPC5554 4K of internal cache used for stack. */
/* Stack Address Parameters */
__SP_END     = 0x40000000;
__STACK_SIZE = 0x10000;
__SP_INIT    = __SP_END + __STACK_SIZE;

/*
Optionally define this variable with the address of heap
__heap_start   =
*/

/*************************************************************************
 *************************************************************************/

/*
 * The P&E default memory map looks like this:
 *
 * +-------------------------+ <- low memory
 * | .rcw                    | Reset Config Half Word and PC value
 * |                         | > flash_rcw
 * +-------------------------+
 * | .acton_intr_branch_table| Interrupts
 * | .intc_vector_table      | > interrupts_flash
 * +-------------------------+
 * | .pecrt0                 | Executable code
 * | .interp                 |
 * | .hash                   |
 * | .dynsym                 |
 * | .dynstr                 |
 * | .text                   |
 * | .init                   |
 * | .fini                   |
 * | .flash_data             |
 * | .rodata                 |
 * | .rodata1                |
 * | .PPC.EMB.apuinfo        |
 * | .intc_vector_table      |
 * | .xcptn                  |
 * | etext = .               |
 * | _etext = .              |
 * | __etext = .             |
 * .						 .
 * .						 .
 * | .acton_def_ext_handler  | > int_flash
 * +-------------------------+
 * | __DATA_ROM = .          | Global/static data
 * | .PPC.EMB.sdata2         |
 * | _SDA2_BASE_ = .         |
 * | __SDATA2_START__ = .    |
 * | .sdata2                 |
 * | .PPC.EMB.sbss2          |
 * | .sbss2                  |
 * | __SBSS2_END__ = .       |
 * | .data                   |
 * | .data1                  |
 * | _GOT_START_ = .         |
 * | .got                    |
 * | .got.plt                |
 * | _GOT_END_ = .           |
 * | .got1                   |
 * | _GOT2_START_ = .        |
 * | .got2                   |
 * | __CTOR_LIST__ = .       |
 * | .ctors                  |
 * | __CTOR_END__ = .        |
 * | __DTOR_LIST__ = .       |
 * | .dtors                  |
 * | __DTOR_END__ = .        |
 * | _FIXUP_START_ = .       |
 * | .fixup                  |
 * | _FIXUP_END_ = .         |
 * | _GOT2_END_ = .          |
 * | .dynamic                |
 * | .plt                    |
 * | __SDATA_ROM = .         |
 * | .sdata                  |
 * | __SDATA_START__ = .     |
 * | _SDA_BASE_ = .          |
 * | .PPC.EMB.sdata0         |
 * | __SDATA_END__ = .       |
 * | __DATA_ROM_END = .      |
 * | edata = .               |
 * | _edata  =  .            |
 * | __edata = .             |
 * |                         | > int_sram
 * +-------------------------+
 * | .sbss                   |
 * | __sbss_start = .        |
 * | ___sbss_start = .       |
 * | .PPC.EMB.sbss0          |
 * | __sbss_end = .          |
 * | ___sbss_end = .         |
 * | __SBSS_END__ = .        |
 * | .bss                    |
 * | __bss_start = .         |
 * | ___bss_start = .        |
 * | __bss_end = .           |
 * | _end = .                | Start of heap, used by sbrk()
 * | (_end=__heap_start)     |
 * | end = _end              |
 * | __end = _end            |
 * |                         | > int_sram
 * +-------------------------+
 * .                         .
 * . __SP_END                .
 * .                         .
 * | __SP_INIT               | Top of stack
 * +-------------------------+
 */

/****************************************************************/

SECTIONS
{

  .rcw :
  {
    *(.rcw)
  } > flash_rcw

  .acton_intr_branch_table :
  {
    *(.acton_intr_branch_table)
  } > interrupts_flash

  /* INTC for software vector mode */
  .intc_vector_table ALIGN (4096):
  {
    *(.intc_hw_branch_table)
  } > interrupts_flash

  /* CRT0 startup code */
  .pecrt0 ALIGN(4) :
  {
    *(.pecrt0)
    PEFILL = .;
  } > int_flash

  .interp  : { *(.interp) } > int_flash

  .hash : { *(.hash) } > int_flash

  .dynsym : { *(.dynsym) } > int_flash

  .dynstr : { *(.dynstr) } > int_flash

  .text :
  {
    *(.text)
    *(.text.*)
    *(.gnu.warning)
    *(.gnu.linkonce.t*)
	__eabi = (.); /*PE*/
	LONG(0x4E800020);
  } > int_flash

  .init :
  {
    PROVIDE (__init = .);
    *(.init)
  } > int_flash

  .fini :
  {
    /*PROVIDE (__fini = .);*/
    *(.fini)
  } > int_flash

  .flash_data :
  {
   *(.flash_data)
  } > int_flash

  .rodata :
  {
    *(.rodata)
    *(.rodata.*)
    *(.gnu.linkonce.r*)
  } > int_flash

  .rodata1 :
  {
    *(.rodata1)
  } > int_flash

  .PPC.EMB.apuinfo :
  {
    *(.PPC.EMB.apuinfo)
  } > int_flash

  /* IVOR4Handler */
  .xcptn :
  {
   *(.xcptn)
  } > int_flash

  etext = .;
  _etext = .;
  __etext = .;

  /******************************************************************/

  NEXT_LOAD_ADDR = . ;
  __DATA_ROM = . ;

  .PPC.EMB.sdata2 : AT (NEXT_LOAD_ADDR)
  {
    _SDA2_BASE_ = .;
    __SDATA2_START__ = .;
    *(.PPC.EMB.sdata2)
  } > int_sram

  NEXT_LOAD_ADDR = NEXT_LOAD_ADDR + SIZEOF(.PPC.EMB.sdata2);

  .sdata2 : AT (NEXT_LOAD_ADDR)
  {
    *(.sdata2)
  } > int_sram

  NEXT_LOAD_ADDR = NEXT_LOAD_ADDR + SIZEOF(.sdata2);

  .PPC.EMB.sbss2 : AT (NEXT_LOAD_ADDR)
  {
    *(.PPC.EMB.sbss2)
  } > int_sram

  NEXT_LOAD_ADDR = NEXT_LOAD_ADDR + SIZEOF(.PPC.EMB.sbss2);

  .sbss2 : AT (NEXT_LOAD_ADDR)
  {
    *(.sbss2)
    __SBSS2_END__ = .;
  } > int_sram

  NEXT_LOAD_ADDR = NEXT_LOAD_ADDR + SIZEOF(.sbss2);

  .data : AT (NEXT_LOAD_ADDR)
  {
    *(.data)
    *(.data.*)
    *(.gnu.linkonce.d*)
    CONSTRUCTORS
  } > int_sram

  NEXT_LOAD_ADDR = NEXT_LOAD_ADDR + SIZEOF(.data);

  .data1 : AT (NEXT_LOAD_ADDR)
  {
    *(.data1)
  } > int_sram

  NEXT_LOAD_ADDR = NEXT_LOAD_ADDR + SIZEOF(.data1);

  PROVIDE (_GOT_START_ = .);

  .got : AT (NEXT_LOAD_ADDR)
  {
    *(.got)
  } > int_sram

  NEXT_LOAD_ADDR = NEXT_LOAD_ADDR + SIZEOF(.got);

  .got.plt : AT (NEXT_LOAD_ADDR)
  {
    *(.got.plt)
  } > int_sram

  PROVIDE (_GOT_END_ = .);

  NEXT_LOAD_ADDR = NEXT_LOAD_ADDR + SIZEOF(.got.plt);

  .got1 : AT (NEXT_LOAD_ADDR)
  {
    *(.got1)
  } > int_sram

  NEXT_LOAD_ADDR = NEXT_LOAD_ADDR + SIZEOF(.got1);

  PROVIDE (_GOT2_START_ = .);

  .got2 : AT (NEXT_LOAD_ADDR)
  {
    *(.got2)
  } > int_sram

  NEXT_LOAD_ADDR = NEXT_LOAD_ADDR + SIZEOF(.got2);

  PROVIDE (__CTOR_LIST__ = .);

  .ctors : AT (NEXT_LOAD_ADDR)
  {
    /*KEEP (*crtbegin.o(.ctors))
    KEEP (*(EXCLUDE_FILE (*crtend.o) .ctors))
    KEEP (*(SORT(.ctors.*)))*/
    KEEP (*(.ctors))
  } > int_sram

  NEXT_LOAD_ADDR = NEXT_LOAD_ADDR + SIZEOF(.ctors);

  PROVIDE (__CTOR_END__ = .);
  PROVIDE (__DTOR_LIST__ = .);

  .dtors : AT (NEXT_LOAD_ADDR)
  {
    /*KEEP (*crtbegin.o(.dtors))
    KEEP (*(EXCLUDE_FILE (*crtend.o) .dtors))
    KEEP (*(SORT(.dtors.*)))*/
    KEEP (*(.dtors))
  } > int_sram

  NEXT_LOAD_ADDR = NEXT_LOAD_ADDR + SIZEOF(.dtors);

  PROVIDE (__DTOR_END__ = .);
  PROVIDE (_FIXUP_START_ = .);

  .fixup : AT (NEXT_LOAD_ADDR)
  {
    *(.fixup)
  } > int_sram

  NEXT_LOAD_ADDR = NEXT_LOAD_ADDR + SIZEOF(.fixup);

  PROVIDE (_FIXUP_END_ = .);
  PROVIDE (_GOT2_END_ = .);

  .dynamic : AT (NEXT_LOAD_ADDR)
  {
    *(.dynamic)
  } > int_sram

  NEXT_LOAD_ADDR = NEXT_LOAD_ADDR + SIZEOF(.dynamic);

  .plt : AT (NEXT_LOAD_ADDR)
  {
    *(.plt)
  } > int_sram

  NEXT_LOAD_ADDR = NEXT_LOAD_ADDR + SIZEOF(.plt);

  /* We want the small data sections together, so single-instruction offsets
     can access them all, and initialized data all before uninitialized, so
     we can shorten the on-disk segment size.  */

  __SDATA_ROM = .;

  .sdata : AT (NEXT_LOAD_ADDR)
  {
    __SDATA_START__ = .;
    _SDA_BASE_ = .;
    *(.sdata)
  } > int_sram

  NEXT_LOAD_ADDR = NEXT_LOAD_ADDR + SIZEOF(.sdata);

  .PPC.EMB.sdata0 : AT (NEXT_LOAD_ADDR)
  {
    __SDATA_END__ = .;
  } > int_sram

  __DATA_ROM_END = .;
  edata = .;
  _edata  =  .;
  __edata = .;

  /******************************************************************/

  .sbss BLOCK (4):
  {
    PROVIDE (__sbss_start = .);
    PROVIDE (___sbss_start = .);
    *(.sbss)
    *(.scommon)
    *(.dynsbss)
  } > int_sram

  .PPC.EMB.sbss0 BLOCK (4):
  {
    *(.PPC.EMB.sbss0)
    PROVIDE (__sbss_end = .);
    PROVIDE (___sbss_end = .);
    PROVIDE (__SBSS_END__ = .);
  } > int_sram

  .bss BLOCK (4):
  {
   PROVIDE (__bss_start = .);
   PROVIDE (___bss_start = .);
   *(.dynbss)
   *(.bss)
   *(COMMON)
   PROVIDE (__bss_end = .);
  } > int_sram

  /******************************************************************/

  /*
  Heap grows from lower to higer addresses
  Stack grows from higer to lower addresses
  */

  /* Define position of heap */
  /* Default to location contiguous with .bss section in RAM */
  _end = DEFINED (__heap_start) ? __heap_start : ALIGN(0x10);
  PROVIDE(end = _end);
  PROVIDE(__end = _end);

  /******************************************************************/

  .gcc_except_table : {*(.gcc_except_table)}

  /* These are needed for ELF backends which have not yet been
     converted to the new style linker.  */
  .stab 0 : { *(.stab) }
  .stabstr 0 : { *(.stabstr) }
  /* DWARF debug sections.
     Symbols in the DWARF debugging sections are relative to the beginning
     of the section so we begin them at 0.  */
  /* DWARF 1 */
  .debug          0 : { *(.debug) }
  .line           0 : { *(.line) }
  /* GNU DWARF 1 extensions */
  .debug_srcinfo  0 : { *(.debug_srcinfo) }
  .debug_sfnames  0 : { *(.debug_sfnames) }
  /* DWARF 1.1 and DWARF 2 */
  .debug_aranges  0 : { *(.debug_aranges) }
  .debug_pubnames 0 : { *(.debug_pubnames) }
  /* DWARF 2 */
  .debug_info     0 : { *(.debug_info) }
  .debug_abbrev   0 : { *(.debug_abbrev) }
  .debug_line     0 : { *(.debug_line) }
  .debug_frame    0 : { *(.debug_frame) }
  .debug_str      0 : { *(.debug_str) }
  .debug_loc      0 : { *(.debug_loc) }
  .debug_macinfo  0 : { *(.debug_macinfo) }
  /* SGI/MIPS DWARF 2 extensions */
  .debug_weaknames 0 : { *(.debug_weaknames) }
  .debug_funcnames 0 : { *(.debug_funcnames) }
  .debug_typenames 0 : { *(.debug_typenames) }
  .debug_varnames  0 : { *(.debug_varnames) }
  /* */
  .eh_frame         : { *(.eh_frame) } > int_flash

  /*****************************************************************/
  .acton_def_ext_handler (ORIGIN(int_flash) + LENGTH(int_flash) - 4) : 
  {
    *(.acton_def_ext_handler)
  } > int_flash

 /DISCARD/ :
  	{ *(.jcr) }

  /******************************************************************/

__SRAM_CPY_START = ADDR(.PPC.EMB.sdata2);
__IVPV_ADDR     = 0x00010000;/* ADDR(.intc_hw_branch_table);*/
__SRAM_LOAD      = (_end);
__SRAM_LOAD_SIZE = (SIZEOF(.flash_data) / 4);

TEMPSIZE = SIZEOF(.PPC.EMB.sdata2)+SIZEOF(.sdata2)+SIZEOF(.PPC.EMB.sbss2)+SIZEOF(.sbss2)+SIZEOF(.data)+SIZEOF(.data1);
TEMPSIZE = TEMPSIZE + SIZEOF(.got)+SIZEOF(.got.plt)+SIZEOF(.got1)+SIZEOF(.got2)+SIZEOF(.ctors);
TEMPSIZE = TEMPSIZE + SIZEOF(.dtors)+SIZEOF(.fixup)+SIZEOF(.dynamic)+SIZEOF(.plt);
TEMPSIZE = TEMPSIZE + SIZEOF(.sdata)+SIZEOF(.PPC.EMB.sdata0);
__ROM_COPY_SIZE  = (TEMPSIZE);

}

  /******************************************************************/
