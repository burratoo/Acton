ENTRY(_start);

MEMORY
{
  flash (rx)  : ORIGIN = 0x08000000, LENGTH = 1024K
  sram (rwx)  : ORIGIN = 0x20000000, LENGTH = 112K
}

/*
 * +---------------------------+
 * |                           | Interrupts
 * | .arm_vector_table         | > vector_ram
 * +---------------------------+
 * | .text                     | Executable code
 * | .glue                     |
 * | .init                     |
 * | .rodata                   |
 * | .ARM.extab                |
 * | .ARM.exidx                |
 * |                           | > int_flash
 * +---------------------------+
 * | __ramtext_ram_start__ = . | Global/static data
 * | .oram                     |
 * | __ramtext_ram_end__ = .   |
 * | __data_ram_start__ = .    |
 * | .data                     |
 * | __data_ram_end__ = .      |
 * |                           | > int_sram
 * +---------------------------+
 * | __bss_start__ = .         |
 * | .bss                      |
 * | __bss_end__ = .           |
 * | _end = .                  | Start of heap, used by sbrk()
 * | (_end=__heap_start)       |
 * | end = _end                |
 * | __end = _end              |
 * |                           | > int_sram
 * +---------------------------+
 * .                           .
 * . __stack_space_end         .
 * .                           .
 * | __stack_space_start       | Top of stack
 * +---------------------------+
 */
 
SECTIONS {
  /* Note that the vector table needs to be aligned to the power of two that is
   * greater than or equal to the number of words in the table. The STM32F4
   * has 98 entries which means an alignment of 512 bytes is required.
   */
   
  .arm_vector_table : {
    KEEP(*(.arm_vector_table))
    . = ALIGN(512); 
  } > flash
   
  .text : {
    *(.text .text.* .gnu.linkonce.t*)
    *(.gnu.warning)
  } > flash

  .rodata :
  {
    *(.lit)
    *(.rodata .rodata.* .gnu.linkonce.r*)
    . = ALIGN(0x4);
    __rom_end = .;
  } > flash

  __data_load = __rom_end;
  .data : AT (__rom_end)
  {
    __data_start = .;
    *(.data .data.* .gnu.linkonce.d*)

    /* Align in 4 byte so the copy function can copy words */

    . = ALIGN(0x4);
    __data_end = .;
  } > sram
  __data_size = SIZEOF (.data) >> 2;

  .bss (NOLOAD): {
   . = ALIGN(0x8);
   __bss_start = .;

   *(.bss .bss.*)
   *(COMMON)

    /* Align in 4 byte so the zero function can use words */

   . = ALIGN(0x4); 
   __bss_end = .;

  } > sram

  __bss_size = SIZEOF (.bss) >> 2;

  __stack_space_end = __bss_end ;
  __stack_space_start = (ORIGIN(sram) + LENGTH(sram));


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
  .debug_info     0 : { *(.debug_info .gnu.linkonce.wi.*) }
  .debug_abbrev   0 : { *(.debug_abbrev) }
  .debug_line     0 : { *(.debug_line) }
  .debug_frame    0 : { *(.debug_frame) }
  .debug_str      0 : { *(.debug_str) }
  .debug_loc      0 : { *(.debug_loc) }
  .debug_macinfo  0 : { *(.debug_macinfo) }
  /* DWARF 3 */
  .debug_pubtypes 0 : { *(.debug_pubtypes) }
  .debug_ranges   0 : { *(.debug_ranges) }
  .gnu.attributes 0 : { KEEP (*(.gnu.attributes)) }
  /DISCARD/ : { *(.note.GNU-stack) *(.gnu_debuglink) *(.gnu.lto_*) }

  /DISCARD/ : { *(.ARM.extab .ARM.exidx) }
}