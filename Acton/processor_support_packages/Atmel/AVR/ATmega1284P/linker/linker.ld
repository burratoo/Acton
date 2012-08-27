OUTPUT_FORMAT("elf32-powerpc")
OUTPUT_ARCH(powerpc)
ENTRY(_start)

MEMORY
{
  iram (rw)  : ORIGIN = 0x40000000, LENGTH = 64K
  sram (rwx) : ORIGIN = 0x00000008, LENGTH = 2M
  rchw 	: ORIGIN = 0x00000000, LENGTH = 0x00000008
}

SECTIONS
{
  .rchw	: {*(.rchw)} > rchw
  /* Read-only sections, merged into text segment */
  .text ORIGIN(sram) :
  {
    *(.text)
    *(.text.*)
  } > sram
  PROVIDE (__etext = .);
  .rodata   :
  {
    *(.rodata)
    *(.rodata.*)
  } > sram

  . = ALIGN(4096);
  .data :
  {
    *(.data)
    *(.data.*)
    SORT(CONSTRUCTORS)
  } > sram

  . = ALIGN(4);
  .sdata :
  {
    PROVIDE ( _SDA_BASE_ = . + 32768);
    *(.sdata)
    *(.sdata.*)
  } > sram
  . = ALIGN(4);

  /* noninitialized small data; will be zeroed by crt0 */
  __bss_start__ = .;
  .sbss       :
  {
   *(.sbss)
   *(.sbss.*)
   *(COMMON)
   . = ALIGN(4);
  } > sram
  . = ALIGN(4);
  __sbss_end__ = . ;


  /* noninitialized data; will be zeroed by crt0*/
  .bss       :
  {
   *(.bss)
   *(.bss.*)
   *(COMMON)
   . = ALIGN(4);
  } > sram
  . = ALIGN(4);
  __bss_end__ = . ;

  PROVIDE (end = .);

  .iram :
  {
    *(.iram)
  } > iram

  /DISCARD/ :
  {
    *(.eh_frame)
  }
}


