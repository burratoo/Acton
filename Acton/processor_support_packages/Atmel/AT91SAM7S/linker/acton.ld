ENTRY(_start)
/* SAM7 linker script source.
 *  This file must be run through a sed script to
 *  generate an ldscript suitable for either a ram or rom image.
 *
 *  The sed script achieves this by selectively copying the lines
 *   with ROM_ONLY and RAM_ONLY.
 *
 *  This script is for a system that runs with the memory 
 *  mapped  so that ram is visible at both 0 and 2M.
 *
 *  Notes on alignment:
 *  1) Sections should be 4-byte aligned otherwise ARM fetches
 *     will be misaligned.
 *  2) The stack should be 8-byte aligned for the APCS. That's because STRD
 *     and LDRD assume that they are on 8-byte boundaries.
 *  3) Align 16 is a good idea for any copied or initialised sections because
 *     this allows the faster LDM/STM code to run instead.
 */


/* 
 * Memory definitions.
 */

/* SAM7S256 */


MEMORY
{
   flash      : ORIGIN = 0x00100000, LENGTH = 256k
   vector_ram : ORIGIN = 2M, LENGTH = 64
   ram : ORIGIN = 2M + 64, LENGTH = 64K - 64
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
 
RAM_BASE = 2M;
RAM_SIZE = 64k;

SECTIONS {

/* The vectors are at 0, but since this is overlapped with 
 * the ram area we need to reserve some space for the vector
 * table
 */
.arm_vector_table : {
    KEEP(*(.arm_vector_table))
    . = ALIGN(16);
} > vector_ram
 AT > flash

__vectors_ram_start__  = ADDR(.arm_vector_table);
__vectors_ram_end__    = (ADDR(.arm_vector_table) + SIZEOF(.arm_vector_table));
__vectors_load_start__ = LOADADDR(.arm_vector_table);
__vectors_load_end__ = (LOADADDR(.arm_vector_table) + SIZEOF(.arm_vector_table));

/* .text is the main program area. This is stored in flash or
 * RAM depending on the build type.
 */

.text : {
    . = ALIGN(16);
    * (*.text *.text.* *.glue*)
    . = ALIGN(16);
    * (*.rodata *.rodata.*)
    . = ALIGN(16);
}
 > flash

  .ARM.extab   : { *(.ARM.extab* .gnu.linkonce.armextab.*) }
 > flash
   PROVIDE_HIDDEN (__exidx_start = .);

  .ARM.exidx   : { *(.ARM.exidx* .gnu.linkonce.armexidx.*) }
 > flash
   PROVIDE_HIDDEN (__exidx_end = .);

/*
 * data is initialised data. This has two addresses:
 * VMA: run-time address
 * LMA: load address.
 * For a ram build, these addresses are the same and since the data 
 * is loaded into the correct address there is no need to copy it.
 * For a rom build, the LMA is where the data is stored in rom and
 * the init code copies it into ram for run-time.
 *
 * NB LOADADDR is the same as ADDR if the VMA and LMA are the same.
 */

/*
 * .ram_text is code that is relocated to RAM for execution. 
 * Typical usage is flash programming code that needs to be in RAM while the
 * flash is busy.
 * NB We copy all program (text) and rodata into the RAM.
 */


.ram_text : {
    . = ALIGN(16);
    *.oram(*.text *.text.* *.glue*)
    . = ALIGN(16);
    *.oram(*.rodata *.rodata.*)
    . = ALIGN(16);
} > ram
  AT > flash

__ramtext_ram_start__ = ADDR(.ram_text);
__ramtext_ram_end__   = ADDR(.ram_text) + SIZEOF(.ram_text);
__ramtext_load_start__ = LOADADDR(.ram_text); 
__ramtext_load_end__ = __ramtext_load_start__ + SIZEOF(.ram_text) ;

.data : {
    * (*.data *.data.*)
    . = ALIGN(16);
} > ram 
  AT > flash

/* LOADADDR is the same as the ADDR if it is a ram build */
__data_ram_start__ = ADDR(.data);
__data_ram_end__   = ADDR(.data) + SIZEOF(.data);
__data_load_start__ = LOADADDR(.data); 
__data_load_end__ = __data_load_start__ + SIZEOF(.data) ;

/* bss is zero initialised data */
.bss : {
    * (*.bss *.bss.*)
    . = ALIGN(16);
    __bss_end__ = . ;
} > ram

__bss_start__ = ADDR(.bss);
__bss_end__   = (ADDR(.bss) + SIZEOF(.bss));


/* end = __bss_end__ ;  used by malloc */

__stack_space_end = __bss_end__ ;
__stack_space_start = (RAM_BASE + RAM_SIZE);
}
