OUTPUT_FORMAT("elf32-powerpc")
OUTPUT_ARCH(powerpc)
ENTRY(__start)

MEMORY
{
	/* MPC5554 2M Internal Flash */
	rchw (ax) 		: ORIGIN = 0x00000000, LENGTH = 0x00000008
	internal_flash (rwx)	: ORIGIN = 0x00000008, LENGTH = 0x001FFFF8

	/* PhyCore 2M External Flash */
	external_flash	(rwx)	: ORIGIN = 0x20000000, LENGTH = 2M

	/* MPC5554 64K Internal SRAM */
	internal_sram (rw)	: ORIGIN = 0x40000000, LENGTH = 60K
	stack_ram		: ORIGIN = 0x4000F000, LENGTH = 4K

	/* PhyCore 512K External SRAM */
	external_sram (rw)	: ORIGIN = 0x3ff80000, LENGTH = 512K
}

__SP_INIT = 0x4000F000;

SECTIONS
{
	/* Internal Flash data */
	.rchw		: { *(.rchw) } > rchw
	.text		: {
		*(.text)
		*(.text.*)
			   } > internal_flash
	.flash_data	: {} > internal_flash
	.rodata		: {
		*(.rodata)
		*(.rodata.*)
			   } > internal_flash

	/* RAM Data */
	.data		: {
		*(.data)
		*(.data.*)
			   } > internal_sram

	_SDA_BASE_ = .;
	.sdata		: {
		*(.sdata)
		*(.sdata.*)
			   } > internal_sram
	.sbss		: {
		*(.sbss)
		*(.sbss.*)
			   } > internal_sram

	_SDA2_BASE_ = .;
	.sdata2		: {} > internal_sram
	.sbss2		: {} > internal_sram

	.bss		: {
		*(.bss)
		*(.bss.*)
			   } > internal_sram

	/DISCARD/ :
  	{ *(.eh_frame) }

}


