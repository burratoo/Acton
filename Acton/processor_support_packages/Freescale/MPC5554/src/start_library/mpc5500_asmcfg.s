#**************************************************************************/
#* FILE NAME: mpc5500_asmcfg.s            COPYRIGHT (c) Freescale 2004    */
#*                                                All Rights Reserved     */
#* DESCRIPTION:                                                           */
#* This file contains functions for the MPC5500 assembly configuration.   */
#=========================================================================*/
#*                                                                        */
#* REV      AUTHOR       DATE       DESCRIPTION OF CHANGE                 */
#* ---   -----------   ----------   ---------------------                 */
#* 0.1   G. Jackson    26/Mar/04    Initial version                       */
#* 0.2   G. Jackson    29/Apr/04    Made compiler names unique for        */
#*                                    assembly configuration.             */
#*                                  Single designation for rcw values.    */
#* 0.3   G. Jackson    13/May/04    Changed definition of FMPLL_SYNCR     */
#*                                    register settings.                  */
#* 0.4   G. Jackson    15/May/04    Removed msync and isync from tlbwe    */
#*                                    commands.                           */
#* 0.5   G. Jackson    25/May/04    Changed __OPCOUNT to __SRAM_LOAD_SIZE */
#*                                  Changed __SRAM_OPCODE to __SRAM_LOAD  */
#*                                  Changed cfg_OPCODE to cfg_SRAM_LOAD   */
#*                                  Changed OPCNT_OFFSET to IP_ADVANCE    */
#* 0.6   G. Jackson    12/Jun/04    Changed TLB entries to work with      */
#*                                  MPC5554 Rev. 0.3 and later for the    */
#*                                  BAM, PBRIDGE_B, and Internal FLASH.   */
#* 0.7   G. Jackson    30/Jun/04    Added entries for RCHW (RCHW_VAL)     */
#* 0.8   G. Jackson    05/Aug/04    Added cfg_PNTRS for R13 and R2        */
#* 0.9   G. Jackson    18/Aug/04    Added cfg_ROMCPY for .data and .sdata */
#* 0.91  G. Jackson    20/Sep/04    cfg_ROMCPY changed to load by bytes.  */
#* 0.92  G. Jackson    11/Oct/04    L1CSR0 checks added for complete      */
#*                                    cache operation.                    */
#* 1.0   G. Jackson    12/Oct/04    Green Hills now does not require      */
#*                                    quotation marks around the section  */
#*                                  Added syntax to generate symbols for  */
#*                                    debug.                              */
#* 1.1   C. Baker      19/Jul/06    Added functions PLL_delay,            */
#*                                    cfg_FMPLL80, cfg_FMPLL132 and       */
#*                                    cfg_FMPLL128_40                     */
#*                                  Added cache check, inhibit and        */
#*                                    uninhibit code to cfg_FMPLL         */
#*                                  Changed cmpli to use full notation    */
#* 1.2   C. Baker      25/Oct/06    Added MPC5561 and MPC5566 cfg_FMPLL   */
#*                                    options                             */
#*                                  Added ABORT check to cache invalidate */
#*                                    to cfg_cache                        */
#*                                  Changed cache inhibit in cfg_FMPLL to */
#*                                    perform a TLB search                */
#*                                  Removed l1csr0 capitalisation         */
#*                                  Fixed and added comments              */
#* 1.3   C. Baker      10/Jan/07    Fixed comments                        */
#**************************************************************************/

#    .ifeq __MPC5500_USERDEFS__
	    .include "../src/start_library/mpc5500_usrdefs.inc"
#   .endif

    .globl cfg_CACHE, cfg_FMPLL, cfg_SRAM
    .globl cfg_FLASH, cfg_MMU, cfg_STACK, cfg_PNTRS, cfg_ROMCPY
    .extern __start          # Primary program entry point defined in __start.s
    .extern __STACK_SIZE     # Defined in the linker file
    .extern __SP_INIT        # Defined in the linker file
    .extern __SP_END         # Defined in the linker file
    .extern __SRAM_LOAD      # Defined in the linker file
    .extern __SRAM_LOAD_SIZE # Defined in the linker file
    .extern _SDA_BASE_       # Defined by the linker EABI
    .extern _SDA2_BASE_      # Defined by the linker EABI
    .extern cfg_SRAM_LOAD    # Defined in mpc5500_SRAM_load.s
    .extern __DATA_ROM       # Defined in the linker file
#                            #   Address of initialized data in Flash
    .extern __ROM_COPY_SIZE  # Defined in the linker file
#                            #   Size of Flash area to be copied to SRAM
    .extern __SRAM_CPY_START # Defined in the linker file
#                            #   Address of ".data" section in SRAM

# set up separate section for the reset configuration half word and vector.
#       This is the start of the .rcw section.
    .if __PEGNU__ | __GNAT__
    .section ".rcw","ax" # The "ax" is required to generate "non-text" code
    .else                # __CWWRKS__ | __DIABCC__ | __GRNHS__
    .section .rcw
    .endif

    .equ RCHW_VAL, (RCHW_WTE | RCHW_PS0 | BOOT_ID)

    .LONG RCHW_VAL
    .LONG __start

#       This is the end of the .rcw section.
#################################################
#       This is the start of the .init section.

    .if __PEGNU__ | __GNAT__
    .section ".init","ax" # The "ax" is required to generate "non-text" code
    .endif

    .if __GRNHS__
    .section .init,ax     # The "ax" generates symbols for debug
    .endif

    .if __DIABCC__
    .section .init,c      # The "c" generates symbols for debug
    .endif

    .if __CWWRKS__
    .section .init,text   # The "text" generates symbols for debug
    .endif


# Constants that should be defined follow:
# Defined assembler input parameters should allready understand:
#    .equ MAS0,624
#    .equ MAS1,625
#    .equ MAS2,626
#    .equ MAS3,627

# init_L2RAM requires the base address and the size of the SRAM.
#    INT_SRAM_BASE, INT_SRAM_SIZE.
# Defined in linker file.

# Cache definitions:
# cfg_CACHE and init_STACK CACHE Settings
#     __STACK_SIZE, __SP_INIT, __SP_END, (defined in linker file)
#    .equ CACHE_CLEAR,(CLFC_NO_OP | CINV_INV_OP | CE_DISABLE)
#    .equ CACHE_SETTINGS, (CWM_COPYBCK | DPB_NOPUSH | CE_ENABLE)


#***********************************************************************************/
# FUNCTION     : cfg_mpc5500as                                                     */
# PURPOSE      : This function provides an example for the overall                 */
#                 configuration for an mpc5500 device.                             */
#                                                                                  */
#                 In cases where (numerous) alternative initializations could      */
#                 occur, a sample configuration is included here. Actual           */
#                 initialization required for a specific system remains the        */
#                 responsibility of the developer.                                 */
# INPUT NOTES  : None                                                              */
# RETURN NOTES : None                                                              */
# WARNING      : None                                                              */
#***********************************************************************************/
#                                                                                  */
# MPC5500 initialization functions in order of call from __start.s                 */
#cfg_mpc5500as:                                                                    */
#                                                                                  */
#   The assembly actions are called in the order seen below.                       */
#   They are called from __start in the file named __start.s                       */
#    bl cfg_CACHE    # Configure cache.                                            */
#                                                                                  */
#    bl cfg_FMPLL    # Configure the Frequency Modulated Phase Locked Loop         */
#                                                                                  */
#    bl cfg_SRAM     # Write to all SRAM locations for ECC functionality.          */
#                                                                                  */
#    bl cfg_FLASH    # Reduce Wait States for Internal and External Flash          */
#                                                                                  */
#    bl cfg_MMU      # Set up the TLB tables with the MMU registers.               */
#                                                                                  */
#    bl cfg_STACK    # Set up the Stack in the Cache                               */
#                                                                                  */
#    bl cfg_PNTRS    # Set up the small data register pointers in R2 and R13       */
#                                                                                  */
#    bl cfg_ROMCPY   # Copy initialized data from FLASH ROM to SRAM                */
#                                                                                  */
#    blr             # End of cfg_mpc5500as                                        */
#***********************************************************************************/

#************************************************************************/
#                       CALLED SEQUENCES                                */
#************************************************************************/

#**************************************************************************/
# FUNCTION     : cfg_CACHE                                                */
# PURPOSE      : This function initializes the CACHE by invalidating and  */
#                  then enabling the cache.                               */
# INPUT NOTES  : CACHE_CLEAR, CACHE_SETTINGS, L1CSR0                      */
# RETURN NOTES : None                                                     */
# WARNING      : Registers used: R5,R7,R8,R9,R10,R11                      */
#**************************************************************************/

cfg_CACHE:

# To activate cache invalidate operation,
# place a "1" in the CINV bit location.  (L1CSR0[30])
#  This operation takes 134 cycles to complete
    lis   r5, CACHE_CLEAR@h			# Load upper L1CSR0 (0x0) into R5
    ori   r5, r5, CACHE_CLEAR@l		# Load lower L1CSR0 (CINV bit) into R5
    mtspr l1csr0,r5					# Move R5 to the L1CSR0 (SPR 1010)register.
#									# This should start the CACHE invalidate
#									#  operation.
# Make sure that CINV is not active/finished
label_CINV_check:					# Make sure that CINV is not active/finished
# The CINV mask bit will be compared to L1CSR0[30] CINV bit
    lis   r8, 0x0000				# Load upper CINV mask (0x0000) into R8
    ori   r8, r8, 0x0002			# Load lower L1CSR0[30] CINV mask bit into R8
	lis   r7, 0x0000
	ori   r7, r7, 0x0004			# Load L1CSR0[29] CABT mask into R7
	lis   r11, 0xFFFF
	ori   r11, r11, 0xFFFB			# Load L1CSR0[29] CABT clear mask into R11
CHECK_CINV:
    mfspr r9, l1csr0				# Move the L1CSR0 register value to R9.

# check for an ABORT of the cache invalidate operation
	and.  r10, r7, r9				# Check if the invalidate was aborted
	beq   NO_ABORT					# CABT = 0
	and.  r10, r11, r9				# CABT = 1, mask L1CSR0[CABT] in R10
	mtspr l1csr0, r10				# Clear L1CSR0[CABT]
	b     cfg_CACHE					# rerun the invalidate operation
NO_ABORT:

# "AND" the two registers together to check for active CINV bit
    and.  r10, r8, r9
#                                  # The "." after "and" activates the condition register
    bne CHECK_CINV                 # Branch if not zero. CINV still=1.

# Enable cache
    mfspr r5, l1csr0               # Retrieve the L1CSR0 register value
    oris  r5, r5, CACHE_SETTINGS@h # OR in CWM and DPB in upper L1CSR0[11:12]
    ori   r5, r5, CACHE_SETTINGS@l # OR in cache enable bit L1CSR0[31]
    isync                          # Required before changing the CE bit to
    msync                          #  prevent disable/enable cache during access
    mtspr l1csr0, r5               # Return value to L1CSR0 to enable the cache

    blr
# End of cfg_CACHE


#************************************************************************/
# FUNCTION     : PLL_delay                                              */
# PURPOSE      : This function waits till the FMPLL locks.              */
# INPUT NOTES  : FMPLL_SYNSRREG, SIU_SRCRREG, SIU_SRCR_SYSRST,          */
#                ERRLOGREG, ERRLOG_MFDERR,                              */
# RETURN NOTES : None                                                   */
# WARNING      : Registers used: R6,R8,R9,R10                           */
#************************************************************************/

PLL_delay:

#*******************************************
# Check for loss-of-lock after MFD change
# Wait for PLL to lock. A timeout will occur after 6000 clocks.
# Set up counter
    lis   r6, 0x0000                  # Load counter value into R6
    ori   r6, r6, 0x1770              # 6000 = 0x1770 for time out clocks
    mtctr r6                          # Move 0x1770 to the counter
# Load the lock bit mask into R8
    lis   r8, 0x0000                  # Load LOCK mask value into R8
    ori   r8, r8, 0x0008              # LOCK bit is FMPLL_SYNSR[28]
# Load address of FMPLL_SYNSR
    lis   r6, FMPLL_SYNSRREG@h        # Load address upper FMPLL_SYNSR into R6
    ori   r6, r6, FMPLL_SYNSRREG@l    # Load address lower FMPLL_SYNSR into R6
PLLnotlocked:
    bdnz  PLLcontinue                 # Check for PLL timeout
#                                     #  Decrement counter, Branch if CTR!=0
#                                     #  (0x1770 has not counted down)

# Timeout has happened. Handle here
# Store MFD ERROR bit into the error log register
    lis   r9, ERRLOGREG@ha            # Load upper address for error log into R9
    ori   r9, r9, ERRLOGREG@l         # Load lower address for error log into R9
# Load MFD ERROR bit into error log (ERRLOG)
    lis   r10, ERRLOG_MFDERR@h        # Load MFD ERROR bit into upper error log (ERRLOG)
    ori   r10, r10, ERRLOG_MFDERR@l   # Load MFD ERROR bit into lower error log (ERRLOG)
    stw   r10, 0(r9)                  # Store error bit into the error log register
# Reset the part with a system reset
    lis   r9, SIU_SRCRREG@ha          # Load upper address for SIU_SRCR into R9
    addi  r9, r9, SIU_SRCRREG@l       # Load lower address for SIU_SRCR into R9
    lis   r10, SIU_SRCR_SYSRST@ha     # Load Software system Reset (SSR bit)
    addi  r10, r10, SIU_SRCR_SYSRST@l #   value into SIU_SRCR
    stw   r10, 0(r9)                  # Store SSR bit into SIU_SRCR[0]
# Device will enter system reset at this time
    blr				                  # System reset is active, so this may never happen.
PLLcontinue:
# Check for lock achieved
    lwz   r9, 0(r6)                   # Load value of FMPLL_SYNSR into R9
    and.  r10, r9, r8                 # AND FMPLL_SYNSR with the LOCK bit mask in R8
#                                     # The "." after "and" activates the condition register
    beq   0,PLLnotlocked              # Branch if equal to zero (CR0[2] = 0)
#                                     # Branch back if PLL is not locked
#                                     # (LOCK has not occured)
# PLL is locked, continue

	blr						# Return to the PLL configuration step that required a lock delay
# End of PLL_delay


#************************************************************************/
# FUNCTION     : cfg_FMPLL                                              */
# PURPOSE      : This function performs cache inhibiting, CPU based     */
#                FMPLL frequency selection, and cache uninhibiting.     */
# INPUT NOTES  : None                                                   */
# RETURN NOTES : None                                                   */
# WARNING      : Registers used: R5,R6,R8,R9,R10                        */
#************************************************************************/

cfg_FMPLL:

	mflr  r4							# Save the LR contents

#*******************************************
# Cache inhibit the flash until the FMPLL changes are complete
# This modifies the BAM setup of TLB1

# Check for cache
    mfspr r6, l1cfg0
    andi. r6, r6, 0xFF
    beq   NOINHIBIT    # Skip the cache setup if the microprocessor has no cache

# Search for TLB entry containing the PLL configuration
	lis    r5, cfg_FMPLL80@h			# base address of the FMPLL configuration
	ori    r5, r5, cfg_FMPLL80@l
	mfmsr  r6							# Get MSR[IS] value
	rlwnm r6,r6,27,31,31				# shift and mask AS value for mas6
	mfspr  r7, pid0						# Get PID0[PID] value
	rlwimi r6,r7,16,8,15				# shift and insert PID value for mas6
	tlbsx  0,r5							# TLB search on the FMPLL base address
	isync

# Cache inhibit the Flash TLB
    mfspr r5, mas2						# Read mas2
    ori   r5, r5, CACHE_INHIBIT			# Set the cache inhibit bit
    mtspr mas2, r5						# Write to mas2
    msync								# synchronize for running out of Flash
    tlbwe								# Write the entry to the TLB
    isync								# synchronize for running out of Flash

NOINHIBIT:			# label is used if there is no cache in the microprocessor

# Call the appropriate FMPLL configuration function for the microprocessor
#
# Maximum system frequencies
#
# MPC5533 - 80MHz
# MPC5534 - 80MHz
# MPC5553 - 132MHz
# MPC5554 - 132MHz
# MPC5561 - 132MHz 8MHz crystal
# MPC5561 - 128MHz 40MHz crystal (used for FlexRay)
# MPC5565 - 132MHz
# MPC5566 - 132MHz
# MPC5567 - 132MHz 8MHz crystal
# MPC5567 - 128MHz 40MHz crystal (used for FlexRay)

	.if _MPC5533_ | _MPC5534_
	bl cfg_FMPLL80				# Set up the FMPLL to 80MHz
	.else
	.if _MPC5561_ | _MPC5567_
	bl cfg_FMPLL128_40			# Set up the FMPLL to 128MHz with a 40MHz crystal
	.else # _MPC5553_ | _MPC5554_ | _MPC5565_ | _MPC5566_
	bl cfg_FMPLL132				# Set up the FMPLL to 132MHz
	.endif
	.endif

#*******************************************
# Remove cache inhibit from the flash now the FMPLL changes are complete

# Check for cache
    mfspr r6, l1cfg0
    andi. r6, r6, 0xFF
    beq NOUNINHIBIT    # Skip the cache setup if the microprocessor has no cache

# Enable caching in the Flash TLB
    lis   r5, 0xFFFF
    ori   r5, r5, 0xFFF7				# Mask to clear cache inhibit bit
    mfspr r6, mas2						# Read mas2
    and   r6, r6, r5					# Clear the cache inhibit bit
    mtspr mas2, r6						# Write to mas2
    msync								# synchronize for running out of Flash
    tlbwe								# Write the entry to the TLB
    isync								# synchronize for running out of Flash

NOUNINHIBIT:    # label is used if there is no cache in the microprocessor

	mtlr  r4							# Restore the LR contents
	blr									# Return to the PLL configuration
# End of cfg_FMPLL


#************************************************************************/
# FUNCTION     : cfg_FMPLL80                                            */
# PURPOSE      : This function configures the FMPLL to 80 MHz. Use for  */
#                maximum frequency on MPC5533 and MPC5534.              */
# INPUT NOTES  : FMPLL_SYNCRREG, FMPLL_SYNSRREG, SIU_SRCRREG,           */
#                FMPLL_SYNCR_16_4, FMPLL_SYNCR_16_1, SIU_SRCR_SYSRST,   */
#                ERRLOGREG, ERRLOG_MFDERR,                              */
# RETURN NOTES : None                                                   */
# WARNING      : Registers used: R5,R6,R8,R9,R10                        */
#************************************************************************/

cfg_FMPLL80:

	mflr  r3						  # Save the LR contents

#*******************************************
# Set the internal clock to 80MHz in the following steps
# freq    PREDIV    MFD    RFD    FMPLL_SYNCR
# reset   1         2+4    4      0x0110_0000
# 48      1         2+4    1      0x0104_0000    FMPLL80_SYNCR_SETTING1
# 80      1         6+4    1      0x0304_0000    FMPLL80_SYNCR_SETTING2

# Set the internal clock to 48 MHz with PREDIV=1, MFD=2, and RFD=1.
# This sequence sets the RFD to 1 in the FMPLL_SYNCR Register.
    lis   r5, FMPLL_SYNCRREG@ha       # Load address upper FMPLL_SYNCR into R5
    addi  r5, r5, FMPLL_SYNCRREG@l    # Load address lower FMPLL_SYNCR into R5
# Load value upper FMPLL_SYNCR into R6
    lis   r6, FMPLL80_SYNCR_SETTING1@h      # Load value upper FMPLL_SYNCR into R6
    ori   r6, r6, FMPLL80_SYNCR_SETTING1@l  # Load value lower FMPLL_SYNCR into R6
    stw   r6, 0(r5)                   # Place value of R6 into FMPLL_SYNCR (address R5)
	msync							  # Ensure PLL change is complete before continuing

# Set the internal clock to 80 MHz with PREDIV=1, MFD=6, and RFD=1.
# This sequence sets the MFD to 6 in the FMPLL_SYNCR Register.
# Load value upper FMPLL_SYNCR into R6
    lis   r6, FMPLL80_SYNCR_SETTING2@h      # Load value upper FMPLL_SYNCR into R6
    ori   r6, r6, FMPLL80_SYNCR_SETTING2@l  # Load value lower FMPLL_SYNCR into R6
    stw   r6, 0(r5)                   # Place value of R6 into FMPLL_SYNCR (address R5)
	msync							  # Ensure PLL change is complete before continuing

	bl    PLL_delay					  # Branch to the PLL lock test loop

#**************************************
# Set or clear the LOCEN, LOLRE, LOCRE, LOCIRQ and LOLIRQ bits in the FMPLL_SYNCR.
# This sequence does not change the MFD and RFD settings.
    lwz   r6,0(r5)						# Load value of FMPLL_SYNCR into R6
    oris  r6,r6,FMPLL_SYNCR_BITSET@h	# OR upper value into R6
    ori   r6,r6,FMPLL_SYNCR_BITSET@l	# OR lower value into R6
    lis   r8,FMPLL_SYNCR_BITCLEAR@h		# Load upper value into R8
    ori   r8,r8,FMPLL_SYNCR_BITCLEAR@l	# OR lower value into R8
    and   r10,r6,r8						# Mask out the bits into R10
    stw   r10,0(r5)						# Store into FMPLL_SYNCR from R10

	mtlr  r3
	blr
# End of cfg_FMPLL80


#************************************************************************/
# FUNCTION     : cfg_FMPLL128_40                                        */
# PURPOSE      : This function configures the FMPLL to 128 MHz assuming */
#                a 40MHz crystal. Use for the MPC5561 and MPC5567 only. */
# INPUT NOTES  : FMPLL_SYNCRREG, FMPLL_SYNSRREG, SIU_SRCRREG,           */
#                SIU_SRCR_SYSRST,                                       */
# RETURN NOTES : None                                                   */
# WARNING      : Registers used: R4,R5,R6,R8,R9,R10                     */
#************************************************************************/

cfg_FMPLL128_40:

	mflr  r3						  # Save the LR contents

#*******************************************
# Set the internal clock to 128MHz in the following steps
# 40MHz crystal, MPC5561, MPC5567
# freq    PREDIV    MFD    RFD    FMPLL_SYNCR
# reset   2         2+4    4      0x1110_0000
# 12      5         2+4    4      0x4114_0000    FMPLL128_SYNCR_SETTING1
# 48      5         2+4    1      0x4104_0000    FMPLL128_SYNCR_SETTING2
# 80      5         6+4    1      0x4204_0000    FMPLL128_SYNCR_SETTING3
# 128     5        12+4    1      0x4604_0000    FMPLL128_SYNCR_SETTING4

# Set the internal clock to 12 MHz with PREDIV=5, MFD=2, and RFD=4.
# This sequence sets the PREDIV to 5 in the FMPLL_SYNCR Register.
    lis   r5, FMPLL_SYNCRREG@ha       # Load address upper FMPLL_SYNCR into R5
    addi  r5, r5, FMPLL_SYNCRREG@l    # Load address lower FMPLL_SYNCR into R5
# Load value upper FMPLL_SYNCR into R6
    lis   r6, FMPLL128_40_SYNCR_SETTING1@h      # Load value upper FMPLL_SYNCR into R6
    ori   r6, r6, FMPLL128_40_SYNCR_SETTING1@l  # Load value lower FMPLL_SYNCR into R6
    stw   r6, 0(r5)                   # Place value of R6 into FMPLL_SYNCR (address R5)
	msync							  # Ensure PLL change is complete before continuing

# Set the internal clock to 48 MHz with PREDIV=5, MFD=2, and RFD=1.
# This sequence sets the RFD to 1 in the FMPLL_SYNCR Register.
# Load value upper FMPLL_SYNCR into R6
    lis   r6, FMPLL128_40_SYNCR_SETTING2@h      # Load value upper FMPLL_SYNCR into R6
    ori   r6, r6, FMPLL128_40_SYNCR_SETTING2@l  # Load value lower FMPLL_SYNCR into R6
    stw   r6, 0(r5)                   # Place value of R6 into FMPLL_SYNCR (address R5)
	msync							  # Ensure PLL change is complete before continuing

	bl    PLL_delay					  # Branch to the PLL lock test loop

# Set the internal clock to 80 MHz with PREDIV=5, MFD=6, and RFD=1.
# This sequence sets the MFD to 6 in the FMPLL_SYNCR Register.
# Load value upper FMPLL_SYNCR into R6
    lis   r6, FMPLL128_40_SYNCR_SETTING3@h      # Load value upper FMPLL_SYNCR into R6
    ori   r6, r6, FMPLL128_40_SYNCR_SETTING3@l  # Load value lower FMPLL_SYNCR into R6
    stw   r6, 0(r5)                   # Place value of R6 into FMPLL_SYNCR (address R5)
	msync							  # Ensure PLL change is complete before continuing

	bl    PLL_delay					  # Branch to the PLL lock test loop

#***************************************
# Set the internal clock to 128 MHz with PREDIV=5, MFD=12, and RFD=1.
# This sequence sets the MFD to 12 in the FMPLL_SYNCR Register.
    lis   r6, FMPLL128_40_SYNCR_SETTING4@h      # Load RFD value upper FMPLL_SYNCR into R6
    ori   r6, r6, FMPLL128_40_SYNCR_SETTING4@l  # Load value lower FMPLL_SYNCR into R6
    stw   r6, 0(r5)                   # Place value of R6 into FMPLL_SYNCR (address R5)
	msync							  # Ensure PLL change is complete before continuing

	bl    PLL_delay					  # Branch to the PLL lock test loop

#**************************************
# Set or clear the LOCEN, LOLRE, LOCRE, LOCIRQ and LOLIRQ bits in the FMPLL_SYNCR.
# This sequence does not change the MFD and RFD settings.
    lwz   r6,0(r5)						# Load value of FMPLL_SYNCR into R6
    oris  r6,r6,FMPLL_SYNCR_BITSET@h	# OR upper value into R6
    ori   r6,r6,FMPLL_SYNCR_BITSET@l	# OR lower value into R6
    lis   r8,FMPLL_SYNCR_BITCLEAR@h		# Load upper value into R8
    ori   r8,r8,FMPLL_SYNCR_BITCLEAR@l	# OR lower value into R8
    and   r10,r6,r8						# Mask out the bits into R10
    stw   r10,0(r5)						# Store into FMPLL_SYNCR from R10

	mtlr  r3
	blr
# End of cfg_FMPLL128_40


#************************************************************************/
# FUNCTION     : cfg_FMPLL132                                           */
# PURPOSE      : This function configures the FMPLL to 132 MHz. Use for */
#                the MPC5553 and MPC5554 only.                          */
# INPUT NOTES  : FMPLL_SYNCRREG, FMPLL_SYNSRREG, SIU_SRCRREG,           */
#                SIU_SRCR_SYSRST,                                       */
# RETURN NOTES : None                                                   */
# WARNING      : Registers used: R4,R5,R6,R8,R9,R10                     */
#************************************************************************/

cfg_FMPLL132:

	mflr  r3						  # Save the LR contents

#*******************************************
# Set the internal clock to 132MHz in the following steps
# Crystal options are selected in mpc5500_usrdefs.inc
#
# 8MHz crystal, MPC5553, MPC5534, MPC5565
# freq    PREDIV    MFD    RFD    FMPLL_SYNCR
# reset   1         2+4    4      0x0110_0000
# 48      1         2+4    1      0x0104_0000    FMPLL132_SYNCR_SETTING1
# 64      1         4+4    1      0x0204_0000    FMPLL132_SYNCR_SETTING2
# 32      2         4+4    1      0x1204_0000    FMPLL132_SYNCR_SETTING3
# 72      2         14+4   1      0x1704_0000    FMPLL132_SYNCR_SETTING4
# 132     2         29+4   1      0x1E84_0000    FMPLL132_SYNCR_SETTING5

# Set the internal clock to 48 MHz with PREDIV=1, MFD=2, and RFD=1.
# This sequence sets the RFD to 1 in the FMPLL_SYNCR Register.
    lis   r5, FMPLL_SYNCRREG@ha       # Load address upper FMPLL_SYNCR into R5
    addi  r5, r5, FMPLL_SYNCRREG@l    # Load address lower FMPLL_SYNCR into R5
# Load value upper FMPLL_SYNCR into R6
    lis   r6, FMPLL132_SYNCR_SETTING1@h      # Load value upper FMPLL_SYNCR into R6
    ori   r6, r6, FMPLL132_SYNCR_SETTING1@l  # Load value lower FMPLL_SYNCR into R6
    stw   r6, 0(r5)                   # Place value of R6 into FMPLL_SYNCR (address R5)
	msync							  # Ensure PLL change is complete before continuing

# Set the internal clock to 64 MHz with PREDIV=1, MFD=4, and RFD=1.
# This sequence sets the MFD to 4 in the FMPLL_SYNCR Register.
# Load value upper FMPLL_SYNCR into R6
    lis   r6, FMPLL132_SYNCR_SETTING2@h      # Load value upper FMPLL_SYNCR into R6
    ori   r6, r6, FMPLL132_SYNCR_SETTING2@l  # Load value lower FMPLL_SYNCR into R6
    stw   r6, 0(r5)                   # Place value of R6 into FMPLL_SYNCR (address R5)
	msync							  # Ensure PLL change is complete before continuing

	bl    PLL_delay					  # Branch to the PLL lock test loop

# Set the internal clock to 32 MHz with PREDIV=1, MFD=4, and RFD=1.
# This sequence sets the PREDIV to 2 in the FMPLL_SYNCR Register.
# Load value upper FMPLL_SYNCR into R6
    lis   r6, FMPLL132_SYNCR_SETTING3@h      # Load value upper FMPLL_SYNCR into R6
    ori   r6, r6, FMPLL132_SYNCR_SETTING3@l  # Load value lower FMPLL_SYNCR into R6
    stw   r6, 0(r5)                   # Place value of R6 into FMPLL_SYNCR (address R5)
	msync							  # Ensure PLL change is complete before continuing

	bl    PLL_delay					  # Branch to the PLL lock test loop

#***************************************
# Set the internal clock to 72 MHz with PREDIV=2, MFD=14, and RFD=1.
# This sequence sets the MFD to 14 in the FMPLL_SYNCR Register.
    lis   r6, FMPLL132_SYNCR_SETTING4@h      # Load RFD value upper FMPLL_SYNCR into R6
    ori   r6, r6, FMPLL132_SYNCR_SETTING4@l  # Load value lower FMPLL_SYNCR into R6
    stw   r6, 0(r5)                   # Place value of R6 into FMPLL_SYNCR (address R5)
	msync							  # Ensure PLL change is complete before continuing

	bl    PLL_delay					  # Branch to the PLL lock test loop

#***************************************
# Set the internal clock to 132 MHz with PREDIV=2, MFD=19, and RFD=1.
# This sequence sets the MFD to 19 in the FMPLL_SYNCR Register.
    lis   r6, FMPLL132_SYNCR_SETTING5@h      # Load RFD value upper FMPLL_SYNCR into R6
    ori   r6, r6, FMPLL132_SYNCR_SETTING5@l  # Load value lower FMPLL_SYNCR into R6
    stw   r6, 0(r5)                   # Place value of R6 into FMPLL_SYNCR (address R5)
	msync							  # Ensure PLL change is complete before continuing

	bl    PLL_delay					  # Branch to the PLL lock test loop

#**************************************
# Set or clear the LOCEN, LOLRE, LOCRE, LOCIRQ and LOLIRQ bits in the FMPLL_SYNCR.
# This sequence does not change the MFD and RFD settings.
    lwz   r6,0(r5)						# Load value of FMPLL_SYNCR into R6
    oris  r6,r6,FMPLL_SYNCR_BITSET@h	# OR upper value into R6
    ori   r6,r6,FMPLL_SYNCR_BITSET@l	# OR lower value into R6
    lis   r8,FMPLL_SYNCR_BITCLEAR@h		# Load upper value into R8
    ori   r8,r8,FMPLL_SYNCR_BITCLEAR@l	# OR lower value into R8
    and   r10,r6,r8						# Mask out the bits into R10
    stw   r10,0(r5)						# Store into FMPLL_SYNCR from R10

	mtlr  r3
	blr
# End of cfg_FMPLL132


#*************************************************************************/
# FUNCTION     : cfg_SRAM                                                */
# PURPOSE      : This function initializes the SRAM by writing 64 bit    */
#                values to every SRAM location. This will set the        */
#                 initial ECC (Error Correction Code).                   */
# INPUT NOTES  : INT_SRAM_BASE, INT_SRAM_128BYTSEGS (INT_SRAM_SIZE >> 7) */
# RETURN NOTES : None                                                    */
# WARNING      : Registers used: R5                                      */
#*************************************************************************/

cfg_SRAM:

# loop counter to get all of SRAM
    lis   r5, INT_SRAM_16BYTSEGS@h     # Number of 16 byte segments
    ori   r5, r5, INT_SRAM_16BYTSEGS@l # Number of 16 byte segments
    mtctr r5                           # configure control register for use with bdnz

# base address of the internal SRAM
    lis   r5,SRAM_BASE_ADDR@h
    ori   r5,r5, SRAM_BASE_ADDR@l

# clear load registers
    li    r31, 0
    li    r30, 0
    li    r29, 0
    li    r28, 0

sram_loop:
    stmw  r28,0(r5)                     # write all 4 registers to SRAM
    addi  r5,r5,16                      # increment the ram ptr
    bdnz  sram_loop                     # loop for all of SRAM

    blr
# End of cfg_SRAM


#**************************************************************************/
# FUNCTION     : cfg_FLASH                                                */
# PURPOSE      : This function reduces the wait states for internal Flash */
#                  and external Flash at CS0                              */
# INPUT NOTES  : IP_ADVANCE, __SRAM_LOAD_SIZE, __SRAM_LOAD,               */
#                cfg_SRAM_LOAD, FLASH_BIUCRREG, FLASH_SETTINGS,           */
#                CS0OR0REG, commented out: CS0_OR_OPTIONS                 */
# RETURN NOTES : None                                                     */
# WARNING      : This SRAM space was set up by the BAM                    */
#                Registers used: R6,R7 (Location is critical for use in   */
#                 SRAM loads).   For local use: R3,R4,R5,R10              */
#                 Registers R8 and R9 are used in commented example for   */
#                  optimization of external Flash boot.                   */
#                __SRAM_LOAD is set to ADDR(.heap)                        */
#**************************************************************************/

cfg_FLASH:
#  Save the Link Register into R3
    mflr  r3

#********************************
# Prepare registers for SRAM load usage


#  Enter address and settings to reduce Internal Flash wait states
# Prepare R6 and R7 to hold the FLASH_BIUCR address and wait state value
# Set R6 to hold address of FLASH_BIUCR
    lis   r6, FLASH_BIUCRREG@h     # Load upper address of FLASH_BIUCR into R6
    ori   r6, r6, FLASH_BIUCRREG@l # Load lower address of FLASH_BIUCR into R6

# Set R7 to hold value of internal Flash BIU control register (FLASH_BIUCR) settings
    lis   r7, FLASH_SETTINGS@h     # Store upper Flash BIU control
    ori   r7, r7, FLASH_SETTINGS@l # Store lower Flash BIU control

# If using external Flash boot or execution, external Flash wait states
#     should be configured here. These are not covered here due to the
#     many external configurations that can occur with external Flash.
#     Example steps are below.

    .if EXT_BOOT

# insert external memory wait state reduction code here

# Prepare R8 and R9  to hold the CS0 address and wait state value
# Set R8 to hold address of CS0OR0REG
#    lis r8, CS0ORREG@h            # Load upper address of CS0 OR0 register
#    ori r8, r8, CS0ORREG@l        # Load lower address of CS0 OR0 register
# Set R9 to hold value of CS0 OR0 register settings
#    lis r9, CS0_OR_OPTIONS@h
#    ori r9, r9, CS0_OR_OPTIONS@l

    .endif

SRAMLOAD:

# Set the counter (CTR) to the count of the SRAM load size
    lis   r10, __SRAM_LOAD_SIZE@h      # Load upper SRAM load size into R10
    ori   r10,r10, __SRAM_LOAD_SIZE@l  # Load lower SRAM load size into R10
    mtctr r10                      # Move the SRAM load size into the counter

    lis   r10, cfg_SRAM_LOAD@ha       # Load address of first SRAM load
    addi  r10,r10, cfg_SRAM_LOAD@l


# Load SRAM base address (__SRAM_LOAD) for loading instructions into R5
#    (__SRAM_LOAD = ADDR(.heap))
    lis   r5, __SRAM_LOAD@h      # Load upper SRAM address into R5
    ori   r5, r5, __SRAM_LOAD@l  # Load lower SRAM address into R5

SRAMLOADLOOP:
    lwz   r4, 0(r10)             # Load the opcode at R10 into R4
    stw   r4, 0(r5)              # Store R4 opcode into SRAM at R5
    addi  r5, r5,IP_ADVANCE      # Advance the SRAM address to next opcode space
    addi  r10, r10, IP_ADVANCE   # Move address to next opcode space
    bdnz  SRAMLOADLOOP           # Decrement the counter;
#                                # Branch if Counter not zero.

# Call to SRAM
CallSRAMFunction:

# Link register is saved at start of this function into R3
    lis   r5, __SRAM_LOAD@ha     # Load upper SRAM address into R5
    addi  r5, r5, __SRAM_LOAD@l  # Load lower SRAM address into R5
    mtctr r5
    bctrl

# The value of the following registers must be preserved **
#  during the SRAM run:                                  **
#   R6 = Address for FLASH_BIUCR at: 0xC3F8801C          **
#   R7 = Data for FLASH_BIUCR at:    0x00086C35          **
# The assembler commands will be loaded as opcodes into  **
#  SRAM. These are located in file mpc5500_SRAM_load.s   **
#  Additional assembly commands may be added by the user.**
#  If the length of this code is longer than 0xE0, then  **
#  additional space will have to be set aside in the     **
#  linker file for the flash_data length.                **

# Return to Flash code operation
#   Restore the Link Register from R3
    mtlr  r3

    blr
# End of cfg_FLASH

#*****************************************************************************/
# FUNCTION     : cfg_STACK                                                   */
# PURPOSE      : This function initializes and locks a 4K Stack region       */
#                 to be located in Cache or SRAM. After the stack and the    */
#                 MMU entries are established, C Code can be used in the     */
#                 application.                                               */
# INPUT NOTES  : __STACK_SIZE, __SP_INIT, __SP_END, (defined in linker file) */
#                CACHE_CLEAR, CACHE_SETTINGS, L1CSR0                         */
# RETURN NOTES : None                                                        */
# WARNING      : Registers used: R0(as 0),R1(to set stack ptr),R5, R6        */
#*****************************************************************************/
#
# Stack Size = __STACK_SIZE = (__SP_END - __SP_INIT = 4096) (0x1000) 32-bit constant
#    .equ __STACK_SIZE, 0x1000 (4K is the default size) -- Defined in the linker file
#    .equ __SP_INIT = 0x4001_1000  -- Defined in the linker file
#    .equ __SP_END, (__SP_INIT - __STACK_SIZE) -- Defined in the linker file
#
#    .equ CACHE_CLEAR,(CLFC_NO_OP | CINV_INV_OP | CE_DISABLE)
#    .equ CACHE_SETTINGS, (CWM_COPYBCK | DPB_NOPUSH | CE_ENABLE)

cfg_STACK:

#****************************
# Check for cache
    mfspr r6, l1cfg0
    andi. r6, r6, 0xFF
    beq NOCACHE                   # Skip the cache setup if the microprocessor has no cache

#****************************
# Confirm that no prior cache invalidate operation is in progress
    li    r6, 0x0002              # Set bit[30] in R6 (others are 0)
WaitForInvalidateComplete1:
    mfspr r5, l1csr0              # Move L1CSR0 into R5
    and   r5, r5, r6              # Mask out all bits except CINV*
    cmpli 0, r5, 0x0002           # Compare if CINV, bit 30, = 1
    beq   WaitForInvalidateComplete1 # Branch if CINV is still 1

#****************************
# Invalidate the Cache first, just to make sure it is cleared

# To activate cache invalidate operation,
# place a "1" in the CINV bit location.  (L1CSR0)
    lis   r5, CACHE_CLEAR@h        # Load upper L1CSR0 (0x0) into R5
    ori   r5, r5, CACHE_CLEAR@l    # Load lower L1CSR0 (CINV bit) into R5
    msync                          # Before writing to L1CSR0, execute msync & isync
    isync
    mtspr l1csr0,r5                # Move R5 to the L1CSR0 (SPR 1010)register.
# This should start the CACHE lock Flash bit clear operation.

#****************************
# Confirm that cache invalidate operation is complete
#   R6 still equals 0x0002 no need for:   li r6, 0x0002
WaitForInvalidateComplete2:
    mfspr r5, l1csr0              # Move L1CSR0 into R5
    and   r5, r5, r6              # Mask out all bits except CINV*
    cmpli 0, r5, 0x0002           # Compare if CINV, bit 30, = 1
    beq   WaitForInvalidateComplete2 # Branch if CINV is still 1

#****************************
# Enable cache (totally unused/invalidated)
    mfspr r5, l1csr0               # Retrieve the L1CSR0 register value
    oris  r5, r5, CACHE_SETTINGS@h # OR in CWM and DPB in upper L1CSR0[11:12]
    ori   r5, r5, CACHE_SETTINGS@l # OR in cache enable bit L1CSR0[31]
    msync                          # Before writing to L1CSR0, execute msync & isync
    isync
    mtspr l1csr0, r5               # Return value to L1CSR0 to enable the cache

#****************************
# Now the Stack in Cache can be set up


# Load size of Cache to be used for Stack (4K)
#    lis   r5, __STACK_SIZE@h       # Stack size is 4KBytes
#    ori   r5, r5, __STACK_SIZE@l
# Each stack lock instruction covers 32 bytes, so divide the input parameter
#    srwi  r5, r5, 5                # Shift the contents of R5 right by 5 bits (size/32)
#    mtctr r5                       # locations per cache line loaded to the CTR (SPR 9) register

# Point R5 to just past the system RAM. Set in the Linker file.
#    lis   r5, __SP_END@h
#    ori   r5, r5, __SP_END@l

# Run the loop to lock the cache lines for the stack
# The loop count is: 4KB / 32B = 128 = 0x80
#_cache_loop:
#    dcbz   r0, r5                  # Establish address in cache for 32 bytes and zero
#    dcbtls 0, r0, r5               # Lock the address into the cache
#    addi   r5, r5, 32              # Increment to start of next cache line (+32 bytes)
#    bdnz   _cache_loop             # Decrement the counter (CTR), branch if nonzero

NOCACHE:    # label is used if there is no cache in the microprocessor

# Set the stack pointer
    lis   r1, (__SP_INIT-0x10)@h
    ori   r1, r1, (__SP_INIT-0x10)@l

    blr
# End of cfg_STACK

#*****************************************************************************/
# FUNCTION     : cfg_PNTRS                                                   */
# PURPOSE      : This function initializes register pointers for small data  */
#                 (.sbss) in R13 and small data2 (.sdata2) in R2.            */
#                                                                            */
# INPUT NOTES  : _SDA_BASE_, _SDA2_BASE_ (defined by the linker EABI)        */
# RETURN NOTES : None                                                        */
# WARNING      : Registers used: R13(to set .sdata pointer ),                */
#                                R2 (to set .sdata2 pointer)                 */
#                 The BASE addresses are offset by 0x8000 for CW, GHS, P&E   */
#                 and offset by 0x7FF0 for Diab to simplify access to small  */
#                 data.                                                      */
#*****************************************************************************/

cfg_PNTRS:

# Set the small data (.sbss) pointer
    lis   r13, (_SDA_BASE_)@h
    ori   r13, r13, (_SDA_BASE_)@l

# Set the small data2 (.sdata2) pointer
    lis   r2, (_SDA2_BASE_)@h
    ori   r2, r2, (_SDA2_BASE_)@l

    blr
# End of cfg_PNTRS


#*****************************************************************************/
# FUNCTION     : cfg_ROMCPY                                                  */
# PURPOSE      : This function copies initialized data from Flash to SRAM    */
#                 for the ".data" and ".sdata" sections.                     */
#                                                                            */
# INPUT NOTES  : __DATA_ROM -- defined by the linker EABI                    */
#                __ROM_COPY_SIZE -- Flash area size, in bytes, to be copied  */
#                                     into SRAM.                             */
#                __SRAM_CPY_START -- Start of ".data" section                */
#                CPY_OFFSET -- (=1) defined in mpc5500_defs.inc. 1 byte      */
#                                 offset to next data byte address.          */
# RETURN NOTES : None                                                        */
# WARNING      : Registers used: R10 -- to set .data or .sdata ROM pointer   */
#                                R9 -- to hold remaining ROM_COPY_SIZE bytes */
#                                R5 -- to set SRAM pointer                   */
#                                R4 -- hold the copy data                    */
#                                                                            */
#*****************************************************************************/

cfg_ROMCPY:

# Set GPR9 to the count of the SRAM load size
    lis    r9, __ROM_COPY_SIZE@ha   # Load upper SRAM load size (# of bytes) into R9
    addic. r9,r9, __ROM_COPY_SIZE@l # Load lower SRAM load size into R9
                                    #  The "." sets the condition flag

    beq ROMCPYEND                   # Exit cfg_ROMCPY if size is zero

    mtctr  r9                       # Store # of bytes to be moved in spr CTR

    lis   r10, __DATA_ROM@ha        # Load address of first SRAM load into R10
    addi  r10,r10, __DATA_ROM@l     # Load lower address of SRAM load into R10
    subi  r10,r10, CPY_OFFSET       # Decrement address to prepare for ROMCPYLOOP

# Load SRAM base address (__SRAM_CPY_START) for loading instructions into R5
#    (__SRAM_CPY_START = ADDR(.data))
    lis   r5, __SRAM_CPY_START@h      # Load upper SRAM address into R5
    ori   r5, r5, __SRAM_CPY_START@l  # Load lower SRAM address into R5
    subi  r5, r5, CPY_OFFSET          # Decrement address to prepare for ROMCPYLOOP

ROMCPYLOOP:
    lbzu   r4, CPY_OFFSET(r10) # Load data byte at R10 into R4,incrementing (update) ROM address
    stbu   r4, CPY_OFFSET(r5)  # Store R4 data byte into SRAM at R5 and update SRAM address
    bdnz   ROMCPYLOOP          # Branch if more bytes to load from ROM

ROMCPYEND:
    blr
# End of cfg_ROMCPY



#*************************************************************************/
#                        MMU Functions                                   */
#*************************************************************************/

#*****************************************************************************/
# FUNCTION     : cfg_MMU                                                     */
# PURPOSE      : This function modifies the MMU TLB (translation lookaside   */
#                 buffer) table by writing to the appropriate MAS registers. */
# INPUT NOTES  : Requires SPRs defined and a data table for the TLB entries  */
#                mmu_tlb0 through mmu_tlb11, mmu_tlb15 from                  */
#                mpc5500_usrdefs.inc.                                        */
# RETURN NOTES : None                                                        */
# WARNING      : Registers used: R3,R5. Commands "msync" and "isync" are not */
#                required around the tlbwe since we are at configuration and */
#                 other background operations cannot be active.              */
#*****************************************************************************/
cfg_MMU:

#***************************************************/
#     setup MMU                                    */
#***************************************************/

# Change the TLB15 BAM in Flash size to 1M.
    lis   r3, mmu_tlb15@h     # base address of MAS Constants
    ori   r3,r3, mmu_tlb15@l
    lwz   r5,0(r3)           # Get MAS0 value
    mtspr mas0,r5            # mtspr MAS0,r5
    lwzu  r5,4(r3)           # Get MAS1 value
    mtspr mas1,r5            # mtspr MAS1,r5
    lwzu  r5,4(r3)           # Get MAS2 value
    mtspr mas2,r5            # mtspr MAS2,r5
    lwzu  r5,4(r3)           # Get MAS3 value
    mtspr mas3,r5            # mtspr MAS3,r5
    tlbwe                    # Write the entry to the TLB


# Change the TLB0 PBRIDGE_B size to 1M.
    lis   r3, mmu_tlb0@h     # base address of MAS Constants
    ori   r3,r3, mmu_tlb0@l
    lwz   r5,0(r3)           # Get MAS0 value
    mtspr mas0,r5            # mtspr MAS0,r5
    lwzu  r5,4(r3)           # Get MAS1 value
    mtspr mas1,r5            # mtspr MAS1,r5
    lwzu  r5,4(r3)           # Get MAS2 value
    mtspr mas2,r5            # mtspr MAS2,r5
    lwzu  r5,4(r3)           # Get MAS3 value
    mtspr mas3,r5            # mtspr MAS3,r5
    tlbwe                    # Write the entry to the TLB

# Change the TLB1 Flash (1) size to 1M. Split into 5 areas.
    lis   r3, mmu_tlb1@h     # base address of MAS Constants
    ori   r3,r3, mmu_tlb1@l
    lwz   r5,0(r3)           # Get MAS0 value
    mtspr mas0,r5            # mtspr MAS0,r5
    lwzu  r5,4(r3)           # Get MAS1 value
    mtspr mas1,r5            # mtspr MAS1,r5
    lwzu  r5,4(r3)           # Get MAS2 value
    mtspr mas2,r5            # mtspr MAS2,r5
    lwzu  r5,4(r3)           # Get MAS3 value
    mtspr mas3,r5            # mtspr MAS3,r5
    msync                    # synchronize for running out of Flash
    tlbwe                    # Write the entry to the TLB
    isync                    # synchronize for running out of Flash

# Change the TLB2 External Flash size to 4M.
    lis   r3, mmu_tlb2@h     # base address of MAS Constants
    ori   r3,r3, mmu_tlb2@l
    lwz   r5,0(r3)           # Get MAS0 value
    mtspr mas0,r5            # mtspr MAS0,r5
    lwzu  r5,4(r3)           # Get MAS1 value
    mtspr mas1,r5            # mtspr MAS1,r5
    lwzu  r5,4(r3)           # Get MAS2 value
    mtspr mas2,r5            # mtspr MAS2,r5
    lwzu  r5,4(r3)           # Get MAS3 value
    mtspr mas3,r5            # mtspr MAS3,r5
    tlbwe                    # Write the entry to the TLB

# Change the TLB3 SRAM size to 64K.
    lis   r3, mmu_tlb3@h     # base address of MAS Constants
    ori   r3,r3, mmu_tlb3@l
    lwz   r5,0(r3)           # Get MAS0 value
    mtspr mas0,r5            # mtspr MAS0,r5
    lwzu  r5,4(r3)           # Get MAS1 value
    mtspr mas1,r5            # mtspr MAS1,r5
    lwzu  r5,4(r3)           # Get MAS2 value
    mtspr mas2,r5            # mtspr MAS2,r5
    lwzu  r5,4(r3)           # Get MAS3 value
    mtspr mas3,r5            # mtspr MAS3,r5
    tlbwe                    # Write the entry to the TLB

# Change the TLB4 PBRIDGE_A size to 1M.
    lis   r3, mmu_tlb4@h     # base address of MAS Constants
    ori   r3,r3, mmu_tlb4@l
    lwz   r5,0(r3)           # Get MAS0 value
    mtspr mas0,r5            # mtspr MAS0,r5
    lwzu  r5,4(r3)           # Get MAS1 value
    mtspr mas1,r5            # mtspr MAS1,r5
    lwzu  r5,4(r3)           # Get MAS2 value
    mtspr mas2,r5            # mtspr MAS2,r5
    lwzu  r5,4(r3)           # Get MAS3 value
    mtspr mas3,r5            # mtspr MAS3,r5
    tlbwe                    # Write the entry to the TLB

## Change the TLB5 to invalid to turn off duplicate entries for internal Flash
    lis   r3, mmu_tlb5@h     # base address of MAS Constants
    ori   r3,r3, mmu_tlb5@l
    lwz   r5,0(r3)           # Get MAS0 value
    mtspr mas0,r5            # mtspr MAS0,r5
    lwzu  r5,4(r3)           # Get MAS1 value
    mtspr mas1,r5            # mtspr MAS1,r5
    tlbwe                    # Write the entry to the TLB

# Change the TLB6 Flash (2) size to 256K.
    lis   r3, mmu_tlb6@h     # base address of MAS Constants
    ori   r3,r3, mmu_tlb6@l
    lwz   r5,0(r3)           # Get MAS0 value
    mtspr mas0,r5            # mtspr MAS0,r5
    lwzu  r5,4(r3)           # Get MAS1 value
    mtspr mas1,r5            # mtspr MAS1,r5
    lwzu  r5,4(r3)           # Get MAS2 value
    mtspr mas2,r5            # mtspr MAS2,r5
    lwzu  r5,4(r3)           # Get MAS3 value
    mtspr mas3,r5            # mtspr MAS3,r5
    tlbwe                    # Write the entry to the TLB

# Change the TLB7 Flash (3) size to 256K.
    lis   r3, mmu_tlb7@h     # base address of MAS Constants
    ori   r3,r3, mmu_tlb7@l
    lwz   r5,0(r3)           # Get MAS0 value
    mtspr mas0,r5            # mtspr MAS0,r5
    lwzu  r5,4(r3)           # Get MAS1 value
    mtspr mas1,r5            # mtspr MAS1,r5
    lwzu  r5,4(r3)           # Get MAS2 value
    mtspr mas2,r5            # mtspr MAS2,r5
    lwzu  r5,4(r3)           # Get MAS3 value
    mtspr mas3,r5            # mtspr MAS3,r5
    tlbwe                    # Write the entry to the TLB

# Change the TLB8 Flash (4) size to 256K.
    lis   r3, mmu_tlb8@h     # base address of MAS Constants
    ori   r3,r3, mmu_tlb8@l
    lwz   r5,0(r3)           # Get MAS0 value
    mtspr mas0,r5            # mtspr MAS0,r5
    lwzu  r5,4(r3)           # Get MAS1 value
    mtspr mas1,r5            # mtspr MAS1,r5
    lwzu  r5,4(r3)           # Get MAS2 value
    mtspr mas2,r5            # mtspr MAS2,r5
    lwzu  r5,4(r3)           # Get MAS3 value
    mtspr mas3,r5            # mtspr MAS3,r5
    tlbwe                    # Write the entry to the TLB

# Change the TLB9 Flash (5) size to 256K.
    lis   r3, mmu_tlb9@h     # base address of MAS Constants
    ori   r3,r3, mmu_tlb9@l
    lwz   r5,0(r3)           # Get MAS0 value
    mtspr mas0,r5            # mtspr MAS0,r5
    lwzu  r5,4(r3)           # Get MAS1 value
    mtspr mas1,r5            # mtspr MAS1,r5
    lwzu  r5,4(r3)           # Get MAS2 value
    mtspr mas2,r5            # mtspr MAS2,r5
    lwzu  r5,4(r3)           # Get MAS3 value
    mtspr mas3,r5            # mtspr MAS3,r5
    tlbwe                    # Write the entry to the TLB

#****************************
# Check for cache
    mfspr r6, l1cfg0
    andi. r6, r6, 0xFF
    beq SKIP_CACHE_TLB       # skip TLB10 setup if the microprocessor has no cache

# Add TLB10 definition for Stack in CACHE size to 4K.
    lis   r3, mmu_tlb10@h    # base address of MAS Constants
    ori   r3,r3, mmu_tlb10@l
    lwz   r5,0(r3)           # Get MAS0 value
    mtspr mas0,r5            # mtspr MAS0,r5
    lwzu  r5,4(r3)           # Get MAS1 value
    mtspr mas1,r5            # mtspr MAS1,r5
    lwzu  r5,4(r3)           # Get MAS2 value
    mtspr mas2,r5            # mtspr MAS2,r5
    lwzu  r5,4(r3)           # Get MAS3 value
    mtspr mas3,r5            # mtspr MAS3,r5
    tlbwe                    # Write the entry to the TLB

SKIP_CACHE_TLB:    # label is used if there is no cache in the microprocessor

# Change the TLB11 External SRAM size to 4M.
    lis   r3, mmu_tlb11@h    # base address of MAS Constants
    ori   r3,r3, mmu_tlb11@l
    lwz   r5,0(r3)           # Get MAS0 value
    mtspr mas0,r5            # mtspr MAS0,r5
    lwzu  r5,4(r3)           # Get MAS1 value
    mtspr mas1,r5            # mtspr MAS1,r5
    lwzu  r5,4(r3)           # Get MAS2 value
    mtspr mas2,r5            # mtspr MAS2,r5
    lwzu  r5,4(r3)           # Get MAS3 value
    mtspr mas3,r5            # mtspr MAS3,r5
    tlbwe                    # Write the entry to the TLB

# Change the TLB14 Shadow Block size to 4K.
    lis   r3, mmu_tlb14@h     # base address of MAS Constants
    ori   r3,r3, mmu_tlb14@l
    lwz   r5,0(r3)           # Get MAS0 value
    mtspr mas0,r5            # mtspr MAS0,r5
    lwzu  r5,4(r3)           # Get MAS1 value
    mtspr mas1,r5            # mtspr MAS1,r5
    lwzu  r5,4(r3)           # Get MAS2 value
    mtspr mas2,r5            # mtspr MAS2,r5
    lwzu  r5,4(r3)           # Get MAS3 value
    mtspr mas3,r5            # mtspr MAS3,r5
    tlbwe                    # Write the entry to the TLB

    blr
# End of cfg_MMU

#*************************************************************************
