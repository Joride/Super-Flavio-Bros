.include "header.inc"    ; contains the bytes necessary for iNES file format

.include "constants.inc" ; contains PPU and IO register defines

.segment "CODE"

; variables
; these next two bytes contain a pointer to the start of a block of memory
; that is about to or being transferred to the PPU
TRANSFER_ADDR_LO = $00
TRANSFER_ADDR_HI = $01
NT_TILES_TO_TRANSFER = $02
NT_ROWS_TO_TRANSFER = $03
PPU_NT_HIBYTE_ADDR = $04
NT_COLUMN_INFO = $05
JOYPAD1_VALUES = $06
JOYPAD2_VALUES = $07
X_SCROLL = $08 ; in pixels
X_SCROLL_NT_SELECT = $09 ; non-zero: scroll position is in NT1
X_SCROLL_NT_LOAD = $0A ; non-zero: scroll position is in NT1
NT_COLUMN_LOAD_INDEX = $0B
NT_COLUMN_APPEAR_TRACKER = $0C

; constants, these should be prepended with `#` when used in code
ATTR_TABLE_SIZE = $40
BG_PALLET_SIZE = $10
NUM_TILES_PER_ROW = $20
NUM_ROWS = $1E
PIXELS_PER_TILE = $08


; interrupt request (IRQ) routine
.proc irq_handler
  RTI
.endproc

; Non-Maskable Interrupt (NMI) routine
.proc nmi_handler
;   LDA #$00
;   STA OAMADDR ; tells the PPU to prepare for a transfer to OAM starting at byte zero (as A was just loaded with #$00)
;   LDA #$02  
;   STA OAMDMA  ; tells the PPU to initiate a high-speed transfer of the 256 bytes from $0200-$02ff into OAM

    JSR read_joypad1

    ; button order from MSB to LSB: A, B, Select, Start, Up, Down, Left, Right
    ; check A button
    LDA JOYPAD1_VALUES
    AND #%00000001
    BEQ not_pressed
        ; button is pressed
        INC X_SCROLL
        BNE stay_in_same_nametable
            ; X_SCROLL is now zero, flip the value from non-zero to zero or vice versa
            LDA X_SCROLL_NT_SELECT
            BNE flip_to_zero
                LDA #$01
                JMP value_flipped

            flip_to_zero:
                LDA #$0

            value_flipped:
            STA X_SCROLL_NT_SELECT

        stay_in_same_nametable:
        
        LDA NT_COLUMN_APPEAR_TRACKER
        CMP #PIXELS_PER_TILE
        BNE no_new_tile_coming_onscreen
            ; a new tile is about the be shown on onscreen
            ; - reset the tracker to zero
            LDA #$00
            STA NT_COLUMN_APPEAR_TRACKER

            LDA NT_COLUMN_LOAD_INDEX
            CMP #NUM_TILES_PER_ROW ; = $20
            BNE no_reset_column_index
                ; reset the column index
                LDA #$00
                STA NT_COLUMN_LOAD_INDEX
                ; flip X_SCROLL_NT_LOAD
                ; ...

            no_reset_column_index:

            lda X_SCROLL_NT_LOAD ; #%10000000 ; nametable 1, column 0
            ORA NT_COLUMN_LOAD_INDEX
            sta NT_COLUMN_INFO
            jsr load_nametable_column

            ; update the column index to be loaded for next iteration
            INC NT_COLUMN_LOAD_INDEX

        no_new_tile_coming_onscreen:
        INC NT_COLUMN_APPEAR_TRACKER

    not_pressed:

    ; load a column of sprites in a nametable
    ;lda #%10000000 ; nametable 1, column 0
    ;sta NT_COLUMN_INFO
    ;jsr load_nametable_column


    ; ### Reset the PPU aaddress
    ; ###
    LDX PPUSTATUS  ; reset address latch (make sure next write is considered high byte)
    LDX #.LOBYTE(PPU_NT0_ADDR)
    STX PPUADDR  ; set high byte of address in PPU space 
    LDX #.HIBYTE(PPU_NT0_ADDR)   ; set low byte of address in PPU space 
    STX PPUADDR

    JSR update_PPU

    ; ### Set the scroll values (this needs to be done as a last step in the
    ; ### NMI handler)
    ; ###    
    LDA PPUSTATUS
    lda X_SCROLL;
    sta PPUSCROLL   ; 1st write: X scroll
    lda #$00
    sta PPUSCROLL   ; 2nd write: Y scroll
    ; ###  

    RTI
.endproc

.proc read_joypad1
    ;latch buttons for both controllers. Writing the values $01 and $00 to the
    ; port for controller 1, prepares both controllers to be read
    lda #$01
    sta JOYPAD1
    lda #$00
    sta JOYPAD1

    lda #$01
    sta JOYPAD1_VALUES
    ; read order is: A, B, Select, Start, Up, Down, Left, Right
    ; this means A will end up in bit 7 (MSB) and Right will end up
    ; in bit 0 (LSB)
    read_next_button:
        LDA JOYPAD1
        AND #$01 ; A now contains 00 (not pressed) or 1 (pressed)
        TAX ; keep the pressed/not pressed value around
        
        LDA JOYPAD1_VALUES ; already collected values
        ASL ; move the bits and OR with the newly read value

        STX JOYPAD1_VALUES
        ORA JOYPAD1_VALUES
        STA JOYPAD1_VALUES

        BCC read_next_button

        ; done reading the buttons
    RTS
.endproc

.proc update_PPU
    ; ### Setup the PPU
    ; ###
    ; bit 0 set: add 256 to the X scroll position
    ; bit 1 set: add 240 to the Y scroll position
    ;
    ; bit 7 set: Generate NMI at start of VBlank;
    ; bit 4 set: Background pattern table address = $0000 (1: $1000)
    LDA X_SCROLL_NT_SELECT
    beq scroll_in_NT0
        LDA #%10010001
        jmp scroll_in_NT_set

    scroll_in_NT0:
        LDA #%10010000

    scroll_in_NT_set:
    STA PPUCTRL
    RTS
.endproc

; Reset handler
.proc reset_handler
vblankwait:
    BIT PPUSTATUS
    BPL vblankwait
    JMP main ; we are now in VBLANK
.endproc

.proc main
    ; initialize
    LDA #$00
    STA X_SCROLL
    STA X_SCROLL_NT_SELECT
    STA NT_COLUMN_LOAD_INDEX
    LDA #PIXELS_PER_TILE ; change this to a value below 0x08 to see the per-column loading in effect (albeit just too late every time then)
    STA NT_COLUMN_APPEAR_TRACKER
    
    LDA #$80 ; nametable1
    STA X_SCROLL_NT_LOAD
    
    jsr set_bgcolor
    jsr load_nametable
    jsr transfer_color_pallettes
    jsr transfer_attribute_tables

    ; init PPU
    LDA #%10010000
    STA PPUCTRL

    LDA #%00001110  
    STA PPUMASK   ; PPUMASK - control the drawing options on the PPU:
        ; bit   value       effect 
        ; ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
        ;   0       0       Greyscale mode disabled
        ;   1       1       Show leftmost 8px of background
        ;   2       1       Show leftmost 8px of foreground
        ;   3       1       Background enabled
        ;   4       1       Foreground enabled
        ;   5       0       No red emphasis
        ;   6       0       No green emphasis
        ;   7       0       No blue emphasis
        ; This PPUCTRL ($2001):
        ; `BGRs bMmG`:
        ; color emphasis (BGR)
        ; sprite enable (s)
        ; background enable (b)
        ; sprite left column enable (M)
        ; background left column enable (m)
        ; greyscale (G)       

    forever:
        JMP forever ; infinite loop to avoid the CPU from running undefined code
.endproc

.proc transfer_attribute_tables
    ; ### Transfer attribute tables to PPU memory
    ; ###
    LDX PPUSTATUS  ; reset address latch (make sure next write is considered high byte)
    LDX #.HIBYTE(PPU_AT0_ADDR) ; #$23
    STX PPUADDR  ; set high byte of address in PPU space 
    LDX #.LOBYTE(PPU_AT0_ADDR) ;#$c0   ; set low byte of address in PPU space 
    STX PPUADDR

    lda #.LOBYTE(attribute_table_0)
    sta TRANSFER_ADDR_LO
    lda #.HIBYTE(attribute_table_0)
    sta TRANSFER_ADDR_HI
    LDY #$00
    transfer_attribute_table_0:
        LDA (TRANSFER_ADDR_LO), y
        STA PPUDATA   ;set the value of the address we just gave. Writing to PPUDATA also

        INY
        CPY #ATTR_TABLE_SIZE
        BNE transfer_attribute_table_0
    ; ###

    rts
.endproc

.proc transfer_color_pallettes
    ; ### Transfer Color Pallettes to PPU memory
    ; ###
    LDX PPUSTATUS  ; reset address latch (make sure next write is considered high byte)
    LDX #.HIBYTE(PPU_BGRP_ADDR) ; #$3f
    STX PPUADDR  ; set high byte of address in PPU space 
    LDX #.LOBYTE(PPU_BGRP_ADDR) ; #$00   ; set low byte of address in PPU space 
    STX PPUADDR

    lda #.LOBYTE(backround_palletes)
    sta TRANSFER_ADDR_LO
    lda #.HIBYTE(backround_palletes)
    sta TRANSFER_ADDR_HI
    LDY #$00
    transfer_background_palletes:
        LDA (TRANSFER_ADDR_LO), y
        STA PPUDATA   ;set the value of the address we just gave. Writing to PPUDATA also

        INY
        CPY #BG_PALLET_SIZE
        BNE transfer_background_palletes
    ; ###
    rts

.endproc

.proc set_bgcolor
    ; ### Set the background color
    ; ###
    ; reset the "address latch" for PPUADDR. 
    ; Reading from PPUSTATUS ensures the next write to PPUADDR will be considered
    ; the high byte of an address
    LDX PPUSTATUS

    LDX #.HIBYTE(PPU_BGRP_ADDR) ; #$3f
    STX PPUADDR   ; set high byte of address in PPU space to #$3f
    LDX #.LOBYTE(PPU_BGRP_ADDR)
    STX PPUADDR   ; set low byte of address in PPU space to #$00 

    LDA #NUM_TILES_PER_ROW   ; background color
    STA PPUDATA   ;set the value of address $3f00 in PPU space to a value from the pallet
    ; ###
    rts
.endproc

.proc load_nametable
    ; ### Load nametable 0 into PPU memory
    ; ###
    LDX PPUSTATUS  ; reset address latch (make sure next write is considered high byte)
    LDX #.HIBYTE(PPU_NT0_ADDR) ;#$20
    STX PPUADDR  ; set high byte of address in PPU space 
    LDX #.LOBYTE(PPU_NT0_ADDR) ;#$00   ; set low byte of address in PPU space 
    STX PPUADDR
    

    ; nametable is 16 (1F) x 30 (3A) bytes
    ; bytecount = 0x3A1F
    ; store memory location of `nametable` in $00 and $01
    ; using INDIRECT INDEXED ADDRESSING here. See the manual, and
    ; a good explanagtion here:
    ; see: https://stackoverflow.com/questions/46262435/indirect-y-indexed-addressing-mode-in-mos-6502

    lda #.LOBYTE(nametable0)
    sta TRANSFER_ADDR_LO     ; store the lobyte at address $00
    lda #.HIBYTE(nametable0)
    sta TRANSFER_ADDR_HI     ; store the hibyte at address $01

    lda #NUM_TILES_PER_ROW ; number of tiles per row
    sta NT_TILES_TO_TRANSFER  ; store number of tiles per row at address $02
    lda #NUM_ROWS ; number of rows of tiles
    sta NT_ROWS_TO_TRANSFER  ; store number of rows of tiles at address $03

    ldy #$00
    transfer_name_table0:
        LDA ($00), y
        STA PPUDATA   ;set the value of the address we just gave. Writing to PPUDATA also
        ; increments the address with one

        DEC NT_TILES_TO_TRANSFER
        BNE skip_update_tiles_and_row_counter

            ; `update_tiles_and_row_counter`:
            ; $02 is equal to 0, a scanline worth of tile-indexes has been transferred to PPU
            ; reset the value of $02
            lda #NUM_TILES_PER_ROW
            sta NT_TILES_TO_TRANSFER

            ; decrement $03, the scanline progress tracker location
            ; if it is then zero, all tiles have been transferred
            DEC NT_ROWS_TO_TRANSFER
            BEQ transfer_name_table0_done
        
        skip_update_tiles_and_row_counter:

        INY
        BNE transfer_name_table0

        ; Y equal to zero, increment MSB of pointer into nametable
        INC TRANSFER_ADDR_HI

        ; the only exit from this subroutine is in `update_tiles_and_row_counter`
        jmp transfer_name_table0

    transfer_name_table0_done:
    rts
    ; ###
.endproc

; NT_COLUMN_INFO ($05) contains the info required to load a column: bits 0-6 (inclusive)
; indicte the index of the column to load, bit at index 7 indicates which nametable
; bit 6 must be 0
.proc load_nametable_column
    lda NT_COLUMN_INFO
    bmi nametable_1
    nametable_0:
        LDX #.HIBYTE(PPU_NT0_ADDR)
        jmp nametableindex_set

    nametable_1:
        LDX #.HIBYTE(PPU_NT1_ADDR)
        
    nametableindex_set:
    
    ; the low-order byte for both nametable 1 and 2 is #$00, so no need to store it somewhere
    STX PPU_NT_HIBYTE_ADDR ; $04 contains the high-order byte of the PPU memory address of the start of the nametable

    lda NT_COLUMN_INFO
    and #%00111111 ; clear the negative bit (and bit 6, is shoud be zero anyway), is was only there to indicate wich nametable, A now contains the index if the column to load in the nametable
    sta NT_COLUMN_INFO ; store the actual address only
    ldx #$00
    load_tile:
        LDA PPUSTATUS  ; reset address latch (make sure next write is considered high byte)
        
        LDA PPU_NT_HIBYTE_ADDR
        STA PPUADDR  ; set high byte of address in PPU space 

        lda NT_COLUMN_INFO
        STA PPUADDR  ; set low byte of address in PPU space         

        ; store tile index at the PPU address we just set
        ;LDA #$13 
        STX PPUDATA  ; using X here just as an example, so tee increasing values top to bottom

        ; update PPU addresses for next iteration
        lda NT_COLUMN_INFO
        clc
        adc #NUM_TILES_PER_ROW ; the number tiles per rows
        sta NT_COLUMN_INFO ; this location now contains the low byte of the PPU address for the next rows
        bcc carry_clear
            ; increment the high-byte of the PPU memory nametable address
            INC PPU_NT_HIBYTE_ADDR

        carry_clear:

        INX
        CPX #NUM_ROWS
        bne load_tile

    RTS
.endproc

.include "nametable0.asm"
.include "pallettes.asm"
.include "pallettes2.asm"
.include "attributetables.asm"


; reset vector addresses 
; the assembler will put these sequentually starting from address $FFFA (as
; defined in nes.cfg)
.segment "VECTORS"
.addr nmi_handler, reset_handler, irq_handler

.segment "CHR"
.incbin "graphics.NESsprites"