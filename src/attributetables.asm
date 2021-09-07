attribute_table_0:
	; each bytes encodes indexes into the pallets for 4x4 tiles.
	; each 2-bit combination is an index into a pallet for 2x2 sprites
	; organized like so:
	; AABBCCDD:
	; AA = bottom right 2x2 sprites of the 4x4 block
	; BB = bottom left 2x2 sprites of the 4x4 block
	; CC = top right 2x2 sprites of the 4x4 block
	; DD = top left 2x2 sprites of the 4x4 block
    .byte %00000000, %00001111, %00001111, %00001111, %00001111, %00001111, %00001111, %00001111
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000

    ; this line encodes only two rows of tiles, meaning the upper 4 bits of each
    ; byte are ignored (outside the screen)
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000