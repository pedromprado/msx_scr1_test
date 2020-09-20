
; Test for Screen 1 - repeated patterns handling
; Done: the ship "record" includes which chars it uses
; ToDo:
;	make the ship record NxN flexible
;	make the ship printing NxN flexible
;	make the char reprogramming NxN flexible
;	make scrolling routines NxN flexible


	org 0x4000
       
; BIOS variables for screen 1 handling
SETT32:	equ 0x007B
T32NAM:	equ 0xF3BD	; BASE(5) - SCREEN 1 name table
T32COL:	equ 0xF3BF	; BASE(6) - SCREEN 1 color table
T32CGP:	equ 0xF3C1	; BASE(7) - SCREEN 1 character pattern table
T32ATR: equ 0xF3C3	; BASE(8) - SCREEN 1 sprite attribute table
T32PAT: equ 0xF3C5	; BASE(9) - SCREEN 1 sprite pattern table

; BIOS calls for screen handling

LDIRVM:	equ 0x005C	; BIOS call to copy from RAM to VRAM
			; HL=RAM, DE=VRAM, BC=length
CHPUT:	equ 0x00A2	; prints the char in A
CLS:	equ 0x00C3	; Clears screen if Z
    
; MSX cartridge header @ 0x4000 - 0x400f
	dw 0x4241
        dw Init
        dw Init
        dw 0
        dw 0
        dw 0
        dw 0
        dw 0

; initialize 
Init:
        call SETT32	; screen 1
        xor a
        call CLS
        
        ld de,ship1_ram
        ld hl,ship1
        ld bc,ship1_patt_end - ship1
        ldir
        
	ld de,ship2_ram
        ld hl,ship2
        ld bc,ship2_patt_end - ship2
        ldir
	
	ld de, ship1
	call Form_ship

	ld b,9
Put_ship1:
        ld hl,ship1_ram
	call Print_ship
        djnz Put_ship1
  
        ld a,13
        call CHPUT
        ld a,10
        call CHPUT
        call CHPUT
        ld b,9
Put_ship2:
        ld hl,ship2_ram
	call Print_ship
        djnz Put_ship2

        

        
Loop:
	ld hl,ship1_pattcol1_ram
        ld ix,ship1_pattcol2_ram
        ld iy,ship1_pattcol3_ram
	call Scroll_ship_right
	ld hl,ship1_pattcol1_ram
        ld ix,ship1_pattcol2_ram
        ld iy,ship1_pattcol3_ram
	call Scroll_ship_right
        
	ld hl,ship2_pattcol1_ram
        ld ix,ship2_pattcol2_ram
        ld iy,ship2_pattcol3_ram
	call Scroll_ship_left

        call Form_ship

pause:	
	halt        
	jp Loop	    	; loop forever
    
    
Form_ship:		; Programs the char patterns
			; for the ship pieces
        ld ix,ship1
        ld iy,ship1_patt_ram
        call Form_ship_char_loop
        ld ix,ship2
        ld iy,ship2_patt_ram
        call Form_ship_char_loop
        ret

        
Form_ship_char_loop:
        ld a,(ix)	; char to reprogram
        or a
        ret z		; 0 = no more chars to reprogram
        call Form_ship_pattern_char_write
        inc ix
        ld bc,8
        add iy,bc
        jr Form_ship_char_loop
        
Form_ship_pattern_char_write:
	ld l,a		; char 
        ld h,0
        add hl,hl	; *2
        add hl,hl	; *4
        add hl,hl	; *8 -> pattern table position
        ld de,(T32CGP)
        add hl,de	; VRAM address to write
        ex de,hl	; on DE
        ld bc,8
        push iy
        pop hl
        call LDIRVM
        
	ret


Print_ship: 		; Put the chars on screen
			; Corresponding to the ship's pattern

Print_ship_loop1:
        ld a,(hl)
        or a
        ret z
        call CHPUT
        inc hl
        jr Print_ship_loop1
    
    
Scroll_ship_right:	; Moves the ship's pattern 1 
			; pixel to the right
                        ; dumb concept for now
                        
        ld a,8
Scroll_ship_right_loop:
	scf
	bit 0,(iy)
        jr nz, Scroll_ship_right_is_not_zero
Scroll_ship_right_is_zero:
	ccf
Scroll_ship_right_is_not_zero:
        rr (hl)
        rr (ix)
        rr (iy)
        inc hl
        inc ix
        inc iy
        dec a
        jr nz,Scroll_ship_right_loop
	ret

Scroll_ship_left:	; Moves the ship's pattern 1 
			; pixel to the left
                        ; dumb concept for now
                        
        ld a,8
Scroll_ship_left_loop:
	scf
	bit 7,(hl)
        jr nz, Scroll_ship_left_is_not_zero
Scroll_ship_left_is_zero:
	ccf
Scroll_ship_left_is_not_zero:
        rl (iy)
        rl (ix)
        rl (hl)
        inc hl
        inc ix
        inc iy
        dec a
        jr nz,Scroll_ship_left_loop
	ret
            
; Ship's shape in 6x3 chars
ship1:  defm "ABC",0
        
ship1_patt:
ship1_pattcol1: 

	db 0x00			; 00000000
	db 0x07			; 00000111
	db 0x0f			; 00001111
	db 0x0c			; 00001100
	db 0x0c			; 00001100
	db 0x0f			; 00001111
        db 0x07			; 00000111
	db 0x00			; 00000000

ship1_pattcol2: 

	db 0x00			; 00000000
	db 0xff			; 11111111
	db 0xff			; 11111111
	db 0x00			; 00000000
	db 0x00			; 00000000
	db 0xff			; 11111111
	db 0xff			; 11111111
	db 0x00			; 00000000

ship1_pattcol3: 

	db 0x00			; 00000000
	db 0xe0			; 11100000
	db 0xf0			; 11110000
	db 0x30			; 00110000
	db 0x30			; 00110000
	db 0xf0			; 11110000
	db 0xe0			; 11100000
	db 0x00			; 00000000
        
ship1_patt_end:


; Ship's shape in 6x3 chars
ship2:  defm "abc",0
        
ship2_patt:
ship2_pattcol1: 

	db 0x00			; 00000000
	db 0x07			; 00000111
	db 0x0f			; 00001111
	db 0x19			; 00011001
	db 0x18			; 00011000
	db 0x0f			; 00001111
        db 0x07			; 00000111
	db 0x00			; 00000000

ship2_pattcol2: 

	db 0x00			; 00000000
	db 0xff			; 11111111
	db 0xff			; 11111111
	db 0x93			; 10010011
	db 0xc9			; 11001001
	db 0xff			; 11111111
	db 0xff			; 11111111
	db 0x00			; 00000000

ship2_pattcol3: 

	db 0x00			; 00000000
	db 0xe0			; 11100000
	db 0xf0			; 11110000
	db 0x18			; 00011000
	db 0x98			; 10011000
	db 0xf0			; 11110000
	db 0xe0			; 11100000
	db 0x00			; 00000000
        
ship2_patt_end:


	; The ship has to be moved to RAM to be manipulated.
        ; These are the RAM addresses.
        
ship1_ram: equ 0x8000
ship1_patt_ram: equ ship1_ram + 4
ship1_pattcol1_ram: equ ship1_patt_ram
ship1_pattcol2_ram: equ ship1_pattcol1_ram + 8
ship1_pattcol3_ram: equ ship1_pattcol2_ram + 8

ship2_ram: equ ship1_pattcol3_ram + 8
ship2_patt_ram: equ ship2_ram + 4
ship2_pattcol1_ram: equ ship2_patt_ram
ship2_pattcol2_ram: equ ship2_pattcol1_ram + 8
ship2_pattcol3_ram: equ ship2_pattcol2_ram + 8