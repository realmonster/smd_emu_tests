; MIT License

; Copyright (c) 2017 realmonster <r57shell@uralweb.ru>

; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:

; The above copyright notice and this permission notice shall be included in all
; copies or substantial portions of the Software.

; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.



; Concept

; This ROM is for finding out m68k opcode sizes by testing the hardware.
; It utilizes internal specifics of exception processing in the m68k,
; especially that when the m68k fails to decode an opcode,
; PC is pushed onto the stack indicating the location of the error.
; If this happened at the first two parsed bytes,
; then PC pushed on the stack will be equal to
; the address of previous opcode plus the size of the opcode.
; So, all we have to do, is somehow guarantee the following:
; no partial decoding occurs and no valid opcodes starts after two bytes.

; Also, there are exceptions: opcodes that change the PC value internally.
; And, to be able to fallback into invalid opcode exception,
; supervisor stack pointer should be valid.
; Thus, we can't test some opcodes that modify stack pointer,
; unless we run the test under user mode. But I'm too lazy to implement this.

; Compilation

; Compiling with asm68k:
;   asm68k /p m68k_opcode_sizes.asm,m68k_opcode_sizes.bin

; Compiling with vasm motorola syntax.
; Replace all @ symbols with dots, then run:
;   vasmm68k_mot -no-opt -Fbin -o m68k_opcode_sizes.bin m68k_opcode_sizes.asm

; Implementation

; First part of code.
; Genesis initialization. No comments.

; Second part of code.
; 1) Tests opcodes and pushes results into VRAM at offset $0000
;    to avoid their corruptions and also to make results visible.
; 2) Runs nearly the same code, only this time to get a list of skipped opcodes.
;    Table of skipped opcodes located at $FF8000 in RAM
; 3) Receive all tiles from VRAM offset $0000 into RAM at offset $FF0000
;    for later use.
; 4) Show four tiles for each crc32 checks.
;    Two blank, one - crc barcode, and one: green if crc is valid, and red if not.
;    Two checks:
;    a) $FF0000 - $FF8000 : crc32 of opcode sizes
;    b) $FF8000 - $FFA000 : crc32 of skipped opcodes


; Third part of code.
; 0) Waits for controller input. A three-button controller works correctly.
;    If you use a six-button controller, then use Start, A, B, C buttons.
;    Other buttons will be bugged.
; 1) Show scaled tiles for 16 screens. It's not a scaled picture;
;    rather some other way of displaying the opcodes' size with 4x4 tiles.
;    After each screen, it waits for input, to let the user take a picture.
; 2) Shows raw tiles again, then jumps to step (0).
;    This is an infinite loop.

; Fourth part of code - helpers: crc32, joypad, fill_tile.

 org 0
 dc.l 0
 dc.l RESET
 dc.l BAD_INT ; Access Fault
 dc.l BAD_INT ; Address Error
 dc.l INVALID_OPCODE
 dc.l JUST_RTE ; Div By Zero
 dc.l JUST_RTE ; CHK, CHK2
 dc.l JUST_RTE ; FTRAPcc, TRAPcc, TRAPV
 dc.l BAD_INT ; Privelege Violation
 dc.l BAD_INT ; Trace
 dc.l INVALID_OPCODE ; Line 1010
 dc.l INVALID_OPCODE ; Line 1111
 dcb.l (32-12), BAD_INT
 dcb.l 16, JUST_RTE ; TRAP
 dcb.l (64-48), BAD_INT
 dc.b 'SEGA GENESIS    '
 dc.b 'ELEKTRO 2017.AUG'
 dc.b 'M68K OPCODE SIZES TEST                          '
 dc.b 'M68K OPCODE SIZES TEST                          '
 dc.b 'GM 00000000-00'
 dc.w $3587
 dc.b 'J               '
 dc.l 0, rom_end-1, $FF0000, $FFFFFF
 dcb.b $C,$20
 dc.b '            '
 dc.b 'Testing Utility                         '
 dc.b 'JUE             '

defaults:
 dc.w $8000   ; d5
 dc.w $3FFF   ; d6 M68k ram size for zero memset
 dc.w $100    ; d7
 dc.l $A00000 ; a0
 dc.l $A11100 ; a1
 dc.l $A11200 ; a2
 dc.l $C00000 ; a3
 dc.l $C00004 ; a4
 ; VDP regs
 dc.b 4   ; 0 palette select
 dc.b $14 ; 1 Display off, DMA on, GENESIS
 dc.b $38 ; 2 Plane A E000
 dc.b $3C ; 3 Window F000
 dc.b 7   ; 4 Plane B E000
 dc.b $44 ; 5 Sprite table at 8800
 dc.b 0   ; 6 Unused
 dc.b 0   ; 7 Backdrop color = 0
 dc.b 0,0 ; 8,9 Unused
 dc.b $FF ; A HBlank counter
 dc.b 0   ; B Full screen scroll
 dc.b $81 ; C Display width 320, No interlace
 dc.b $21 ; D HSCROLL table at 8200
 dc.b 0   ; E Unused
 dc.b 1   ; F Autoincrement 1
 dc.b 1   ; 10 Planes size 64x32
 dc.b 0   ; 11 Window x pos = disabled
 dc.b 0   ; 12 Window y pos = disabled
 dc.b $FF ; 13 DMA len low
 dc.b $FF ; 14 DMA len high
 dc.b 0   ; 15 DMA source low
 dc.b 0   ; 16 DMA source mid
 dc.b $80 ; 17 DMA source high -> FILL
 dc.l $40000080 ; DMA -> VRAM offset 0
 ; z80 A00000-A00026
z80_start:
 dc.l $AF01D91F, $11270021, $2600F977, $EDB0DDE1
 dc.l $FDE1ED47, $ED4FD1E1, $F108D9C1, $D1E1F1F9
 dc.w $F3ED, $5636, $E9E9
 dc.l $81048F02 ; display, vblank, dma OFF, rows 28, autoincrement 2
z80_end:
 dc.l $C0000000 ; CRAM from offset 0
 dc.l $40000010 ; VSRAM from offset 0
 dc.b $9F,$BF,$DF,$FF ; PSG mute

Palette:
 dc.w    0, $400, $800, $E00, $040, $080, $0E0, $004
 dc.w $008, $00E, $444, $888, $EEE, $404, $808, $E0E

init:
 dcb.l  16, $FFFFE000

test_opcode:
 movem.l (init).w,d0-d7/a0-a7
opcode:
 dc.w   0
 dcb.w  32,$F000
test_end:

RESET:
 tst.l   ($A10008).l
 bne.s   @portA
 tst.w   ($A1000C).l

@portA:
 bne.s   start
 lea     (defaults).w,a5
 movem.w (a5)+,d5-d7
 movem.l (a5)+,a0-a4
 move.b  -$10FF(a1),d0 ; read from A10000
 andi.b  #$F,d0
 beq.s   @reset_system
 move.l  #'SEGA',$2F00(a1)

@reset_system:
 moveq   #0,d0
 movea.l d0,a6
 move.l  a6,usp

 moveq   #$17,d1 ; set regs from 0 to $17

@vdp_registers:
 move.b  (a5)+,d5
 move.w  d5,(a4)
 add.w   d7,d5
 dbf     d1,@vdp_registers

 move.l  (a5)+,(a4) ; set VRAM FILL destination
 move.w  d0,(a3) ; trigger FILL
 move.w  d7,(a1) ; $100 -> A11100 z80 BUS request
 move.w  d7,(a2) ; $100 -> A11200 z80 RESET

@z80_bus_req:
 btst    d0,(a1)
 bne.s   @z80_bus_req

 moveq   #((z80_end-z80_start)/2)-1,d2

@z80_load:
 move.w  (a5)+,(a0)+
 dbf     d2,@z80_load
 move.w  d0,(a2) ; $100 -> A11200 z80 RESET off
 move.w  d0,(a1) ; $000 -> A11100 z80 BUS release
 move.w  d7,(a2) ; $100 -> A11200 z80 RESET

@clear_ram:
 move.l  d0,-(a6) ; clear M68k RAM
 dbf     d6,@clear_ram

 move.l  (a5)+,(a4)
 move.l  (a5)+,(a4) ; CRAM write
 moveq   #$1F,d3

@clear_cram:
 move.l  d0,(a3)
 dbf     d3,@clear_cram

 move.l  (a5)+,(a4) ; VSRAM write
 moveq   #$13,d4

@clear_vsram:
 move.l  d0,(a3)
 dbf     d4,@clear_vsram

 moveq   #3,d5

@mute_psg:
 move.b  (a5)+,$11(a3)
 dbf     d5,@mute_psg

 move.w  d0,(a2)
 movem.l (a6),d0-a6
 move    #$2700,sr

opcode_loc equ $FFFFFF80
counter equ $FFFFFFCA
result equ $FFFFFFCC
is_end equ $FFFFFFCE

start:
 lea     ($C00000).l,a1
 lea     4(a1),a2
 move.l  #$8F028144,(a2) ; Autoincrement 2 ; Display on, GENESIS

 move.l  #$C0000000,(a2) ; CRAM
 lea     (Palette).w,a0
 moveq   #$F,d0
@load_pal:
 move.w  (a0)+,(a1)
 dbf     d0,@load_pal

 move.l  #$60000003,(a2) ; E000
 moveq   #0,d3
 moveq   #((224/8)-1),d1
@loop_map_vert:
 moveq   #63,d0
@loop_map_row:
 move.w  d3,(a1)
 addq.w  #1,d3
 dbf     d0,@loop_map_row
 subi.w  #(64-(320/8)),d3
 dbf     d1,@loop_map_vert

 lea     (test_opcode).w,a0
 lea     (opcode_loc).w,a3
 moveq   #((test_end-test_opcode)/2-1),d0

@load_test:
 move.w  (a0)+,(a3)+
 dbf     d0,@load_test

 move.l  #$40000000,(a2)
 move.b  #4,(counter).w
 clr.b   (is_end).w
 jmp     (opcode_loc).w

checks:
 dc.w $F100, $6000 ; bra / bcc
 dc.w $FF00, $6100 ; bsr
 dc.w $F1F8, $51C8 ; dbcc
 dc.w $00C0 ; d4
 dc.w $DEF0 ; d5
 dc.w $003F ; d6
 dc.w $003D ; d7
 dc.w $F23F, $520F ; addq subq
 dc.w $EFC0, $2E40 ; movea
 dc.w $FFF8, $DEE8 ; adda

 dc.w $4E72 ; stop #imm
 dc.w $4E73 ; rte
 dc.w $4E75 ; rts
 dc.w $4E77 ; rtr
 dc.w $4EB9 ; jsr (xxx).l
 dc.w $4EF9 ; jmp (xxx).l

INVALID_OPCODE:
 move.w  4(sp),d1
 subi.w  #(opcode-test_opcode+opcode_loc),d1
FOR_STOP:
 tst.b   (is_end).w
 bne.w   OVERRIDE
 lsr.w   #1,d1
 cmpi.w  #$F,d1
 bcs.s   @set_result
 moveq   #$F,d1

@set_result:
 move.w  (result).w,d0
 lsl.w   #4,d0
 or.w    d1,d0
 move.w  d0,(result).w

 subq.b  #1,(counter).w
 bne.s   @next_test
 move.w  d0,($C00000).l
 move.b  #4,(counter).w

@next_test:
 move.w  (opcode-test_opcode+opcode_loc).w,d0
 addq.w  #1,d0
 move.w  d0,(opcode-test_opcode+opcode_loc).w
 beq.w   DONE

; d0 - opcode for skip-check
; LEN2, LEN4 = skip opcode
next_check:
 lea     (checks).w,a0
 move.w  d0,d1
 and.w   (a0)+,d1 ; $F100
 cmp.w   (a0)+,d1 ; $6000 ; bcc
 bne.s   @bcc_skip

@branch:
 tst.b   d0
 bne.s   LEN2 ; short
 bra.s   LEN4 ; word

@bcc_skip:
 move.w  d0,d1
 and.w   (a0)+,d1 ; $FF00
 cmp.w   (a0)+,d1 ; $6100 ; bsr
 beq.s   @branch

 move.w  d0,d1
 and.w   (a0)+,d1 ; $F1F8
 cmp.w   (a0)+,d1 ; $51C8 ; dbcc with condition false -> branch
 beq.s   LEN2

 movem.w (a0)+,d4-d7

 move.w  d0,d1
 and.w   (a0)+,d1 ; $F23F
 cmp.w   (a0)+,d1 ; $520F
 bne.s   skip_addq_subq
 move.w  d0,d1
 and.w   d4,d1 ; $C0
 beq.s   skip_addq_subq
 cmp.w   d4,d1 ; $C0
 beq.s   skip_addq_subq
 ;bra.s   LEN2 ; addq subq #odd, a7
 ;zero offset jump

LEN2:
 moveq  #2,d1
 bra.s  FOR_STOP

LEN4:
 moveq  #4,d1
 bra.s  FOR_STOP

skip_addq_subq:
 move.w  d0,d1
 and.w   (a0)+,d1 ; $EFC0
 cmp.w   (a0)+,d1 ; $2E40 ; movea <ea>,a7
 bne.s   @skip_movea
 move.w  d0,d1
 and.w   d6,d1 ; $3F
 cmp.w   d7,d1 ; $3D
 bcs.s   LEN2

@skip_movea:
 move.w  d0,d1
 and.w   (a0)+,d1 ; $FFF8
 cmp.w   (a0)+,d1 ; $DEE8 ; adda.w ($F000,an),a7
 beq.s   LEN4
 cmp.w   d5,d1    ; $DEF0 ; adda.w ($00,an,a7.w),a7
 beq.s   LEN4

 cmp.w   (a0)+,d0 ; $4E72 ; stop #imm
 beq.s   LEN4
 cmp.w   (a0)+,d0 ; $4E73 ; rte
 beq.s   LEN2
 cmp.w   (a0)+,d0 ; $4E75 ; rts
 beq.s   LEN2
 cmp.w   (a0)+,d0 ; $4E77 ; rtr
 beq.s   LEN2
 cmp.w   (a0)+,d0 ; $4EB9 ; jsr (xxx).l
 beq.s   LEN2
 cmp.w   (a0)+,d0 ; $4EF9 ; jmp (xxx).l
 beq.s   LEN2

; For Tests
; cmp.w  #$DEE8, d0
; bcs    @skip
; nop
;@skip:
 tst.b   (is_end).w
 bne.s   TESTED
 lea     ($FFFFE008).w,a0
 moveq   #0,d0
 move.l  d0,-(a0)
 move.l  d0,-(a0)
 move.l  d0,-(a0)
 move.l  d0,-(a0)
 move.l  d0,-(a0)
 move.l  d0,-(a0)
 andi.b  #$E0,ccr
 jmp     (opcode_loc).w

DONE:
 move.b  #1,(is_end).w
 clr.w   d0
 lea     ($FFFF8000).w,a0
 moveq   #8,d1
 moveq   #0,d2
next_check2:
 movem.l d0-d2/a0,-(sp)
 bra.w   next_check

OVERRIDE:
 moveq   #0,d3
 bra.s   set_skip_val

TESTED:
 moveq   #1,d3

set_skip_val:
 movem.l (sp)+,d0-d2/a0
 add.w   d2,d2
 or.w    d3,d2
 subq.w  #1,d1
 bne.s   @skip_set
 move.b  d2,(a0)+ ; set val
 moveq   #8,d1
@skip_set:
 addq.w  #1,d0
 bne.s   next_check2

DONE2:
 bsr.w   crc32_build
 lea     ($C00000).l,a1
 lea     ($FF0000).l,a0
 movea.l a0,a2
 movea.l a0,a3

 move.l  #0,4(a1)
 move.w  #$1FFF,d0
@loop:
 move.l  (a1),(a2)+ ; receive lengths from VRAM
 dbf     d0,@loop

 move.l  #$40000002,4(a1) ; VRAM $8000

 ; opcode lengths crc32
 ; a0 = 0xFF0000 - RAM start
 move.w  #$8000,d1
 move.l  #$5C6DA501,d2
 bsr.w   crc_check

 ; opcode skip crc32
 lea     ($FFFF8000).w,a0
 move.w  #$2000,d1
 move.l  #$20AC2324,d2
 bsr.w   crc_check

 bsr.s   joy_init

@reset_loop:
 bsr.s   joy_await
 movea.l a3,a0
 moveq   #16-1,d5
@loop2:
 move.l  #$40000000,4(a1)
 move.w  #($800)-1,d4
@loop1:
 moveq   #0,d3
 move.b  (a0)+,d3 ; d3 = ab
 ror.l   #4,d3 ; 0a
 move.w  d3,d1 ; 0a
 lsl.w   #4,d3 ; a0
 or.w    d3,d1 ; aa
 move.w  d1,d3 ; aa
 lsl.w   #8,d1 ; aa00
 or.w    d3,d1 ; aaaa
 swap    d1
 swap    d3
 move.w  d3,d1 ; b000
 lsr.w   #4,d3 ; bb00
 or.w    d3,d1 ; bb00
 move.w  d1,d3 ; bb00
 lsr.w   #8,d1 ; 00bb
 or.w    d3,d1 ; bbbb
 move.l  d1,(a1)
 move.l  d1,(a1)
 move.l  d1,(a1)
 move.l  d1,(a1)
 dbf     d4,@loop1
 bsr.s   joy_await
 dbf     d5,@loop2
 move.l  #$40000000,4(a1)
 move.w  #($8000/4)-1,d4
 movea.l a3,a0
@loop_small:
 move.l  (a0)+,(a1)
 dbf     d4,@loop_small
 bra.s   @reset_loop

; ==== JOYPAD ====

joy_data equ $FFFFA000

; d0 - trash
joy_init:
 moveq   #$40,d0
 move.b  d0,($A10009).l ; reset joypad
 move.b  d0,($A10003).l
 move.b  #$FF,(joy_data).w
 rts

; a2/d0-d2 - trash
joy_await:
 ;movem.l d0-d2/a2,-(sp)
 lea     ($A10003),a2

@loop:
 moveq   #$40,d2
 moveq   #0,d1
 move.b  d2,(a2)
 move.b  (a2),d0
 move.b  d1,(a2)
 andi.b  #$3F,d0
 move.b  (a2),d1
 move.b  d2,(a2)
 add.b   d1,d1
 add.b   d1,d1
 andi.b  #$C0,d1
 or.b    d1,d0
 move.b  (joy_data).w,d1
 move.b  d0,(joy_data).w
 and.b   d0,d1
 eor.b   d0,d1
 beq.s   @loop

 ;movem.l (sp)+,d0-d2/a2
 rts

; ==== CRC32 ====

crc32_table equ $FFFFA010

; d0-d3/a0 - trash
crc32_build:
 ;movem.l d0-d3/a0,-(sp)
 lea     (crc32_table+(256*4)).w,a0
 move.l  #$EDB88320,d3
 move.l  #255,d2
@one:
 move.l  d2,d0
 moveq   #7,d1
@calc:
 lsr.l   d0
 bcc.s   @skip
 eor.l   d3,d0
@skip:
 dbf     d1,@calc
 move.l  d0,-(a0)
 dbf     d2,@one

 ;movem.l (sp)+,d0-d3/a0
 rts

; a0   src
; a2   trash
; d1.w len -> trash
; d0.l result
; d4.l trash
crc32_calc:
 ;move.l  a2,-(sp)
 ;move.l  d4,-(sp)
 lea     (crc32_table).w,a2
 moveq   #-1,d0
 bra.s   @check

@loop:
 clr.w   d4
 move.b  (a0)+,d4
 eor.b   d0,d4
 lsr.l   #8,d0
 add.w   d4,d4
 add.w   d4,d4
 move.l  (a2,d4.w),d4
 eor.l   d4,d0

@check:
 dbf     d1,@loop

 ;move.l  (sp)+,d4
 ;move.l  (sp)+,a2
 not.l   d0
 rts

; a0 - src
; a1 - $C00000
; d1 - length -> trash
; d2 - correct crc32
; d0 - result
; d4 - trash
crc_check:
 moveq   #0,d0

 bsr.s   fill_tile
 bsr.s   crc32_calc
 bsr.s   fill_tile

 move.l  d0,d3
 moveq   #0,d0
 bsr.s   fill_tile

 move.l  #$66666666,d0 ; correct
 cmp.l   d2,d3
 beq.s   fill_tile
 move.l  #$99999999,d0 ; incorrect
 ; continue

; a1   - C00000
; d0.l - data
; d4.w - trash
fill_tile:
 moveq   #7,d4
@crc_loop1:
 move.l  d0,(a1)
 dbf     d4,@crc_loop1
 rts

JUST_RTE:
 rte

BAD_INT:
 lea     ($C00000).l,a1
 move.l  #$40000002,4(a1)
 move.w  (opcode-test_opcode+opcode_loc).w,d0
 move.w  d0,d1
 swap    d0
 move.w  d1,d0
 moveq   #0,d1
 movea.l d1,sp
 bsr.s   fill_tile
@loop:
 bra.s   @loop

 dcb.b $10000-*,0
rom_end:
