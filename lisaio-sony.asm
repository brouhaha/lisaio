; Lisa I/O board firmware for Sony 3.5-inch disk drives
; Original code by Apple Computer
; Disassembly Copyright 1993, 1997, 2000, 2019 Eric Smith

; This version is for use on the earlier Lisa I/O board:
;
;			Original	Newer
;			I/O Board	I/O Board
;			--------------	-----------------------
;	hardware ID	$a8		$88
;	used in		Lisa, Lisa 2/5	Lisa 2/10, Macintosh XL
;
;	controller	TTL chip	IWM ASIC
;	circuitry	and state PROM
;
;	NiCd battery	yes		no
;	for memory
;	backup

; Note: the original I/O board was designed for use with the Twiggy
; 5.25-inch drive.  To use it with the Sony 400K single-sided 3.5-inch
; drive, a "Lisa Lite" adapter board is installed in the drive cage to
; generate the PWM motor speed control signal and to adapt the pinout.
; The Sony double-sided 800K drives do not require the motor speed
; signal, so they may work without a Lisa Lite adapter if a suitably
; wired cable is used.


		ifndef	new_io
new_io		equ	0
		endif


; This source file can be assembled for either the standard 400K drive
; version, or the later 800K (dual-sided) version, by changing the value
; of sony_800k.

		ifndef	sony_800k
sony_800k	equ	0
		endif


fillto	macro	addr, val
	while	* < addr
size	set	addr-*
	if	size > 256
size	set	256
	endif
	fcb	[size] val
	endm
	endm


		if	new_io && sony_800k
fill_byte	equ	$ff
		else
fill_byte	equ	$00
		endif


		if	new_io
hardware_id	equ	$88
		else
hardware_id	equ	$a8
		endif


max_track	equ	$4f	; tracks $00 .. $4f
max_sect	equ	$0b	; sectors $00 .. $0b
max_zone	equ	$04

; command codes

cmd_completed		equ	$00

cmd_handshake		equ	$80
cmd_rwts		equ	$81
cmd_seek		equ	$83
cmd_call		equ	$84
cmd_clr_int_stat	equ	$85
cmd_drive_enable	equ	$86
cmd_drive_disable	equ	$87
cmd_rom_wait		equ	$88
cmd_go_away		equ	$89

; rwts function codes

rwts_fcn_read		equ	$00
rwts_fcn_write		equ	$01
rwts_fcn_unclamp	equ	$02
rwts_fcn_format		equ	$03
rwts_fcn_verify		equ	$04
rwts_fcn_fmt_trk	equ	$05
rwts_fcn_vfy_trk	equ	$06
rwts_fcn_read_brute	equ	$07
rwts_fcn_write_brute	equ	$08
rwts_fcn_clamp		equ	$09

max_rwts_fcn		equ	rwts_fcn_clamp

; error status codes

noerr		equ	$00
err_badcmd	equ	$01
err_baddrive	equ	$02
err_badside	equ	$03
err_badsect	equ	$04
err_badtrack	equ	$05
err_badmask	equ	$06
err_nodisk	equ	$07
err_drvdis	equ	$08
err_intpend	equ	$09
err_invfmt	equ	$0a
err_romerr	equ	$0b
err_bad_int	equ	$0c

err_wprot	equ	$14
err_cant_vfy	equ	$15
err_cant_clamp	equ	$16
err_cant_read	equ	$17
err_cant_write	equ	$18
err_cant_uncl	equ	$19
err_cant_cal	equ	$1a
err_cant_setspd	equ	$1b
err_cant_wrcal	equ	$1c
err_wr_underrun	equ	$1f



; $00 .. $0f: IOB

		org	$00

iob:
iob_cmd:	rmb	1
		rmb	6
iob_fmt_conf:	rmb	1
iob_errstat:	rmb	1
iob_diskid:	rmb	1

iob_side_cnt:	rmb	1	; $01 for single-sided, $02 for double-sided
D0b:		rmb	1
D0c:		rmb	1
D0d:		rmb	1

		rmb	2	; $0e .. $0f apparently unused


; $10 .. $1c: drive parameters

		org	$10

drive_parms:
zone_spd:	rmb	max_zone+1	; indexed by drv_zone
D15:		rmb	1
D16:		rmb	1
montime:	rmb	1	; motor on time, copied to high byte of mtimer
		rmb	1	; hardware ID
max_retry:	rmb	1
D1a:		rmb	1
D1b:		rmb	1
D1c:		rmb	1

		rmb	3	; $1d .. $1f apparently unused


		org	$20

disk_clamped:	rmb	1
motor_on:	rmb	1
drv_trk:	rmb	1
drv_zone:	rmb	1
D24:		rmb	1
format_type:	rmb	1	; AKA volume
retry_cnt:	rmb	1
recal_cnt:	rmb	1
D28:		rmb	1
D29:		rmb	1

cmd_save:	rmb	1	; temp in command dispatch

format_gap:	rmb	1

int_mask:	rmb	1	; bitmap of pending interrupts

		rmb	1	; $2d apparently unused

int_pend:	rmb	1	; bitmap of intterupt mask

int_flags:	rmb	1


; $30 .. $37: IIOB (Internal IOB)

		org	$30

iiob:		rmb	1

iiob_arg:
iiob_fcn:	rmb	1	; rwts function

iiob_drv:	rmb	1
iiob_side:	rmb	1
iiob_sect:	rmb	1
iiob_trk:	rmb	1

		rmb	2

addr_mark:	rmb	5

misc_nib_4:	rmb	1
misc_nib_5:	rmb	1

		rmb	1	; $3f apparently unused

mtimer:		rmb	3	; motor on timer

data_ptr:	rmb	2

D45:		rmb	1
D46:		rmb	1

misc_nib_3:	rmb	1

errcnt_tbl:
D48:		rmb	1	; bitslip (data field header)
D49:		rmb	1	; bitslip (data field trailer)
D4a:		rmb	1	; checksum
D4b:		rmb	1	; bitslip (address field header)
D4c:		rmb	1	; bitslip (address field trailer)
D4d:		rmb	1	; wrong sector
D4e:		rmb	1	; wrong track
D4f:		rmb	1	; addr field checksum
errcnt_tbl_size	equ	*-errcnt_tbl

; an address field read from disk
hdr_buf:	rmb	1
hdr_vol:	rmb	1
hdr_side:	rmb	1
hdr_sect:	rmb	1
hdr_trk:	rmb	1

hdr_csum:	rmb	1	; temp during find_addr

D56:		rmb	1
D57:		rmb	1
D58:		rmb	1
D59:		rmb	1

		rmb	2	; $5a .. $5b apparently unused

D5c:		rmb	1
D5d:		rmb	1

misc_nib_1:	rmb	1
misc_nib_2:	rmb	1
misc_nib_6:	rmb	1

chksum:		rmb	3
chksum2:	rmb	3

fmt_sect_cnt:	rmb	1
brute_flag:	rmb	1	;  read brute-force flag
D69:		rmb	1

		rmb	1	; $6a apparently unused
D6b:		rmb	1
D6c:		rmb	1
D6d:		rmb	1
D6e:		rmb	1
D6f:		rmb	1

data_mark:	rmb	5

fmt_sect_num:	rmb	2	; two counters used for 2:1 interleave
fmt_even_odd:	rmb	1	; format even/odd sector flag, used to
				;   index fmt_sect_num
fmt_sect_cntr	rmb	1

D79:		rmb	1

		rmb	1	; $7a apparently unused

D7b:		rmb	1

		rmb	1	; $7c apparently unused

; $80 .. $bf are used as an IOB trace history buffer

trace_buf_idx:	rmb	1	; index into trace buffer
trace_buf_ptr:	rmb	2	; pointer to base of trace buffer

		org	$80
trace_buf_size	equ	$40

trace_buf_base:	rmb	trace_buf_size

; $c0 .. $ff are used by the Lisa as non-volatile storage

D0100	equ	$0100

; stack starts at $1cf and grows toward $100

bad_sec_total	equ	$01d0
error_track_num	equ	$01d1
error_sect_num	equ	$01d2
bad_sect_map	equ	$01d3	; max_sect bytes, from here up to ???

; data buffer from $01f4 to $03ff

D01f4	equ	$01f4
D0200	equ	$0200
D0300	equ	$0300


		if	hardware_id==$88
iobase		equ	$0800
		else
iobase		equ	$0400
		endif

ca0_low		equ	iobase+$00	; phase0 is also used for PWM data
ca0_high	equ	iobase+$01
ca1_low		equ	iobase+$02
ca1_high	equ	iobase+$03
ca2_low		equ	iobase+$04
ca2_high	equ	iobase+$05
lstrb_low	equ	iobase+$06
lstrb_high	equ	iobase+$07
ioport_08	equ	iobase+$08
ioport_09	equ	iobase+$09
ioport_0a	equ	iobase+$0a
ioport_0b	equ	iobase+$0b
q6l		equ	iobase+$0c
q6h		equ	iobase+$0d
q7l		equ	iobase+$0e
q7h		equ	iobase+$0f
ioport_10	equ	iobase+$10
ioport_13	equ	iobase+$13
pwm_clk_low	equ	iobase+$16
pwm_clk_high	equ	iobase+$17
mem_68k_ena	equ	iobase+$18
mem_68k_dis	equ	iobase+$19
ioport_1a	equ	iobase+$1a
ioport_1c	equ	iobase+$1c
ioport_1d	equ	iobase+$1d
int_68k_dis	equ	iobase+$1e
int_68k_ena	equ	iobase+$1f

		if	new_io==1
pwm_reg		equ	iobase+$20	; new I/O board has directly
					; writable PWM register
		endif

	org	$1000

; four copies of nibble table (avoids a masking step)

nib_tab:
	fcb	$96,$97,$9a,$9b,$9d,$9e,$9f,$a6
	fcb	$a7,$ab,$ac,$ad,$ae,$af,$b2,$b3
	fcb	$b4,$b5,$b6,$b7,$b9,$ba,$bb,$bc
	fcb	$bd,$be,$bf,$cb,$cd,$ce,$cf,$d3
	fcb	$d6,$d7,$d9,$da,$db,$dc,$dd,$de
	fcb	$df,$e5,$e6,$e7,$e9,$ea,$eb,$ec
	fcb	$ed,$ee,$ef,$f2,$f3,$f4,$f5,$f6
	fcb	$f7,$f9,$fa,$fb,$fc,$fd,$fe,$ff

	fcb	$96,$97,$9a,$9b,$9d,$9e,$9f,$a6
	fcb	$a7,$ab,$ac,$ad,$ae,$af,$b2,$b3
	fcb	$b4,$b5,$b6,$b7,$b9,$ba,$bb,$bc
	fcb	$bd,$be,$bf,$cb,$cd,$ce,$cf,$d3
	fcb	$d6,$d7,$d9,$da,$db,$dc,$dd,$de
	fcb	$df,$e5,$e6,$e7,$e9,$ea,$eb,$ec
	fcb	$ed,$ee,$ef,$f2,$f3,$f4,$f5,$f6
	fcb	$f7,$f9,$fa,$fb,$fc,$fd,$fe,$ff

	fcb	$96,$97,$9a,$9b,$9d,$9e,$9f,$a6
	fcb	$a7,$ab,$ac,$ad,$ae,$af,$b2,$b3
	fcb	$b4,$b5,$b6,$b7,$b9,$ba,$bb,$bc
	fcb	$bd,$be,$bf,$cb,$cd,$ce,$cf,$d3
	fcb	$d6,$d7,$d9,$da,$db,$dc,$dd,$de
	fcb	$df,$e5,$e6,$e7,$e9,$ea,$eb,$ec
	fcb	$ed,$ee,$ef,$f2,$f3,$f4,$f5,$f6
	fcb	$f7,$f9,$fa,$fb,$fc,$fd,$fe,$ff

	fcb	$96,$97,$9a,$9b,$9d,$9e,$9f,$a6
	fcb	$a7,$ab,$ac,$ad,$ae,$af,$b2,$b3
	fcb	$b4,$b5,$b6,$b7,$b9,$ba,$bb,$bc
	fcb	$bd,$be,$bf,$cb,$cd,$ce,$cf,$d3
	fcb	$d6,$d7,$d9,$da,$db,$dc,$dd,$de
	fcb	$df,$e5,$e6,$e7,$e9,$ea,$eb,$ec
	fcb	$ed,$ee,$ef,$f2,$f3,$f4,$f5,$f6
	fcb	$f7,$f9,$fa,$fb,$fc,$fd,$fe,$ff


D1100:	fcb	$03,$23,$02,$e2,$02,$9c,$02,$57
	fcb	$02,$15,$03,$43,$02,$fe,$02,$b7
	fcb	$02,$70,$02,$2b,$03,$30,$02,$ed
	fcb	$02,$a7,$02,$62,$02,$1e,$03,$36
	fcb	$02,$f3,$02,$ad,$02,$66,$02,$22


; argument validation mask bits


vb_t0s0		equ	$80
vb_wr_en	equ	$40
vb_fmt		equ	$20
vb_mask		equ	$10
vb_track	equ	$08
vb_sect		equ	$04
vb_side		equ	$02
vb_drive	equ	$01


rwts_cmd_val_mask:
	fcb	vb_track + vb_sect + vb_side + vb_drive	; read
	fcb	vb_wr_en + vb_track + vb_sect + vb_side + vb_drive	; write
	fcb	vb_drive		; unclamp
	fcb	vb_t0s0 + vb_wr_en + vb_fmt + vb_track + vb_side + vb_drive	; format disk  (don't really need vb_track or vb_side)
	fcb	vb_t0s0 + vb_track + vb_side + vb_drive		; verify disk (don't really need vb_track or vb_side)
	fcb	vb_wr_en + vb_fmt + vb_track + vb_side + vb_drive	; format track
	fcb	vb_track + vb_side + vb_drive		; verify track
	fcb	vb_track + vb_sect + vb_side + vb_drive	; read (no checksum)
	fcb	vb_wr_en + vb_track + vb_sect + vb_side + vb_drive	; write (no checksum)
	fcb	$00		; clamp


gen_cmd_val_mask:
	fcb	vb_track + vb_side + vb_drive		; seek
	fcb	$00		; go
	fcb	$00		; clr int stat
	fcb	vb_mask		; set int mask
	fcb	vb_mask		; clr int mask
	fcb	$00		; test
	fcb	$00		; halt


def_drv_parm_tbl:
	fcb	$d5,$c0,$a7,$89,$64	; default motor speed indexed by zone
	fcb	$1e
	fcb	$04
	fcb	$09		; default motor on time
	fcb	hardware_id
	fcb	$64		; default retry count
	fcb	$02
	fcb	$82
	fcb	$4f
def_drv_parm_tbl_size	equ	*-def_drv_parm_tbl


; sectors per track by zone
spt_tab:	fcb	$0c,$0b,$0a,$09,$08


def_addr_mark:
	fcb	$d5,$aa,$96,$de,$aa

def_data_mark:
	fcb	$d5,$aa,$ad,$de,$aa


; figure out which zone contains the current track number

getzone:
	lda	iiob_trk
	ldy	#max_zone
L1159:	cmp	zone_trk_tbl,y
	bcs	L1161
	dey
	bne	L1159
L1161:	rts

; zone starting track numbers
zone_trk_tbl:
	fcb	$00,$10,$20,$30,$40


; given three bytes in A, X, and Y, of the form
;   aa------, xx------, and yy------
; pack the most significant two bits of each byte into one byte
; of the form
;       00yyxxaa
; and look up the result in the nibble table, and return it in A

pack_ms_nib:
	lsr	a
	lsr	a
	sta	D6e
	txa
	and	#$c0
	ora	D6e
	lsr	a
	lsr	a
	sta	D6e
	tya
	and	#$c0
	ora	D6e
	lsr	a
	lsr	a
	tax
	lda	nib_tab,x
	rts

	fillto	$1196,fill_byte

denib_tab	equ	$1100

; note that valid entries are in the range $00..$3f
; invalid entries contain their own index, which is greater than $80

	fcb	$00,$01
	fcb	$98,$99,$02,$03,$9c,$04,$05,$06
	fcb	$a0,$a1,$a2,$a3,$a4,$a5,$07,$08
	fcb	$a8,$a9,$aa,$09,$0a,$0b,$0c,$0d
	fcb	$b0,$b1,$0e,$0f,$10,$11,$12,$13
	fcb	$b8,$14,$15,$16,$17,$18,$19,$1a
	fcb	$c0,$c1,$c2,$c3,$c4,$c5,$c6,$c7
	fcb	$c8,$c9,$ca,$1b,$cc,$1c,$1d,$1e
	fcb	$d0,$d1,$d2,$1f,$d4,$d5,$20,$21
	fcb	$d8,$22,$23,$24,$25,$26,$27,$28
	fcb	$e0,$e1,$e2,$e3,$e4,$29,$2a,$2b
	fcb	$e8,$2c,$2d,$2e,$2f,$30,$31,$32
	fcb	$f0,$f1,$33,$34,$35,$36,$37,$38
	fcb	$f8,$39,$3a,$3b,$3c,$3d,$3e,$3f


; format a sector
; two entry points!

format_sect_0:
	ldy	#$00
	beq	L1206

format_sect_n:
	ldy	format_gap

L1206:	bit	iiob_trk	; track nibble only can store 0..63,
	bvc	L1210		;   so bit 7 goes into bit 0 of side nibble
	lda	#$01
	ora	iiob_side
	sta	iiob_side

	if	new_io==0

L1210:	jsr	write_sync_pat

	lda	addr_mark	; write address mark header
	jsr	S126c
	lda	addr_mark+1
	jsr	S126c
	lda	addr_mark+2
	jsr	S126c

	ldx	iiob_trk	; write track/sector/head/volume
	jsr	S1260
	ldx	iiob_sect
	jsr	S1260
	ldx	iiob_side
	jsr	S1260
	ldx	format_type
	jsr	S1260

	lda	iiob_trk	; compute and write checksum
	eor	iiob_sect
	eor	iiob_side
	eor	format_type
	tax
	lda	nib_tab,x
	sta	q6h
	sta	q6l
	nop
	nop
	nop

	lda	addr_mark+3	; write address mark trailer
	jsr	S126c
	lda	addr_mark+4
	jsr	S126c
	lda	D46
	jsr	S126c

	jsr	S1bd3
	jmp	write_data_field

S1260:	lsr	D6e
	lda	nib_tab,x

S1265:	sta	q6h
	sta	q6l

S126b:	rts

S126c:	nop
	nop
	clc
	bcc	S1265


; find an address field
; reads address field into hdr_buf
find_addr:
	jsr	S13cd
	ldy	#$00
	ldx	#$08
	lda	q6l
L127b:	iny
	bne	L1281
	dex
	beq	L12ec
L1281:	lda	q7l
	bpl	L1281

	endif

	if	new_io==1

L1210:	lda	#$01
	jsr	write_sync_pat
	lda	addr_mark+2
	jsr	S1aec
	ldy	#$02
L121c:	ldx	iiob_side,y
	jsr	S1ae9
	dey
	bpl	L121c
	ldx	format_type
	jsr	S1ae9
	lda	iiob_trk
	eor	iiob_sect
	eor	iiob_side
	eor	format_type
	jsr	S1ae8
	lda	addr_mark+3
	jsr	S1aec
	lda	addr_mark+4
	jsr	S1aec
	jsr	S1b93
	lda	#$1e
	bcs	L1248
	jmp	write_data_field
L1248:	rts

; find an address field
; reads address field into hdr_buf
find_addr:
	lda	#iobase&$ff
	sta	D5c
	lda	#iobase>>8
	sta	D5c+1
	jsr	$139b		; XXX set_r_mode
	lda	q6l
L127b:	inc	D5c
	bne	L125f
	dec	D5c+1
	beq	L12ec
L125f:	ldx	#$00
L1261:	lda	q7l
	bmi	L1286
	dex
	bne	L1261
	beq	L12ec
	endif

L1286:	cmp	addr_mark
	bne	L127b
	if	new_io==0
	stx	D5d
	endif
L128c:	lda	q7l
	bpl	L128c
	cmp	addr_mark+1
	bne	L1286
	if	new_io==0
	sty	D5c
	endif
L1297:	lda	q7l
	bpl	L1297
	cmp	addr_mark+2
	bne	L1286
	ldx	#$04
	lda	#$00
L12a4:	sta	hdr_csum
L12a6:	ldy	q7l
	bpl	L12a6
	lda	denib_tab,y
	sta	hdr_buf,x
	eor	hdr_csum
	dex
	bpl	L12a4
	tax
	bne	L12fb

L12b8:	lda	q7l	; check for the trailer
	bpl	L12b8
	cmp	addr_mark+3
	bne	L12f1

	lda	#$01		; bit 0 of side nibble is really bit 7 of
	bit	hdr_side	; track nibble
	beq	L12cd
	lda	#$40
	ora	hdr_trk
	sta	hdr_trk

L12cd:	lda	q7l
	bpl	L12cd
	cmp	addr_mark+4
	bne	L12f1

	lda	iiob_trk	; right track?
	cmp	hdr_trk
	bne	L1300

	lda	iiob_sect	; right sector?
	cmp	hdr_sect
	bne	L12f6

	clc
	lda	hdr_vol
	sta	iob_diskid
L12e7:	clv
	lda	q6h
	rts

L12ec:	inc	D4b		; inc. too many retries
	sec
	bcs	L12e7

L12f1:	inc	D4c		; inc. trailer error
	sec
	bcs	L12e7

L12f6:	inc	D4d		; inc. wrong sector count
	sec
	bcs	L12e7

L12fb:	inc	D4f		; inc. address field checksum error
	sec
	bcs	L12e7

L1300:	inc	D4e		; inc. wrong track 
	sec
	bcs	L12e7


select_side:
	ldx	#$00
	lda	#$20
	bit	iiob_side
	if	new_io==0
	bne	L130e
	else
	beq	L130e
	endif
	inx
L130e:	lda	ca0_high
	lda	ca1_high
	if	new_io
	lda	ioport_1a,x	; select side
	else
	lda	ioport_08,x	; select side
	endif
	if	new_io==1
	txa
	endif
	rts


select_side_0:
	if	new_io
	ldx	#$00
	beq	L130e
	else
	ldx	#$01
	bne	L130e
	endif


select_side_1:
	if	new_io
	ldx	#$01
	bne	L130e
	else
	ldx	#$00
	beq	L130e
	endif


; returns with carry clear if disk is write-protected

get_wr_prot:
	jsr	select_side_1
L1323:	lda	ca1_low
L1326:	lda	ca2_low

S1329:	lda	q6h
	lda	q7l
	asl	a
	rts


S1331:	clc
L1332:	jsr	select_side_1
	lda	ca0_low
	bcc	L1326
	bcs	L1323

S133c:	sec
	bcs	L1332

S133f:	sta	lstrb_high
	nop
	nop
	sta	lstrb_low
	rts

S1348:	sta	ca2_high
	lda	#$00
	beq	L1354

S134f:	sta	ca2_low
	lda	#$ff
L1354:	sta	motor_on
	jsr	select_side_0
	sta	ca0_low
	jmp	S133f

S135f:	jsr	S134f
	lda	#$50
	jsr	long_delay

S1367:	jsr	select_side_1
	lda	ca2_low
	rts

S136e:	stx	D59

S1370:	jsr	select_side_0
	sta	ca0_low
	sta	ca1_low
	ldx	D59
	sta	ca2_low,x
	jmp	S133f

S1381:	lda	#$01
	sta	D6e
	jsr	select_side_0
	sta	ca1_low
	sta	ca2_low
	jsr	S133f
L1391:	jsr	S1dbf
	jsr	S1329
	bcs	L13a1
	lda	#$0f
	sta	D0d
	dec	D6e
	bpl	L1391
L13a1:	rts

S13a2:	jsr	S1370
L13a5:	jsr	S1381
	dec	D58
	bne	L13a5
	rts


; set_drive_speed determines the current zone, and sends the appropriate
; PWM speed control byte to the Lisa Lite card.
;
; set_drive_speed_y assumes that the current zone is in Y.

set_drive_speed:
	jsr	getzone

set_drive_speed_y:
	sty	drv_zone
	lda	zone_spd,y

	if	new_io==0

	ldy	#$07
L13b7:	sta	ca0_low
	asl	a
	bcc	L13c0
	sta	ca0_high
L13c0:	sta	pwm_clk_high
	sta	pwm_clk_low
	dey
	bpl	L13b7
	sta	ca0_high

	else

	sta	pwm_reg

	endif

	rts


S13cd:	jsr	select_side
	sta	ca0_low
	sta	ca1_low
	sta	ca2_high
	rts


unclamp_cmd:
	jsr	select_side_0
	sta	ca2_high
	lda	lstrb_high
	lda	#$96
	jsr	long_delay
	lda	lstrb_low
	lda	#$00
	sta	disk_clamped
	clc
	rts

	if	new_io==0
S13f1:	ldx	#$1e
L13f3:	sta	ca0_low,x
	dex
	dex
	bpl	L13f3
	lda	q6h
	sta	ioport_13
	ldy	#$00
	jmp	set_drive_speed_y
	endif

	if	new_io==1
S13f1:	ldx	#$0e
L13c1:	sta	ca0_low,x
	dex
	dex
	bpl	L13c1
	lda	#$ca
	jsr	$1da0	; XXX
	ldx	#$00
	stx	D6e
L13d1:	dex
	bne	L13d8
	dec	D6e
	beq	L1407
L13d8:	lda	q6h
	lda	q7l
	tay
	and	#$20
	bne	L13d1
	tya
	and	#$1f
	cmp	#$1f
	beq	L13f5
	lda	#$1f
	sta	q7h
	lda	q7l
	jmp	L13d1
L13f5:	sta	ioport_0b
	sta	ioport_09
	ldx	zone_spd
	stx	ioport_10
	stx	pwm_reg
	stx	pwm_clk_high
	rts
L1407:	lda	#$0e
	sta	iob_errstat
	jmp	$14dd	; XXX
	endif

; check whether the drive is double-sided
; returns with 1 or 2 in iob_side_cnt

test_dbl_side:
	lda	#$01
	sta	iob_side_cnt
	jsr	select_side_0
	sta	ca2_high
	sec
	lda	q7l
	bmi	L1428
	dec	D24
	clc
	sta	ca0_low
	lda	#$02
	ldx	q7l
	bpl	L1426
	inc	iob_side_cnt
	ora	#$20
L1426:	sta	format_type
L1428:	rts


reset:	cld
	sei
L142b:	sta	ioport_1c
	sta	int_68k_dis
	sta	mem_68k_ena

	ldx	#$cf		; init stack
	txs

	jsr	S13f1

	if	new_io
	jsr	S14a4
	else
	nop
	nop
	nop
	endif

	if	(new_io==0)&(sony_800k==1)
	nop
	nop
	nop
	else
	jsr	S14e4
	endif

	lda	#$00
	jsr	long_delay

	if	new_io==0
	lda	#$00
	jsr	long_delay
	endif

	ldx	#$bf		; clear RAM from $00 .. $bf
	lda	#$00
L144e:	sta	$0000,x
	dex
	bne	L144e

	sta	iob_cmd		; command done

; initialize drive parms
	ldx	#def_drv_parm_tbl_size	; BUG!!! should be def_drv_parm_tbl_size-1
L1457:	lda	def_drv_parm_tbl,x
	sta	drive_parms,x
	dex
	bpl	L1457

	ldx	#$04		; init default addr and data marks
L1461:	lda	def_addr_mark,x
	sta	addr_mark,x
	lda	def_data_mark,x
	sta	data_mark,x
	dex
	bpl	L1461

	dec	D46
	ldx	#trace_buf_size-1
	stx	trace_buf_idx
	ldx	#trace_buf_base&$ff
	stx	trace_buf_ptr
	lda	montime
	sta	mtimer+2
	bne	L1488

idle:	inc	D28		; update counters
	dec	mtimer
	bne	L1496
	dec	mtimer+1
	bne	L1496
L1488:	jsr	S14e5
	dec	mtimer+2
	bne	L1496
	jsr	S1348		; turn off motor ???
	lda	montime
	sta	mtimer+2

L1496:	lda	iob_cmd		; is there a command to execute?
	bpl	idle
	jsr	docmd
	jmp	idle


S14a4:	rts

; start of unreferenced code !!!
	lda	#$00
	tay
	sta	D5c
	sta	D5d
	sta	data_ptr
	lda	#$10
	sta	data_ptr+1
	tax
foo1:	clc
	iny
	lda	D5c
	adc	(data_ptr),y
	sta	D5c
	dey
	lda	D5d
	adc	(data_ptr),y
	sta	D5d
	clc
	bpl	foo2
	sec
foo2:	rol	D5c
	rol	D5d
	iny
	iny
	bne	foo1
	inc	data_ptr+1
	dex
	bne	foo1
	lda	D5c
	bne	foo3
	lda	D5d
	beq	S14e4
foo3:	lda	#$0b
	sta	D0b
	sta	mem_68k_ena
	sta	ioport_1d
	jmp	L17de
; end of unreferenced code !!!


S14e4:	rts

S14e5:	lda	D24
	bne	L14ee
	jsr	test_dbl_side
	bcs	L1515
L14ee:	sta	ioport_1c
	jsr	S133c
	bcs	L1511
	lda	disk_clamped
	bne	L1515
	lda	#$c8
	jsr	long_delay
	jsr	S1d54
	dec	disk_clamped
	jsr	S1722
	bcc	L1515
	lda	#$10
	ora	int_flags
	sta	int_flags
	bne	L1515
L1511:	lda	#$00
	sta	disk_clamped
L1515:	sta	ioport_1d
L1518:	lda	int_flags
	and	#$70
	beq	L1522
	ora	#$80
	sta	int_flags
L1522:	and	int_mask
	sta	int_pend
	beq	L152c
	sta	int_68k_ena
	rts
L152c:	sta	int_68k_dis
	rts


sync_pat_tbl:
	fcb	$ff,$fc,$f3,$cf,$3f,$ff
sync_pat_size	equ	*-sync_pat_tbl


	if	new_io==0
; write a sync pattern, length in Y

write_sync_pat:
	lda	#$ff
	sta	ioport_0b
	sta	q7h
	lda	q6l
	ldx	#sync_pat_size-1
	stx	D6e
L1545:	nop
	nop
	nop
L1548:	pha
	pla
	nop
	lda	sync_pat_tbl,x
	sta	q6h
	lda	q6l
	dex
	bpl	L1545
	ldx	#sync_pat_size-1
	dey
	bne	L1548
	lda	#$ff
	jmp	$126c		; XXX


pack_misc_nib:
	ldy	D01f4
	ldx	D01f4+1
	lda	D01f4+2
	jsr	pack_ms_nib
	sta	misc_nib_1

	ldy	D0200
	ldx	D0200+1
	lda	D0200+2
	jsr	pack_ms_nib
	sta	misc_nib_2

	ldy	D0200+$ff
	ldx	D0300
	lda	D0300+1
	jsr	pack_ms_nib
	sta	misc_nib_3
	ldy	D0300+2
	ldx	D0300+3
	lda	D0300+4
	jsr	pack_ms_nib
	sta	misc_nib_4
	ldy	D0300+$fe
	ldx	D0300+$ff
	lda	#$00
	jsr	pack_ms_nib
	sta	misc_nib_5
	ldy	chksum
	ldx	chksum+1
	lda	chksum+2
	jsr	pack_ms_nib
	sta	misc_nib_6
	rts

	endif

	if	new_io

write_sync_pat:
	sty	D29
	sta	D6e
	lda	#$ff
	sta	q7h
L1543:	ldy	#$05
L1545:	lda	sync_pat_tbl,y
L1548:	ldx	q6l
	bpl	L1548
	sta	q6h
	dey
	bpl	L1545
	dec	D29
	bne	L1543
L1557:	ldx	q6l
	bpl	L1557
	sta	q6h
	lda	D6e
	bne	L156d
	lda	data_mark+0
	jsr	S1aec
	lda	data_mark+1
	jmp	S1aec
L156d:	lda	addr_mark+0
	jsr	S1aec
	lda	addr_mark+1
	jmp	S1aec
	
pack_misc_nib:
	ldy	D0200+$ff
	ldx	D0300
	lda	D0300+1
	jsr	pack_ms_nib
	sta	misc_nib_1
	ldy	D0300+$fe
	ldx	D0300+$ff
	lda	#$00
	jsr	pack_ms_nib
	sta	misc_nib_2
	ldy	chksum+0
	ldx	chksum+1
	lda	chksum+2
	jsr	pack_ms_nib
	sta	misc_nib_6
	rts
	
	endif

docmd:	ldy	trace_buf_idx		; copy first 8 bytes of iob into
	ldx	#$07			; iiob and trace buffer
L15b6:	lda	iob,x
	sta	iiob,x
	sta	(trace_buf_ptr),y
	dey
	dex
	bpl	L15b6
	sty	trace_buf_idx
	iny
	bne	L15c9
	ldy	#trace_buf_size-1
	sty	trace_buf_idx

L15c9:	cmp	#cmd_rwts		; command $81 - RWTS
	beq	rwts
	cmp	#cmd_handshake		; command $80 - handshake
	beq	L15f8
	sec
	sbc	#cmd_seek
	bcc	badcmd
	cmp	#cmd_go_away-cmd_seek+2	; BUG!!! should be +1
	bcc	L15de

badcmd:	lda	#err_badcmd
	bne	cmd_done

L15de:	tax
	sta	cmd_save
	lda	gen_cmd_val_mask,x
	jsr	validate_args
	bcs	cmd_done
	lda	cmd_save
	asl	a
	tax
	jsr	call_cmd
	lda	cmd_save
	beq	L1600
	cmp	#$01
	beq	L1600
L15f8:	lda	#noerr

cmd_done:
	sta	iob_errstat		; command complete, store result
	lda	#cmd_completed
	sta	iob_cmd
L1600:	rts


call_cmd:
	lda	cmd_tbl+1,x
	pha
	lda	cmd_tbl,x
	pha
	rts

cmd_tbl:
	fdb	seekcmd-1	; seek to side/track
	fdb	gocmd-1		; go command (execute machine code)
	fdb	clrintcmd-1	; clear interrupt status
	fdb	setmskcmd-1	; set interrupt mask
	fdb	clrmskcmd-1	; clear interrupt mask
	fdb	testcmd-1	; test command
	fdb	haltcmd-1	; halt


rwts:	lda	int_pend	; check for pending interrupt
	beq	L1623
	lda	#err_intpend

L161e:	jsr	cmd_done
	beq	L164c		; always taken

L1623:	ldx	iiob_fcn
	cpx	#max_rwts_fcn+1
	bcc	L162d
	lda	#err_badcmd
	bne	L161e		; always taken

L162d:	lda	rwts_cmd_val_mask,x	; validate arguments
	jsr	validate_args

	jsr	cmd_done	; huh??? we're not done yet
	bcs	L164c
	jsr	start_disk_op	; init counters & timer

	lda	iiob_fcn	; dispatch 
	asl	a
	tax
	jsr	call_rwts_cmd

	sta	mem_68k_ena
	sta	ioport_1d

	bcc	L165f
	sta	iob_errstat

L164c:	ldy	trace_buf_idx
	cpy	#trace_buf_size-1
	bne	L1656
	ldy	#$06
	bne	L165b
L1656:	tya
	clc
	adc	#$07
	tay
L165b:	lda	iob_errstat
	sta	(trace_buf_ptr),y

L165f:	lda	#$40		; set rwts done flag
	ora	int_flags
	sta	int_flags
	jmp	L1518


call_rwts_cmd:
	lda	rwts_cmd_tbl+1,x
	pha
	lda	rwts_cmd_tbl,x
	pha
	sta	ioport_1c
	sta	mem_68k_dis
	rts

rwts_cmd_tbl:
	fdb	read_cmd-1
	fdb	write_cmd-1
	fdb	unclamp_cmd-1
	fdb	format_cmd-1
	fdb	vfy_cmd-1
	fdb	format_cmd-1
	fdb	vfy_trk_cmd-1
	fdb	read_brute_cmd-1
	fdb	write_brute_cmd-1
	fdb	clamp_cmd-1


validate_args:
	sta	D6e
	beq	L16a6
	ldx	#validate_jmp_tab_size-2
L1691:	lsr	D6e
	bcc	L16a2
	stx	D6f
	lda	validate_jmp_tab+1,x
	pha
	lda	validate_jmp_tab,x
	pha
	rts


L16a0:	ldx	D6f
L16a2:	dex
	dex
	bpl	L1691
L16a6:	lda	#$00
	clc
	rts
L16aa:	sec
	rts


; Note that drive $00 (or any other bad drive number) is reported
; as err_nodisk rather than err_baddrive.  Presumably this is for
; compatability with Lisa 1 software, which had two drives and would
; probably get confused if drive $00 didn't exist.

validate_drive:
	lda	iiob_drv
	cmp	#$80
	bne	L16b6
	lda	disk_clamped
	bne	L16ba
L16b6:	lda	#err_nodisk
	bne	L16aa

L16ba:	jsr	S1722
	bcs	L16a0
	lda	#err_drvdis
	bne	L16aa


validate_side:
	lda	#err_badside
	ldy	iiob_side
	beq	L16a0		; side 0 is always OK
	cpy	#$01
	bne	L16aa		; sides other than 0 and 1 are never OK
	ldy	#$02		; side 1 is only OK on two-sided disks
	cpy	iob_side_cnt
	bne	L16aa
	beq	L16a0


validate_sect:
	jsr	getzone
	lda	iiob_sect
	cmp	spt_tab,y
	bcc	L16a0
	lda	#err_badsect
	bne	L16aa


validate_track:
	lda	iiob_trk
	cmp	#max_track+1
	bcc	L16a0
	lda	#err_badtrack
	bne	L16aa


validate_mask:
	lda	iiob_arg
	and	#$77
	beq	L16a0
	lda	#err_badmask
	bne	L16aa


validate_fmt:
	inc	iob_fmt_conf
	beq	L16a0
	lda	#err_invfmt
	bne	L16aa


validate_wr_en:
	jsr	get_wr_prot
	bcs	L16a0
	lda	#err_wprot
	bne	L16aa


validate_t0s0:
	lda	iiob_trk
	ora	iiob_side
	beq	L16a0
	lda	#err_invfmt
	bne	L16aa


validate_jmp_tab:
	fdb	validate_t0s0-1
	fdb	validate_wr_en-1
	fdb	validate_fmt-1
	fdb	validate_mask-1
	fdb	validate_track-1
	fdb	validate_sect-1
	fdb	validate_side-1
	fdb	validate_drive-1
validate_jmp_tab_size	equ	*-validate_jmp_tab


S1722:	lda	#$80
	and	int_mask
	adc	#$f8
	rts


; start a disk operation:
;   init retry counters
;   clear error counters
;   start motor timer

start_disk_op:
	lda	max_retry	; init retry counter
	sta	retry_cnt
	lda	D1a
	sta	recal_cnt

	lda	montime		; init motor timer
	sta	mtimer+2

	lda	iiob_side
	beq	L173d
	lda	#$20
	sta	iiob_side
L173d:	lda	#$00
	sta	mtimer
	sta	mtimer+1
	sta	D7b

	ldx	#errcnt_tbl_size-1	; clear error counters
L1747:	sta	errcnt_tbl,x
	dex
	bpl	L1747
	rts


; go command - execute machine code

gocmd:
	if	new_io
	jsr	S17e1
	else
	jsr	S17e4
	endif
	jmp	(iiob_arg)


; clear interrupt status command

clrintcmd:
	lda	iiob_arg
	eor	#$ff
	and	int_flags
	sta	int_flags
	jmp	L1518


; set interrupt mask command

setmskcmd:
	lda	iiob_arg
	ora	int_mask
	sta	int_mask
	jmp	L1518


; clear interrupt mask command

clrmskcmd:
	lda	iiob_arg
	eor	#$ff
	and	int_mask
	sta	int_mask
	jmp	L1518


; seek command

seekcmd:
	jsr	start_disk_op	; init counters & timer
	jsr	cmd_done

S1778:	ldx	#$00
	stx	D56
	stx	D57
	stx	D59
	dex
	stx	D0c
	lda	motor_on
	bne	L178c
	dec	D57
	jsr	S134f

L178c:	lda	iiob_trk	; are we on the correct track
	cmp	drv_trk
	bne	L1798
	lda	D57
	beq	L17d1
	bne	L17bf
L1798:	dec	D56
	sec
	sbc	drv_trk
	bcs	L17a5
	eor	#$ff
	adc	#$01
	inc	D59
L17a5:	sta	D58
	lda	D16
	sta	D69
	jsr	getzone
	cpy	drv_zone
	beq	L17bc
	jsr	set_drive_speed_y
	lda	D58
	jsr	S1d37
	sta	D69
L17bc:	jsr	S13a2
L17bf:	lda	D1c
	ldx	D57
	bne	L17c7
	lda	D69
L17c7:	jsr	long_delay

	lda	iiob_trk	; remember track
	sta	drv_trk

	jsr	S1e7f
L17d1:	jsr	S134f
	ldx	#$00
	stx	D0c
	jmp	select_side


haltcmd:
	jsr	S17e1
L17de:	jmp	L17de

S17e1:	jsr	S1348

S17e4:	lda	#$00
	jmp	cmd_done


; test command
; keep 6504 busy

testcmd:
	jsr	S17e1
L17ec:	lda	iob_cmd		; wait until iob_cmd = $69
	cmp	#$69
	bne	L17ec

L17f2:	cmp	iob_cmd		; keep waiting while iob_cmd = $69
	beq	L17f2

	sec			; if it hasn't been complemented, start over
	adc	iob_cmd
	bne	L17ec

	jmp	L142b		; done !


	fillto	$1800,fill_byte

; read a data field (after the proper address mark has been read)

read_data_field:
	jsr	S13cd
	jsr	find_data_header
	bcc	L1809
	rts

L1809:	ldy	#$f4		; start at $01f4
L180b:	inc	data_ptr+1
L180d:	ldx	q7l
	bpl	L180d
	lda	denib_tab,x
	asl	a
	asl	a
	tax
	asl	a
	asl	a
	sta	D6e
	txa
	and	#$c0
L181f:	ldx	q7l
	bpl	L181f
	ora	denib_tab,x
	sta	(data_ptr),y
	lda	D6e
	iny
	bne	L1830
	inc	data_ptr+1
L1830:	ldx	q7l
	bpl	L1830
	and	#$c0
	ora	denib_tab,x
	sta	(data_ptr),y
	iny
	beq	L1854
	lda	D6e
	asl	a
	asl	a
L1843:	ldx	q7l
	bpl	L1843
	and	#$c0
	ora	denib_tab,x
	sta	(data_ptr),y
	iny
	beq	L180b
	bne	L180d

L1854:	ldx	q7l
	bpl	L1854
	lda	denib_tab,x
	asl	a
	asl	a
	tay
	and	#$c0
L1861:	ldx	q7l
	bpl	L1861
	ora	denib_tab,x
	sta	chksum
	tya
	asl	a
	asl	a
	tay
	and	#$c0
L1871:	ldx	q7l
	bpl	L1871
	ora	denib_tab,x
	sta	chksum+1
	tya
	asl	a
	asl	a
L187e:	ldx	q7l
	bpl	L187e
	ora	denib_tab,x
	sta	chksum+2

L1888:	lda	q7l		; verify data field trailer
	bpl	L1888
	cmp	data_mark+3
	bne	L189f
L1891:	lda	q7l
	bpl	L1891
	cmp	data_mark+4
	bne	L189f
	clc
L189b:	lda	q6h
	rts

L189f:	inc	D49
	sec
	bcs	L189b

	inc	D4a
	sec
	bcs	L189b

L18a9:	inc	D48
	sec
	bcs	L189b


; find a data field header

find_data_header:
	lda	q6l
	ldx	#$00
	ldy	#$20
L18b5:	dey
	beq	L18a9
L18b8:	lda	q7l
	bpl	L18b8
L18bd:	cmp	data_mark
	bne	L18b5
	stx	data_ptr
L18c3:	lda	q7l
	bpl	L18c3
	cmp	data_mark+1
	bne	L18bd
	stx	data_ptr+1
L18ce:	lda	q7l
	bpl	L18ce
	cmp	data_mark+2
	bne	L18bd
	clc
L18d8:	lda	q7l
	bpl	L18d8
	rts


read_brute_cmd:
	lda	#$ff
	bne	L18e4

read_cmd:
	lda	#$00
L18e4:	sta	brute_flag
	jsr	seekcmd
L18e9:	jsr	find_addr		; find address field
	bcs	L18fe
	jsr	read_data_field		; read data field
	bcs	L18fe

	ldx	brute_flag
	bne	L1905

	jsr	vfy_cksm		; verify data field checksum
	bcc	L1905
	inc	D4a
L18fe:	jsr	S1906
	bcc	L18e9
	lda	#err_cant_read
L1905:	rts


S1906:	dec	retry_cnt
	beq	L191d
	clc
	lda	D4b
	adc	D4e
	beq	L192b
	cmp	recal_cnt
	beq	L192c
	cmp	D7b
	beq	L192b
	sta	D7b
	bne	L1921
L191d:	dec	recal_cnt
	beq	L192c
L1921:	jsr	S1d5b
	jsr	S1778
	lda	max_retry
	sta	retry_cnt
L192b:	clc
L192c:	rts


calc_cksm:
	lda	#$00
	sta	chksum
	sta	chksum+1
	sta	data_ptr
	sta	data_ptr+1
	inc	data_ptr+1
	ldy	#$f4
L193b:	asl	a
	php
	adc	#$00
	sta	chksum+2
	plp
	lda	(data_ptr),y
	tax
	eor	chksum+2
	sta	(data_ptr),y
	txa
	adc	chksum
	sta	chksum
	iny
	bne	L1953
	inc	data_ptr+1
L1953:	lda	(data_ptr),y
	tax
	adc	chksum+1
	sta	chksum+1
	txa
	eor	chksum
	sta	(data_ptr),y
	iny
	beq	L1973
	lda	(data_ptr),y
	tax
	eor	chksum+1
	sta	(data_ptr),y
	txa
	adc	chksum+2
	iny
	bne	L193b
	inc	data_ptr+1
	bne	L193b
L1973:	rts


; verify data field checksum

vfy_cksm:
	lda	#$00
	sta	chksum2
	sta	chksum2+1
	sta	data_ptr
	sta	data_ptr+1
	inc	data_ptr+1
	ldy	#$f4
L1982:	asl	a
	php
	adc	#$00
	sta	chksum2+2
	plp
	lda	(data_ptr),y
	eor	chksum2+2
	sta	(data_ptr),y
	adc	chksum2
	sta	chksum2
	iny
	bne	L1998
	inc	data_ptr+1
L1998:	lda	(data_ptr),y
	eor	chksum2
	sta	(data_ptr),y
	adc	chksum2+1
	sta	chksum2+1
	iny
	beq	L19b4
	lda	(data_ptr),y
	eor	chksum2+1
	sta	(data_ptr),y
	adc	chksum2+2
	iny
	bne	L1982
	inc	data_ptr+1
	bne	L1982
L19b4:	cmp	chksum+1
	bne	L19c6
	lda	chksum2+2
	cmp	chksum+2
	bne	L19c6
	lda	chksum2
	cmp	chksum
	bne	L19c6
	clc
	rts
L19c6:	sec
	rts


write_cmd:
	jsr	calc_cksm

write_brute_cmd:
	jsr	pack_misc_nib
	jsr	seekcmd
L19d1:	jsr	find_addr
	bcc	write_data_field
	jsr	S1906
	bcc	L19d1
	lda	#err_cant_write
	rts

	if	new_io==0

S19de:	jsr	S126b		; just a delay
	jsr	S1af7
	ldx	D0300
	lda	nib_tab,x
	nop
	nop
	jsr	S1af7
	ldx	D0300+1
	lda	nib_tab,x
	pha
	pla
	jmp	S1af7

	endif


	fillto	$1a00,fill_byte

	if	new_io==0

write_data_field:
	jsr	write_data_mark
	lda	misc_nib_1
	ldy	#$00
	sty	data_ptr+1
	nop
	nop
L1a0b:	nop
	nop
	nop
	sta	q6h
	lda	q6l
	lda	D01f4+5,y
	inc	D6e
	lsr	a
	lsr	a
	sta	D6e
	ldx	D01f4,y
	lda	nib_tab,x
	sta	q6h
	lda	q6l
	lda	D01f4+4,y
	and	#$c0
	ora	D6e
	lsr	a
	lsr	a
	sta	D6e
	ldx	D01f4+1,y
	lda	nib_tab,x
	sta	q6h
	lda	q6l
	lda	D01f4+3,y
	and	#$c0
	ora	D6e
	lsr	a
	lsr	a
	sta	D6e
	ldx	D01f4+2,y
	lda	nib_tab,x
	sta	q6h
	lda	q6l
	ldx	D6e
	lda	nib_tab,x
	iny
	iny
	iny
	cpy	#$09
	bne	L1a0b

	ldx	#$fd
	inc	data_ptr+1
	sta	q6h
	lda	q6l
	jsr	write_chunk

	lda	misc_nib_2
	ldy	#$00
L1a74:	nop
	nop
	nop
	sta	q6h
	lda	q6l
	lda	D0200+5,y
	inc	D6e
	lsr	a
	lsr	a
	sta	D6e
	ldx	D0200,y
	lda	nib_tab,x
	sta	q6h
	lda	q6l
	lda	D0200+4,y
	and	#$c0
	ora	D6e
	lsr	a
	lsr	a
	sta	D6e
	ldx	D0200+1,y
	lda	nib_tab,x
	sta	q6h
	lda	q6l
	lda	D0200+3,y
	and	#$c0
	ora	D6e
	lsr	a
	lsr	a
	sta	D6e
	ldx	D0200+2,y
	lda	nib_tab,x
	sta	q6h
	lda	q6l
	ldx	D6e
	lda	nib_tab,x
	iny
	iny
	iny
	cpy	#$fc
	bne	L1a74

	inc	data_ptr+1
	ldx	#$fc
	sta	q6h
	lda	q6l
	jsr	write_chunk

	ldy	misc_nib_3
	ldx	D0200+$ff
	lda	nib_tab,x
	sty	q6h
	ldy	q6l
	jsr	S19de
	pha
	pla
	lda	misc_nib_4
	ldy	#$02
	bne	write_data_field_2
	nop
	inc	D6e
	ldx	D6e

S1af7:	sta	q6h
	lda	q6l
	rts


	fillto	$1b00,fill_byte

L1b00:	nop
	nop
write_data_field_2:
	nop
	sta	q6h
	lda	q6l
	lda	D0300+5,y
	inc	D6e
	lsr	a
	lsr	a
	sta	D6e
	ldx	D0300,y
	lda	nib_tab,x
	sta	q6h
	lda	q6l
	lda	D0300+4,y
	and	#$c0
	ora	D6e
	lsr	a
	lsr	a
	sta	D6e
	ldx	D0300+1,y
	lda	nib_tab,x
	sta	q6h
	lda	q6l
	lda	D0300+3,y
	and	#$c0
	ora	D6e
	lsr	a
	lsr	a
	sta	D6e
	ldx	D0300+2,y
	lda	nib_tab,x
	sta	q6h
	lda	q6l
	ldx	D6e
	lda	nib_tab,x
	iny
	iny
	iny

	cpy	#$fb
	bne	L1b00

	ldx	#$fb
	inc	data_ptr+1
	sta	q6h
	lda	q6l
	jsr	write_chunk

	lda	misc_nib_5
	ldx	#$fd
	stx	data_ptr
	cmp	D6f
	sta	q6h
	lda	q6l
	jsr	S1b9a
	lda	misc_nib_6

	ldx	#$61
	ldy	D45
	sty	data_ptr+1
	sta	q6h
	lda	q6l
	jsr	write_chunk
	nop

	lda	data_mark+3	; write data mark trailer
	jsr	S1265
	lda	data_mark+4
	jsr	S126c
	lda	D46
	jsr	S126c

	jmp	S1bd3


S1b9a:	ldy	#$01
	bne	L1ba2		; always taken


write_chunk:
	stx	data_ptr
	ldy	#$00
L1ba2:	lda	(data_ptr),y
	nop
	tax
	lda	nib_tab,x
	sta	q6h
	lda	q6l
	iny
	cpy	#$03
	beq	L1bb7
	nop
	bne	L1ba2
L1bb7:	rts


write_data_mark:
	ldy	#$01
	jsr	write_sync_pat

	lda	data_mark	; write data mark header
	jsr	S126c
	lda	data_mark+1
	jsr	S126c
	lda	data_mark+2
	jsr	S126c
	ldx	iiob_sect
	stx	D6e
	jmp	S1260


S1bd3:	lda	q7l
	lda	q6h
	lda	ioport_0a
	rts

	endif

	if	new_io=1

write_data_field:
	ldx	iiob_sect
	lda	nib_tab,x
	sta	D6d
	ldy	#0
	sty	data_ptr+1
	tya
	iny
	jsr	write_sync_pat
	ldy	#$f4
	lda	#$ad		; last byte of data mark
	bne	L1a18

L1a16:	stx	D6d
L1a18:	ldx	q6l
	bpl	L1a18
	sta	q6h
	ldx	D0100+2,y
	lda	nib_tab,x
	sta	D6c
	txa
	lsr	a
	lsr	a
	sta	D6e
	ldx	D0100+1,y
	lda	nib_tab,x
	sta	D6b
	txa
	and	#$c0
	ora	D6e
	lsr	a
	ldx	D6d
	stx	q6h
	lsr	a
	sta	D6e
	lda	D0100,y
	and	#$c0
	ora	D6e
	lsr	a
	lsr	a
	tax
	lda	nib_tab,x
	sta	q6h
	ldx	D0100,y
	iny
	iny
	lda	nib_tab,x
L1a5b:	ldx	q6l
	bpl	L1a5b
	sta	q6h

	lda	D6b
	ldx	D6c
	iny
	bne	L1a16

L1a6a:	stx	D6d
L1a6c:	ldx	q6l
	bpl	L1a6c
	sta	q6h
	ldx	D0200+2,y
	lda	nib_tab,x
	sta	D6c
	txa
	lsr	a
	lsr	a
	sta	D6e
	ldx	D0200+1,y
	lda	nib_tab,x
	sta	D6b
	txa
	and	#$0c0
	ora	D6e
	lsr	a
	ldx	D6d
	stx	q6h
	lsr	a
	sta	D6e
	lda	D0200,y
	and	#$c0
	ora	D6e
	lsr
	lsr
	tax
	lda	nib_tab,x
	sta	q6h
	ldx	D0200,y
	iny
	iny
	lda	nib_tab,x
L1aaf:	ldx	q6l
	bpl	L1aaf
	sta	q6h
	lda	D6b
	ldx	D6c
	iny
	cpy	#$ff
	bne	L1a6a
	jsr	S1ae0
	ldx	D0300+1
	lda	nib_tab,x
	sta	D6d
	lda	misc_nib_1
	jsr	S1aec
	ldy	#$02
	ldx	D0200+$ff
	jsr	S1ae9
	ldx	D0300
	lda	nib_tab,x
	bne	L1b02

S1ae0:	jsr	S1aec
	lda	D6c
	jmp	S1aec

S1ae8:	tax
S1ae9:	lda	nib_tab,x
S1aec:	ldx	q6l
	bpl	S1aec
	sta	q6h
	rts

	fillto	$1b00

L1b00:	stx	D6d
l1b02:	ldx	q6l
	bpl	L1b02
	sta	q6h
	ldx	D0300+2,y
	lda	nib_tab,x
	sta	D6c
	txa
	lsr	a
	lsr	a
	sta	D6e
	ldx	D0300+1,y
	lda	nib_tab,x
	sta	D6b
	txa
	and	#$c0
	ora	D6e
	lsr	a
	ldx	D6d
	stx	q6h
	lsr	a
	sta	D6e
	lda	D0300,y
	and	#$c0
	ora	D6e
	lsr	a
	lsr	a
	tax
	lda	nib_tab,x
	sta	q6h
	ldx	D0300,y
	iny
	iny
	lda	nib_tab,x
L1b45:	ldx	q6l
	bpl	L1b45
	sta	q6h
	lda	D6b
	ldx	D6c
	iny
	cpy	#$fe
	bne	L1b00

	jsr	S1ae0
	ldx	D0300+$ff
	lda	nib_tab,x
	sta	D6c
	lda	misc_nib_2
	jsr	S1aec
	ldx	D0300+$fe
	lda	nib_tab,x
	jsr	S1ae0
	ldy	#chksum
	sty	data_ptr
	lda	misc_nib_6
L1b75:	ldx	q6l
	bpl	L1b75
	sta	q6h
	ldy	#0
L1b7f:	lda	(data_ptr),y
	jsr	S1ae8
	iny
	cpy	#3
	bne	L1b7f
	lda	data_mark+3
	jsr	S1aec
	lda	data_mark+4
	jsr	S1aec

S1b93:	lda	#$ff
	jsr	S1aec
	jsr	S1aec
	clc
	lda	q6l
	and	#$40		; IWM underrun bit
	bne	L1ba6
	lda	#err_wr_underrun
	sec
L1ba6:	sta	q6h
	ldx	q7l
	rts

	endif

format_cmd:
	jsr	clr_data_buf
	lda	#$07
	sta	format_gap
	jsr	S1d54
	bcs	L1c62
L1be9:	jsr	S1778
	ldy	drv_zone
	lda	spt_tab,y
	sta	fmt_sect_cnt
	jsr	format_track
	lda	iiob_fcn
	cmp	#rwts_fcn_fmt_trk
	beq	L1c22

	lda	iob_side_cnt	; two-sided disk?
	cmp	#$02
	bne	L1c0d

	lda	#$20		; do side 1
	sta	iiob_side
	jsr	format_track

	lda	#$00		; switch back to side 0
	sta	iiob_side

L1c0d:	inc	iiob_trk
	lda	iiob_trk
	cmp	#max_track+1
	bcc	L1be9
	dec	iiob_trk
	bne	L1c22

vfy_cmd:
	lda	#max_track
	sta	iiob_trk

vfy_trk_cmd:
	jsr	S1d54
	bcs	L1c62
L1c22:	jsr	S1778
	ldy	drv_zone
	lda	spt_tab,y
	sta	fmt_sect_cnt
	jsr	verify_track
	bcs	L1c50

	lda	iiob_fcn		; doing entire disk?
	cmp	#rwts_fcn_fmt_trk
	bcs	L1c4e			; no

	lda	iob_side_cnt		; two-sided disk?
	cmp	#$02
	bne	L1c4a

	lda	#$20			; do side 1
	sta	iiob_side
	jsr	verify_track
	bcs	L1c50

	lda	#$00			; switch back to side 0
	sta	iiob_side

L1c4a:	dec	iiob_trk		; advance to next track
	bpl	L1c22

L1c4e:	clc
	rts

L1c50:	lda	iiob_trk
	sta	error_track_num
	lda	iiob_side
	and	#$20
	beq	L1c5d
	lda	#$01
L1c5d:	sta	error_sect_num
	lda	#err_cant_vfy
L1c62:	rts


D1c63:	fcb	$00,$05,$0a,$0f,$14,$19,$1e,$23
	fcb	$28,$2d,$32,$37,$3c,$41,$46,$4b

D1c73:	fcb	$6c,$63,$5a,$51,$48


; format a track with fmt_sect_cnt sectors,
; using 2:1 interleave

format_track:
	jsr	select_side

	lda	fmt_sect_cnt
	sta	fmt_sect_cntr

	lsr	a		; set up interleave
	adc	#$00
	sta	fmt_sect_num+1
	lda	#$00
	sta	fmt_even_odd
	sta	fmt_sect_num
	sta	iiob_sect

	jsr	format_sect_0	; format sector 0

	dec	fmt_sect_cntr
	inc	fmt_sect_num

L1c93:	ldx	fmt_even_odd	; are we doing an even or odd sector?
	beq	L1c99
	ldx	#$ff
L1c99:	inx
	stx	fmt_even_odd

	lda	fmt_sect_num,x	; get the sector number
	sta	iiob_sect
	inc	fmt_sect_num,x	; advance the counter

	jsr	format_sect_n	; format a sector

	dec	fmt_sect_cntr	; more sectors to be formatted?
	bne	L1c93

	lda	#$00
	sta	iiob_sect
	jsr	find_addr
	bcs	L1cd3
	lda	D5d
	cmp	#$08
	bne	L1cca
	ldy	format_gap
	lda	D1c63,y
	cmp	D5c
	bcs	L1cd3
	ldy	drv_zone
	adc	D1c73,y
	cmp	D5c
	bcs	L1cd2
L1cca:	lda	#$0e
	cmp	format_gap
	beq	L1cd2
	inc	format_gap
L1cd2:	rts
L1cd3:	lda	#$04
	cmp	format_gap
	beq	L1cd2
	dec	format_gap
	bne	format_track


; verify a track with fmt_sect_cnt sectors

verify_track:
	jsr	select_side
	lda	#$00
	sta	iiob_sect
	sta	bad_sec_total
L1ce7:	lda	max_retry
	sta	retry_cnt
L1ceb:	jsr	find_addr
	bcs	L1d09
	jsr	read_data_field
	bcs	L1d09
	jsr	vfy_cksm
	bcs	L1d09
L1cfa:	inc	iiob_sect
	lda	iiob_sect
	cmp	fmt_sect_cnt
	bne	L1ce7
	lda	bad_sec_total
	bne	L1d08
	clc
L1d08:	rts
L1d09:	dec	retry_cnt
	bne	L1ceb
	ldy	bad_sec_total
	lda	iiob_sect
	sta	bad_sect_map,y
	inc	bad_sec_total
	bne	L1cfa		; always taken


clr_data_buf:
	lda	#$00		; clear data buffer and chksum
	tay
L1d1d:	sta	D0200,y
	sta	D0300,y
	iny
	bne	L1d1d
	ldy	#$0b
L1d28:	sta	D01f4,y
	dey
	bpl	L1d28
	sta	chksum
	sta	chksum+1
	sta	chksum+2

	jmp	pack_misc_nib


S1d37:	cmp	#$0d
	bcs	L1d4c
	tay
	lda	#$00
L1d3e:	adc	#$02
	dey
	bne	L1d3e
	sta	D6e
	lda	D15
	sec
	sbc	D6e
	bcs	L1d4f
L1d4c:	lda	D16
	rts
L1d4f:	cmp	D16
	bcc	L1d4c
	rts

S1d54:	lda	motor_on
	bne	S1d5b
	jsr	S135f

S1d5b:	ldy	#$00
	sty	drv_trk
	jsr	set_drive_speed_y
	dey
	sty	D0c
	ldx	#$00
	jsr	S136e
	ldx	#$04
	stx	D79
L1d6e:	jsr	S1381
	dec	D79
	bne	L1d6e
	lda	#$08
	jsr	long_delay
	ldx	#$64
	stx	D79
L1d7e:	jsr	S1331
	bcs	L1d8c
	jsr	S1381
	dec	D79
	bne	L1d7e
	beq	L1db3
L1d8c:	ldx	#$01
	jsr	S136e
	ldx	#$64
	stx	D6f
L1d95:	dec	D6f
	beq	L1db7
	jsr	S1381
	jsr	S1331
	bcs	L1d95
	lda	#$64
	sbc	D6f
	jsr	S1d37
	jsr	long_delay
	jsr	S1e7f
L1dae:	ldx	#$00
	stx	D0c
	rts
L1db3:	lda	#$10
	bne	L1db9
L1db7:	lda	#$0d
L1db9:	sta	D0b
	sec
	bcs	L1dae		; always taken


clamp_cmd:
	rts


S1dbf:	lda	D1b

delay:	ldx	#$23
L1dc3:	dex
	bne	L1dc3
	ldx	#$26
	nop
	sec
	sbc	#$01
	bne	L1dc3
	nop
	rts


long_delay:
	tay
L1dd1:	lda	#$32
	jsr	delay
	dey
	bne	L1dd1
	rts


S1dda:	lda	#$05
	sta	D6f
	clc
	jsr	S1dfe
	bcs	L1dfd
	jsr	S1e02
	bcs	L1dfd
	lda	#$00
	sta	D5c
	sta	D5d
L1def:	jsr	S1dfe
	bcs	L1dfd
	jsr	S1e02
	bcs	L1dfd
	dec	D6f
	bpl	L1def
L1dfd:	rts

S1dfe:	bit	D45
	bvc	L1e06

S1e02:	bit	D46
	bvs	L1e06
L1e06:	ldy	#$00
	ldx	#$a0
L1e0a:	dey
	bne	L1e12
	dex
	bne	L1e14
	sec
	rts
L1e12:	nop
	nop
L1e14:	inc	D5c
	bne	L1e1c
	inc	D5d
	bne	L1e1e
L1e1c:	pha
	pla
L1e1e:	lda	q7l
	bvc	L1e27
	bpl	L1e29
	nop
L1e26:	rts
L1e27:	bpl	L1e26
L1e29:	bcc	L1e0a

S1e2b:	lda	#$05
	sta	D6e
	lda	#$00
	beq	L1e39

S1e33:	lda	#$01
	sta	D6e
	lda	#$14
L1e39:	sta	data_ptr
	lda	drv_zone
	asl	a
	tay
	lda	D5d
	cmp	(data_ptr),y
	bcc	L1e66
	bne	L1e51
	iny
	lda	D5c
	cmp	(data_ptr),y
	bcc	L1e66
	beq	L1e64
	clc
L1e51:	tya
	adc	#$09
	tay
	lda	(data_ptr),y
	cmp	D5d
	bcc	L1e72
	bne	L1e64
	iny
	lda	(data_ptr),y
	cmp	D5c
	bcc	L1e72
L1e64:	clc
	rts
L1e66:	ldy	drv_zone
	lda	zone_spd,y
	adc	D6e
	sta	zone_spd,y
	sec
	rts
L1e72:	ldy	drv_zone
	lda	zone_spd,y
	sec
	sbc	D6e
	sta	zone_spd,y
	sec
	rts


S1e7f:
	if	sony_800k

	jsr	select_side_1
	lda	ca2_high
	lda	ca1_low
L1e88:	ldx	q6h
	lda	q7l
	ldx	q6l
	asl	a
	bcs	L1e88
	rts

	endif

	if	(sony_800k==0)|(new_io=1)

	lda	#$11
	sta	data_ptr+1
	jsr	S1367
	jsr	S1dda
	bcs	L1ec1
	jsr	S1e2b
	bcc	L1ec3
	lda	#$65
	sta	D29
L1e94:	dec	D29
	beq	L1ec1
	jsr	set_drive_speed
	lda	D15
	jsr	long_delay
	jsr	S1dda
	bcs	L1ec1
	jsr	S1e2b
	bcs	L1e94
L1eaa:	sec
	dec	D29
	beq	L1ec1
	jsr	S1e33
	bcc	L1ec3
	jsr	set_drive_speed
	lda	D15
	jsr	long_delay
	jsr	S1dda
	bcc	L1eaa
L1ec1:	lda	#err_cant_setspd
L1ec3:	rts

	endif

	fillto	$1ff3,fill_byte

	fcc	"C83APPLE"
	jmp	reset

	if	new_io==0
	fcb	$46,$ff
	endif

	if	new_io==1
	if	sony_800k==1
	fcb	$00,$00
	else
	fcb	$80,$ff
	endif
	endif

	end
