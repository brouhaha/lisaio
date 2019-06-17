; Lisa I/O board firmware for Twiggy drives
; Original code by Apple Computer
; Disassembly Copyright 1993, 1997, 2000, 2019 Eric Smith

; The Twiggy drives can only be used with the original Lisa I/O board,
; not the newer I/O board as used in the Lisa 2/10 and Macintosh XL.


hardware_id		equ	$40	; identifies as Twiggy controller


fillto	macro	addr, val
	while	* < addr
size	set	addr-*
	if	size > 256
size	set	256
	endif
	fcb	[size] val
	endm
	endm

fill_byte		equ	$ff


max_track		equ	$2d	; tracks $00 .. $2d
max_sect		equ	$15	; sectors $00 .. $15
max_zone		equ	$07

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



; $00 .. $0f: IOB

		org	$00

iob:
iob_cmd:	rmb	1
		rmb	6
iob_fmt_conf:	rmb	1
iob_errstat:	rmb	1	; error status
iob_diskid:	rmb	1
		rmb	6	; $0a .. $0f apparently unused


; $10 .. $1c: drive parameters

		org	$10

drive_parms:
drv_trk_offset:	rmb	1	; indexed by drive_idx
D11:		rmb	1
		rmb	1	; other half of drv_trk_offset
		rmb	1	; $13 apparently unused
spd_up_settle_time:
		rmb	1	; delay time after speed increase
slow_dn_settle_time:
		rmb	1	; delay time after speed decrease
seek_settle_time:
		rmb	1	; delay time after seeking
montime:	rmb	1	; motor on time, copied to high byte of mtimer
		rmb	1	; hardware ID
max_retry:	rmb	1
max_reseek:	rmb	1
max_track_offset:
		rmb	1	; maximum track offset to try
D1c:		rmb	1

		rmb	3	; $1d .. $1f apparently unused


		org	$20

; an address field read from disk
hdr_buf:	rmb	1	; checksum
hdr_vol:	rmb	1
hdr_side:	rmb	1	; side
hdr_sect:	rmb	1	; sector
hdr_trk:	rmb	1	; track

D25:		rmb	1	; indexed by drive_idx
D26:		rmb	1	; indexed by drive_idx
		rmb	1	; other half of D25
		rmb	1	; other half of D26

		rmb	3	; $29 .. $2b apparently unused

retry_cnt:	rmb	1
reseek_cnt:	rmb	1
D2e:		rmb	1
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
iiob_spd:	rmb	1

		rmb	1

		rmb	8	; $38 .. $3f apparently unused

req_act_pos:	rmb	2	; requested actuator position

drive_idx:	rmb	1	; $00 or $02 for lower and upper

int_pend:	rmb	1	; bitmap of pending interrupts
int_mask:	rmb	1	; bitmap of intterupt mask

mtimer:		rmb	3	; motor on timer

step_delay_time:
		rmb	1
D49:		rmb	1

drv_req_speed:	rmb	1	; indexed by drive_idx
D4b:		rmb	1	; indexed by drive_idx
		rmb	1	; other half of drv_req_speed
		rmb	1	; other half of D4b

		rmb	1	; $4e apparently unused

D4f:		rmb	1

drv_side:	rmb	1	; indexed by drive_idx
drv_trk:	rmb	1	; indexed by drive_idx
		rmb	1	; other half of drv_side
		rmb	1	; other half of drv_trk

drv_act_pos:	rmb	4	; actuator position, two bytes per drive,
				;   indexed by drive_idx

D58:		rmb	1	; indexed by drive_idx

cmd_save:	rmb	1	; temp in command dispatch

		rmb	1	; other half of D58

errcnt_tbl:
D5b:		rmb	1	; bitslip (data field header)
D5c:		rmb	1	; bitslip (data field trailer)
D5d:		rmb	1	; data field checksum
D5e:		rmb	1	; bitslip (address field header)
D5f:		rmb	1	; bitslip (address field trailer)
D60:		rmb	1	; wrong sector
D61:		rmb	1	; wrong track
D62:		rmb	1	; addr field checksum
errcnt_tbl_size	equ	*-errcnt_tbl

data_ptr:	rmb	2

D65:		rmb	1
D66:		rmb	1
D67:		rmb	1
act_pos:	rmb	2	; actuator position
D6a:		rmb	2
act_step_count:	rmb	2	; how many steps we need to move the actuator
D6e:		rmb	1
D6f:		rmb	1
brute_flag:	rmb	1	; read brute-force flag
spd_chg_settle:	rmb	1	; settle time needed for speed change
spd_chg_flag:	rmb	1	; used by seek to flag whether speed changed
fmt_sect_cnt:	rmb	1
unclamp_cnt:	rmb	1	; temp used by unclamp

		rmb	1	; $75 apparently unused

D76:		rmb	1
D77:		rmb	1

misc_nib_1:	rmb	1
misc_nib_2:	rmb	1
misc_nib_3:	rmb	1
misc_nib_4:	rmb	1
misc_nib_5:	rmb	1
misc_nib_6:	rmb	1
					
chksum:		rmb	3	; three bytes
chksum2:	rmb	3	; three bytes

hdr_csum:	rmb	1	; temp during find_addr

		rmb	3	; $85..$87 apparently unused

fmt_sect_num:	rmb	2	; two counters used for 2:1 interleave
fmt_even_odd:	rmb	1	; format even/odd sector flag, used to
				; index fmt_sect_num
fmt_sect_cntr:	rmb	1

		rmb	9	; $8c..$94 apparently unused

drv_speed_delta:
		rmb	1	; indexed by drive_idx

D96:		rmb	1

		rmb	1	; $97 apparently unused

D98:		rmb	1
D99:		rmb	1
D9a:		rmb	1
D9b:		rmb	1
D9c:		rmb	1	; indexed by drive_idx
D9d:		rmb	1
		rmb	1	; other half of D9c

; $9f .. $bf apparently unused

; $c0 .. $ff are used by the Lisa as non-volatile storage

; stack starts at $0180 and grows toward $100

; $180 .. $1cf apparently unused

bad_sec_total	equ	$01d0
error_track_num	equ	$01d1
error_sect_num	equ	$01d2
bad_sect_map	equ	$01d3	; max_sect bytes, from here up to $1e7

; $01e8 .. $01f3 apparently unused

; data buffer from $01f4 to $03ff

D01f4	equ	$01f4
D01f5	equ	$01f5
D01f6	equ	$01f6
D01f7	equ	$01f7
D01f8	equ	$01f8
D01f9	equ	$01f9

D0200	equ	$0200
D0201	equ	$0201
D0202	equ	$0202
D0203	equ	$0203
D0204	equ	$0204
D0205	equ	$0205
D02ff	equ	$02ff

D0300	equ	$0300
D0301	equ	$0301
D0302	equ	$0302
D0303	equ	$0303
D0304	equ	$0304
D0305	equ	$0305
D03fe	equ	$03fe
D03ff	equ	$03ff


iobase		equ	$0400

phase0_low	equ	iobase+$00	; phase0 is also used for PWM data
phase0_high	equ	iobase+$01
phase2_low	equ	iobase+$04
phase2_high	equ	iobase+$05
ioport_08	equ	iobase+$08
ioport_09	equ	iobase+$09
ioport_0a	equ	iobase+$0a
ioport_0b	equ	iobase+$0b
q6l		equ	iobase+$0c
q6h		equ	iobase+$0d
q7l		equ	iobase+$0e
q7h		equ	iobase+$0f
ioport_10	equ	iobase+$10
ioport_11	equ	iobase+$11
ioport_12	equ	iobase+$12
ioport_13	equ	iobase+$13
pwm_clk_low	equ	iobase+$14
pwm_clk_high	equ	iobase+$15
ioport_16	equ	iobase+$16
mem_68k_ena	equ	iobase+$18
mem_68k_dis	equ	iobase+$19
ioport_1c	equ	iobase+$1c
ioport_1d	equ	iobase+$1d
int_68k_dis	equ	iobase+$1e
int_68k_ena	equ	iobase+$1f

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


; argument validation mask bits

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
	fcb	vb_wr_en + vb_fmt + vb_track + vb_side + vb_drive	; format disk (BUG??? shouldn't have vb_track or vb_side)
	fcb	vb_track + vb_side + vb_drive		; verify disk (BUG??? shouldn't have vb_track or vb_side)
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
	fcb	$00		; drive 0 track offset
	fcb	$04
	fcb	$00		; drive 1 track offset
	fcb	$a0		; unused?
	fcb	$50		; delay time after speed increase
	fcb	$1e		; delay time after speed decrease
	fcb	$02		; delay time after seek
	fcb	$08		; default motor on time
	fcb	hardware_id
	fcb	$64		; default retry count
	fcb	$01		; default reseek count
	fcb	$02		; max track offset to try
	fcb	$48
def_drv_parm_tbl_size	equ	*-def_drv_parm_tbl


; stepper motor phase constants

ph1_on	equ	%10000000
ph1_off	equ	%01000000
ph2_on	equ	%00100000
ph2_off	equ	%00010000
ph3_on	equ	%00001000
ph3_off	equ	%00000100
ph4_on	equ	%00000010
ph4_off	equ	%00000001

all_phases_off	equ	ph1_off + ph2_off + ph3_off + ph4_off


; stepper motor phase tables

step_phase_tbl_1:
		fcb	ph1_off + ph2_off + ph3_on  + ph4_on
		fcb	ph1_off + ph2_on  + ph3_on  + ph4_off
		fcb	ph1_on  + ph2_on  + ph3_off + ph4_off

step_phase_tbl_2:
		fcb	ph1_on  + ph2_off + ph3_off + ph4_on
		fcb	ph1_off + ph2_off + ph3_on  + ph4_on
		fcb	ph1_off + ph2_on  + ph3_on  + ph4_off
		fcb	ph1_on  + ph2_on  + ph3_off + ph4_off

; Note that the previous two tables could more efficiently be stored like
; this:
;
; step_phase_tbl_2:
;		fcb	ph1_on  + ph2_off + ph3_off + ph4_on
; step_phase_tbl_1:
;		fcb	ph1_off + ph2_off + ph3_on  + ph4_on
;		fcb	ph1_off + ph2_on  + ph3_on  + ph4_off
;		fcb	ph1_on  + ph2_on  + ph3_off + ph4_off
;		fcb	ph1_on  + ph2_off + ph3_off + ph4_on


; all entries in this table have a single phase on
step_single_phase_tbl:
		fcb	ph1_off + ph2_off + ph3_off + ph4_on
		fcb	ph1_off + ph2_off + ph3_on  + ph4_off
		fcb	ph1_off + ph2_on  + ph3_off + ph4_off
		fcb	ph1_on  + ph2_off + ph3_off + ph4_off


; sectors per track by zone
spt_tab:	fcb	$16,$15,$14,$13,$12,$11,$10,$0f

; zone speed bytes
zonespd:
	fcb	$dc,$cc,$b8,$a4,$8e,$75,$59,$38


D1139:	fcb	$41,$21,$17,$17

D113d:	fcb	$46,$26,$1c,$1c


; figure out which zone contains the current track number

getzone:
	lda	iiob_trk
	ldy	#max_zone
L1145:	cmp	zone_trk_tbl,y
	bcs	L114d
	dey
	bne	L1145
L114d:	rts


; zone starting track numbers
zone_trk_tbl:
	fcb	$00,$04,$0b,$11,$17,$1d,$23,$2a


; write a sync pattern, length in Y

write_sync_pat:
	lda	ioport_0b
	lda	#$ff
	sta	q7h
	lda	q6l
	nop
	nop
	nop
	inc	D6e
L1166:	jsr	S1271
L1169:	lda	D67
	jsr	S1af5
	dey
	bne	L1166
	lsr	D6e
	dec	D77
	bpl	L1169
	lda	#$ff
	sta	q6h
	lda	q6l
	rts


; test command
; keep 6504 busy

testcmd:
	jsr	S173f
L1183:	lda	iob_cmd		; wait until iob_cmd = $69
	cmp	#$69
	bne	L1183

L1189:	cmp	iob_cmd		; keep waiting while iob_cmd = $69
	beq	L1189

	sec			; if it hasn't been complemented, start over
	adc	iob_cmd
	bne	L1183

	jmp	L1303		; done!


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
	lda	#$0f
	ldx	#$09		; ten timing nibbles
	bne	L1209

format_sect_n:
	lda	#$00
	tax			; no timing nibbles
L1209:	sta	D77
	lda	#$01
	sta	hdr_vol
	sta	iob_diskid
	ldy	#$21
	jsr	write_sync_pat

	txa			; need timing nibbles?
	beq	L121e
	jsr	write_timing_pat
	inc	D6e

L121e:	ldy	#$96		; write address mark header
	jsr	write_mark_1

	ldx	iiob_trk	; write track/sector/head/volume
	jsr	S1261
	ldx	iiob_sect
	jsr	S1261
	ldx	iiob_side
	jsr	S1261
	ldx	hdr_vol
	jsr	S1261

	lda	iiob_trk	; compute and write checksum
	eor	iiob_sect
	eor	iiob_side
	eor	hdr_vol
	tax
	lda	nib_tab,x
	sta	q6h
	lda	q6l
	nop
	nop
	nop

	lda	#$de		; write address mark trailer
	jsr	S1267
	lda	#$aa
	jsr	S1267
	lda	#$eb
	jsr	S1267
	jsr	S1be7

	jmp	write_data_field	; write a data field and return


S1261:	lda	nib_tab,x
	nop
	bne	S126b			; always taken

S1267:	lsr	D6e
	lsr	D6e

S126b:	sta	q6h
	lda	q6l

S1271:	rts


; find an address field
; reads address field into hdr_buf
; doesn't check side!!!

find_addr:
	lda	q6l
	ldy	#$f0
	sty	D77
L1279:	iny
	bne	L1280
	inc	D77
	beq	L12dd

L1280:	lda	q7l	; first find the address mark
	bpl	L1280
L1285:	cmp	#$d5
	bne	L1279
	nop
L128a:	lda	q7l
	bpl	L128a
	cmp	#$aa
	bne	L1285
	nop
L1294:	lda	q7l
	bpl	L1294
	cmp	#$96
	bne	L1285

	ldx	#$04		; now read the address field
	lda	#$00		; start with checksum of 0
L12a1:	sta	hdr_csum
L12a3:	ldy	q7l
	bpl	L12a3
	lda	denib_tab,y
	sta	hdr_buf,x
	eor	hdr_csum
	dex
	bpl	L12a1

	tax			; test checksum
	bne	L12eb

L12b5:	lda	q7l	; check for the trailer
	bpl	L12b5
	cmp	#$de
	bne	L12e1

	lda	iiob_trk	; right track?
	cmp	hdr_trk
	bne	L12f0

	lda	iiob_sect	; right sector?
	cmp	hdr_sect
	bne	L12e6

L12ca:	lda	q7l
	bpl	L12ca
	cmp	#$aa
	bne	L12e1

	clc
	lda	hdr_vol
	sta	iob_diskid
L12d8:	clv
L12d9:	lda	q6h
	rts

L12dd:	inc	D5e		; inc. too many retries
	bne	L12f2

L12e1:	inc	D5f		; inc. trailer error
	sec
	bcs	L12d8

L12e6:	inc	D60		; inc. wrong sector count
	sec
	bcs	L12d8

L12eb:	inc	D62		; inc. address field checksum error
	sec
	bcs	L12d8

L12f0:	inc	D61		; inc. wrong track error
L12f2:	bit	D67
	sec
	bcs	L12d9


reset:	cld
	sei

	ldx	#$00		; wait a bit
	ldy	#$00
L12fd:	dex
	bne	L12fd
	dey
	bne	L12fd

L1303:	sta	ioport_1c
	sta	int_68k_dis

	ldx	#$80		; init stack
	txs

	sta	mem_68k_ena
	sta	ioport_11
	sta	ioport_13
	jsr	S1373
	jsr	S1748

	lda	#all_phases_off
	jsr	set_stepper_phases

	lda	q7l
	lda	q6h

	ldx	#$9e		; clear RAM from $00 .. $9e
	lda	#$00
L132a:	sta	$0000,x
	dex
	bne	L132a

	sta	iob_cmd		; command done

; initialze drive parms
	ldx	#def_drv_parm_tbl_size	; BUG!!! should be def_drv_parm_tbl_size-1
L1333:	lda	def_drv_parm_tbl,x
	sta	drive_parms,x
	dex
	bpl	L1333

	lda	#$aa
	sta	D76
	lda	#$c0
	sta	D66
	dec	D67
	jsr	S137b
	sta	ioport_1d

idle:	inc	D2e		; update counters
	dec	mtimer
	bne	L1363
	dec	mtimer+1
	bne	L1363
	jsr	S137b
	dec	mtimer+2
	bne	L1363
	jsr	S1745		; turn of motor ???
	lda	montime
	sta	mtimer+2

L1363:	lda	iob_cmd		; is there a command to execute?
	bpl	idle
	jsr	docmd
	jsr	S1754
	jsr	S137b
	jmp	idle


S1373:	sta	phase2_high
	sta	phase2_low
	rts

	fcb	$60


S137b:	sta	ioport_1c
	ldx	#$02
L1380:	lda	ioport_11,x
	jsr	S1e98
	bcc	L13b2
	lda	D25,x
	bne	L13b8
	jsr	S1570
	bcc	L13a5
	lda	D9c,x
	bne	L13b8
	dec	D9c,x

	lda	#$01		; set intterupt flag for appropriate drive
	cpx	#$00
	beq	L139f
	lda	#$10
L139f:	ora	int_flags
	sta	int_flags
	bne	L13b8		; always taken


L13a5:	stx	drive_idx
	jsr	S1dc5
	ldx	drive_idx
	lda	montime
	sta	mtimer+2
	bne	L13b8
L13b2:	lda	#$00
	sta	D25,x
	sta	D9c,x
L13b8:	jsr	S1ea1
	bcc	L13e1
	lda	D25,x
	beq	L13e1
	jsr	S1570
	bcc	L13da

	lda	D58,x
	bne	L13e5
	dec	D58,x

	lda	#$02		; set interrupt flag for appropriate drive
	cpx	#$00
	beq	L13d4
	lda	#$20
L13d4:	ora	int_flags
	sta	int_flags
	bne	L13e5		; always taken

L13da:	stx	drive_idx
	jsr	unclamp_cmd
	ldx	drive_idx
L13e1:	lda	#$00
	sta	D58,x

L13e5:	lda	ioport_10,x
	dex
	dex
	beq	L1380
	sta	ioport_1d
L13ef:	lda	int_flags
	and	#$07
	beq	L13f7
	ora	#$08
L13f7:	sta	D6e
	lda	int_flags
	and	#$70
	beq	L1401
	ora	#$80
L1401:	ora	D6e
	sta	int_flags
	and	int_mask
	sta	int_pend
	beq	L140f
	sta	int_68k_ena
	rts
L140f:	sta	int_68k_dis
	rts


; write timing pattern, X+1 $a9 nibbles
write_timing_pat:
	lda	#$a9
	jsr	S126b
	inc	D6e
	dex
	bne	write_timing_pat
	lda	#$a9
	nop
	nop
	jmp	S126b


docmd:	ldx	#$07		; copy first 8 bytes of command block
L1426:	lda	iob,x
	sta	iiob,x
	dex
	bpl	L1426

; note that this leaves iob_cmd in A

	cmp	#cmd_rwts	; command $81 - RWTS
	beq	rwts
	cmp	#cmd_handshake	; command $80 - handshake
	bne	L1439
	lda	#noerr
	beq	cmd_done

L1439:	sec
	sbc	#cmd_seek
	bcc	badcmd
	cmp	#cmd_go_away-cmd_seek+2	; BUG!!! should be +1
	bcc	L1446

badcmd:	lda	#err_badcmd
	bne	cmd_done

L1446:	tax
	sta	cmd_save
	lda	gen_cmd_val_mask,x
	jsr	validate_args
	bcs	cmd_done
	lda	cmd_save
	asl	a
	tax
	jsr	call_cmd
	lda	cmd_save
	beq	L1468
	cmp	#$01
	beq	L1468
	lda	#noerr

cmd_done:
	sta	iob_errstat		; command complete, store result
	lda	#cmd_completed
	sta	iob_cmd
L1468:	rts


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
	beq	L148b
	lda	#err_intpend

L1486:	jsr	cmd_done
	beq	rwts_done	; always taken

L148b:	ldx	iiob_fcn
	cpx	#max_rwts_fcn+1
	bcc	L1495
	lda	#err_badcmd
	bne	L1486		; always taken

L1495:	lda	rwts_cmd_val_mask,x	; validate arguments
	jsr	validate_args

	jsr	cmd_done	; huh???  we're not done yet
	bcs	rwts_done

	jsr	start_disk_op	; init counters & timer, select side

	lda	iiob_fcn	; displatch
	asl	a
	tax
	jsr	call_rwts_cmd

	sta	mem_68k_ena

	bcc	rwts_done
	sta	iob_errstat

rwts_done:
	lda	#$04		; set rwts done flag for appropriate
	ldx	iiob_drv	;   drive
	beq	L14b9
	lda	#$40
L14b9:	ora	int_flags
	sta	int_flags
	jmp	L13ef


call_rwts_cmd:
	lda	rwts_cmd_tbl+1,x
	pha
	lda	rwts_cmd_tbl,x
	pha
	sta	mem_68k_dis
	rts

rwts_cmd_tbl:
	fdb	read_cmd-1	; read
	fdb	write_cmd-1	; write
	fdb	unclamp_cmd-1	; unclamp
	fdb	fmt_ver_cmd-1	; format disk
	fdb	fmt_ver_cmd-1	; verify disk
	fdb	fmt_ver_cmd-1	; format track
	fdb	fmt_ver_cmd-1	; verify track
	fdb	read_brute_cmd-1	; read (no checksum)
	fdb	write_brute_cmd-1		; write (no checksum)
	fdb	clamp_cmd-1	; clamp


validate_args:
	sta	D6e
	beq	L14fb
	ldx	#validate_jmp_tab_size-2
L14e6:	lsr	D6e
	bcc	L14f7
	stx	D6f
	lda	validate_jmp_tab+1,x
	pha
	lda	validate_jmp_tab,x
	pha
	rts


L14f5:	ldx	D6f
L14f7:	dex
	dex
	bpl	L14e6
L14fb:	lda	#$00
	clc
	rts
L14ff:	sec
	rts


validate_drive:
	ldx	iiob_drv
	beq	L150f
	cpx	#$80
	beq	L150d
	lda	#err_baddrive
	bne	L14ff
L150d:	ldx	#$02
L150f:	stx	drive_idx
	sta	ioport_11,x
	lda	D25,x
	bne	L151c
	lda	#err_nodisk
	bne	L14ff

L151c:	jsr	S1570
	bcs	L14f5
	lda	#err_drvdis
	bne	L14ff


validate_side:
	lda	iiob_side
	and	#$fe
	beq	L14f5
	lda	#err_badside
	bne	L14ff


validate_sect:
	jsr	getzone		; find what zone we're in
	lda	iiob_sect
	cmp	spt_tab,y	; is the sector number valid for this zone?
	bcc	L14f5
	lda	#err_badsect
	bne	L14ff


validate_track:
	lda	iiob_trk
	cmp	#max_track+1
	bcc	L14f5
	lda	#err_badtrack
	bne	L14ff


validate_mask:
	lda	iiob_arg
	and	#$77
	beq	L14f5
	lda	#err_badmask
	bne	L14ff


validate_fmt:	
	inc	iob_fmt_conf
	beq	L14f5
	lda	#err_invfmt
	bne	L14ff


validate_wr_en:
	jsr	get_wr_prot
	bcc	L14f5
	lda	#err_wprot
	bne	L14ff


validate_jmp_tab:
	fdb	validate_wr_en-1
	fdb	validate_fmt-1
	fdb	validate_mask-1
	fdb	validate_track-1
	fdb	validate_sect-1
	fdb	validate_side-1
	fdb	validate_drive-1
validate_jmp_tab_size	equ	*-validate_jmp_tab


S1570:	lda	#$80
	cpx	#$00
	bne	L1578
	lda	#$08
L1578:	and	int_mask
	adc	#$f8
	rts


; start a disk operation:
;   init retry counters
;   clear error counters
;   start motor timer
;   select disk side

start_disk_op:
	lda	max_retry	; init retry counter
	sta	retry_cnt
	lda	max_reseek
	sta	reseek_cnt

	lda	#$00		; clear error counters
	ldx	#errcnt_tbl_size-1
L1589:	sta	errcnt_tbl,x
	dex
	bpl	L1589

	sta	mtimer		; init motor timer
	sta	mtimer+1
	lda	montime
	sta	mtimer+2

	jmp	select_side


; go command - execute machine code

gocmd:	jsr	S173f
	jmp	(iiob_arg)


; clear interrupt status command

clrintcmd:
	lda	iiob_arg
	pha
	eor	#$ff
	and	int_flags
	sta	int_flags
	pla
	and	#$22
	beq	L15bf
	lsr	a
	lsr	a
	beq	L15b6
	sta	ioport_13
	bcc	L15b9
L15b6:	sta	ioport_11
L15b9:	jsr	S1373
	jsr	S1754
L15bf:	jmp	L13ef


; set interrupt mask command

setmskcmd:
	lda	iiob_arg
	ora	int_mask
	sta	int_mask
	jmp	L13ef


; clear interrupt mask command

clrmskcmd:
	lda	iiob_arg
	eor	#$ff
	and	int_mask
	sta	int_mask
	jmp	L13ef


; seek command

seekcmd:	
	jsr	start_disk_op	; init counters & timer, select side
	lda	#noerr
	jsr	cmd_done

seek1:	ldx	drive_idx	; are we on the correct side?
	lda	iiob_side
	cmp	drv_side,x
	bne	seek2
	lda	iiob_trk	; are we on the correct track?
	cmp	drv_trk,x
	bne	seek2

	lda	drv_act_pos,x	; yes, just stay where we are
	sta	req_act_pos
	lda	drv_act_pos+1,x
	sta	req_act_pos+1

	jmp	seek3


seek2:	ldx	drive_idx	; need to move, figure out where
	lda	#$10
	clc
	adc	drv_trk_offset,x
	sta	D6e
	lda	#$00
	sta	req_act_pos+1
	sta	D26,x

	lda	iiob_trk	; remember track
	sta	drv_trk,x

	ldy	iiob_side	; remember side
	sty	drv_side,x
	bne	L1615

	lda	#max_track	; if back side, complement track number
	sec
	sbc	iiob_trk

L1615:	asl	a		; multiply track by 8 and add D6e to
	asl	a		; get desired actuator position
	asl	a
	rol	req_act_pos+1
	adc	D6e
	sta	req_act_pos
	bcc	seek3
	inc	req_act_pos+1


seek3:	lda	#$00
	sta	spd_chg_flag	; clear speed change flag
	sta	spd_chg_settle
	ldx	drive_idx
	jsr	getzone		; find what zone we're in

	lda	iiob_spd	; are we already going the right speed?
	clc
	adc	zonespd,y
	clc
	adc	drv_speed_delta,x
	cmp	drv_req_speed,x
	beq	L1642		; no speed change

	dec	spd_chg_flag	; set speed change flag
	ldy	slow_dn_settle_time
	sty	spd_chg_settle
	sta	drv_req_speed,x

L1642:	ldy	D4b,x
	bne	L164e
	dec	spd_chg_flag	; set speed change flag
	dec	D4b,x
	ldy	spd_up_settle_time
	sty	spd_chg_settle

L164e:	ldy	drv_act_pos,x	; get remembered actuator position
	sty	act_pos
	ldy	drv_act_pos+1,x
	sty	act_pos+1

	ldy	spd_chg_flag	; speed change?
	beq	L165d

	jsr	set_drv_speed

L165d:	lda	req_act_pos+1	; does actuator position match requested
	cmp	act_pos+1	;   position?
	bne	L166f
	lda	req_act_pos
	cmp	act_pos
	bne	L166f

	lda	spd_chg_settle		; speed change delay?
	bne	L16d7
	beq	L16da

L166f:	lda	req_act_pos
	sec
	sbc	D11
	sta	D6a
	lda	req_act_pos+1
	sbc	#$00
	sta	D6a+1

	clc
	lda	act_pos
	sbc	D6a
	sta	D6a
	sta	act_step_count
	lda	act_pos+1
	sbc	D6a+1
	bcc	L16a2
	sta	D6a+1
	sta	act_step_count+1

L168f:	jsr	calc_step_time
	jsr	step_act_in

	sec
	lda	act_step_count
	sbc	#$01
	sta	act_step_count
	bcs	L168f
	dec	act_step_count+1
	bpl	L168f

L16a2:	clc
	lda	req_act_pos
	sbc	act_pos
	sta	D6a
	sta	act_step_count
	lda	req_act_pos+1
	sbc	act_pos+1
	sta	D6a+1
	sta	act_step_count+1

L16b3:	jsr	calc_step_time
	jsr	step_act_out

	sec
	lda	act_step_count
	sbc	#$01
	sta	act_step_count
	bcs	L16b3
	dec	act_step_count+1
	bpl	L16b3

	lda	act_pos
	and	#$03
	tax
	lda	step_single_phase_tbl,x
	jsr	set_stepper_phases

	lda	spd_chg_settle
	bne	L16d7
	lda	seek_settle_time
L16d7:	jsr	long_delay
L16da:	lda	#all_phases_off
	jsr	set_stepper_phases

	lda	act_pos		; save current phase
	ldx	drive_idx
	sta	drv_act_pos,x
	lda	act_pos+1
	sta	drv_act_pos+1,x

	rts


; shift the drive speed in A out serially to the drive
set_drv_speed:
	sta	pwm_clk_high,x
	ldy	#$07
L16ef:	lsr	a
	sta	phase0_low
	bcc	L16f8
	sta	phase0_high
L16f8:	sta	pwm_clk_low,x
	sta	pwm_clk_high,x
	dey
	bpl	L16ef
	sta	phase0_low
	rts


calc_step_time:
	ldx	#$03		
	lda	act_step_count+1
	bne	L1712
	lda	act_step_count
	cmp	#$03
	bcs	L1712
	tax
L1712:	lda	D1139,x
	sta	D6e
	ldx	#$03
	sec
	lda	D6a
	sbc	act_step_count
	sta	D6f
	lda	D6a+1
	sbc	act_step_count+1
	bne	L172d
	lda	D6f
	cmp	#$03
	bcs	L172d
	tax
L172d:	lda	D1139,x
	cmp	D6e
	bcs	L1736
	lda	D6e
L1736:	sta	step_delay_time
	rts


haltcmd:
	jsr	S173f
L173c:	jmp	L173c

S173f:	jsr	S1745
	jmp	cmd_done

S1745:	jsr	S1978

S1748:	lda	#$00
	sta	D4b
	sta	D4b+2

	sta	pwm_clk_low
	sta	ioport_16

S1754:	sta	ioport_10
	sta	ioport_12
	rts


calc_cksm:
	lda	#$00
	sta	chksum
	sta	chksum+1
	sta	data_ptr
	sta	data_ptr+1
	inc	data_ptr+1
	ldy	#$f4
L1769:	asl	a
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
	bne	L1781
	inc	data_ptr+1
L1781:	lda	(data_ptr),y
	tax
	adc	chksum+1
	sta	chksum+1
	txa
	eor	chksum
	sta	(data_ptr),y
	iny
	beq	L17a1
	lda	(data_ptr),y
	tax
	eor	chksum+1
	sta	(data_ptr),y
	txa
	adc	chksum+2
	iny
	bne	L1769
	inc	data_ptr+1
	bne	L1769
L17a1:	rts


pack_misc_nib:
	ldy	D01f4
	ldx	D01f5
	lda	D01f6
	jsr	pack_ms_nib
	sta	misc_nib_1
	ldy	D0200
	ldx	D0201
	lda	D0202
	jsr	pack_ms_nib
	sta	misc_nib_2
	ldy	D02ff
	ldx	D0300
	lda	D0301
	jsr	pack_ms_nib
	sta	misc_nib_3
	ldy	D0302
	ldx	D0303
	lda	D0304
	jsr	pack_ms_nib
	sta	misc_nib_4
	ldy	D03fe
	ldx	D03ff
	lda	#$00
	jsr	pack_ms_nib
	sta	misc_nib_5
	ldy	chksum
	ldx	chksum+1
	lda	chksum+2
	jsr	pack_ms_nib
	sta	misc_nib_6
	rts


	fillto	$1800,fill_byte

; read a data field (after the proper address mark has been read)

read_data_field:
	jsr	find_data_header	; find a data header
	bcc	L1806
	rts

L1806:	ldy	#$f4			; start at $01f4
L1808:	inc	data_ptr+1
L180a:	ldx	q7l
	bpl	L180a
	lda	denib_tab,x
	asl	a
	asl	a
	tax
	asl	a
	asl	a
	sta	D6e
	txa
	and	#$c0
L181c:	ldx	q7l
	bpl	L181c
	ora	denib_tab,x
	sta	(data_ptr),y
	lda	D6e
	iny
	bne	L182d
	inc	data_ptr+1
L182d:	ldx	q7l
	bpl	L182d
	and	#$c0
	ora	denib_tab,x
	sta	(data_ptr),y
	iny
	beq	L1851
	lda	D6e
	asl	a
	asl	a
L1840:	ldx	q7l
	bpl	L1840
	and	#$c0
	ora	denib_tab,x
	sta	(data_ptr),y
	iny
	beq	L1808
	bne	L180a

L1851:	ldx	q7l	; now read four nibbles to make three
	bpl	L1851		; user checksum bytes
	lda	denib_tab,x
	asl	a
	asl	a
	tay
	and	#$c0
L185e:	ldx	q7l
	bpl	L185e
	ora	denib_tab,x
	sta	chksum
	tya
	asl	a
	asl	a
	tay
	and	#$c0
L186e:	ldx	q7l
	bpl	L186e
	ora	denib_tab,x
	sta	chksum+1
	tya
	asl	a
	asl	a
L187b:	ldx	q7l
	bpl	L187b
	ora	denib_tab,x
	sta	chksum+2
	jsr	S18a1

L1888:	lda	q7l	; verify data field trailer
	bpl	L1888
	cmp	#$de
	bne	L18a2
	jsr	S18a1
L1894:	lda	q7l
	bpl	L1894
	cmp	#$aa
	bne	L18a2
	clc
L189e:	lda	q6h
S18a1:	rts

L18a2:	inc	D5c
	sec
	bcs	L189e

	inc	D5d
	sec
	bcs	L189e

L18ac:	inc	D5b
	sec
	bcs	L189e


; find a data field header

find_data_header:
	lda	q6l
	ldx	#$00
	ldy	#$20
L18b8:	dey
	beq	L18ac
L18bb:	lda	q7l
	bpl	L18bb
L18c0:	cmp	#$d5
	bne	L18b8
	stx	data_ptr
L18c6:	lda	q7l
	bpl	L18c6
	cmp	#$aa
	bne	L18c0
	stx	data_ptr+1
L18d1:	lda	q7l
	bpl	L18d1
	cmp	#$ad
	bne	L18c0
L18da:	lda	q7l
	bpl	L18da
	clc
	rts


read_brute_cmd:
	lda	#$ff
	bne	L18e7

read_cmd:
	lda	#$00
L18e7:	sta	brute_flag
	jsr	seekcmd
L18ec:	jsr	find_addr	; find address field
	bcs	L1902
	jsr	read_data_field	; read data field
	bcs	L1901

	ldx	brute_flag
	bne	L1909

	jsr	vfy_cksm	; verify data field checksum
	bcc	L1909
	inc	D5d
L1901:	clv
L1902:	jsr	retry
	bcc	L18ec
	lda	#err_cant_read
L1909:	rts


retry:	bvs	L1960		;  retry counter expired?
	dec	retry_cnt
	beq	L1912		; yes, maybe do a reseek
	clc			;  no, just try again
	rts

L1912:	lda	max_retry	; reinit retry counter
	sta	retry_cnt

	lda	iiob_fcn	; are we doing a write?
	cmp	#rwts_fcn_write
	beq	L1960
	cmp	#rwts_fcn_write_brute
	beq	L1960

	ldx	drive_idx	; must be a read, we can try positioner offsets
	lda	D26,x
	cmp	max_track_offset
	beq	L1960
	eor	#$ff
	clc
	adc	#$01
	sta	D26,x
	clc
	adc	drv_act_pos,x
	sta	req_act_pos
	lda	#$00
	ldy	D26,x
	bpl	L193c
	lda	#$ff
L193c:	adc	drv_act_pos+1,x
	sta	req_act_pos+1
	ldy	D26,x
	beq	L1946
	bpl	L1947
L1946:	dey
L1947:	tya
	sta	D26,x
	clc
	adc	req_act_pos
	sta	req_act_pos
	lda	#$00
	ldy	D26,x
	bpl	L1957
	lda	#$ff
L1957:	adc	req_act_pos+1
	sta	req_act_pos+1
	jsr	seek3
	clc
	rts

L1960:	dec	reseek_cnt
	bmi	L1976
	lda	#$00
	sta	D26,x
	lda	#$ff
	sta	D4f
	jsr	S1d2e
	bcs	L1975
	jsr	seek1
	clc
L1975:	rts
L1976:	sec
	rts


S1978:	ldx	#$02
L197a:	lda	D4b,x
	beq	L199d
	lda	ioport_11,x
	stx	drive_idx

	lda	#$00
	sta	iiob_trk
	sta	iiob_side

	lda	#$01		; req_act_pos = $0180
	sta	req_act_pos+1
	lda	#$80
	sta	req_act_pos

	jsr	seek3

	ldx	drive_idx
	lda	ioport_10,x
	lda	#$ff
	sta	drv_trk,x
L199d:	dex
	dex
	beq	L197a
	rts


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


write_cmd:
	jsr	calc_cksm
write_brute_cmd:
	jsr	pack_misc_nib
	jsr	seek2		; shouldn't this be seekcmd???
L19c4:	jsr	find_addr	; find address field
	bcc	j_write_data_field
	jsr	retry
	bcc	L19c4
	lda	#err_cant_write
	rts

S19d1:	jsr	S1271
	jsr	S1af5
	ldx	D0300
	lda	nib_tab,x
	nop
	nop
	jsr	S1af5
	ldx	D0301
	lda	nib_tab,x
	pha
	pla
	jmp	S1af5


	fillto	$1a00,fill_byte

; write data field contents

j_write_data_field:
	jmp	write_data_field

write_data_field_2:
	lda	misc_nib_1
	ldy	D65
	sty	data_ptr+1
L1a09:	nop
	nop
	nop
	sta	q6h
	lda	q6l
	lda	D01f9,y
	inc	D6e
	lsr	a
	lsr	a
	sta	D6e
	ldx	D01f4,y
	lda	nib_tab,x
	sta	q6h
	lda	q6l
	lda	D01f8,y
	and	#$c0
	ora	D6e
	lsr	a
	lsr	a
	sta	D6e
	ldx	D01f5,y
	lda	nib_tab,x
	sta	q6h
	lda	q6l
	lda	D01f7,y
	and	#$c0
	ora	D6e
	lsr	a
	lsr	a
	sta	D6e
	ldx	D01f6,y
	lda	nib_tab,x
	sta	q6h
	lda	q6l
	ldx	D6e
	lda	nib_tab,x
	iny
	iny
	iny
	cpy	#$09
	bne	L1a09

	ldx	#$fd
	inc	data_ptr+1
	sta	q6h
	lda	q6l
	jsr	write_chunk

	lda	misc_nib_2
	ldy	#$00
L1a72:	nop
	nop
	nop
	sta	q6h
	lda	q6l
	lda	D0205,y
	inc	D6e
	lsr	a
	lsr	a
	sta	D6e
	ldx	D0200,y
	lda	nib_tab,x
	sta	q6h
	lda	q6l
	lda	D0204,y
	and	#$c0
	ora	D6e
	lsr	a
	lsr	a
	sta	D6e
	ldx	D0201,y
	lda	nib_tab,x
	sta	q6h
	lda	q6l
	lda	D0203,y
	and	#$c0
	ora	D6e
	lsr	a
	lsr	a
	sta	D6e
	ldx	D0202,y
	lda	nib_tab,x
	sta	q6h
	lda	q6l
	ldx	D6e
	lda	nib_tab,x
	iny
	iny
	iny
	cpy	#$fc
	bne	L1a72
	inc	data_ptr+1

	ldx	#$fc
	sta	q6h
	lda	q6l
	jsr	write_chunk

	ldy	misc_nib_3
	ldx	D02ff
	lda	nib_tab,x
	sty	q6h
	ldy	q6l
	jsr	S19d1
	pha
	pla
	lda	misc_nib_4
	ldy	#$02
	bne	write_data_field_3

S1af0:	nop
	inc	D6e

S1af3:	ldx	D6e

S1af5:	sta	q6h
	lda	q6l
	rts


	fillto	$1b00,fill_byte

L1b00:	nop
	nop
write_data_field_3:
	nop
	sta	q6h
	lda	q6l
	lda	D0305,y
	inc	D6e
	lsr	a
	lsr	a
	sta	D6e
	ldx	D0300,y
	lda	nib_tab,x
	sta	q6h
	lda	q6l
	lda	D0304,y
	and	#$c0
	ora	D6e
	lsr	a
	lsr	a
	sta	D6e
	ldx	D0301,y
	lda	nib_tab,x
	sta	q6h
	lda	q6l
	lda	D0303,y
	and	#$c0
	ora	D6e
	lsr	a
	lsr	a
	sta	D6e
	ldx	D0302,y
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
	jsr	S1b99
	lda	misc_nib_6

	ldx	#$7e
	ldy	D65
	sty	data_ptr+1
	sta	q6h
	lda	q6l
	jsr	write_chunk

	lda	#$de		; write trailer
	jsr	S1af3
	lda	#$aa
	jsr	S1af0
	lda	#$ff
	jsr	S1af0
	jmp	S1be7


S1b99:	ldy	#$01
	bne	L1ba1		; always taken


write_chunk:
	stx	data_ptr
	ldy	#$00
L1ba1:	lda	(data_ptr),y
	nop
	tax
	lda	nib_tab,x
	sta	q6h
	lda	q6l
	iny
	cpy	#$03
	beq	L1bb6
	nop
	bne	L1ba1
L1bb6:	rts


; write a data field

write_data_field:
	ldy	#$05
	lda	#$ff
	sta	D77
	jsr	write_sync_pat
	ldy	#$ad		; write a data mark
	jsr	write_mark
	ldx	iiob_sect
	jsr	S1261
	jmp	write_data_field_2	; write data field contents and trailer


write_mark:
	inc	D6e		; BUG???  seems useless since next instruction
				; will overwrite it

write_mark_1:
	sta	D6e		; write an address or data mark
	lda	#$d5
	sta	q6h
	lda	q6l
	nop
	nop
	nop
	lda	#$aa
	jsr	S1267
	tya
	cmp	D6e
	jmp	S1267


S1be7:	lda	q7l
	lda	q6h
	lda	ioport_0a
	rts


fmt_ver_cmd:
	lda	#max_track	; set starting track number, in case we're
	sta	D9d		; doing a full disk

	jsr	clr_data_buf
fmt_ver_side:
	jsr	select_side
	beq	L1c01
	lda	#$dc
	bne	L1c03
L1c01:	lda	#$38
L1c03:	jsr	change_speed
	jsr	S1d2e
	bcc	L1c0e
	lda	#$07
L1c0d:	rts

L1c0e:	lda	iiob_fcn	; are we formatting the enire disk?
	cmp	#rwts_fcn_format
	beq	fmt_entire_disk

	cmp	#rwts_fcn_verify ; are we verifying the entire disk?
	beq	L1c3e

	bne	fmt_ver_single_track

; enter here if we're formatting an entire disk.  first do the timing track

fmt_entire_disk:
	lda	#$3f		; start with track 63
	sta	iiob_trk
	lda	#$15
	ldy	iiob_side
	bne	L1c32
	lda	#$0e
	sta	fmt_sect_cnt
	jsr	format_track
	jsr	S1f7f
	bcs	L1c0d
	lda	#$0f
L1c32:	sta	fmt_sect_cnt
	jsr	format_track
	jsr	verify_track

	lda	#err_cant_wrcal
	bcs	fmt_rts

; if we get here, we're doing a full disk format or verify

L1c3e:	lda	D9d
	sta	iiob_trk

; enter here if we're only doing a single track format or verify

fmt_ver_single_track:	
	jsr	seek1
	jsr	getzone		; find what zone we're in
	lda	spt_tab,y	; get the sectors per track
	sta	fmt_sect_cnt

	ldy	iiob_fcn	; are we formatting or verifying?
	cpy	#rwts_fcn_format
	beq	L1c57
	cpy	#rwts_fcn_fmt_trk
	bne	L1c5a

L1c57:	jsr	format_track

L1c5a:	jsr	verify_track
	bcs	vfy_err

	lda	iiob_fcn	; one track or entire disk?
	cmp	#rwts_fcn_fmt_trk
	bcs	fmt_done_noerr	; just one track, we're done

	ldy	iiob_side
	bne	L1c76

; track done, side 0

	dec	iiob_trk	; advance to next track
	bpl	fmt_ver_single_track

	lda	#$00
	sta	D9d
	inc	iiob_side	; advance to side 1
	jmp	fmt_ver_side

; track done, side 1

L1c76:	inc	iiob_trk
	lda	iiob_trk
	cmp	#max_track+1
	bne	fmt_ver_single_track
fmt_done_noerr:
	clc
	rts

vfy_err:
	lda	iiob_trk
	sta	error_track_num
	lda	iiob_side
	sta	error_sect_num
	lda	#err_cant_vfy
fmt_rts:
	rts


; format a track with fmt_sect_cnt sectors,
; using 2:1 interleave

format_track:
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

L1ca5:	ldx	fmt_even_odd	; are we doing an even or odd sector?
	beq	L1cab
	ldx	#$ff
L1cab:	inx
	stx	fmt_even_odd

	lda	fmt_sect_num,x	; get the sector number
	sta	iiob_sect
	inc	fmt_sect_num,x	; advance the counter

	jsr	format_sect_n	; format a sector

	dec	fmt_sect_cntr	; more sectors to be formatted?
	bne	L1ca5
	rts


verify_track:
	lda	#$00
	sta	iiob_sect
	sta	bad_sec_total

L1cc3:	lda	max_retry	; init retry counter
	sta	retry_cnt

L1cc7:	jsr	find_addr	; find address field
	bcs	L1ce5
	jsr	read_data_field	; read data field
	bcs	L1ce5
	jsr	vfy_cksm	; verify data field checksum
	bcs	L1ce5
L1cd6:	inc	iiob_sect
	lda	iiob_sect
	cmp	fmt_sect_cnt
	bne	L1cc3
	lda	bad_sec_total
	bne	L1ce4
	clc
L1ce4:	rts
L1ce5:	dec	retry_cnt
	bne	L1cc7
	ldy	bad_sec_total
	lda	iiob_sect
	sta	bad_sect_map,y
	inc	bad_sec_total
	bne	L1cd6		; always taken


clr_data_buf:
	lda	#$00		; clear data buffer and chksum
	tay
L1cf9:	sta	D0200,y
	sta	D0300,y
	iny
	bne	L1cf9
	ldy	#$0b
L1d04:	sta	D01f4,y
	dey
	bpl	L1d04
	sta	chksum		; clear user checksum
	sta	chksum+1
	sta	chksum+2
	sta	D4f
	sta	iiob_spd
	jmp	pack_misc_nib


; change the drive speed and wait appropriately
change_speed:
	ldx	drive_idx
	clc
	adc	drv_speed_delta,x
	sta	drv_req_speed,x
	jsr	set_drv_speed
	lda	slow_dn_settle_time
	ldy	D4b,x
	bne	L1d2b
	dec	D4b,x
	lda	spd_up_settle_time
L1d2b:	jmp	long_delay


S1d2e:	ldy	#$00
	sty	act_pos
L1d32:	lda	D1139,y
	sta	step_delay_time
	jsr	step_act_in
	iny
	cpy	#$04
	bne	L1d32
L1d3f:	jsr	S1e98
	bcc	L1db3
	jsr	S1e8f
	bcs	L1d53
	ldy	#$03
L1d4b:	jsr	step_act_in
	dey
	bpl	L1d4b
	bmi	L1d3f
L1d53:	ldy	#$03
L1d55:	lda	D113d,y
	sta	step_delay_time
	jsr	step_act_in
	dey
	bpl	L1d55
	lda	#$ff
	sta	step_delay_time
L1d64:	ldy	#$03
L1d66:	jsr	step_act_out
	dey
	bpl	L1d66
	lda	step_single_phase_tbl
	jsr	set_stepper_phases
	jsr	S1e8f
	bcs	L1d64
	lda	D1139
	sta	step_delay_time
	ldy	#$0b
L1d7e:	jsr	step_act_in
	dey
	bpl	L1d7e
	ldy	#$03
L1d86:	jsr	step_act_out
	dey
	bpl	L1d86
	lda	seek_settle_time
	jsr	long_delay

	lda	#all_phases_off
	jsr	set_stepper_phases

	ldx	drive_idx		; save phase $0008
	lda	#$08
	sta	drv_act_pos,x
	ldy	#$00
	sty	drv_act_pos+1,x

	dey				; track $ff = unknown?
	sty	drv_trk,x

	lda	D4f
	beq	L1db1
	lda	ioport_09
	jsr	S1f7f
	bcc	L1db1
	bit	D67
L1db1:	clc
	rts
L1db3:	jsr	unclamp_cmd
	lda	#err_cant_clamp
	sec
	rts


clamp_cmd:
	ldx	iiob_drv	; convert iiob_drv to drive_idx
	beq	L1dc0
	ldx	#$02
L1dc0:	stx	drive_idx

	sta	ioport_11,x

S1dc5:	lda	#$ff
	sta	D49
	sta	D4f
	lda	#$00
	sta	act_pos
	lda	#$38
	jsr	change_speed
	jsr	S1d2e
	bcs	L1de6
	ldy	#$00
	ldx	drive_idx
	sty	drv_trk_offset,x
	sty	D58,x
	sty	D9c,x
	dey
	sty	D25,x
L1de6:	rts


unclamp_cmd:
	ldx	drive_idx
	lda	pwm_clk_low,x
	lda	#$00
	sta	D4b,x
	sta	act_pos
	ldx	#$00
L1df4:	lda	D113d,x
	sta	step_delay_time
	stx	unclamp_cnt
	jsr	step_act_out
	ldx	unclamp_cnt
	inx
	cpx	#$04
	bne	L1df4
	lda	D1c
	asl	a
	sta	unclamp_cnt
L1e0a:	jsr	S1e98
	bcc	L1e22
	lda	#$03
	sta	D77
L1e13:	jsr	step_act_out
	dec	D77
	bpl	L1e13
	dec	unclamp_cnt
	bne	L1e0a
	lda	#err_cant_uncl
	sec
	rts

L1e22:	lda	D1c
	sta	D77
L1e26:	jsr	step_act_out
	dec	D77
	bne	L1e26
	lda	#all_phases_off
	jsr	set_stepper_phases
	ldx	drive_idx
	lda	#$00
	sta	D25,x
	sta	ioport_10,x
	lda	#$05
	jmp	delay


; step actuator in (toward track 0)
step_act_in:
	lda	act_pos
	bne	L1e46
	dec	act_pos+1
L1e46:	dec	act_pos
	and	#$03
	tax
	lda	step_phase_tbl_2,x
	jsr	set_stepper_phases
	jmp	step_delay


; step actuator out (away from track 0)
step_act_out:
	lda	act_pos
	and	#$03
	tax
	lda	step_phase_tbl_1,x
	jsr	set_stepper_phases
	inc	act_pos
	bne	L1e65
	inc	act_pos+1
L1e65:	jmp	step_delay


set_stepper_phases:
	ldx	#$08
L1e6a:	dex
	asl	a
	bcc	L1e71
	sta	phase0_low,x
L1e71:	bne	L1e6a
	rts


step_delay:
	lda	step_delay_time

delay:	ldx	#$23
L1e78:	dex
	bne	L1e78
	ldx	#$26
	nop
	sec
	sbc	#$01
	bne	L1e78
	nop
	rts


long_delay:
	tay
L1e86:	lda	#$50
	jsr	delay
	dey
	bne	L1e86
	rts


S1e8f:	lda	ioport_09
	lda	phase0_high
	jmp	L1eb0

S1e98:	lda	phase0_high
	lda	ioport_08
	jmp	L1eb0

S1ea1:	lda	ioport_08
	lda	phase0_low
	jmp	L1eb0


; returns with carry set if disk is write-protected

get_wr_prot:
	lda	ioport_09
	lda	phase0_low
L1eb0:	lda	q6h
	lda	q7l
	lsr	a
	lda	phase0_low

select_side:
	sta	ioport_09	; select side 0
	lda	iiob_side	; is that correct?
	beq	L1ec4
	sta	ioport_08	; nope, select side 1
L1ec4:	rts


; verify data field checksum

vfy_cksm:
	lda	#$00
	sta	chksum2
	sta	chksum2+1
	sta	data_ptr
	sta	data_ptr+1
	inc	data_ptr+1
	ldy	#$f4
L1ed3:	asl	a
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
	bne	L1ee9
	inc	data_ptr+1
L1ee9:	lda	(data_ptr),y
	eor	chksum2
	sta	(data_ptr),y
	adc	chksum2+1
	sta	chksum2+1
	iny
	beq	L1f05
	lda	(data_ptr),y
	eor	chksum2+1
	sta	(data_ptr),y
	adc	chksum2+2
	iny
	bne	L1ed3
	inc	data_ptr+1
	bne	L1ed3
L1f05:	cmp	chksum+1
	bne	L1f17
	lda	chksum2+2
	cmp	chksum+2
	bne	L1f17
	lda	chksum2
	cmp	chksum
	bne	L1f17
	clc
	rts
L1f17:	sec
	rts


S1f19:	sta	q6l
	lda	#$03
	sta	D96
L1f20:	lda	#$ff
	sta	D98
	dec	D96
	bne	L1f2b
	sec
	bcs	L1f74
L1f2b:	ldy	#$00
	sty	D77
L1f2f:	inc	D6e
	nop
L1f32:	lda	q7l
	cmp	#$a9
	beq	L1f4f
	iny
	bne	L1f2f
L1f3c:	inc	D77
	bne	L1f32
	beq	L1f20
L1f42:	lda	D1f78,x
	clc
	sty	D6e
	adc	D6e
	tay
	bcs	L1f3c
	bcc	L1f2f
L1f4f:	ldx	#$06
L1f51:	lda	q7l
	bpl	L1f51
	cmp	#$a9
	bne	L1f42
	dex
	bne	L1f51
	ldx	#$08
L1f5f:	lda	q7l
	bpl	L1f5f
	cmp	#$d5
	beq	L1f6d
	dex
	bne	L1f5f
	beq	L1f20
L1f6d:	inc	D98
	beq	L1f2b
	lda	D77
	clc
L1f74:	ldx	q6h
	rts

D1f78:	fcb	$00,$0a,$08,$06,$05,$03,$02

S1f7f:	lda	#$1f
	sta	D9b
L1f83:	jsr	S1f19
	bcc	L1f8b
	lda	#err_cant_cal
	rts
L1f8b:	ldx	drive_idx
	sty	D99
	sta	D9a
	cmp	#$49
	bcc	L1fa9
	bne	L1f9d
	cpy	#$4f
	bcc	L1fa9
	clc
	rts
L1f9d:	cmp	#$49
	bne	L1fa5
	cpy	#$e6
	bcc	L1fba
L1fa5:	dec	drv_speed_delta,x
	bne	L1fab
L1fa9:	inc	drv_speed_delta,x
L1fab:	dec	D9b
	bmi	L1fb7
	lda	#$38
	jsr	change_speed
	jmp	L1f83
L1fb7:	sec
	lda	#err_cant_setspd
L1fba:	rts


	fillto	$1ffb,fill_byte

	jmp	reset
	fdb	reset

	end
