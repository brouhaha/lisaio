# lisaio - Apple Lisa I/O processor firmware, partially reverse-engineered

Original code by Apple Computer Inc.
Disassembly including labels and comments copyright 1993-2019 Eric Smith <spacewar@gmail.com>

lisaio development is hosted at the
[lisaio Github repository](https://github.com/brouhaha/lisaio/).

## Introduction

The Apple Lisa, Lisa 2, and Macintosh XL computers used a 6504 microprocessor
(6502 variant) on the I/O board to control floppy disk drives. The floppy disk
controller hardware is essentially equivalent to the Apple Disk II controller,
but run at double the bit rate to provide higher disk capacity.

The original Lisa (aka Lisa 1) included two Apple "Twiggy" 5.25-inch
floppy disk drives, which required special Apple Fileware floppy disks,
and stored approximately 871KB per disk. The Twiggy drives were notoriously
unreliable, so all subsequent Lisa models used Sony 3.5-inch floppy drives
instead, storing 400KB on a single-sided disk.

After the Lisa and Macintosh XL were discontinued, Sun Remarketing
offered upgardes to double-sided drives, though this was only supported
by MacWorks, and not by any native Lisa software.

This repository contains partially reverse-engineered source code for
several versions of the Lisa I/O firmware.

* lisaio-twiggy.asm: Lisa 1 I/O firmware for Twiggy drives
* lisaio-sony.asm: Lisa 2 and Macintosh XL I/O firmware, builds four
  variants using conditional assembly:
    * Lisa 2/5 for Sony single-sided (400K) drive
    * Lisa 2/5 for Sony double-sided (800K) drive
    * Lisa 2/10 or Macintosh XL for Sony single-sided (400K) drive
    * Lisa 2/10 or Macintosh XL for Sony double-sided (800K) drive

The cross-assembler used is the [Macroassembler AS](http://john.ccac.rwth-aachen.de:8000/as/).
