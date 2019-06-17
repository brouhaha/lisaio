all: 	lisaio-twiggy-check \
	lisaio-old-sony400k-check lisaio-old-sony800k-check \
	lisaio-new-sony400k-check lisaio-new-sony800k-check

%.p %.lst: %.asm
	asl -cpu 6502 -L $<

%.bin: %.p
	p2bin -r '$$1000-$$1fff' $<

lisaio-old-sony400k.p lisaio-old-sony400k.lst: lisaio-sony.asm
	asl -cpu 6502 -D new_io=0 -D sony_800k=0 -o lisaio-old-sony400k.p -L -OLIST lisaio-old-sony400k.lst lisaio-sony.asm

lisaio-old-sony800k.p lisaio-old-sony800k.lst: lisaio-sony.asm
	asl -cpu 6502 -D new_io=0 -D sony_800k -o lisaio-old-sony800k.p -L -OLIST lisaio-old-sony800k.lst lisaio-sony.asm

lisaio-new-sony400k.p lisaio-new-sony400k.lst: lisaio-sony.asm
	asl -cpu 6502 -D new_io=1 -D sony_800k=0 -o lisaio-new-sony400k.p -L -OLIST lisaio-new-sony400k.lst lisaio-sony.asm

lisaio-new-sony800k.p lisaio-new-sony800k.lst: lisaio-sony.asm
	asl -cpu 6502 -D new_io=1 -D sony_800k -o lisaio-new-sony800k.p -L -OLIST lisaio-new-sony800k.lst lisaio-sony.asm

lisaio-twiggy-check:	lisaio-twiggy.bin
	echo '9306b364ee66acd90e6b9c452e5bad0258f692b63a859b5bb66db77fcb6aa179 lisaio-twiggy.bin' | sha256sum -c

lisaio-old-sony400k-check:	lisaio-old-sony400k.bin
	echo 'f7e6b15f7ca95ddefde8fc0854c84734802916f9bfff1ed420a4f9a2bf106dc4 lisaio-old-sony400k.bin' | sha256sum -c

lisaio-old-sony800k-check:	lisaio-old-sony800k.bin
	echo 'fa5f5218f1e29d6f193b05b9fd15cdb6b09dcd04594541aa62ed643317fcfd52 lisaio-old-sony800k.bin' | sha256sum -c

lisaio-new-sony400k-check:	lisaio-new-sony400k.bin
	echo '201b4e3958171a30a4df1f5166791111ccd3bf75363352346ac95d552b09228d lisaio-new-sony400k.bin' | sha256sum -c

lisaio-new-sony800k-check:	lisaio-new-sony800k.bin
	echo '674eccd80c81860bd371ec09d2b0f7593a9baba7a9df5e09fede54b675a01be5 lisaio-new-sony800k.bin' | sha256sum -c

clean:
	rm -rf *~ *.p *.lst *.bin
