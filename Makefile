
ACME=/home/marco/Scaricati/acme091/src/acme
EXO=/home/marco/Scaricati/exomizer209/src/exomizer
C1541=/usr/bin/c1541
KICK=/home/marco/KickAssembler/KickAss.jar

all:
	rm -f *.d64
	rm -f *.prg
	rm -f *.sym
	
	java -jar $(KICK) demo.asm -execute "x64" -aom
	$(EXO) sfx sys demo.prg -o demo-exo.prg -n
	$(C1541) -format diskname,id d64 demo.d64 -attach demo.d64 -write demo-exo.prg demo-rm

clean:
	rm -f *.prg
	rm -f *.d64
	rm -f *.sym
	
