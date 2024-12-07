profile:
	stack build --profile
	stack exec --profile -- blackjack-exe +RTS -sstderr -p -hT
#	stack exec --profile -- blackjack-exe +RTS -p -hy
#	stack exec --profile -- blackjack-exe +RTS -p -hr
#	stack exec --profile -- blackjack-exe +RTS -p -hr -hcenumSearch.next\'

time:
	stack build
	time stack exec -- blackjack-exe 2> timing.txt
	echo >> timing.txt
	time stack exec -- blackjack-exe 2>> timing.txt
	echo >> timing.txt
	time stack exec -- blackjack-exe 2>> timing.txt

time1:
	stack build
	time stack exec -- blackjack-exe 2> timing.txt

displayflame: displayheap
	cat blackjack-exe.prof | ghc-prof-flamegraph > blackjack-exe.prof.svg
	cat blackjack-exe.prof | ghc-prof-flamegraph --alloc > blackjack-exe.alloc.prof.svg

displayheap:
	hp2ps -c -e8in blackjack-exe.hp

move: cleanprof
	mkdir prof
	mv blackjack-exe.* prof

cleanprof:
	rm -rf prof

profileall: profile displayflame move

show:
	"mnt/c/Program Files/Mozilla Firefox/firefox.exe" blackjack-exe.prof.svg
	"mnt/c/Program Files/Mozilla Firefox/firefox.exe" blackjack-exe.alloc.prof.svg