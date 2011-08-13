##############################################################################
## Benchmarks
##############################################################################

## Config
#########

GHC = ghc

# running example: CSV file generation
csv:
	$(GHC) --make -O2 -fforce-recomp -main-is CSV CSV.hs
	./CSV --resamples 10000

core-csv:
	$(GHC)-core -- --make -O2 -fforce-recomp -main-is CSV CSV.hs
