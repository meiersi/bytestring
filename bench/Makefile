##############################################################################
## Benchmarks
##############################################################################

## Config
#########

GHC = ghc


# benchmarking all functions
bench-all:
	$(GHC) --make -O2 -fforce-recomp -main-is BenchAll BenchAll.hs
	./BenchAll

bench-all-core:
	$(GHC)-core -- --make -O2 -fforce-recomp -main-is BenchAll BenchAll.hs


# running example: CSV file generation
csv:
	$(GHC) --make -O2 -fforce-recomp -main-is CSV CSV.hs
	./CSV

core-csv:
	$(GHC)-core -- --make -O2 -fforce-recomp -main-is CSV CSV.hs

# benchmarking regressions test
regression:
	$(GHC) --make -O2 -fforce-recomp -main-is Regression Regression.hs
	./Regression

regression-core:
	$(GHC)-core -- --make -O2 -fforce-recomp -main-is Regression Regression.hs



