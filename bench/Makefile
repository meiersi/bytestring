##############################################################################
## Benchmarks
##############################################################################

## Config
#########

GHC = ghc


# benchmarking all functions
bench-all:
	$(GHC) --make -O2 -fforce-recomp -main-is BenchAll BenchAll.hs
	./BenchAll --resamples 10000

bench-all-core:
	$(GHC)-core -- --make -O2 -fforce-recomp -main-is BenchAll BenchAll.hs


# running example: CSV file generation
csv:
	$(GHC) --make -O2 -fforce-recomp -main-is CSV CSV.hs
	./CSV --resamples 10000

core-csv:
	$(GHC)-core -- --make -O2 -fforce-recomp -main-is CSV CSV.hs

# benchmarking regressions test
regression:
	$(GHC) --make -O2 -fforce-recomp -main-is BuilderRegression BuilderRegression.hs
	./BuilderRegression --resamples 10000

regression-core:
	$(GHC)-core -- --make -O2 -fforce-recomp -main-is BuilderRegression BuilderRegression.hs



