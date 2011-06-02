##############################################################################
## Benchmarks
##############################################################################

## Config
#########

GHC6 = ghc-6.12.3
GHC7 = ghc-7.0.1

GHC = ghc

# DEBUG = -ddump-rule-firings -ddump-simpl-stats -dppr-debug

# regression testing against blaze-builder and old bytestring
regression:
	$(GHC) --make -O2 $(DEBUG) -fforce-recomp -main-is Regression Regression.hs
	./Regression --resamples 10000

core-regression:
	$(GHC)-core -- --make -O2 -fforce-recomp -main-is Regression Regression.hs
