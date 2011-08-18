##############################################################################
## Benchmarks
##############################################################################

## Config
#########

GHC = ghc

# DEBUG = -ddump-rule-firings -ddump-simpl-stats -dppr-debug

# regression testing against blaze-builder and old bytestring
test-all:
	$(GHC) --make -O2 $(DEBUG) -fforce-recomp -main-is TestAll TestAll.hs
	./TestAll -j2 -a1000

# regression testing against blaze-builder and old bytestring
regression:
	$(GHC) --make -O2 $(DEBUG) -fforce-recomp -main-is Regression Regression.hs
	./Regression --resamples 10000

core-regression:
	$(GHC)-core -- --make -O2 -fforce-recomp -main-is Regression Regression.hs

# test writeList fusion 
encodinglist:
	$(GHC) --make -O2 $(DEBUG) -fforce-recomp -main-is EncodingListFusion EncodingListFusion.hs
	./EncodingListFusion --resamples 10000

core-encodinglist:
	$(GHC)-core -- --make -O2 -fforce-recomp -main-is EncodingListFusion EncodingListFusion.hs

# reduced test for writeList fusion 
test_RewriteFoldr:
	$(GHC) --make -O2 $(DEBUG) -fforce-recomp -main-is Test_RewriteFoldr Test_RewriteFoldr.hs -ddump-rule-firings
	./Test_RewriteFoldr --resamples 10000

core-test_RewriteFoldr:
	$(GHC)-core -- --make -O2 -fforce-recomp -main-is Test_RewriteFoldr Test_RewriteFoldr.hs

# running example: CSV file generation
csv:
	$(GHC) --make -O2 $(DEBUG) -fforce-recomp -main-is CSV CSV.hs
	./CSV --resamples 10000

core-csv:
	$(GHC)-core -- --make -O2 -fforce-recomp -main-is CSV CSV.hs
