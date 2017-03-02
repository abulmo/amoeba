#
# Makefile to compile amoeba
#

AMOEBA_SRC = amoeba.d util.d board.d eval.d kpk.d move.d search.d uci.d weight.d
EVOLVE_SRC = evolve.d util.d board.d eval.d game.d kpk.d move.d search.d weight.d
TOURNEY_SRC = tourney.d board.d game.d move.d util.d 
TUNE_SRC = tune.d util.d board.d eval.d game.d kpk.d move.d search.d weight.d

ifeq ($(BUILD),)
	BUILD = fast
endif

ifeq ($(DC),)
	DC = gdc
endif

#gdc
ifeq ($(DC),gdc)
	ifeq ($(BUILD),fast)
		DFLAGS = -W -Wno-uninitialized -Wall -Wpedantic -Wextra -O3 -frelease -fno-bounds-check -o $(EXE) -mtune=haswell -fversion=withPopCount -msse4.2 
	else ifeq ($(BUILD),nopopcount)
		DFLAGS = -W -Wno-uninitialized -Wall -Wpedantic -Wextra -O3 -frelease -fno-bounds-check -o $(EXE) -mtune=haswell
	else ifeq ($(BUILD),debug) 
		DFLAGS = -O0 -g -fdebug -o $(EXE)
	else ifeq ($(BUILD),unittest)
		DFLAGS = -W -Wno-uninitialized -Wall -Wpedantic -Wextra -O3 -funittest -fno-bounds-check -o $(EXE) -mtune=haswell -fversion=withPopCount -msse4.2 
	endif
	PGO_GEN = -fprofile-generate -lgcov
	PGO_USE = -fprofile-use
	PGO_MERGE = 
endif

#lcd2
ifeq ($(DC),ldc2)
	ifeq ($(BUILD),fast)
		DFLAGS = -O3 -release -boundscheck=off -singleobj -w -dw -d-version=withPopCount -mcpu=sandybridge -mattr=+sse4.2,popcnt,avx
	else ifeq ($(BUILD),nopopcount)
		DFLAGS = -O3 -release -boundscheck=off -singleobj -w -dw
	else ifeq ($(BUILD),debug)
		DFLAGS = -O0 -g -gc
	else ifeq ($(BUILD),unittest)
		DFLAGS = -O3 -unittest -singleobj -w -dw -d-version=withPopCount -mcpu=sandybridge -mattr=+sse4.2,popcnt,avx
	endif
	PGO_GEN = -fprofile-instr-generate
	PGO_USE = -fprofile-instr-use=amoeba.profdata
	PGO_MERGE = ldc-profdata merge -output=amoeba.profdata amoeba-*.profraw

endif

#dmd
ifeq ($(DC),dmd)
	ifeq ($(BUILD),fast)
		DFLAGS = -O -release -inline -boundscheck=off -w -version=withPopCount
	else ifeq ($(BUILD),nopopcount)
		DFLAGS = -O -release -inline -boundscheck=off -w 
	else ifeq ($(BUILD),debug)
		DFLAGS = -g -gc -debug
	else ifeq ($(BUILD),unittest)
		DFLAGS = -unittest -boundscheck=off -w -version=withPopCount -cov
	endif
	PGO_GEN = 
	PGO_USE = 
	PGO_MERGE = 
endif

# executable target
ifeq ($(EXE),)
	EXE = amoeba
endif

#amoeba
ifeq ($(EXE),amoeba)
	SRC = $(AMOEBA_SRC)
#evolve
else ifeq ($(EXE),evolve)
	SRC = $(EVOLVE_SRC)
#tourney
else ifeq ($(EXE),tourney)
	SRC = $(TOURNEY_SRC)
#tune
else ifeq ($(EXE),tune)
	SRC = $(TUNE_SRC)
endif


build :
	$(DC) $(DFLAGS) $(SRC)

build-pgo :
	$(MAKE) clean
	$(DC) $(DFLAGS) $(PGO_GEN) $(SRC)
	LLVM_PROFILE_FILE=amoeba-%p.profraw ./$(EXE) perft --depth=6 | grep perft; 
	LLVM_PROFILE_FILE=amoeba-%p.profraw ./$(EXE) bench --depth=12 --file bk.epd
	$(PGO_MERGE)
	$(DC) $(DFLAGS) $(PGO_USE) $(SRC)

debug : $(AMOEBA_SRC)
	$(MAKE) BUILD=debug EXE=amoeba	

pgo: $(AMOEBA_SRC)
	$(MAKE) build-pgo EXE=amoeba	

unittest:$(AMOEBA_SRC)
	$(MAKE) BUILD=unittest EXE=amoeba	

amoeba: $(AMOEBA_SRC)
	$(MAKE) EXE=amoeba

evolve: $(EVOLVE_SRC)
	$(MAKE) EXE=evolve

tourney: $(TOURNEY_SRC)
	$(MAKE) EXE=tourney

tune: $(TUNE_SRC)
	$(MAKE) EXE=tune	

clean:
	$(RM) *.o trace.* *.log *.gcda *.profraw *.profdata

cleanall:
	$(MAKE) clean
	$(RM) amoeba-* amoeba tourney tune core.*
	


