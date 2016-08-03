#
# Makefile to compile amoeba
#

TUNE_SRC = tune.d util.d board.d eval.d game.d kpk.d move.d search.d weight.d
AMOEBA_SRC = amoeba.d util.d board.d eval.d kpk.d move.d search.d uci.d weight.d
TOURNEY_SRC = tourney.d board.d game.d move.d util.d 

ifeq ($(BUILD),)
	BUILD = fast
endif

ifeq ($(DC),)
	DC = gdc
endif

#gdc
ifeq ($(DC),gdc)
	ifeq ($(BUILD),popcount)
		DFLAGS = -W -Wno-uninitialized -Wall -Wpedantic -Wextra -O3 -flto -fwhole-program -frelease -fno-bounds-check -o $(EXE) -mtune=haswell -fversion=withPopCount -msse4.2 
	else ifeq ($(BUILD),fast)
		DFLAGS = -W -Wno-uninitialized -Wall -Wpedantic -Wextra  -O3 -flto -fwhole-program -frelease -fno-bounds-check -o $(EXE) -mtune=haswell
	else
		DFLAGS = -O0 -g -fdebug -o $(EXE)
	endif
	PGO_GEN = -fprofile-generate -lgcov
	PGO_USE = -fprofile-use
	PGO_MERGE = 
endif

#lcd2
ifeq ($(DC),ldc2)
	ifeq ($(BUILD),popcount)
		DFLAGS = -O3 -release -boundscheck=off -singleobj -w -dw -d-version=withPopCount -mattr=+sse4.2,popcnt
	else ifeq ($(BUILD),fast)
		DFLAGS = -O3 -release -boundscheck=off -singleobj -w -dw
	else
		DFLAGS = -O0 -g -gc
	endif

	PGO_GEN = -fprofile-instr-generate
	PGO_USE = -fprofile-instr-use=amoeba.profdata
	PGO_MERGE = ldc-profdata merge -output=amoeba.profdata amoeba-*.profraw

endif

#dmd
ifeq ($(DC),dmd)
	ifeq ($(BUILD),popcount)
		DFLAGS = -O -release -inline -boundscheck=off -w -version=withPopCount
	else ifeq ($(BUILD),fast)
		DFLAGS = -O -release -inline -boundscheck=off -w 
	else
		DFLAGS = -g -gc -debug
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
#tune
else ifeq ($(EXE),tune)
	SRC = $(TUNE_SRC)
#tourney
else ifeq ($(EXE),tourney)
	SRC = $(TOURNEY_SRC)
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

debug :
	$(MAKE) BUILD=debug

pgo: $(AMOEBA_SRC)
	$(MAKE) build-pgo EXE=amoeba	

tune: $(TUNE_SRC)
	$(MAKE) EXE=tune
	
amoeba: $(AMOEBA_SRC)
	$(MAKE) EXE=amoeba

tourney: $(TOURNEY_SRC)
	$(MAKE) EXE=tourney

clean:
	$(RM) *.o trace.* *.log *.gcda *.profraw


