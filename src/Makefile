#
# Makefile to compile amoeba
#

AMOEBA_SRC = amoeba.d util.d board.d eval.d kpk.d move.d search.d uci.d weight.d
TOURNEY_SRC = tourney.d board.d game.d move.d util.d 
TUNE_SRC = tune.d util.d board.d eval.d game.d kpk.d move.d search.d weight.d

ifeq ($(DC),)
	DC = gdc
endif

ifeq ($(POPCOUNT),)
	POPCOUNT=true
endif

ifeq ($(CPU),)
	CPU=sandybridge
endif

ifeq ($(BUILD),)
	BUILD=fast
endif

#gdc
ifeq ($(DC),gdc)
	DFLAGS = -W -Wno-uninitialized -Wall -Wpedantic -Wextra -mtune=$(CPU) -o $(EXE)
	ifeq ($(BUILD),fast)
		DFLAGS +=  -O3 -frelease -fno-bounds-check  
	else ifeq ($(BUILD),debug) 
		DFLAGS += -O0 -g -fdebug 
	endif
	ifeq ($(POPCOUNT),true)
		DFLAGS += -fversion=withPopCount -msse4.2 
	endif
	ifeq ($(GAME),true)
		DFLAGS += -fversion=withGameSupport
	endif
	PGO_GEN = -fprofile-generate -lgcov
	PGO_USE = -fprofile-use
	PGO_MERGE = 
endif

#lcd2
ifeq ($(DC),ldc2)
	DFLAGS = -singleobj -w -dw -mcpu=$(CPU)
	ifeq ($(BUILD),fast)
		DFLAGS += -O -release -boundscheck=off -flto=full
	else ifeq ($(BUILD),debug)
		DFLAGS = -O0 -g -gc
	else ifeq ($(BUILD),cov)
		DFLAGS = -O -release -boundscheck=off -flto=full -cov
	endif
	ifeq ($(POPCOUNT),true)
		DFLAGS += -d-version=withPopCount -mattr=+sse4.2,+popcnt
	endif
	ifeq ($(GAME),true)
		DFLAGS += -d-version=withGameSupport
	endif
	PGO_GEN = -fprofile-instr-generate
	PGO_USE = -fprofile-instr-use=amoeba.profdata
	PGO_MERGE = ldc-profdata merge -output=amoeba.profdata amoeba-*.profraw
endif

#dmd
ifeq ($(DC),dmd)
	DFLAGS=-w
	ifeq ($(BUILD),fast)
		DFLAGS = -O -release -inline -boundscheck=off
	else ifeq ($(BUILD),debug)
		DFLAGS = -g -gc -debug
	else ifeq ($(BUILD),cov)
		DFLAGS = -O -release -inline -boundscheck=off -cov
	endif
	ifeq ($(POPCOUNT),true)
		DFLAGS += -version=withPopCount
	endif
	ifeq ($(GAME),true)
		DFLAGS += -version=withGameSupport
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
	LLVM_PROFILE_FILE=amoeba-%p.profraw ./$(EXE) < bench.uci > /dev/null
	$(PGO_MERGE)
	$(DC) $(DFLAGS) $(PGO_USE) $(SRC)

debug : $(AMOEBA_SRC)
	$(MAKE) BUILD=debug EXE=amoeba CPU=$(CPU) POPCOUNT=$(POPCOUNT)

amoeba: $(AMOEBA_SRC)
	$(MAKE) build-pgo EXE=amoeba CPU=$(CPU) POPCOUNT=$(POPCOUNT)

tourney: $(TOURNEY_SRC)
	$(MAKE) EXE=tourney CPU=$(CPU) POPCOUNT=$(POPCOUNT) GAME=true

tune: $(TUNE_SRC)
	$(MAKE) EXE=tune CPU=$(CPU) POPCOUNT=$(POPCOUNT)  GAME=true

clean:
	$(RM) *.o trace.* *.log *.gcda *.profraw *.profdata

cleanall:
	$(MAKE) clean
	$(RM) amoeba-* amoeba tourney tune core.*
	


