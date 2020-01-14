#
# Makefile to compile amoeba & some utilities
#

AMOEBA_SRC = amoeba.d util.d board.d eval.d kpk.d move.d search.d uci.d weight.d
TOURNEY_SRC = tourney.d board.d engine.d game.d move.d util.d
TUNE_SRC = tune.d util.d board.d eval.d game.d kpk.d move.d search.d weight.d
POSTMORTEM_SRC = postmortem.d game.d util.d board.d eval.d kpk.d move.d search.d uci.d weight.d engine.d

ifeq ($(DC),)
	DC = ldc2
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
	DFLAGS = -singleobj -w -dw -mcpu=$(CPU) -dip25
	ifeq ($(BUILD),fast)
		DFLAGS += -O -release -boundscheck=off -flto=full
	else ifeq ($(BUILD),debug)
		DFLAGS = -O -g -gc
	else ifeq ($(BUILD),perf)
		DFLAGS = -O1 -boundscheck=off
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
#postmortem
else ifeq ($(EXE),postmortem)
	SRC = $(POSTMORTEM_SRC)
endif

default :
	$(DC) $(DFLAGS) $(SRC)

pgo-gen:
	$(DC) $(DFLAGS) $(PGO_GEN) $(SRC)

pgo-use:
	$(PGO_MERGE)
	$(DC) $(DFLAGS) $(PGO_USE) $(SRC)
pgo :
	$(MAKE) clean
	$(MAKE) pgo-gen CPU=$(CPU) POPCOUNT=$(POPCOUNT)
	LLVM_PROFILE_FILE=amoeba-%p.profraw ./$(EXE) < bench.uci > /dev/null
	$(MAKE) pgo-use CPU=$(CPU) POPCOUNT=$(POPCOUNT)

debug : $(AMOEBA_SRC)
	$(MAKE) BUILD=debug EXE=amoeba CPU=$(CPU) POPCOUNT=$(POPCOUNT)

perf : $(AMOEBA_SRC)
	$(MAKE) BUILD=perf EXE=amoeba CPU=$(CPU) POPCOUNT=$(POPCOUNT)

amoeba: $(AMOEBA_SRC)
	$(MAKE) pgo EXE=amoeba CPU=$(CPU) POPCOUNT=$(POPCOUNT)

tourney: $(TOURNEY_SRC)
	$(MAKE) EXE=tourney CPU=$(CPU) POPCOUNT=$(POPCOUNT) GAME=true

tune: $(TUNE_SRC)
	$(MAKE) EXE=tune CPU=$(CPU) POPCOUNT=$(POPCOUNT) GAME=true

pgo-postmortem:
	$(MAKE) clean
	$(MAKE) pgo-gen EXE=postmortem CPU=$(CPU) POPCOUNT=$(POPCOUNT) GAME=true
	LLVM_PROFILE_FILE=amoeba-%p.profraw ./postmortem -c 4 -f lasker_vs_herzfeld_1895.pgn -d 10
	$(MAKE) pgo-use EXE=postmortem CPU=$(CPU) POPCOUNT=$(POPCOUNT) GAME=true

postmortem: $(POSTMORTEM_SRC)
	$(MAKE) EXE=postmortem CPU=$(CPU) POPCOUNT=$(POPCOUNT)  GAME=true


cloc:
	@echo "Amoeba's engine lines of code"
	countloc $(AMOEBA_SRC)
	@echo "Amoeba's tuner lines of code"
	countloc $(TUNE_SRC)
	@echo "Amoeba's tourney manager lines of code"
	countloc $(TOURNEY_SRC)
	@echo "Amoeba's game postmortem analyzer"
	countloc $(POSTMORTEM_SRC)

clean:
	$(RM) *.o trace.* *.log *.gcda *.profraw *.profdata

cleanall:
	$(MAKE) clean
	$(RM) amoeba-* amoeba postmortem tourney tune core.*



