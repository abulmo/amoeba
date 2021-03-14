#
# Makefile to compile amoeba & some utilities
#

#sources
SRC = amoeba.d board.d eval.d kpk.d move.d search.d tt.d uci.d util.d weight.d

#default setting
ifeq ($(DC),)
	DC = ldc2
endif

ifeq ($(POPCOUNT),)
	POPCOUNT=true
endif

ifeq ($(CPU),)
	CPU=native
endif

ifeq ($(BUILD),)
	BUILD=fast
endif

#gdc
ifeq ($(DC),gdc)
	DFLAGS = -W -Wno-uninitialized -Wall -Wpedantic -Wextra -mtune=$(CPU) -o amoeba
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
	DFLAGS = -singleobj -w -dw -mcpu=$(CPU) -dip25 -preview=intpromote $(OS)
	ifeq ($(BUILD),fast)
		DFLAGS += -O -release -boundscheck=off -flto=full --frame-pointer=none
	else ifeq ($(BUILD),debug)
		DFLAGS = -O0 -g -gc --checkaction=C
	else ifeq ($(BUILD),perf)
		DFLAGS = -O1 -boundscheck=off
	else ifeq ($(BUILD),cov)
		DFLAGS = -O0 -release -boundscheck=off -flto=full -cov
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

amoeba: $(SRC)
	$(DC) $(DFLAGS) $(SRC)

pgo : $(SRC)
	$(MAKE) clean
	$(DC) $(DFLAGS) $(PGO_GEN) $(SRC) 
	LLVM_PROFILE_FILE=amoeba-%p.profraw ./amoeba perft -d 6
	LLVM_PROFILE_FILE=amoeba-%p.profraw ./amoeba bench -c 4 -d 12
	$(PGO_MERGE)
	$(DC) $(DFLAGS) $(PGO_USE) $(SRC) 

debug : $(SRC)
	$(MAKE) BUILD=debug

perf : $(SRC)
	$(MAKE) BUILD=perf

cloc:
	@echo "Amoeba's engine lines of code"
	countloc $(SRC)

clean:
	$(RM) *.o trace.* *.log *.gcda *.profraw *.profdata

cleanall:
	$(MAKE) clean
	$(RM) amoeba



