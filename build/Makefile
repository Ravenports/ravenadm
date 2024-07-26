# BSD Makefile
#
# If WITH_DISPLAY is set in environment, build with ncurses
# Assumes src/portscan-operations.adb
#         src/pilot.adb
# have already been modified by sed for that feature
#
# Expected to be executed at ${WRKSRC}/some-obj-directory

all: ravenadm

unix_core.o: ../src/unix_core.c
	${CC} -c ${CFLAGS} ${.ALLSRC} -o ${.TARGET}

walkdir.o: ../rvn-format/src/unix/walkdir.c
	${CC} -c ${CFLAGS} ${.ALLSRC} -o ${.TARGET}

open.o: ../rvn-format/src/unix/open.c
	${CC} -c ${CFLAGS} ${.ALLSRC} -o ${.TARGET}

stat_opsc.o: ../rvn-format/src/unix/stat_opsc.c
	${CC} -c ${CFLAGS} ${.ALLSRC} -o ${.TARGET}

dprint2.o: ../rvn-format/src/unix/dprint2.c
	${CC} -c ${CFLAGS} ${.ALLSRC} -o ${.TARGET}

embedded_exec.o: ../src/embedded_exec.c
	${CC} -c ${CFLAGS} ${.ALLSRC} -o ${.TARGET}

ADAFLAGS= -gnatA -gnatyaAbBcdehfiklM100nOprsStx -fstack-check
COBJS= unix_core.o walkdir.o open.o stat_opsc.o dprint2.o embedded_exec.o
BASELIBS= -lucl_pic -lblake3_pic -lzstd_pic -lm
BASEINCS= -I../rvn-format/src -I../rvn-format/src/ucl -I../rvn-format/src/blake3 \
	-I../rvn-format/src/zstandard -I../rvn-format/src/unix -I../rvn-format/src/elf

ravenadm: ${COBJS}
.if empty(WITH_DISPLAY)
	gnatmake -I../src ${BASEINCS} ../ravenadm.adb -o ${.TARGET}\
		${CFLAGS} ${ADAFLAGS} -largs ${COBJS} ${BASELIBS}
.else
	gnatmake -I../src ${BASEINCS} ../ravenadm.adb -o ${.TARGET}\
		-aI${LOCALBASE}/include/adacurses\
		-aO${LOCALBASE}/lib/adacurses\
		${CFLAGS} ${ADAFLAGS}\
		-largs ${COBJS} ${BASELIBS} -lncurses -ltinfo -lAdaCurses
.endif
