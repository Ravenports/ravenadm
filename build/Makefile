# BSD Makefile
#
# If WITH_DISPLAY is set in environment, build with ncurses
# Assumes src/portscan-operations.adb
#         src/pilot.adb
# have already been modified by sed for that feature
#
# Expected to be executed at ${WRKSRC}/some-obj-directory

all: ravenadm ravenexec

unix_core.o: ../src/unix_core.c
	${CC} -c ${CFLAGS} ${.ALLSRC} -o ${.TARGET}

ravenexec: ../ravenexec/ravenexec.c
	${CC} ${CFLAGS} ${.ALLSRC} -o ${.TARGET}

ADAFLAGS= -gnatA -gnatyaAbBcdehfiklM99nOprsStx -fstack-check

ravenadm: unix_core.o
.if empty(WITH_DISPLAY)
	gnatmake -I../src ../ravenadm.adb -o ${.TARGET}\
		${CFLAGS} ${ADAFLAGS} -largs unix_core.o
.else
	gnatmake -I../src ../ravenadm.adb -o ${.TARGET}\
		-aI${LOCALBASE}/include/adacurses\
		-aO${LOCALBASE}/lib/adacurses\
		${CFLAGS} ${ADAFLAGS}\
		-largs unix_core.o -lncurses -ltinfo -lAdaCurses
.endif
