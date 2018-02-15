APP="./app"
TDIR="t"
TESTS="lannot q"

if [ "$1" = "update" ]; then
	until
		sh $0
	do
		T=$?
		echo "Update the test results? [no]"
		read YN
		case "$YN" in
			("y"|"Y"|"yes") sh $0 `printf "t%02d" $T` ;;
			(*) exit 1 ;;
		esac
	done
	exit
fi

if [ -f "$TDIR/$1.while" ]; then
	echo "generating test outputs for $TDIR/$1.while"
	for TEST in $TESTS; do
		echo "-- $TEST"
		$APP -t$TEST < $TDIR/$1.while 2>&1 \
			| tee $TDIR/$TEST/$1.while
		echo ""
	done
	exit 0
fi

for TFILE in $TDIR/t*.while; do
	printf "testing $TFILE "
	TNUM=${TFILE#$TDIR/t}; TNUM=${TNUM%.while}
	for TEST in $TESTS; do
		$APP -t$TEST < $TFILE 2>&1 \
			| diff - $TDIR/$TEST/`basename $TFILE` > /tmp/testdiff
		[ $? -ne 0 ] && {
			echo " [$TEST failure]";
			echo "*** diff"; cat /tmp/testdiff; echo;
			exit $TNUM;
		}
		printf "[$TEST ok] "
	done
	echo ""
done
