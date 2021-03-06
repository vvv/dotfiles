#!/bin/sh

BAT=/sys/class/power_supply/BAT1
cd $BAT 2>/dev/null || {
    echo "`basename $0`: cannot access $BAT/" 1>&2
    exit 1
}

STATUS=$(cat status)
[ "$STATUS" = 'Full' ] && exit 0

CURRENT=$(cat current_now)
CAPACITY=$(cat energy_full_design)
REMAINING=$(cat energy_now)

REPORT="$(($REMAINING * 100 / $CAPACITY))% ("

case "$STATUS" in
    'Discharging')
	MINS=$((60 * $REMAINING / $CURRENT))
	REPORT="${REPORT}~"
	if [ $MINS -lt 20 ]; then
	    COLOR='red'
	elif [ $MINS -lt 40 ]; then
	    COLOR='green'
	fi
	;;
    'Charging')
	MINS=$((60 * ($CAPACITY - $REMAINING) / $CURRENT))
	STATUS='+'
	;;
    *) #echo "|| XXX $STATUS "
	exit 0;;
esac

if [ $MINS -lt 60 ]; then
    REPORT="${REPORT}$MINS')"
else
    REPORT="${REPORT}$(printf "%d:%02d\n" $(($MINS / 60)) $(($MINS % 60))))"
fi

REPORT="$STATUS $REPORT"
[ -n "$COLOR" ] && REPORT="^fg($COLOR)$REPORT^fg()"

echo "|| $REPORT "
