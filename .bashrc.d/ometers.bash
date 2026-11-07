#### https://m.xkcd.com/2060/

#### Aranet4 air quality monitoring settings

function co2  {
    "$MY_VIRTUALENV_DIR/claranet4/bin/claranet4" read -r "$MY_ARANET4" 2> /dev/null | jq ".co2"
}

function co2-verbose {
    echo "CO2 was $(co2) ppm at $(date '+%I:%M %P') on $(d)"
}

#### PurpleAir air quality monitor settings

function air {
    local RESPONSE_FILE
    RESPONSE_FILE=$(mktemp "$HOME/tmp/air.XXXX")

    if curl -L 'https://www.purpleair.com/json?show=119745&key=NPLQPTWKC54229MU' \
             > "$RESPONSE_FILE" \
             2> /dev/null
    then
        echo "Problem contacting the PurpleAir API server; maybe rate-limiting?"
        echo "Try waiting ten minutes and then re-running."
    else
        local PM25 STATS TSLM NUMBER_OF_MINUTES INFLECTED

        PM25="$(jq --raw-output '.results[0]."PM2_5Value"' < "$RESPONSE_FILE")"
        STATS="$(jq --raw-output '.results[0].Stats' < "$RESPONSE_FILE")"
        TSLM="$(echo "$STATS" | jq --raw-output '.timeSinceModified')"

        # Convert milliseconds to minutes
        NUMBER_OF_MINUTES=$(( TSLM / 60 ))
        
        if [[ "$NUMBER_OF_MINUTES" -eq 1 ]]; then
            INFLECTED="minute"
        else
            INFLECTED="minutes"
        fi

        echo "PM 2.5 is ${PM25}ppm (as of $NUMBER_OF_MINUTES $INFLECTED ago)"
    fi
}
