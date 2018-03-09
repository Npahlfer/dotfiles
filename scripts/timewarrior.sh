#!/bin/sh

active=`timew get dom.active`
fallback='internal'
session=$1
exceptions=("mondayMeeting lunch helping off none")

if [ $active == 1 ]; then
    tag=`timew get dom.active.tag.1`
    duration=`timew get dom.active.duration`
    subDuration=${duration#*PT}
    status=' ? '
    minute=`date '+%M'`
    minute=${minute:1}
    minutes=(0 1 2 3)

    if [[ " ${minutes[@]} " =~ " ${minute} " ]]; then
        idle=`ioreg -c IOHIDSystem | awk '/HIDIdleTime/ {print $NF/1000000000; exit}'`
        idleInt=${idle%.*}

        if [[ "$idleInt" -ge "1200" ]]; then
            if [[ $tag == "idle ${tag}" ]]; then
                echo "${tag}: ${subDuration}";
                return 0
            fi

            timew start "idle ${session}" :quiet
            echo "${tag}: ${subDuration}";
            return 0
        fi
    fi

    if [[ ! " ${exceptions[@]} " =~ " ${tag} " ]]; then
        if [ $session == '' ]; then
            $session = $fallback
        fi

        if echo $session | grep -Fq $tag; then
            status=''
        else
            timew start $session :quiet
        fi
    else
        status=''
    fi

    echo "${check}${status}${tag}: ${subDuration}";
else
    # currentH=`date '+%H'`

    # if [[ ! " ${exceptions[@]} " =~ " ${tag} " ]]; then
    #     if [[ "$currentH" -ge "9" ]] && [[ "$currentH" -le "19" ]]; then
    #         timew start $fallback :quiet

    #         echo "${fallback}: ${subDuration}";
    #     else
    #         echo "Track your time!";
    #     fi
    # else
        echo "Track your time!";
    # fi
fi
