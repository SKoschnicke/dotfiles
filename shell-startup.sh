#!/bin/sh
tmux new-session -d -s startup -n first
# start the day with updates
tmux send-keys -t startup:first "trizen -Syu" Enter
tmux attach -t startup:first
