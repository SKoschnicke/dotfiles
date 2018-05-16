#!/bin/sh
tmux new-session -d -s startup -n first
# start the day with updates
tmux send-keys -t startup:first "yaourt -Syu" Enter
tmux attach -t startup:first
