#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Create TODO
# @raycast.mode compact

# Optional parameters:
# @raycast.icon 🤖
# @raycast.argument1 { "type": "text", "placeholder": "Task description" }
# @raycast.packageName SKoschnicke

# Documentation:
# @raycast.description Create a TODO item in emacs org-mode
# @raycast.author Sven Koschnicke
# @raycast.authorURL https://sven.guru

emacsclient -e "(my/add-to-refile  \"* TODO $1\nSCHEDULED: <$(date "+%Y-%m-%d %a")>\n[$(date "+%Y-%m-%d %a %H:%M")]\")"
echo "Created TODO"
