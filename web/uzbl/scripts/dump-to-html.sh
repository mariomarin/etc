#!/bin/sh -e

# Sanitize filename
filename=$(echo $UZBL_TITLE | tr -C '[:alnum:]\n' '_')

# Ask user for better filename
filename=$(zenity --file-selection --confirm-overwrite --save --filename $filename.html)

echo 'js document.documentElement.outerHTML' | \
    socat - unix-connect:"$UZBL_SOCKET" > $filename
