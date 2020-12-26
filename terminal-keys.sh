#!/usr/bin/env sh

# Here is a rough cheatsheet for syntax.
#
# Key Modifiers
# ^ : Ctrl
# $ : Shift
# ~ : Option (Alt)
# @ : Command (Apple)
# # : Numeric Keypad

defaults write -g NSUserKeyEquivalents -dict-add 'Emoji & Symbols' '\0'

# defaults write -app Terminal NSUserKeyEquivalents '{
# 	 "Copy"="^$C";
# 	 "Paste"="^$V";
# }'

defaults write -app Terminal NSUserKeyEquivalents -dict-add 'Copy' '^$C'
defaults write -app Terminal NSUserKeyEquivalents -dict-add 'Cut' '^$X'
defaults write -app Terminal NSUserKeyEquivalents -dict-add 'Paste' '^$V'
