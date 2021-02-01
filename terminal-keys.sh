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
defaults write -app Terminal NSUserKeyEquivalents -dict-add 'Close Window' '\0'
defaults write -app Terminal NSUserKeyEquivalents -dict-add 'Close Tab' '\0'

function totally-unacceptable {
    # Catalina's notarization is a disaster for cli apps (aka the real world).
    sudo spctl --master-disable

    # and the show goes on.
    DevToolsSecurity -enable
}

totally-unacceptable
