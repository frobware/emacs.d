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

defaults write -app Emacs NSUserKeyEquivalents -dict-add 'Emoji & Symbols' '\0'
defaults write -app Emacs NSUserKeyEquivalents -dict-add 'Quit Emacs' '\0'

defaults write -app XQuartz NSUserKeyEquivalents -dict-add 'Copy' '\0'
defaults write -app XQuartz NSUserKeyEquivalents -dict-add 'Close' '\0'
defaults write -app XQuartz NSUserKeyEquivalents -dict-add 'Minimize' '\0'
defaults write -app XQuartz NSUserKeyEquivalents -dict-add 'Terminal' '\0'
defaults write -app XQuartz NSUserKeyEquivalents -dict-add 'Preferences' '\0'
defaults write -app XQuartz NSUserKeyEquivalents -dict-add 'Hide X11' '\0'
defaults write -app XQuartz NSUserKeyEquivalents -dict-add 'Hide Others' '\0'
defaults write -app XQuartz NSUserKeyEquivalents -dict-add 'Quit X11' '\0'

# defaults write -app Terminal NSUserKeyEquivalents '{
# 	 "Copy"="^$C";
# 	 "Paste"="^$V";
# }'

defaults write -app Terminal NSUserKeyEquivalents -dict-add 'Cut' '^$X'
defaults write -app Terminal NSUserKeyEquivalents -dict-add 'Copy' '^$C'
defaults write -app Terminal NSUserKeyEquivalents -dict-add 'Paste' '^$V'

defaults write -app Terminal NSUserKeyEquivalents -dict-add 'Close Tab' '\0'
defaults write -app Terminal NSUserKeyEquivalents -dict-add 'Close Window' '\0'

defaults write -app Safari NSUserKeyEquivalents -dict-add 'Cut' '^X'
defaults write -app Safari NSUserKeyEquivalents -dict-add 'Copy' '^C'
defaults write -app Safari NSUserKeyEquivalents -dict-add 'Paste' '^V'
defaults write -app Safari NSUserKeyEquivalents -dict-add 'Close Tab' '^w'

#$ defaults delete Safari NSUserKeyEquivalents

defaults write -g NSUserKeyEquivalents -dict-add 'Cut' '^X'
defaults write -g NSUserKeyEquivalents -dict-add 'Copy' '^C'
defaults write -g NSUserKeyEquivalents -dict-add 'Paste' '^V'
defaults write -g NSUserKeyEquivalents -dict-add 'Close Tab' '^w'
