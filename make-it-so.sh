#!/usr/bin/env sh

totally_unacceptable() {
    # Catalina's notarization is a disaster for cli apps (aka the real world).
    sudo spctl --master-disable

    # and the show goes on.
    DevToolsSecurity -enable
}
