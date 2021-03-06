all: \
	$(HOME)/.config/gnus/gnus.el \
	$(HOME)/.mbsyncrc \
	$(HOME)/.config/gnus/dovecotrc-work-mbsync \
	$(HOME)/.notmuch-config

$(HOME)/.config/gnus/gnus.el: gnus.el
	install -D -m 0600 $< $@

$(HOME)/.config/gnus/dovecotrc-work-mbsync: dovecotrc-work-mbsync
	install -D -m 0600 $< $@

$(HOME)/.mbsyncrc: mbsyncrc
	install -D -m 0600 $< $@

$(HOME)/.notmuch-config: notmuch-config
	install -D -m 0600 $< $@
