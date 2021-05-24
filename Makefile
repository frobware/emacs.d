LINKS := \
	$(HOME)/.config/gnus/gnus.el \
	$(HOME)/.mbsyncrc \
	$(HOME)/.config/gnus/dovecotrc-work-mbsync \
	$(HOME)/.notmuch-config \
	$(HOME)/.msmtprc

.PHONY: install

install: | $(HOME)/.config/gnus
	mkdir -p $@
	rm -f $(LINKS)
	ln -s $(realpath gnus.el)  ~/.config/gnus
	ln -s $(realpath dovecotrc-work-mbsync)  ~/.config/gnus
	ln -s $(realpath .notmuch-config)  ~/.notmuch-config
	ln -s $(realpath .mbsyncrc)  ~/.mbsyncrc
	ln -s $(realpath .msmtprc)  ~/.msmtprc

$(HOME)/.config/gnus:
	mkdir -p $@
