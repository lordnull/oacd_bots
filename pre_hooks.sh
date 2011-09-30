case $1 in
	"compile")
		# hack for reltool
		if [ ! -d oacd_bots ]; then
			mkdir oacd_bots
			ln -sf ../ebin oacd_bots/ebin
			ln -sf ../src oacd_bots/src
			ln -sf ../include oacd_bots/include
			ln -sf ../priv oacd_bots/priv
			ln -sf ../deps oacd_bots/deps
		fi
esac
