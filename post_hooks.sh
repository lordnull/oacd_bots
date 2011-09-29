#! /bin/bash

case $1 in
	"compile")
		rm -rf plugin
		mkdir plugin
		mkdir plugin/oacd_bots
		cp -R ebin include priv plugin/oacd_bots

		echo "***************************************************************"
		echo "* Compile success!                                            *"
		echo "*                                                             *"
		echo "* There is a new directory inside this one called 'plugin'.   *"
		echo "* This is able to function as an independant applicaiton, so  *"
		echo "* there is no requirement to do a plugin installation.        *"
		echo "*                                                             *"
		echo "* Plugin install:                                             *"
		echo "*     cp plugin/* /path/to/OpenACD/plugins/dir/               *"
		echo "*     1> cpx:reload_plugins()                                 *"
		echo "*                                                             *"
		echo "* Application Launch:                                         *"
		echo "*     ./devboot                                               *"
		echo "***************************************************************";;

	"get-deps")
		cd deps/OpenACD
		./rebar get-deps
		cd ../..
esac
