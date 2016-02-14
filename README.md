# lyrebot

Lyrebot is a tiny IRC bot for forwarding messages to offline users.

You can build it using [car](https://github.com/jonathanyc/car):

    $ car reqs              # List dependencies.
    $ opam install ...      # Install the ones you don't have.
    $ car opt               # Build, output to ./main.native.

`./main.native -help` displays a help message:

    $ ./main.native -help
	Usage: lyrebot [options]
	Lyrebot IRC Bot
	Copyright 2016 Jonathan Y. Chan <jyc@fastmail.fm>
	All rights reserved.
	  -help     Print a synopsis of options.
	  -host     The host to connect to.
	  -service  The service or port to connect to.
	  -nick     The nick to use. Also used for realname, user, etc.
	  -channel  The channel to connect to.

Then when you send a message like this (in the channel or as a private message):

    <jyc`> @jyc Long time no see! How are you doing?

... you get a confirmation as a private message:

    <lyrebot> I'll forward your message the next time jyc logs in.

... and jyc gets a private message the next time he's online:

    <lyrebot> *** You've got mail! ***
    <lyrebot> 10:14 2/3 (5d ago)  <jyc`> @jyc Long time no see! How are you doing?
