gutenberg: gut is good!
=========
[![Gitter](https://badges.gitter.im/Join Chat.svg)](https://gitter.im/unbalancedparentheses/gut?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Gutenberg is a massive template printing, aka scaffolding, tool for Erlang. Like rails generate or yeoman but for Erlang. Gutenberg was highly inspired by [lfetool](https://github.com/lfe/lfetool).

#roadmap

- Present - v0.8: Basic functionality
- 12 November - v0.9: Better logs
- 15 November - v1.0: Support many variables in generators. Better documentation for example on how to create your own generators. Odds and ends.

#tl;dr. install

###curl
```bash
curl -L -O https://github.com/unbalancedparentheses/gut/raw/master/bin/gut
chmod +x ./gut
sudo mv ./gut /usr/local/bin/
```

###wget
```bash
wget https://github.com/unbalancedparentheses/gut/raw/master/bin/gut
chmod +x ./gut
sudo mv ./gut /usr/local/bin/
```

#RTFM

```bash
> gut help
escriptize # Turn your erlang application into an escript
find       # Find available generators
help       # Prints help information
implode    # Removes gut and all its associated files
new        # Creates a new project or file
update     # Get the latest version of the gut executable and update.gens
version    # Prints gutenberg version
```

```bash
> gut find
Fetching list of generators from github...
NAME                  DESCRIPTION                               USER                  STARS
ranch                 ranch project generator                   unbalancedparentheses     3
library               library project generator                 jfacorro                  2
application           application project generator             jfacorro                  2
supervisor            supervisor file generator                 unbalancedparentheses     2
genserver             gen_server file generator                 unbalancedparentheses     2
supervised-gen-server supervised gen_server project generator   jfacorro                  2
cowboy                cowboy project generator                  unbalancedparentheses     1
cowboy-websocket      Cowboy Websocket handler                  igaray                    1
cowboy-rest-basic     Basic Cowboy REST handler                 igaray                    1
cowboy-rest-full      Full Cowboy REST handler                  igaray                    1
cowboy-http-basic     Basic Cowboy HTTP handler                 igaray                    1
genfsm                gen_fsm file generator                    jfacorro                  0
cowboy-lasse          cowboy sse handler generator              jfacorro                  0
genevent              gen_event file generator                  jfacorro                  0
cowboy-crud-json      Cowboy CRUD JSON handler                  igaray                    0
```

This downloads the ranch generator and instantiates it with the provided name (the name is stalin in this case):
```bash
> gut new ranch stalin
Cloning ranch hosted at https://github.com/unbalancedparentheses/ranch-gutenberg-generator
Please submit a github issue if you find any problem

The generator wants to run the following list of commands:
1. make
Are you sure you want to continue: [y/n] n

Message from generator:
Run `make app shell` to launch the application.

* creating .gitignore
* creating Makefile
* creating README.md
* creating erlang.mk
* creating rel/sys.config
* creating src/stalin.app.src
* creating src/stalin.erl
* creating src/stalin_protocol.erl

The job is done, boss.
> tree stalin/
stalin/
├── Makefile
├── README.md
├── erlang.mk
├── rel
│   └── sys.config
└── src
    ├── stalin.app.src
    ├── stalin.erl
    └── stalin_protocol.erl
```

##heapster
![Hipster Gutenberg](https://raw.githubusercontent.com/unbalancedparentheses/gut/master/gutenberg.jpg)
