gutenberg: gut is good!
=========
[![Gitter](https://badges.gitter.im/Join Chat.svg)](https://gitter.im/unbalancedparentheses/gut?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Gutenberg is a massive template printing, aka scaffolding, tool for Erlang. Like rails generate or yeoman but for Erlang.

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

```bash
> gut new ranch name
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
* creating src/name.app.src
* creating src/name.erl
* creating src/name_protocol.erl

The job is done, boss.
```

##heapster
![Hipster Gutenberg](https://raw.githubusercontent.com/unbalancedparentheses/gut/master/gutenberg.jpg)
