gutenberg: gut is good!
=========
[![Gitter](https://badges.gitter.im/Join Chat.svg)](https://gitter.im/unbalancedparentheses/gut?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Gutenberg is a massive template printing, aka scaffolding, tool for Erlang. Like rails generate or yeoman.
Gutenberg was highly inspired by [lfetool](https://github.com/lfe/lfetool).

#CAUTION

Erlang 17 or superior is needed. HOWEVER due to an SSL bug in 17.3, 17.3 is not supported. Please update to 17.3 rev2 from [erlang solutions](https://www.erlang-solutions.com/downloads/download-erlang-otp) or downgrade to a previous 17 version.

#roadmap
Version 1 will be released this week after I get some input and comments from the community. Please test it and let me know what you think! Obviously this tool is meant for erlang newcomers or people like me that like the tools available in the nodejs, clojure, ruby and python communities.

- Present - v0.9: Basic functionality
- 15 November - v1.0: Support many variables in generators. Better documentation for example on how to create your own generators. Odds and ends

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
The generators are searched and downloaded from github (like yeoman) so that anybody can create a generator and use it with gut
```bash
> gut help
escriptize # Turn your erlang application into an escript
help       # Prints help information
implode    # Removes gut and all its associated files
new        # Creates a new project or file
search     # Search available generators
update     # Get the latest version of the gut executable and update.gens
version    # Prints gutenberg version
```

```bash
> gut search
Fetching list of generators from github...
NAME                  DESCRIPTION                                                                     OWNER                 STARS
ranch                 ranch project generator                                                         unbalancedparentheses     3
library               library project generator                                                       jfacorro                  2
application           application project generator                                                   jfacorro                  2
supervisor            supervisor file generator                                                       unbalancedparentheses     2
genserver             gen_server file generator                                                       unbalancedparentheses     2
cowboy-bones          An Erlang Cowboy web app Gutenberg generator with i18n, templating, and more    gmr                       2
supervised-gen-server supervised gen_server project generator                                         jfacorro                  2
cowboy-websocket      Cowboy Websocket handler                                                        igaray                    1
cowboy                cowboy project generator                                                        unbalancedparentheses     1
cowboy-crud-json      Cowboy CRUD JSON handler                                                        igaray                    1
cowboy-rest-basic     Basic Cowboy REST handler                                                       igaray                    1
cowboy-http-basic     Basic Cowboy HTTP handler                                                       igaray                    1
cowboy-rest-full      Full Cowboy REST handler                                                        igaray                    1
cowboy-lasse          cowboy sse handler generator                                                    jfacorro                  0
genfsm                gen_fsm file generator                                                          jfacorro                  0
elli                  elli project generator - web server                                             jfacorro                  0
genevent              gen_event file generator                                                        jfacorro                  0
```

This downloads the ranch generator and instantiates it with the provided name (the name is eventi in this case)
```bash
> gut new ranch eventi
Cloning ranch hosted at https://github.com/unbalancedparentheses/ranch-gutenberg-generator
Please submit a github issue if you find any problem with this generator

Working dir /Users/unbalancedparentheses/projects/gut/eventi
* creating .gitignore
* creating Makefile
* creating README.md
* creating erlang.mk
* creating rel/sys.config
* creating src/eventi.app.src
* creating src/eventi.erl
* creating src/eventi_protocol.erl

The generator wants to run the following list of commands:
1. make
Are you sure you want to continue: [y/n] n

Message from generator:
Run `make app shell` to launch the application.

The job is done, boss.
> tree eventi
eventi
├── Makefile
├── README.md
├── erlang.mk
├── rel
│   └── sys.config
└── src
    ├── eventi.app.src
    ├── eventi.erl
    └── eventi_protocol.erl

2 directories, 7 files
```

##heapster
![Hipster Gutenberg](https://raw.githubusercontent.com/unbalancedparentheses/gut/master/gutenberg.jpg)
