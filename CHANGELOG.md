# Change Log

## [0.9](https://github.com/unbalancedparentheses/gut/tree/0.9) (2014-11-19)

[Full Changelog](https://github.com/unbalancedparentheses/gut/compare/0.2...0.9)

**Implemented enhancements:**

- Delete README from generator and replace with standard content [\#59](https://github.com/unbalancedparentheses/gut/issues/59)

- Sort results from find command by its stars [\#55](https://github.com/unbalancedparentheses/gut/issues/55)

- Allow user to add local generators  [\#21](https://github.com/unbalancedparentheses/gut/issues/21)

- Add templates support [\#15](https://github.com/unbalancedparentheses/gut/issues/15)

**Fixed bugs:**

- Output from `new` command doesn't replace the `name` of target files  [\#63](https://github.com/unbalancedparentheses/gut/issues/63)

- Current gut binary is not working [\#43](https://github.com/unbalancedparentheses/gut/issues/43)

- Erlang 17.0 not working [\#100](https://github.com/unbalancedparentheses/gut/issues/100)

**Closed issues:**

- Change gut find to gut search [\#103](https://github.com/unbalancedparentheses/gut/issues/103)

- timeout error [\#101](https://github.com/unbalancedparentheses/gut/issues/101)

- Allow users to star generators using the gut command [\#93](https://github.com/unbalancedparentheses/gut/issues/93)

- gut compile code should not move files [\#84](https://github.com/unbalancedparentheses/gut/issues/84)

- Before executing comands from postinstall ask the user if he wants to do it [\#75](https://github.com/unbalancedparentheses/gut/issues/75)

- Make optional postinstall commands, message and other settings from the config [\#73](https://github.com/unbalancedparentheses/gut/issues/73)

- Add before cloning with the url of the generator or template so that the user can create an issue [\#72](https://github.com/unbalancedparentheses/gut/issues/72)

- pretty print gut find [\#68](https://github.com/unbalancedparentheses/gut/issues/68)

- gut new ranch dira/dirb doesn't work [\#62](https://github.com/unbalancedparentheses/gut/issues/62)

- New command deps to get the deps [\#60](https://github.com/unbalancedparentheses/gut/issues/60)

- Use erlang github [\#56](https://github.com/unbalancedparentheses/gut/issues/56)

- Post install hook for generators [\#54](https://github.com/unbalancedparentheses/gut/issues/54)

- Add sync to shell target in Makefile  [\#48](https://github.com/unbalancedparentheses/gut/issues/48)

- Configuration for each generator [\#37](https://github.com/unbalancedparentheses/gut/issues/37)

- Change find command by search command [\#99](https://github.com/unbalancedparentheses/gut/issues/99)

- Change user by owner in gut find [\#98](https://github.com/unbalancedparentheses/gut/issues/98)

- Delete LICENSE and .gitignore files if present. [\#92](https://github.com/unbalancedparentheses/gut/issues/92)

- Correct logs to make them useful [\#91](https://github.com/unbalancedparentheses/gut/issues/91)

- Generators to create [\#90](https://github.com/unbalancedparentheses/gut/issues/90)

- create module to do the cleanup only in one place [\#89](https://github.com/unbalancedparentheses/gut/issues/89)

- create module for dealing with paths [\#88](https://github.com/unbalancedparentheses/gut/issues/88)

- Abstract error 403 from github: too many requests [\#87](https://github.com/unbalancedparentheses/gut/issues/87)

- Make code from \#82 cleaner [\#86](https://github.com/unbalancedparentheses/gut/issues/86)

- Printed git url is wrong [\#85](https://github.com/unbalancedparentheses/gut/issues/85)

- Check that directory does not exist before copying [\#83](https://github.com/unbalancedparentheses/gut/issues/83)

- Don't show gut.yaml in creating log messages [\#81](https://github.com/unbalancedparentheses/gut/issues/81)

- Unify generator and template concepts and ui [\#80](https://github.com/unbalancedparentheses/gut/issues/80)

- Message stating that we are cloning a generator should use short name [\#79](https://github.com/unbalancedparentheses/gut/issues/79)

- Print message from generator in default color but add a green sentence before [\#78](https://github.com/unbalancedparentheses/gut/issues/78)

- Correctly manage timeouts in http [\#77](https://github.com/unbalancedparentheses/gut/issues/77)

- Control-c should stop script [\#71](https://github.com/unbalancedparentheses/gut/issues/71)

- Errors should go to stderr and should be printed in red [\#70](https://github.com/unbalancedparentheses/gut/issues/70)

- ASCII art FTW! [\#69](https://github.com/unbalancedparentheses/gut/issues/69)

- create gut implode [\#65](https://github.com/unbalancedparentheses/gut/issues/65)

- Unix tools guidelines [\#50](https://github.com/unbalancedparentheses/gut/issues/50)

**Merged pull requests:**

- First version that unifies generators and templates [\#82](https://github.com/unbalancedparentheses/gut/pull/82) ([unbalancedparentheses](https://github.com/unbalancedparentheses))

- Add a Gitter chat badge to README.md [\#49](https://github.com/unbalancedparentheses/gut/pull/49) ([gitter-badger](https://github.com/gitter-badger))

- \[Closes \#46\] Implemented clone and copy in two functions. [\#47](https://github.com/unbalancedparentheses/gut/pull/47) ([jfacorro](https://github.com/jfacorro))

## [0.2](https://github.com/unbalancedparentheses/gut/tree/0.2) (2014-09-21)

**Implemented enhancements:**

- Separate clone and copy in two function in gut\_generators [\#46](https://github.com/unbalancedparentheses/gut/issues/46)

- Makefile target to publish new version [\#45](https://github.com/unbalancedparentheses/gut/issues/45)

- Remove gut\_sup since there's currently no need for it [\#42](https://github.com/unbalancedparentheses/gut/issues/42)

- Change the replacement string for filenames from "name" to something more complicated [\#33](https://github.com/unbalancedparentheses/gut/issues/33)

- Help should be split into two views [\#30](https://github.com/unbalancedparentheses/gut/issues/30)

- Update generators [\#28](https://github.com/unbalancedparentheses/gut/issues/28)

- Generators should be downloaded on installation [\#20](https://github.com/unbalancedparentheses/gut/issues/20)

- Download generators and templates to ~/.gut/ [\#19](https://github.com/unbalancedparentheses/gut/issues/19)

- Create generators [\#16](https://github.com/unbalancedparentheses/gut/issues/16)

- Fork colors dep and include Makefile so that it works with erlang.mk [\#14](https://github.com/unbalancedparentheses/gut/issues/14)

**Fixed bugs:**

- Don't set a node name in the gut escript  [\#23](https://github.com/unbalancedparentheses/gut/issues/23)

- Remove colon from escriptize:: en Makefile [\#22](https://github.com/unbalancedparentheses/gut/issues/22)

- Fix update gens [\#58](https://github.com/unbalancedparentheses/gut/issues/58)

- Gut update should provide a way to chose destination of update if it cannot find executable [\#32](https://github.com/unbalancedparentheses/gut/issues/32)

- Gut update should ask for sudo password if it cannot write to file [\#31](https://github.com/unbalancedparentheses/gut/issues/31)

- Manage edge cases where the directory or template file already exist [\#29](https://github.com/unbalancedparentheses/gut/issues/29)

**Closed issues:**

- Make script generation work [\#17](https://github.com/unbalancedparentheses/gut/issues/17)

- Github search to search for generators [\#2](https://github.com/unbalancedparentheses/gut/issues/2)

- Add padding to gut find as gut help has [\#52](https://github.com/unbalancedparentheses/gut/issues/52)

- Make gut\_thor a separate project [\#51](https://github.com/unbalancedparentheses/gut/issues/51)

- Return understandable errors [\#36](https://github.com/unbalancedparentheses/gut/issues/36)

- Add useful logging [\#35](https://github.com/unbalancedparentheses/gut/issues/35)

- Bash gut installation [\#18](https://github.com/unbalancedparentheses/gut/issues/18)

- `find` command should print generator names without suffix [\#11](https://github.com/unbalancedparentheses/gut/issues/11)

- All commands should use abbreviated or full generators name [\#10](https://github.com/unbalancedparentheses/gut/issues/10)

- Move commands command information to help [\#7](https://github.com/unbalancedparentheses/gut/issues/7)

**Merged pull requests:**

- \[Closes \#11\] Show generators names without suffix. [\#27](https://github.com/unbalancedparentheses/gut/pull/27) ([jfacorro](https://github.com/jfacorro))

- \[Closes \#7\] Removed --commands option. Show commands on help. [\#26](https://github.com/unbalancedparentheses/gut/pull/26) ([jfacorro](https://github.com/jfacorro))

- \[Fixes \#23\] Removed -name arg from generated escript. [\#25](https://github.com/unbalancedparentheses/gut/pull/25) ([jfacorro](https://github.com/jfacorro))

- \[Fixes \#22\] Remove colon. [\#24](https://github.com/unbalancedparentheses/gut/pull/24) ([jfacorro](https://github.com/jfacorro))

- escriptize working [\#6](https://github.com/unbalancedparentheses/gut/pull/6) ([jfacorro](https://github.com/jfacorro))

- Parse options for the command line interface. [\#5](https://github.com/unbalancedparentheses/gut/pull/5) ([jfacorro](https://github.com/jfacorro))

- escriptize [\#4](https://github.com/unbalancedparentheses/gut/pull/4) ([unbalancedparentheses](https://github.com/unbalancedparentheses))

- \[Closes \#2\] Generators github search. [\#3](https://github.com/unbalancedparentheses/gut/pull/3) ([jfacorro](https://github.com/jfacorro))



\* *This Change Log was automatically generated by [github_changelog_generator](https://github.com/skywinder/Github-Changelog-Generator)*