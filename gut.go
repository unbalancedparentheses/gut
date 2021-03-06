package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"os/exec"
	"path"
	"path/filepath"
	"strings"

	"github.com/codegangsta/cli"
	"github.com/fatih/color"
	"github.com/google/go-github/github"
	"github.com/hoisie/mustache"
	git "github.com/libgit2/git2go"
	"gopkg.in/yaml.v2"
)

var (
	version        = "0.1"
	suffix         = "-gut-template"
	templateFile   = "gut.template"
	templateConfig configuration
	cwd            string
	name           string
	fullPath       string
)

var (
	boldCyan  *color.Color
	boldWhite *color.Color
)

type configuration struct {
	Variables map[string]string `yaml:"variables"`
	Options   map[string]string `yaml:"options"`
	Commands  []string          `yaml:"commands"`
}

func main() {
	boldCyan = color.New(color.FgCyan).Add(color.Bold)
	boldWhite = color.New(color.FgWhite).Add(color.Bold)

	cwd, _ = filepath.Abs(filepath.Dir(os.Args[0]))

	app := cli.NewApp()
	app.Name = "gut"
	app.Usage = "tool that retrieves templates and compile them to scaffold projects and create standalone files"
	app.Version = version
	app.Author = "Federico Carrone"
	app.Email = "federico.carrone@gmail.com"

	app.Commands = []cli.Command{
		{
			Name:    "search",
			Aliases: []string{"s"},
			Usage:   "search templates",
			Action: func(c *cli.Context) {
				filter := c.Args().First()
				search(filter)
			},
		},
		{
			Name:    "new",
			Aliases: []string{"n"},
			Usage:   "new",
			Action: func(c *cli.Context) {
				if len(c.Args()) < 2 {
					fmt.Print("Not enough arguments")
				}
				templateName := c.Args().First()
				name = c.Args()[1]
				new(templateName)
			},
		},
		{
			Name:    "describe",
			Aliases: []string{"d"},
			Usage:   "describe",
			Action: func(c *cli.Context) {
				templateName := c.Args().First()
				describe(templateName)
			},
		},
		{
			Name:    "version",
			Aliases: []string{"v"},
			Usage:   "version",
			Action: func(c *cli.Context) {
				fmt.Printf("gut version %s\n", version)
			},
		},
	}

	app.Run(os.Args)
}

func search(filter string) {
	search := filter + suffix
	client := github.NewClient(nil)
	opt := &github.SearchOptions{
		Sort: "stars",
	}

	results, _, error := client.Search.Repositories(search, opt)

	if error != nil {
		log.Fatal(error)
	}

	for _, repository := range results.Repositories {
		repoName := *repository.Name
		repoUsername := *(*repository.Owner).Login
		repoDescription := *repository.Description
		repoStars := *repository.StargazersCount

		print := strings.HasSuffix(repoName, suffix)

		if print {
			repoName = strings.TrimSuffix(repoName, suffix)

			boldCyan.Printf("%s/", repoUsername)
			boldWhite.Printf("%s", repoName)
			fmt.Printf(" (%d)\n", repoStars)
			fmt.Printf("    %s\n", repoDescription)
		}
	}
}

func new(templateName string) {
	repoName := templateName + suffix
	repoURL := "https://github.com/" + repoName + ".git"

	fullPath = path.Join(cwd, name)
	gitopts := &git.CloneOptions{}

	boldWhite.Printf("Retrieving %s via %s\n", templateName, repoURL)
	_, err := git.Clone(repoURL, fullPath, gitopts)

	if err != nil {
		log.Fatal(err)
	} else {
		println()
	}

	config(name, path.Join(fullPath, templateFile))
	askForVariables()
	clean()
	compile()
	commands()
	fmt.Printf("Your %s project was created successfully.\n", templateName)
}

func config(name string, configPath string) {
	configFile, errFile := ioutil.ReadFile(configPath)

	if errFile != nil {
		log.Fatal("Configuration file not present inside template\nError: ", errFile)
	}

	errYaml := yaml.Unmarshal(configFile, &templateConfig)
	if errYaml != nil {
		log.Fatal("Configuration file is not correctly formated\nError: ", errYaml)
	}

	templateConfig.Variables["name"] = name
}

func askForVariables() {
	boldWhite.Println("Assign values to variables [default value]")
	for key, defaultValue := range templateConfig.Variables {
		if key != "name" {
			fmt.Printf("%s [%s]: ", key, defaultValue)

			input := ""
			fmt.Scanln(&input)

			if input != "" {
				templateConfig.Variables[key] = input
			} else {
				templateConfig.Variables[key] = defaultValue
			}
		}
	}
	println()
}

func clean() {
	os.Remove(path.Join(fullPath, templateFile))
	os.RemoveAll(path.Join(fullPath, ".git/"))
	os.Remove(path.Join(fullPath, ".gitignore"))
}

func compile() {
	boldWhite.Printf("Generating fresh 'gut new' %s project:\n", templateConfig.Variables["name"])
	filepath.Walk(fullPath, compileDirs)
	filepath.Walk(fullPath, compileFiles)
	println()
}

func compileDirs(fullPath string, f os.FileInfo, err error) error {
	if f.IsDir() {
		newPath := mustache.Render(fullPath, templateConfig.Variables)
		os.Remove(fullPath)
		os.Mkdir(newPath, f.Mode())
		printCreating(newPath)
	}

	return nil
}

func printCreating(path string) {
	relativePath, err := filepath.Rel(cwd, path)

	if err != nil {
		log.Fatal(err)
	}

	color.New(color.FgGreen).Print("* creating")
	fmt.Printf(" %s\n", relativePath)
}

func compileFiles(fullPath string, f os.FileInfo, err error) error {
	if !f.IsDir() {
		newPath := mustache.Render(fullPath, templateConfig.Variables)
		rendered := mustache.RenderFile(fullPath, templateConfig.Variables)

		os.Remove(fullPath)
		ioutil.WriteFile(newPath, []byte(rendered), f.Mode())

		printCreating(newPath)
	}

	return nil
}

func commands() {
	if len(templateConfig.Commands) != 0 {
		boldWhite.Printf("Template wants to run the following commands with path=%s:\n", fullPath)

		for n, command := range templateConfig.Commands {
			cmdCompiled := mustache.Render(command, templateConfig.Variables)
			templateConfig.Commands[n] = cmdCompiled

			fmt.Printf("%d. ", n+1)
			fmt.Printf("%s", cmdCompiled)
			println()
		}

		println()
		boldWhite.Printf("Do you want to run this commands? [N/y] ")
		input := ""
		fmt.Scanln(&input)

		if input == "y" || input == "Y" {
			for _, cmdTemplate := range templateConfig.Commands {
				color.New(color.FgGreen).Print("* running ")
				fmt.Printf("%s", cmdTemplate)
				println()

				cmdArray := strings.Split(cmdTemplate, " ")
				cmdString := cmdArray[0]
				arguments := cmdArray[1:len(cmdArray)]

				command := exec.Command(cmdString, arguments...)
				command.Dir = fullPath

				out, err := command.Output()
				if err != nil {
					log.Fatal(err)
				} else if string(out) != "" {
					fmt.Printf("> %s", out)
				}
			}
		}
		println()
	}
}

func describe(templateName string) {
	repoName := templateName + suffix
	repoURL := "https://github.com/" + repoName + ".git"

	tempDir := os.TempDir()
	fullPath = path.Join(tempDir, "prueba")
	gitopts := &git.CloneOptions{}

	_, err := git.Clone(repoURL, fullPath, gitopts)
	if err != nil {
		os.RemoveAll(fullPath)
		log.Fatal(err)
	} else {
		println()
	}

	config(name, path.Join(fullPath, templateFile))

	boldWhite.Println("Variables in template")
	for key, defaultValue := range templateConfig.Variables {
		if key != "name" {
			fmt.Printf("%s [%s]\n", key, defaultValue)
		}
	}

	println()

	boldWhite.Println("Commands in template")
	for n, command := range templateConfig.Commands {
		fmt.Printf("%d. ", n+1)
		fmt.Printf("%s", command)
		println()
	}

	os.RemoveAll(fullPath)
}

// inmediate tasks:
// remove tmp path if issue while executing config in describe
// in describe print author, github description and url
// print help a la mix help.  execute command to check it.
// if in describe and new the repo does not exist you get 2016/03/13 17:57:58 authentication required but no callback set

// support templates that remove the cloned folder using the delete folder option
// work on temporary directory? if yes, then use os tempdir function. think if this is useful

// add subcommand to compile current working dir. useful for developing templates
// develop erlang cowboy template

// add option to fill from shell like ./rebar3 new plugin name=demo author_name="Fred H."

// store templates in /home/.config/gut/templates/. only fetch if template not found in that path
// update stored templates

// clone templates with depth one
// paginate to get all answers in search

// add curl command in readme like docker compose has. generate gox builds
// add homebrew formulas
// add documentation in the readme
