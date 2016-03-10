package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"os/exec"
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
)

var (
	boldCyan  *color.Color
	boldWhite *color.Color
)

type configuration struct {
	Variables map[string]string `yaml:"variables"`
	Options   map[string]string `yaml:"options"`
	Commands  [][]string        `yaml:"commands"`
}

func main() {
	boldCyan = color.New(color.FgCyan).Add(color.Bold)
	boldWhite = color.New(color.FgWhite).Add(color.Bold)

	cwd, _ = filepath.Abs(filepath.Dir(os.Args[0]))

	app := cli.NewApp()
	app.Name = "gut"
	app.Usage = "make an explosive entrance"

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
				providedPath := c.Args()[1]
				new(templateName, providedPath)
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

func new(templateName string, providedPath string) {
	repoName := templateName + suffix
	repoURL := "https://github.com/" + repoName + ".git"
	fullPath := cwd + "/" + providedPath + "/"
	gitopts := &git.CloneOptions{}

	boldWhite.Printf("Retrieving %s via %s\n", templateName, repoURL)
	_, err := git.Clone(repoURL, fullPath, gitopts)

	if err != nil {
		log.Fatal(err)
	} else {
		println()
	}

	name := providedPath //TODO get name after last slash
	config(name, fullPath+templateFile)
	askForVariables()
	clean(fullPath)
	compile(fullPath)
	commands(fullPath)
	fmt.Printf("Your %s project was created successfully.\n", templateName)
}

func config(name string, path string) {
	file, _ := ioutil.ReadFile(path)
	yaml.Unmarshal(file, &templateConfig) //TODO check for erros
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

func clean(fullPath string) {
	os.Remove(fullPath + templateFile)
	os.RemoveAll(fullPath + ".git/")
	os.Remove(fullPath + ".gitignore")
}

func compile(fullPath string) {
	//if delete folder option is set, change full path to path without name
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
	relativePath := strings.TrimPrefix(path, cwd+"/")

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

func commands(fullPath string) {
	boldWhite.Println("Commands:")

	for n, v := range templateConfig.Commands {
		command := v[0]
		arguments := v[1:len(v)]

		fmt.Printf("%d. ", n+1)
		printCommand(command, arguments)
		println()
	}

	println()
	boldWhite.Printf("Do you want to run this commands? [N/y] ")
	input := ""
	fmt.Scanln(&input)

	if input == "y" || input == "Y" {
		for _, v := range templateConfig.Commands {
			cmdString := v[0]
			arguments := v[1:len(v)]

			color.New(color.FgGreen).Print("* running ")
			printCommand(cmdString, arguments)
			println()

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

func printCommand(command string, arguments []string) {
	fmt.Printf("%s", command)
	for _, argument := range arguments {
		fmt.Printf(" %s", argument)
	}
}

// inmediate tasks:
// use paths functions instead of concatenaning strings
// store path in configuration instead of passing it each time
// add curl command in makefile like docker compose has
// paginate to get all answers
// work on temporary directory? if yes, then use os tempdir function. don't know if this is useful

// new features:
// name is the string after the last slash from the provided name. always work on current working dir. check if this is the default behaviour on other similar tools
// add describe template subcommand
// clone templates with depth one
// ask for specific tag or commit id of generator
// support templates that remove the cloned folder using the delete folder option
// add subcommand to compile current working dir. useful for developing templates
// store templates in /home/.gut/templates/. only fetch if template not found in that path
// update stored templates
// add option to fill variables from a file
