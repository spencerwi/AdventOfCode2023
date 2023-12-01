# My 2023 Advent of Code solutions, in F#

## Adding new solutions

This repo comes with its own nice project template for my solution project structure (using FsUnit+NUnit for tests)

Just run:

```shell
$ cd ./project_template
$ dotnet new install ./
```

...then you can use `./newday.sh DAY_NUMBER` to scaffold out a new day's problem space!

## Running solutions

In every day, you can cd into the directory and run `dotnet run <INPUT_TXT_FILE_NAME` if you've saved your sample input to a text file, or something like this:

```
dotnet run <<EOF
yoursample
inputgoes
here
EOF
```

if you've just got it on hand to copy-paste.
