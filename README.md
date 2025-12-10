# Advent of Code Haskell template

## Requirements

The solution to day 10, part 2, requires Z3 to be installed:

```bash
sudo apt install z3
```

## Workflow

### When starting a new year

* Create a new repo from this template and clone it.
* Rename [the cabal file](./aocxxxx.cabal) to reflect the relevant year, and also rename the project inside this cabal file.
* Move the [VSCode settings](./settings.json) into a `.vscode` folder.

### When starting a new day

* Copy the [cabal snippet](./template/cabal) to the cabal file and rename the executable name and source folder to reflect the relevant day.
* Copy the [source folder template](./template/dayxx/) to the root of the repository, and rename it to reflect the relevant day.
