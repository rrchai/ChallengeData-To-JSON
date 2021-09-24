# ChallengeData-To-JSON

`converter.R` is a tool to convert the data from the Challenges Landscape spreadsheet to json format.

## Setup

1. Install R dependencies:
   R -e "renv::consent(provided=TRUE)"
   R -e "renv::restore()"
2. modify the config file
   cp example_config.R config.R

## Usage

To retrieve google sheet, open the `RStudio` and run the [converter.R script](./converter.R). Set `overwirte` to `TRUE` to save all json files in [seedData](./seedData) folder.
