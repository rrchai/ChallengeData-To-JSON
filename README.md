# ChallengeData-To-JSON

`converter.R` is a tool to convert the data from the Challenges Landscape spreadsheet to json format for the [ROCC App], syncing with the latest [ROCC Schemas] version: `0.5.0`

## Setup

1.  Use the `rocc-service` conda environment from [ROCC Service]

        conda activate rocc-service

2.  modify the config file

        cp example_config.R config.R

## Usage

Open the `RStudio` and run the [converter.R](./converter.R).

- set `overwirte = TRUE` to save all json files in [seedData](./seedData) folder.

<!-- Links -->

[rocc app]: https://github.com/Sage-Bionetworks/rocc-app
[rocc schemas]: https://github.com/Sage-Bionetworks/rocc-schemas
[rocc service]: https://github.com/Sage-Bionetworks/rocc-service#running-with-python
