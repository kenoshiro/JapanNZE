# Japan NZE scenario analysis

## Introduction

- This repository includes source code for data analysis and figure production for the net-zero emissions scenario analysis by AIM/Technology-Japan.

## How to use

- To run this script, scenario data file `scenario_data.csv` needs to be downloaded and copied to `./data/`. The instruction for the data file download can be found in the Data Availability statement in the paper.
- Execute `./prog/main.R` on the command line or main console on RStudio. The figures are generated in `./output/`.
- Following R packages are required: tidyverse, cowplot.
