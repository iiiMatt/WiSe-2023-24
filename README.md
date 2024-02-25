# WiSe 2023/24 GitHub Project Group 7

## Authors
- Máté Benke (mate.benke@tu-dortmund.de)
- Mia Macarena Bedarf (mia.bedarf@tu-dortmund.de)
- Guy Loïc Sabze (guy-loic.sabze@tu-dortmund.de)
- Johanna Hohmann (johanna.hohmann@tu-dortmund.de)
- Neo Hong (neo.hong@tu-dortmund.de)
- Helen Meyer zu Altenschildesche (helen.meyer-zu-altenschildesche@tu-dortmund.de)

## Files
### [titanic.csv](titanic.csv)
The original dataset.
### [preprocessing.R](preprocessing.R)
Preprocesses the data (Task 1).
### [titanic_cleaned.Rds](titanic_cleaned.Rds)
Contains the preprocessed dataset. Can be loaded with: dataset <- readRDS("titanic_cleaned.Rds")
### [utils.R](utils.R)
Functions used for analysis (Task 2).
### [helpers.R](helpers.R)
Internal helper functions that are only called from utils.R (Task 2).
### [documentation/*](documentation)
Documentation for utils.R, helpers.R and preprocessing.R (analysis.R only calls functions) (Task 3).
### [analysis.R](analysis.R)
Main file that calls preprocessing.R, then calls functions from utils.R to analyse the data (Task 4).
### [Rplots.pdf](Rplots.pdf)
The plots that analysis.R produces as a pdf export.

## How to run
It is enough to run [analysis.R](analysis.R) as this will call [preprocessing.R](preprocessing.R) and generate a new [titanic_cleaned.Rds](titanic_cleaned.Rds) which it reads the data from. The working directory has to be set to WiSe-2023-24 (the base folder of the repository) beforehand so it can find the function scripts and the csv file.

## Insights
We discussed our conclusions from the data in [this issue](../../issues/30).
