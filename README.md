# WiSe 2023/24 GitHub Project Group 7
## Authors
- Máté Benke (mate.benke@tu-dortmund.de)
- Mia Macarena Bedarf (mia.bedarf@tu-dortmund.de)
- Guy Loïc Sabze (guy-loic.sabze@tu-dortmund.de)
- Johanna Hohmann (johanna.hohmann@tu-dortmund.de)
- Neo Hong (neo.hong@tu-dortmund.de)
- Helen Meyer zu Altenschildesche (helen.meyer-zu-altenschildesche@tu-dortmund.de)
## Files
### titanic.csv
The original dataset.
### preprocessing.R
Preprocesses the data (Task 1).
### titanic_cleaned.Rds
Contains the preprocessed dataset. Can be loaded with: dataset <- readRDS("titanic_cleaned.Rds")
### utils.R
Functions used for analysis (Task 2).
### helpers.R
Internal helper functions that are only called from utils.R (Task 2).
### documentation/*
Documentation for utils.R, helpers.R and preprocessing.R (analysis.R only calls functions) (Task 3).
### analysis.R
Main file that calls preprocessing.R, then calls functions from utils.R to analyse the data (Task 4).
