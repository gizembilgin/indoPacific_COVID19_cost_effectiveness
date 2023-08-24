## Folder contents
This folder contains the deterministic COVID-19 transmission model underlying this paper. 
A summary of this model is provided in the Supplementary Material associated with this paper.

## Model schematic
![transmission_model_schematic](https://github.com/gizembilgin/GitHub_vaxAllocation/assets/37473520/a2e66348-9d5b-4b3e-8d8b-ce2b07a90173)

## Scripts included in this folder
| Folder or script | Purpose | 
| ----------- | ----------- |
01_inputs/ | all underlying data files
02_fit/ | scripts for fitting the transmission model to our study settings
03_mech_shop/ | scripts which create a subset of parameters used by the model
04_functions/ | all functions used in the model
99_workshop/ | 'workshop' files are generally temporary files for troubleshooting
(0)_fitting.R | the initial conditions for maintaining consistency across fitting scripts (*02_fit/*)
(#)* | the sub-scripts of the COVID-19 transmission model
(antiviral)* | scripts which generates the results for the stochastic outcomes projection model
(sensitivity)* | sensitivity analysis
(silho)*  | reconstructe the vaccination history of our study settings
CommandDeck.R | runs all sub-scripts of the COVID-19 transmission model to complete one standard 'run' of the model
