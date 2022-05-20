# Green hydrogen upscaling

This repository contains the full model code to run the technology diffusion simulation and reproduce all figures of the article:

Odenweller, A., Ueckerdt, F., Nemet, G. F., Jensterle, M., and Luderer, G.: *Probabilistic feasibility space of scaling up green hydrogen supply*

## Contents
* `main.Rmd` - The main R Markdown file
* `01_input_data` - Input data files
* `02_output_plots` - Output folder for simulation data and plots
* `03_functions` - Helper functions
* `04_plotting` - Plotting functions

## Instructions
There are two options to use this code, which are controlled by a switch in `main.Rmd`
* Reproduction mode (default)
  * Download the [pre-run simulation output from Zenodo](https://doi.org/10.5281/zenodo.6567669) and put all files into `02_output_plots`
  * Run `main.Rmd`
  * This imports the simulation output, reproduces all article figures, and saves them into `02_output_plots` 
* Simulation mode
  * Set `model.mode = "simulation"` in `main.Rmd`
  * Adjust the sample size `n` of the simulation as desired
  * Run `main.Rmd`
  * This starts the simulation, which will take some time, depending on the sample size
  * Figures are saved into `02_output_plots` and might deviate slightly from article figures due to randomness