#################################################################
# Title: Thermal Safety Margin Manuscript Data Analysis & Visualization
# Purpose: (1) Document temperature at CCSP
#         (2) Calculate LT50s for benthic mussel life stages
#         (3) Body temp for benthic mussel life stages
#         (4) TSM calculation
# Created by: L Pandori
# Created: 03/31/2020
# Last edited: 03/31/2020
################################################################
##### Data prep and package upload #####

# Clear workspace
rm(lists=ls())

# Load libraries
library(tidyr)        # reshape, vis and analyze data
library(RColorBrewer) # make color palettes

# Load data
