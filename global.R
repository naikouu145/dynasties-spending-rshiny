# Load required libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(plotly)
library(corrplot)
library(stringr)

# Load data (update paths as needed)
votes <- read.csv("data/pres.csv")
vp_votes <- read.csv("data/vp.csv")
sen_votes <- read.csv("data/sen.csv")
sen_votes_2025 <- read.csv("data/senate25-final_updated.csv")
election_data <- read.csv("data/election_dataWorksheet.csv")
fiscal_data <- read.csv("data/revised_fiscal_data.csv")

# Clean fiscal data
fiscal_data$lgutype <- str_trim(fiscal_data$lgutype)
fiscal_data$lgutype <- str_to_title(fiscal_data$lgutype)

# Source all modules
source("R/utils.R")
source("modules/mod_overview.R")
source("modules/mod_presidential.R")
source("modules/mod_vice_presidential.R")
source("modules/mod_senatorial.R")
source("modules/mod_turnout.R")
source("modules/mod_dynasties.R")
source("modules/mod_fiscal.R")