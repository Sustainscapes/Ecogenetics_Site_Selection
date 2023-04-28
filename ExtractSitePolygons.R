library(terra)
library(tidyverse)
library(foreign)

Grasslands <- c("Frit areal (overdrev)", "Græs-indlandsklit", "Kalkoverdrev", "Overdrev", "Slette, Overdrev (græsset)","Slette, Overdrev (overdrev)", "Slette, Overdrev (Slette)", "Surt overdrev", "tør overdrev på kalkholdigt sand", "skrænt")

PlotsToExtract <- read_csv("GrasslandsALL_INFO.csv") |> dplyr::select(akt_id, lon, lat)
