Köppen–Geiger Climate Classification (Robinson Projection)

This repository provides R source code to load, process, and visualize the Köppen–Geiger Climate Classification using a Robinson map projection.
The script uses common spatial and visualization libraries in R to generate publication-quality climate maps.

🚀 Features

Load high-resolution Köppen–Geiger climate data

Reproject data to the Robinson projection for visually balanced world maps

Use ggplot2, rasterVis, and related packages for clear, customizable climate visualizations

Integrate natural earth boundaries, shapefiles, and additional layers

Fully reproducible R workflow

📦 Required R Packages

Your script makes use of the following R libraries:

library(raster)
library(rasterVis)
library(rworldxtra)
library(sf)
library(rnaturalearth)
library(ggplot2)
library(dplyr)
library(extrafont)
