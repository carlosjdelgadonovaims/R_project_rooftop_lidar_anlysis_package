# R_project_rooftop_lidar_anlysis_package
R package for rooftop statistics and visualization from LIDAR data
---
title: "*RooftopsHubAnalysisLidar*: A great package for the rooftop shape assessment of the buildings with the aim of establishing suitable hubs locations for air-taxis"
author: "Carlos Delgado"
date: "9 of August 2019"
output: html_vignette
vignette: >
  %\VignetteIndexEntry{Vignette Title}
  %\VignetteEngine{knitr::rmarkdown}
  \usepackage[utf8]{inputenc}
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


##Introduction

**RooftopsHubsAnalysisLidar** is a package implemented in R that seeks to support the location of potential hubs (Vertihubs, Vertistops, Vertiports) for air-taxis in New York City. Subsequently, the package has two approaches. The first is designed to find out the available areas in the city to establish the hubs of these aircrafts with eVTOL technology. The second approach leads to the location with high precision of the roofs of the buildings with the greatest potential to develop this type of structures by considering the shape of the rooftop. Thus, the first approach uses different sources of information for New York City. These sources are Parks (Reserve areas), Educational centers (Schools, Universities), Hospitals, Embassies, Graveyards, Airports (Flightpaths) and finally the noise map (air-traffic and road-traffic noise). All these spatial entities have been previously considered in concepts of several space agencies such as **ESA**, **NASA** and **Federal Aviation Administration (FAA)**. They have also been the subject of research for other projects such as the analysis of Least Cost Networks for this type of vehicles *(Hildemann and Delgado, 2019)*. Nevertheless, this tool offers the advantage that it also calculates the average noise level for each of the roofs in an available area. On the other hand, the second approach seeks greater accuracy of the potential roofs of buildings that are in suitable areas evaluating the shape of the rooftops. To carry out this function, Lidar files with point clouds and the rooftops print as shapefile files are used as the main source. In this order of ideas, the functions offered by this package lead to the development of high-level research projects. Some of them, such as the 3D suitability analysis for the location of hubs for air taxis by considering machine learning approaches and the optimization of least cost networks through genetic algorithms.


##Data description


The data implemented which will restrict the areas where aircraft can fly mostly are from Open Street Maps and New York Open Data. These data are provided in shapefile format. In contrast, the noise map, although not considered restrictive, plays a very important role in decision making. This is because the ideal location of the Hubs should be in areas that have a moderate noise. Meaning the noise should not exceeding 55 db according to the **US Enviromental Protection Agency (EPA)**. This noise map is taken from the US Department Transportation and is downloaded in .tif format. On the other hand, to perform the detailed analysis of the rooftop shapes, we are using the information of rooftops prints and the Lidar point cloud files from New York Open Data. The latter due to they are data with a high level of detail occupy a large physical space on disk and therefore its processing time is a function of the memory of the machine where the functions are run.


##Calculating the Air-Traffic and Road-Traffic Noise of potential rooftops within available areas.

This first analysis has several stages. The first stage consists in the calculation of the restricted area. At this stage the algorithm takes the information from the shapefiles of parks, hospitals, schools, cemeteries, flightpaths, embassies and performs a spatial buffer according to certain distances taken from previously reviewed concepts. Once the shapefiles are projected and transformed when necessary, we proceed to make a union of them. Then, the algorithm performs a dissolve to generate a unique geometry of the restriction zones. These are then the input to perform a within (negative) operation with the New York buildings. For the example below, only one sample is taken due to the large volume of roofs in New York. Having this result, the roofs that are in the available areas are overlapped with the noise map. In other words they are performing a raster extraction of the air-traffic and road-traffic noise. Finally, a table with all the statistics of each of the roofs is consolidated.

Because the plots are being generated using the lidR library, they will appear as pop-up windows


```{r fapp}
library(RooftopsHubsAnalyisisLidar)
p_s_u <- system.file("extdata/final_schools_uni_man.shp", package = "RooftopsHubsAnalyisisLidar")
p_g_e <- system.file("extdata/final_graveyard_embassy_man.shp", package = "RooftopsHubsAnalyisisLidar")
p_h <- system.file("extdata/final_hospitals_man.shp", package = "RooftopsHubsAnalyisisLidar")
p_p_r <- system.file("extdata/final_park_reserve_man.shp", package = "RooftopsHubsAnalyisisLidar")
p_f_p <- system.file("extdata/final_flightpaths_man.shp", package = "RooftopsHubsAnalyisisLidar")
p_b_n <- system.file("extdata/boundaries_manhattan.shp", package = "RooftopsHubsAnalyisisLidar")
b_man <- system.file("extdata/sample_buildings_manhattan.shp", package = "RooftopsHubsAnalyisisLidar")
n_m_r <- system.file("extdata/noise_m.tif", package = "RooftopsHubsAnalyisisLidar")
new_statis <- availableAreaHubs_noiseAnalysis(p_s_u, p_g_e, p_h, p_p_r, p_f_p, p_b_n, b_man, n_m_r)
new_statis
```

## Calculating the 3d cloud point statistics per each potential rooftop

This second approach of the package, aims to provide the user with different options to visualize the shape of the roofs when analyzing the heights given by the Lidar points. In addition, to know the behavior of the statistical and spatial distribution of the heights on each roof. To do this, first of all the package offers a function to crop Lidar files taking into account the edges of the print rooftops. Additionally, the function saves these clippings in a temporary folder or in a folder selected by the user. Consequently, after having a folder with these clippings there are several functions that allow to visualize histograms, boxplots and grid metrics to visualize the homogeneity of the heights.

The following are the plots of each of the three functions that will allow us to have better tools to define the potential roofs.


```{r sapp}
library(RooftopsHubsAnalyisisLidar)
path_las <- system.file("extdata/new_lidar.las", package = "RooftopsHubsAnalyisisLidar")
sample_rooftops_print <- system.file("extdata/sample_roofs.shp", package = "RooftopsHubsAnalyisisLidar")
list_las_elements <- clipping_lidar_buildings_opPlot(path_las, sample_rooftops_print)
#Because the plots are being generated using the lidR library, they will appear as pop-up windows 
```

## Calculating the 3d cloud point statistics per each potential rooftop

This second approach of the package, aims to provide the user with different options to visualize the shape of the roofs when analyzing the heights given by the Lidar points. In addition, to know the behavior of the statistical and spatial distribution of the heights on each roof. To do this, first of all the package offers a function to crop Lidar files taking into account the edges of the print rooftops. Additionally, the function saves these clippings in a temporary folder or in a folder selected by the user. Consequently, after having a folder with these clippings there are several functions that allow to visualize histograms, boxplots and grid metrics to visualize the homogeneity of the heights.

The Boxplot diagrams below show that the roofs of buildings 10, 12 and 17 show a distribution that could cover a small range of elevations. While the roofs of buildings 2 5 and 7 show a distribution with a wider range of elevations. This means that what has a low distribution range could have a flatter surface and suitable for the development of the hubs.


```{r tapp, echo=FALSE ,fig.align = "center", fig.height=8, fig.width=7}
library(RooftopsHubsAnalyisisLidar)
library(ggplot2)
path_folder_las <- system.file("extdata/sample_las", package = "RooftopsHubsAnalyisisLidar")
list_las <- base::list.files(path_folder_las,full.names = TRUE)
print(list_las)
dist_heigh_boxplots_lidar(list_las)
```


A very similar situation can be reflected in the histograms of the elevations.

```{r capp, echo=FALSE, fig.align = "center", fig.height=8, fig.width=7}
library(RooftopsHubsAnalyisisLidar)
library(ggplot2)
path_folder_las <- system.file("extdata/sample_las", package = "RooftopsHubsAnalyisisLidar")
list_las <- base::list.files(path_folder_las,full.names = TRUE)
dist_heigh_histogrmas_lidar(list_las)
```


Finally, the grid statistics are shown below, in this case evaluated through the average and with a default cell size of 5 square meters. In this case we can confirm the previous statement where buildings 10, 12 and 17 have a roof with characteristics of being mostly flat.

```{r qapp, echo=FALSE, fig.align = "center", fig.height=10, fig.width=9}
library(RooftopsHubsAnalyisisLidar)
library(RColorBrewer)
library(stringr)
path_folder_las <- system.file("extdata/sample_las", package = "RooftopsHubsAnalyisisLidar")
list_las <- base::list.files(path_folder_las,full.names = TRUE)
buildings_grid_metrics_lidar(list_las)
```

##Results and discussion

As we could see, the function related to the first approach allows the user to have in great detail the noise levels for each building that is in the available area. The results given by the functions of the second approach yield the following characteristics. Boxplots help verify if there is a large or smaller distribution of heights for each building. Similarly also histograms. On the other hand, grila metrics are very important in the sense that they allow the user to visually know the homogeneity of the roof with the naked eye.


##References


*Civil Aviation Authority (2014). Managing Aviation Noise (No. CAP 1165). London. Retrieved from https://publicapps.caa.co.uk/modalapplication.aspx?appid=11&mode=detail&id=6251
Hildemann, M., & Delgado, C. (Eds.) 2019. An adaptable and scalable least cost network for air-taxis in urban areas Study area: Manhattan, New York. Accepted Short Papers and Posters: AGILE. Retrieved from https://agile-online.org/images/conference_2019/documents/short_papers/40_Upload_your_PDF_file.pdf
Hurtley, C. (2009). Night noise guidelines for Europe. Copenhagen: World Health Organization Europe. Retrieved from http://www.euro.who.int/%5F%5Fdata/assets/pdf%5Ffile/0017/43316/E92845.pdf 
Sean Captain (2017, April 24). Uber’s Flying Car Chief On Noise Pollution And The Future Of Sky Taxis. Fast Company. Retrieved from https://www.fastcompany.com/40411391/inside-ubers-ambitious-project-to-fill-the-sky-with-flying-taxis
Sustainable Aviation (2019). Sustainable Aviation Noise Road-Map. Retrieved from https://www.sustainableaviation.co.uk/wp-content/uploads/2018/06/SA-Noise-Road-Map-Report.pdf 
Office of noise abatement and control 12, U.S. Environmental Protection Agency 1974.
Uber Elevate (2016). Fast-Forwarding to a Future of On-Demand Urban Air Transportation: White paper. Retrieved from Uber website: https://www.uber.com/us/en/elevate/ 
Bureau of Statistics (2014). Comma Seperated Value BTS 15-17.
World Health Organization, G. (1999). Guidelines for Community Noise. Retrieved from https://www.nh.gov/osi/energy/programs/documents/sb99-who-guidelines-community-noise.pdf*
