#' availableAreaHubs_noiseAnalysis Function
#'
#' This function is responsible for taking the main sources of information to
#' calculate the area available for the flight of air-taxis over New York City (Manhattan).
#' For this, it is responsible for carrying out the calculation of the restricted
#' areas considering the presence of Schools, Parks, Embassies, Cemeteries, Hospitals and
#' Flight paths. Once this area is estimated, the function is responsible for obtaining
#' what are the roofs of the benefits that are on this available area. Having these
#' potential roofs, the crossing is made with the noise map of the city, which is
#' very important for the establishment of the hubs.
#' The function returns the noise statistics of each of the roofs of the buildings.
#' @name availableAreaHubs_noiseAnalysis
#' @importFrom sf st_read st_buffer st_union st_difference st_within
#' @importFrom stats filter
#' @keywords lidar, shapefile, rooftops, plot, buildings, clip, area available
#' @param p_schools_uni This is the path where the shapefile of schools and universities is located
#' @param p_grave_embassy This is the path where the shapefile of graveyards and embassies is located
#' @param p_hospitals This is the path where the shapefile of hospitals is locates
#' @param p_park_reserve This is the path where the shapefile of parks and reserve areas is located
#' @param p_flight_paths This is the path where the shapefile of the flight restricted areas (Due to the presence of airports)is located
#' @param p_ny_neigh This is the path where the shapefile of border of New York or Manhattan (It depends of the processing time considered the selection of the whole New York or only Manhattan) district is located
#' @param p_buildings_manhattan This is the path where the shapefile of the rooftops of the buildings is located (It could be the buildings of the whole New York, Manhattan or a sample. It depends on the processing time considered)
#' @param p_noise_map_raster This is the path where the noise map in raster format is located
#' @return summary_stats_build The function returns a data frame with the noise statistics presented in each of the roofs of the selected buildings
#' @examples
#' path_schools_uni <- system.file("extdata/final_schools_uni_man.shp", package = "RooftopsHubsAnalyisisLidar")
#' path_grave_embassy <- system.file("extdata/final_graveyard_embassy_man.shp", package = "RooftopsHubsAnalyisisLidar")
#' path_hospitals <- system.file("extdata/final_hospitals_man.shp", package = "RooftopsHubsAnalyisisLidar")
#' path_park_reserve <- system.file("extdata/final_park_reserve_man.shp", package = "RooftopsHubsAnalyisisLidar")
#' path_fligh_paths <- system.file("extdata/final_flightpaths_man.shp", package = "RooftopsHubsAnalyisisLidar")
#' path_ny_neigh <- system.file("extdata/boundaries_manhattan.shp", package = "RooftopsHubsAnalyisisLidar")
#' buildings_manhattan <- system.file("extdata/sample_buildings_manhattan.shp", package = "RooftopsHubsAnalyisisLidar")
#' noise_map_raster <- system.file("extdata/noise_m.tif", package = "RooftopsHubsAnalyisisLidar")
#' availableAreaHubs_noiseAnalysis(path_schools_uni, path_grave_embassy, path_hospitals, path_park_reserve, path_fligh_paths, path_ny_neigh, buildings_manhattan, noise_map_raster)
#' @export

availableAreaHubs_noiseAnalysis <- function(p_schools_uni, p_grave_embassy, p_hospitals,
                                            p_park_reserve, p_flight_paths, p_ny_neigh,
                                            p_buildings_manhattan, p_noise_map_raster){

  shp_sch_uni <- sf::st_read(p_schools_uni)
  shp_grave_embassy <- sf::st_read(p_grave_embassy)
  shp_hosp <- sf::st_read(p_hospitals)
  shp_park_res <- sf::st_read(p_park_reserve)
  shp_flightp<- sf::st_read(p_flight_paths)
  shp_ny_neigh <- sf::st_read(p_ny_neigh)
  shp_buildings_manhattan <- sf::st_read(p_buildings_manhattan)

  #Verifying the projection and epsg of all the shpefiles as inputs of the analysis

  #Verifying the projection and epsg of all the shpefiles as inputs of the analysis
  shp_sch_uni <- verify_projection_epsg_shp(shp_sch_uni)
  shp_grave_embassy <- verify_projection_epsg_shp(shp_grave_embassy)
  shp_hosp <- verify_projection_epsg_shp(shp_hosp)
  shp_park_res <- verify_projection_epsg_shp(shp_park_res)
  shp_flightp <- verify_projection_epsg_shp(shp_flightp)
  shp_ny_neigh <- verify_projection_epsg_shp(shp_ny_neigh)
  shp_buildings_manhattan <- verify_projection_epsg_shp(shp_buildings_manhattan)

  summary_stats_build = ""

  buffer_sch_uni <- sf::st_buffer(shp_sch_uni, 100)
  buffer_grave_emb <- sf::st_buffer(shp_grave_embassy, 300)
  buffer_hosp <- sf::st_buffer(shp_hosp, 300)
  buffer_park_res <- sf::st_buffer(shp_park_res, 100)
  buffer_flightp <- sf::st_buffer(shp_flightp, 50)

  buffer_list <- list(buffer_sch_uni, buffer_grave_emb,
                      buffer_hosp, buffer_park_res,
                      buffer_flightp)

  union_buffer_300m_alt <- sf::st_union(buffer_grave_emb,
                                        buffer_hosp,
                                        buffer_park_res,
                                        buffer_flightp)
  union_buffer_300m_alt$buffers <- "buff_300m"

  #Make the dissolve of the polygons with restricted area
  diss_union_buffers <- make_dissolve_one_f(union_buffer_300m_alt, "buffers")
  #mapView(diss_union_buffers)

  #Performing the difference operation in order to have the final available area where air taxis can fly
  area_available_flight <- sf::st_difference(shp_ny_neigh, diss_union_buffers)
  #mapView(area_available_flight)

  #Performing the mask of the noise map with the available area
  raster_noise_map <- raster::raster(p_noise_map_raster)

  #verifying the projection of the raster on order to have the same on as the vector mask
  raster_noise_map <- verify_projection_raster(raster_noise_map)
  cropped_noise_map <- raster::mask(x = raster_noise_map, mask = area_available_flight)

  #Selecting the buildings that are withing the available area
  shp_buildings_manhattan$indicator <- sf::st_within(shp_buildings_manhattan,
                                                     area_available_flight) %>% lengths > 0
  print("Evaluated buildings...")
  print(shp_buildings_manhattan)
  indicator = "indicator"
  #shp_buildings_manhattan <- shp_buildings_manhattan %>% stats::filter(indicator == FALSE)
  shp_buildings_manhattan <- shp_buildings_manhattan[shp_buildings_manhattan$indicator == FALSE, ]
  #mapView(shp_buildings_manhattan)
  print("Saved...")
  print(shp_buildings_manhattan)

  #calculating noise statistics for each building
  summary_stats_build <- noise_statistics_per_building(raster_noise_map,
                                                       shp_buildings_manhattan)
  if (verify_id_field_shapefile(shp_buildings_manhattan, "OBJECTID") == TRUE){

    summary_stats_build$Objectid <- shp_buildings_manhattan$OBJECTID
    print(summary_stats_build)

  }else{

    summary_stats_build$Objectid <- shp_buildings_manhattan$FID
    print(summary_stats_build)

  }
  return(summary_stats_build)
}

#' make_dissolve_one_f Function
#'
#' Function to perform the dissolve of a shapefile according to a certain field
#' @name make_dissolve_one_f
#' @importFrom sf st_cast
#' @importFrom dplyr summarise group_by
#' @importFrom magrittr %>%
#' @param layer_obj Path of the shapefile to perform the validation
#' @param field_diss Name of the field which is going to be used to perform the dissolve
#' @export

make_dissolve_one_f <- function(layer_obj, field_diss){
  print("Dissolving...")

  layer_obj %>%
    dplyr::group_by(toString(field_diss)) %>%
    dplyr::summarise() %>%
    sf::st_cast() -> new_dissolve_union

  return(new_dissolve_union)
}

#' verify_id_field_shapefile Function
#'
#' Function to verify if the field provided by the user exists or not
#' @name verify_id_field_shapefile
#' @param shp_buildings Shapefile to perform the validation
#' @param name_id_field name_id_field Name of the field to verify if it exits within the shapefile
#' @export

verify_id_field_shapefile <- function(shp_buildings, name_id_field){
  #list of attributes from the shapefile
  print("Verifying...")
  l <-  base::names(shp_buildings)
  validation <- name_id_field %in% l
  print(validation)
  return(validation)
}

#' noise_statistics_per_building Function
#'
#' Function to perform the calculation of the noise pixel statistics for each building
#' @name noise_statistics_per_building
#' @importFrom plyr ldply
#' @importFrom raster extract
#' @param noise_map Noise map in raster format
#' @param shape_buildings_p Shapefile of the potential building for develpoing Air-Taxis Hubs
#' @export

noise_statistics_per_building <- function(noise_map, shape_buildings_p){


  print("Processing statistics of noise per building...")
  print(shape_buildings_p)
  i <- 1
  summary_statistics_buildings <- ""
  val_objid <- verify_id_field_shapefile(shape_buildings_p, "OBJECTID")
  if (val_objid == FALSE){
    val_fid <- verify_id_field_shapefile(shape_buildings_p, "FID")
    if (val_fid == FALSE){
      val_validation <- FALSE
    }else{
      val_validation <- TRUE

      #Case when the id field is FID
      list_noise_buildings = list()

      for (row in 1:(dim(shape_buildings_p)[1])){

        fid_shp <- shape_buildings_p$FID[i]
        subset_shp <- subset(shape_buildings_p, FID==fid_shp)
        #Extracting the noise pixels from the raster map
        noise_map_building <- raster::extract(x = noise_map, y = subset_shp, df=TRUE)
        #print(noise_map_building)

        list_noise_buildings[[i]] <- noise_map_building$noise_m
        i <- i+1
      }
      summary_statistics_buildings <- plyr::ldply(list_noise_buildings, summary)
    }
  }else{
    val_validation <- TRUE

    #Case when the id field is OBJECTID
    list_noise_buildings = list()

    for (row in 1:(dim(shape_buildings_p)[1])){

      fid_shp <- shape_buildings_p$OBJECTID[i]
      subset_shp <- subset(shape_buildings_p, OBJECTID==fid_shp)
      #Extracting the noise pixels from the raster map
      noise_map_building <- raster::extract(x = noise_map, y = subset_shp, df=TRUE)
      #print(noise_map_building)

      list_noise_buildings[[i]] <- noise_map_building$noise_m
      i <- i+1
    }
    summary_statistics_buildings <- plyr::ldply(list_noise_buildings, summary)
  }

  if (val_validation == FALSE){
    print("Please verify the field selected which has the id of each feature")
    summary_statistics_buildings <- "Error"
  }

  return(summary_statistics_buildings)
}

#' verify_projection_epsg_shp Function
#'
#' Function that perform the validation of the projection and epsg of a shapefile
#' @name verify_projection_epsg_shp
#' @importFrom sf st_crs st_transform
#' @param shp_entity Shapefile of interest to perform the validation
#' @export

verify_projection_epsg_shp <- function(shp_entity){

  if (is.na(sf::st_crs(shp_entity))){
    new_proj <- "+proj=lcc +lat_1=40.66666666666666 +lat_2=41.03333333333333 +lat_0=40.16666666666666 +lon_0=-74 +x_0=300000 +y_0=0 +datum=NAD83 +units=m +no_def"
    shp_entity<- sf::st_transform(sf::st_geometry(shp_entity), crs=new_proj)
  }

  if (is.na(sf::st_crs(shp_entity)$epsg)){
    sf::st_crs(shp_entity)$epsg <- 26918
  }
  return(shp_entity)
}

#' verify_projection_raster Function
#'
#' Function that perform the validation of the projection of a raster
#' @name verify_projection_raster
#' @param raster_entity Noise map in raster format
#' @export

verify_projection_raster <- function(raster_entity){
  new_proj <- "+proj=lcc +lat_1=40.66666666666666 +lat_2=41.03333333333333 +lat_0=40.16666666666666 +lon_0=-74 +x_0=300000 +y_0=0 +datum=NAD83 +units=m +no_def"

  if (new_proj != toString(raster::crs(raster_entity))){
    print("Projecting raster...")
    raster_entity <- raster::projectRaster(raster_entity, crs=new_proj)
  }
  return(raster_entity)
}
