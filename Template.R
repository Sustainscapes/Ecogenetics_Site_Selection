library(sf)
library(terra)
library(tidyverse)
library(terra)
library(foreign)
library(raster)
basemap <- terra::rast("O:/Nat_Sustain-proj/_user/HanneNicolaisen_au704629/Data/Land_cover_maps/Basemap/Basemap03_public_geotiff/basemap03_2011_2016_2018/lu_agg_2018.tif")
rat <- read.dbf("O:/Nat_Sustain-proj/_user/HanneNicolaisen_au704629/Data/Land_cover_maps/Basemap/Basemap03_public_geotiff/basemap03_2018/lu_00_2018.tif.vat.dbf")



rcl_d <- c(110000, NA,
           121000, NA,
           121110, NA,
           122000, NA,
           122110, NA,
           123000, NA,
           123110, NA,
           124000, NA,
           124110, NA,
           125000, NA,
           125110, NA,
           126000, NA,
           126110, NA,
           130000, NA,
           130110, NA,
           141000, NA,
           142000, NA,
           150000, NA,
           150110, NA,
           160000, NA,
           211000, NA,
           212000, NA,
           220000, NA,
           230000, NA,
           311000, NA,
           312000, NA,
           321000, 1,
           321220, 1,
           322000, NA,
           322220, NA,
           411000, NA,
           412000, NA,
           420000, NA,
           800000, NA,
           999999, NA)

rclmat_d <- matrix(rcl_d, ncol=2, byrow=TRUE)
basemap_d<- classify(basemap,rclmat_d)
saveRDS(basemap_d, "DryNature.rds")
#basemap_d2 <- terra::project(basemap_d, "+proj=utm +zone=32 +ellps=intl +units=m +no_defs", method = "near")


Data <- read_csv("O:/Nat_Sustain-proj/_user/ZsofiaKoma_au700510/DiffRS/Field_data/ForZsofia/NOVANAAndP3_tozsofia/NOVANAAndP3_tozsofia.tsv")

NOVANAAndP3_tozsofia <- read_delim("O:/Nat_Sustain-proj/_user/ZsofiaKoma_au700510/DiffRS/Field_data/ForZsofia/NOVANAAndP3_tozsofia/NOVANAAndP3_tozsofia.tsv",
                                   delim = "\t", escape_double = FALSE,
                                   trim_ws = TRUE)


Novana_plots <- read_sf("O:/Nat_Sustain-proj/_user/ZsofiaKoma_au700510/DiffRS/Field_data/ForZsofia/NOVANAAndP3_tozsofia/Novana_plots_utm.shp")


Biow_plots <- read_sf("O:/Nat_Sustain-proj/_user/ZsofiaKoma_au700510/DiffRS/Field_data/ForZsofia/NOVANAAndP3_tozsofia/data_plot_forshp_biow_utm.shp")


AllPlots <- st_join(Novana_plots,Biow_plots, suffix=c("",".y")) %>%
  mutate(Habitat = case_when(is.na(Habitat) ~ Habitat.y,
                             !is.na(Habitat) ~ Habitat.y),
         HabitatID = case_when(is.na(HabitatID) ~ HabitatID.y,
                             !is.na(HabitatID) ~ HabitatID.y)) %>%
  dplyr::select(-ends_with(".y")) %>%
  vect() %>%
  terra::project(terra::crs(basemap_d))


Green <-  basemap_d %>% terra::extract(AllPlots) %>% pull(C_01)
Green <- !is.na(Green)

DryNaturePlot <- AllPlots[Green,]

key <- rat %>% dplyr::select(C_11, C_13) %>% distinct() %>% rename(value = C_11, habitat_type = C_13)

key$habitat_type <- tm::removeNumbers(as.character(key$habitat_type)) %>% str_trim()

distances <- c(500, 1000, 5000, 10000)

MAXES <- list()

for(x in 1:1000){
  if(x == 1){
    NewDry <- DryNaturePlot
    Max <- NewDry[NewDry$SpRichness == max(NewDry$SpRichness),]
    Tables <- list()
    for(i in 1:length(distances)){
      Buffer <- Max %>% terra::buffer(distances[i]) %>% st_as_sf() %>% dplyr::select(geometry) %>% vect()

      Area <- basemap %>% terra::crop(Buffer) %>% terra::mask(Buffer)
      Table <- Area %>% terra::freq() %>% left_join(key) %>% arrange(desc(count))

      Table$Prop <- round(100*(Table$count/sum(Table$count)), 2)
      Tables[[i]] <- Table %>% dplyr::filter(habitat_type %in%
                                               c("Sea", "Agriculture, intensive, temporary crops", "Nature, dry", "Low built up", "Forest", "Nature, wet", "Agriculture, intensive, permanent crops", "Stream", "Lake", "Industry / business", "Railway", "Building", "Other built up", "High built up",  "City centre", "Road, paved", "Road, not paved")) %>%
        dplyr::select(habitat_type, Prop) %>% pivot_wider(names_from = habitat_type, values_from = Prop) %>% mutate(distance = distances[i], AktID = Max$AktID) %>% dplyr::relocate(AktID, .before = everything()) %>%
        janitor::clean_names()

    }


    Tables <- Tables %>% purrr::reduce(bind_rows) %>% replace(is.na(.), 0)

    P <- basemap_d %>% terra::crop(Buffer) %>% terra::mask(Buffer)%>%  patches()

    Val <- terra::extract(P, Max) %>% pull(C_01) %>% unique()

    P[P != Val] <- NA


    z = cellSize(P,unit="ha") |> zonal(P, sum)

    Tables$Patch_Size <- round(z$area, 2)

    Max2 <- Max %>% st_as_sf() %>%
      janitor::clean_names() %>%
      full_join(Tables)
    MAXES[[x]] <- Max2

    Selected <- Max2$akt_id

    Intersected <- terra::intersect(DryNaturePlot, Buffer) %>%
      as.data.frame() %>%
      pull(AktID)

    IDs <- append(Selected, Intersected)
    NewDry <- NewDry[!(NewDry$AktID %in% IDs),]
  } else if (x != 1){
    Max <- NewDry[NewDry$SpRichness == max(NewDry$SpRichness),]
    Max <- Max[1,]
    Tables <- list()
    for(i in 1:length(distances)){
      Buffer <- Max %>% terra::buffer(distances[i]) %>% st_as_sf() %>% dplyr::select(geometry) %>% vect()

      Area <- basemap %>% terra::crop(Buffer) %>% terra::mask(Buffer)
      Table <- Area %>% terra::freq() %>% left_join(key) %>% arrange(desc(count))

      Table$Prop <- round(100*(Table$count/sum(Table$count)), 2)
      Tables[[i]] <- Table %>% dplyr::filter(habitat_type %in%
                                               c("Sea", "Agriculture, intensive, temporary crops", "Nature, dry", "Low built up", "Forest", "Nature, wet", "Agriculture, intensive, permanent crops", "Stream", "Lake", "Industry / business", "Railway", "Building", "Other built up", "High built up",  "City centre", "Road, paved", "Road, not paved")) %>%
        dplyr::select(habitat_type, Prop) %>% pivot_wider(names_from = habitat_type, values_from = Prop) %>% mutate(distance = distances[i], AktID = Max$AktID) %>% dplyr::relocate(AktID, .before = everything()) %>%
        janitor::clean_names()

    }


    Tables <- Tables %>% purrr::reduce(bind_rows) %>% replace(is.na(.), 0)

    P <- basemap_d %>% terra::crop(Buffer) %>% terra::mask(Buffer)%>%  patches()

    Val <- terra::extract(P, Max) %>% pull(C_01) %>% unique()

    P[P != Val] <- NA


    z = cellSize(P,unit="ha") |> zonal(P, sum)

    Tables$Patch_Size <- round(z$area, 2)

    Max2 <- Max %>% st_as_sf() %>%
      janitor::clean_names() %>%
      full_join(Tables)

    MAXES[[x]] <- Max2

    Selected <-append(Selected, Max2$akt_id) %>% unique()

    Intersected <- terra::intersect(DryNaturePlot, Buffer) %>%
      as.data.frame() %>%
      pull(AktID)

    IDs <- append(IDs, Intersected)
    IDs <- append(Selected, Intersected) %>% unique()
    NewDry <- NewDry[!(NewDry$AktID %in% IDs),]
  }
  message(paste(x, "ready", terra::nrow(NewDry), Sys.time(), "area =", round(z$area, 2)))
}



SuperMax <- MAXES %>% bind_rows()  %>%
  replace(is.na(.), 0)

saveRDS(SuperMax, "SuperMax.rds")


SuperMaxCoords <- SuperMax %>%
  sf::st_transform('+proj=longlat +datum=WGS84') %>%
  st_coordinates() %>%
  as.data.frame() %>%
  rename(lon = X, lat = Y)

SuperMaxCoords$akt_id <- SuperMax$akt_id

SuperMaxCoords <- SuperMaxCoords %>%
  distinct()

saveRDS(SuperMaxCoords, "SuperMaxCoords.rds")

Summary <- SuperMax %>%
  as.data.frame() %>%
  replace(is.na(.), 0) %>%
  dplyr::select(akt_id, year, habitat, habitat_id, Patch_Size, sp_richness, nature_dry, lake, forest, agriculture_intensive_temporary_crops, agriculture_intensive_permanent_crops) %>%
  dplyr::group_by(akt_id, year, habitat, habitat_id, Patch_Size, sp_richness) %>%
  summarise_all(.funs = list(mean = mean, sd = sd, cv = cv))

saveRDS(Summary, "Summary.rds")


MoreThanFive <- Summary %>%
  as.data.frame() %>%
  replace(is.na(.), 0) %>%
  group_by(habitat) %>%
  summarise(n = n()) %>%
  dplyr::filter(n > 5) %>%
  arrange(desc(n))


ggplot(Summary, aes(x = nature_dry_mean, y = nature_dry_sd)) + geom_point() +
  theme_bw()

ggplot(Summary, aes(x = nature_dry_mean, y = nature_dry_cv)) + geom_point() +
  theme_bw() +
  geom_hline(yintercept = median(Summary$nature_dry_cv), lty = 2, color = "red")

ggplot(Summary, aes(x = nature_dry_mean, y = nature_dry_cv)) + geom_point(aes(size = Patch_Size)) +
  theme_bw() +
  geom_hline(yintercept = median(Summary$nature_dry_cv), lty = 2, color = "red")

### For habitats with more than five

Summary %>% dplyr::filter(habitat %in% MoreThanFive$habitat) %>%
  ggplot(aes(x = reorder(habitat, nature_dry_mean), y = nature_dry_mean)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = "Habitat", y = "Average proportion of nature dry")


Candidates <- Summary %>%
  dplyr::filter(habitat %in% MoreThanFive$habitat,
                nature_dry_cv < median(Summary$nature_dry_cv)) %>%
  left_join(SuperMaxCoords)

saveRDS(Candidates, "Candidates.rds")

SuperCandidates <- SuperMax %>% dplyr::filter(akt_id %in% Candidates$akt_id) %>%
  sf::st_transform('+proj=longlat +datum=WGS84')

Candidates$sp_richness %>% hist()

library(leaflet)
library(sf)

leaflet() %>% addTiles() %>%
  addCircleMarkers(data = Candidates, popup = ~sp_richness)

library(crosstalk)

