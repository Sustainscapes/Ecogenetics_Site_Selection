
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Ecogenetics_Site_Selection

<!-- badges: start -->
<!-- badges: end -->

The goal of Ecogenetics_Site_Selection is to select the sites for the
ecogenetics field design, here we will reproduce the Summary_2500.rds
and Summary_2500.rds and CandidatesDryCont_2500.rds that are used in the
`Site_Selection_Site.Rmd` and `Site_Selection_Pres.Rmd`, and build in
the `Template.R` script

## Pacakges needed

First we will load the needed packages that are needed:

``` r
library(sf)
library(terra)
library(tidyverse)
library(tidyterra)
library(terra)
library(foreign)
library(raster)
library(maptools)
library(geodata)
library(tm)
library(plotKML)
```

## Get the dry nature

First we read basemap, and we only use the codes that are considered to
be dry-nature

``` r
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
terra::writeRaster(basemap_d, "basemap_d.tif", overwrite = T)
```

and we will get a polygon of Denmark for further plots

``` r
basemap_d <- terra::rast("basemap_d.tif")
DK <- geodata::gadm(country = "Denmark", level = 0, path = getwd(), version = "4.0") %>%
  terra::project(terra::crs(basemap_d))
```

Here we can see were dry nature currently is in Denmark

``` r
basemap_d <- terra::rast("basemap_d.tif")
ggplot() + 
  geom_spatvector(data = DK, fill = "grey") + 
  geom_spatraster(data = basemap_d) + theme_bw() +
  tidyterra::scale_fill_hypso_c()
#> SpatRaster resampled to ncells = 500703
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

## Get all potential plots:

``` r
basemap_d <- terra::rast("basemap_d.tif")
NOVANAAndP3_tozsofia <- read_delim("O:/Nat_Sustain-proj/_user/ZsofiaKoma_au700510/forArchiving/FeasStudy/processing/field/novana/NOVANAAndP3_tozsofia/NOVANAAndP3_tozsofia.tsv",
                                   delim = "\t", escape_double = FALSE,
                                   trim_ws = TRUE)
#> Rows: 5292804 Columns: 21
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr  (5): ProgNavn, LatArt, DelAktNavn, Objekt_id, Habitat
#> dbl (16): ProgID, AktID, Plot5mID, Plot15mID, ArtID, TaxonomiID, DelAktID, S...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.


Novana_plots <- read_sf("O:/Nat_Sustain-proj/_user/ZsofiaKoma_au700510/forArchiving/FeasStudy/processing/field/novana/NOVANAAndP3_tozsofia/Novana_plots_utm.shp")


Biow_plots <- read_sf("O:/Nat_Sustain-proj/_user/ZsofiaKoma_au700510/forArchiving/FeasStudy/processing/field/novana/NOVANAAndP3_tozsofia/data_plot_forshp_biow_utm.shp")


AllPlots <- st_join(Novana_plots,Biow_plots, suffix=c("",".y")) %>%
  mutate(Habitat = case_when(is.na(Habitat) ~ Habitat.y,
                             !is.na(Habitat) ~ Habitat.y),
         HabitatID = case_when(is.na(HabitatID) ~ HabitatID.y,
                             !is.na(HabitatID) ~ HabitatID.y)) %>%
  dplyr::select(-ends_with(".y")) %>%
  vect() %>%
  terra::project(terra::crs(basemap_d))

terra::writeVector(AllPlots, "AllPlots.shp", overwrite = T)
```

as an example we can see the habitat type

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

## Find candidate plots

We now find only the plots that are part over what basemap considers to
be dry nature:

``` r
AllPlots <- terra::vect("AllPlots.shp")
basemap_d <- terra::rast("basemap_d.tif")
Green <-  basemap_d %>% terra::extract(AllPlots) %>% pull(C_01)
Green <- !is.na(Green)

DryNaturePlot <- AllPlots[Green,]

DryNaturePlot <-  DryNaturePlot[DryNaturePlot$Habitat %in% c("Kalkoverdrev",
"Overdrev",
"Surt overdrev",
"Tør overdrev på kalkholdigt sand"),]
terra::writeVector(DryNaturePlot, "DryNaturePlot.shp", overwrite = T)
```

This reduces our universe from 100,594 to 2,385, those plots are shown
in the following map:

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

## NOw to select the sites

``` r
basemap <- terra::rast("O:/Nat_Sustain-proj/_user/HanneNicolaisen_au704629/Data/Land_cover_maps/Basemap/Basemap03_public_geotiff/basemap03_2011_2016_2018/lu_agg_2018.tif")
rat <- read.dbf("O:/Nat_Sustain-proj/_user/HanneNicolaisen_au704629/Data/Land_cover_maps/Basemap/Basemap03_public_geotiff/basemap03_2018/lu_00_2018.tif.vat.dbf")

DryNaturePlot <- terra::vect("DryNaturePlot.shp")

basemap_d <- terra::rast("basemap_d.tif")

key <- rat %>% dplyr::select(C_11, C_13) %>% distinct() %>% rename(value = C_11, habitat_type = C_13) |> 
  mutate(value = as.character(value))

key$habitat_type <- tm::removeNumbers(as.character(key$habitat_type)) |> 
     str_trim()

cls <- key |> dplyr::rename(id = value) |> dplyr::mutate(id = as.numeric(id))

# key <- rat %>% dplyr::select(C_11, C_13) %>% distinct() %>% rename(id = C_11, habitat_type = C_13) 
# 
# key$habitat_type <- tm::removeNumbers(as.character(key$habitat_type)) |> 
#   str_trim() 
# 
# levels(basemap) <- key

distances <- c(2500)

dir.create("Polygons")
#> Warning in dir.create("Polygons"): 'Polygons' already exists

dir.create("Polygons/shp")
#> Warning in dir.create("Polygons/shp"): 'Polygons\shp' already exists
dir.create("Polygons/kml")
#> Warning in dir.create("Polygons/kml"): 'Polygons\kml' already exists

# MAXES <- list()
# 
# for(x in 1:1000){
#   if(x == 1){
#     NewDry <- DryNaturePlot
#     Max <- NewDry[NewDry$SpRichness == max(NewDry$SpRichness),]
#     Buffer <- Max %>% terra::buffer(2500) 
#     Area <- basemap %>% terra::crop(Buffer) %>% terra::mask(Buffer)
#     Table <- Area %>% terra::freq() %>% left_join(key) %>% arrange(desc(count))
#     Area_Class <- Area
#     levels(Area_Class) <- cls
#     Area_Class_SF <- as.polygons(Area_Class, dissolve = TRUE)
#       
#     ToFilter <- extract(Area_Class, Max)
#       
#       
#     Polygon <- Area_Class_SF[Area_Class_SF$habitat_type == ToFilter$habitat_type,] 
#       
#     Polygon <- Polygon |> 
#         sf::st_as_sf() |> 
#         sf::st_cast(to = "POLYGON") |> 
#         terra::vect()
# 
#     Table$Prop <- round(100*(Table$count/sum(Table$count)), 2)
#     Total_Nature <- Table %>% dplyr::filter(habitat_type %in% c("Agriculture, intensive, permanent crops", "Agriculture, intensive, temporary crops","Forest", "Forest, wet","Nature, dry", "Nature, wet", "Lake", "Stream", "Sea")) %>%
#         dplyr::select(Prop) %>%
#         summarize_all(sum) %>%
#         mutate(habitat_type = "Total nature")
# 
#     Table <- Table %>% bind_rows(Total_Nature)
# 
# 
#     Tables <- Table %>% dplyr::filter(habitat_type %in%
#                                                c("Sea", "Agriculture, intensive, temporary crops", "Nature, dry", "Low built up", "Forest", "Nature, wet", "Agriculture, intensive, permanent crops", "Stream", "Lake", "Industry / business", "Railway", "Building", "Other built up", "High built up",  "City centre", "Road, paved", "Road, not paved", "Total nature")) %>%
#         dplyr::select(habitat_type, Prop) %>% pivot_wider(names_from = habitat_type, values_from = Prop) %>% mutate(distance = 2500, AktID = Max$AktID) %>% dplyr::relocate(AktID, .before = everything()) %>%
#         janitor::clean_names()
# 
#     
# 
# 
#     Tables <- Tables %>% replace(is.na(.), 0)
#     
#     try(terra::writeVector(Polygon, paste0("Polygons/shp/", Tables$akt_id, ".shp"), overwrite=TRUE))
#     try(plotKML::plotKML(st_as_sf(terra::project(Polygon, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")), paste0("Polygons/kml/", Tables$akt_id)))
#     
# 
#     P <- basemap_d %>% terra::crop(Buffer) %>% terra::mask(Buffer)%>%  patches()
# 
#     Val <- terra::extract(P, Max) %>% pull(patches) %>% unique()
# 
#     P[P != Val] <- NA
# 
# 
#     z = cellSize(P,unit="ha") |> zonal(P, sum)
# 
#     Tables$Patch_Size <- round(z$area, 2)
# 
#     Max2 <- Max %>% st_as_sf() %>%
#       janitor::clean_names() %>%
#       full_join(Tables)
#     MAXES[[x]] <- Max2
# 
#     Selected <- Max2$akt_id
# 
#     Intersected <- terra::intersect(DryNaturePlot, Buffer) %>%
#       as.data.frame() 
#     
#     Intersected[,1:6]
#     
#     Intersected <- Intersected$AktID
# 
#     IDs <- append(Selected, Intersected)
#     NewDry <- NewDry[!(NewDry$AktID %in% IDs),]
#   } else if (x != 1){
# 
#     if((x %% 2) == 0){
#       Max <- NewDry[NewDry$SpRichness == max(NewDry$SpRichness),]
#       print("Max")
#     } else if ((x %% 2) != 0){
#       Max <- NewDry[NewDry$SpRichness == min(NewDry$SpRichness),]
#       print("Min")
#     }
# 
#     Max <- Max[1,]
#     Buffer <- Max %>% terra::buffer(2500) 
#     Area <- basemap %>% terra::crop(Buffer) %>% terra::mask(Buffer)
#     Table <- Area %>% terra::freq() %>% left_join(key) %>% arrange(desc(count))
#     Area_Class <- Area
#     levels(Area_Class) <- cls
#     Area_Class_SF <- as.polygons(Area_Class, dissolve = TRUE)
#     
#     ToFilter <- extract(Area_Class, Max)
#     
#     
#     Polygon <- Area_Class_SF[Area_Class_SF$habitat_type == ToFilter$habitat_type,] 
#     
#     Polygon <- Polygon |> 
#       sf::st_as_sf() |> 
#       sf::st_cast(to = "POLYGON") |> 
#       terra::vect()
#     
#     Table$Prop <- round(100*(Table$count/sum(Table$count)), 2)
#     Total_Nature <- Table %>% dplyr::filter(habitat_type %in% c("Agriculture, intensive, permanent crops", "Agriculture, intensive, temporary crops","Forest", "Forest, wet","Nature, dry", "Nature, wet", "Lake", "Stream", "Sea")) %>%
#       dplyr::select(Prop) %>%
#       summarize_all(sum) %>%
#       mutate(habitat_type = "Total nature")
#     
#     Table <- Table %>% bind_rows(Total_Nature)
#     
#     
#     Tables <- Table %>% dplyr::filter(habitat_type %in%
#                                         c("Sea", "Agriculture, intensive, temporary crops", "Nature, dry", "Low built up", "Forest", "Nature, wet", "Agriculture, intensive, permanent crops", "Stream", "Lake", "Industry / business", "Railway", "Building", "Other built up", "High built up",  "City centre", "Road, paved", "Road, not paved", "Total nature")) %>%
#       dplyr::select(habitat_type, Prop) %>% pivot_wider(names_from = habitat_type, values_from = Prop) %>% mutate(distance = 2500, AktID = Max$AktID) %>% dplyr::relocate(AktID, .before = everything()) %>%
#       janitor::clean_names()
#     
#     
#     
#     
#     Tables <- Tables %>% replace(is.na(.), 0)
#     
#     try(terra::writeVector(Polygon, paste0("Polygons/shp/", Tables$akt_id, ".shp"), overwrite=TRUE))
#     try(plotKML::plotKML(st_as_sf(terra::project(Polygon, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")), paste0("Polygons/kml/", Tables$akt_id)))
#     
#     
#     P <- basemap_d %>% terra::crop(Buffer) %>% terra::mask(Buffer)%>%  patches()
#     
#     Val <- terra::extract(P, Max) %>% pull(patches) %>% unique()
#     
#     P[P != Val] <- NA
#     
#     
#     z = cellSize(P,unit="ha") |> zonal(P, sum)
#     
#     Tables$Patch_Size <- round(z$area, 2)
#     
#     Max2 <- Max %>% st_as_sf() %>%
#       janitor::clean_names() %>%
#       full_join(Tables)
#     MAXES[[x]] <- Max2
#     
#     Selected <- Max2$akt_id
#     
#         Intersected <- terra::intersect(DryNaturePlot, Buffer) %>%
#       as.data.frame() 
#     
#     Intersected[,1:6]
#     
#     Intersected <- Intersected$AktID
#     
#     IDs <- append(Selected, Intersected)
#     NewDry <- NewDry[!(NewDry$AktID %in% IDs),]
#   }
#   message(paste(x, "ready", terra::nrow(NewDry), Sys.time(), "area =", round(z$area, 2)))
# }
```

And now make the summary dataset

``` r
# SuperMax <- MAXES %>% bind_rows()  %>%
#   replace(is.na(.), 0)
# 
# saveRDS(SuperMax, "SuperMax_2500_b.rds")
# 
# 
# SuperMaxCoords <- SuperMax %>%
#   sf::st_transform('+proj=longlat +datum=WGS84') %>%
#   st_coordinates() %>%
#   as.data.frame() %>%
#   rename(lon = X, lat = Y)
# 
# SuperMaxCoords$akt_id <- SuperMax$akt_id
# 
# SuperMaxCoords <- SuperMaxCoords %>%
#   distinct()
# 
# saveRDS(SuperMaxCoords, "SuperMaxCoords_2500_b.rds")
# 
# Summary <- SuperMax %>%
#   as.data.frame() %>%
#   replace(is.na(.), 0) %>%
#   dplyr::select(akt_id, year, habitat, habitat_id, Patch_Size, sp_richness, nature_dry, lake, forest, agriculture_intensive_temporary_crops, agriculture_intensive_permanent_crops, total_nature) %>%
#   dplyr::group_by(akt_id, year, habitat, habitat_id, Patch_Size, sp_richness) %>%
#   summarise_all(.funs = list(mean = mean, sd = sd, cv = cv))
# 
# saveRDS(Summary, "Summary_2500_b.rds")
# 
# 
# MoreThanFive <- Summary %>%
#   as.data.frame() %>%
#   replace(is.na(.), 0) %>%
#   group_by(habitat) %>%
#   summarise(n = n()) %>%
#   dplyr::filter(n > 5) %>%
#   arrange(desc(n))
# 
# 
# 
# ggplot(Summary, aes(x = total_nature_mean, y = total_nature_cv)) + geom_point() +
#   theme_bw() +
#   geom_hline(yintercept = median(Summary$total_nature_cv), lty = 2, color = "red")
# 
# ggplot(Summary, aes(x = total_nature_mean, y = total_nature_cv)) + geom_point(aes(size = Patch_Size)) +
#   theme_bw() +
#   geom_hline(yintercept = median(Summary$total_nature_cv), lty = 2, color = "red")
# 
# ### For habitats with more than five
# 
# Summary %>% dplyr::filter(habitat %in% MoreThanFive$habitat) %>%
#   ggplot(aes(x = reorder(habitat, nature_dry_mean), y = nature_dry_mean)) +
#   geom_boxplot() +
#   theme_bw() +
#   labs(x = "Habitat", y = "Average proportion of nature dry")
# 
# 
# Candidates <- Summary %>%
#   dplyr::filter(habitat %in% MoreThanFive$habitat,
#                 total_nature_cv < median(Summary$total_nature_cv)) %>%
#   left_join(SuperMaxCoords)
# 
# saveRDS(Candidates, "Candidates_2500_b.rds")
# 
# SuperCandidates <- SuperMax %>% dplyr::filter(akt_id %in% Candidates$akt_id) %>%
#   sf::st_transform('+proj=longlat +datum=WGS84')
```
