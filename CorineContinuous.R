unzip("C:/Users/au687614/Downloads/3bafef4f5489fe0c5ddc2674daef46d3f915e459.zip")
unzip("C:/Users/au687614/Downloads/83684d24c50f069b613e0dc8e12529b893dc172f.zip")
unzip("C:/Users/au687614/Downloads/9ed7266120d5e2869c20b5142f637148b4cf569d.zip")
unzip("C:/Users/au687614/Downloads/7be11e321982cd1df8a9e71b7ebf6e7839fd1424.zip")
unzip("C:/Users/au687614/Downloads/f610d7aa9b4c715ad9ddacf0b057b95e6f4a1cdc.zip")

unzip("u2018_clc2012_v2020_20u1_raster100m.zip")
unzip("u2012_clc2006_v2020_20u1_raster100m.zip")
unzip("u2006_clc2000_v2020_20u1_raster100m.zip")
unzip("u2006_clc2000_v2020_20u1_raster100m.zip")
unzip("u2000_clc1990_v2020_20u1_raster100m.zip")


Test <- rast("u2018_clc2012_v2020_20u1_raster100m/DATA/U2018_CLC2012_V2020_20u1.tif")
Test2 <- rast("u2018_clc2018_v2020_20u1_raster100m/DATA/U2018_CLC2018_V2020_20u1.tif")
Test3 <- rast("u2012_clc2006_v2020_20u1_raster100m/DATA/U2012_CLC2006_V2020_20u1.tif")
Test4 <- rast("u2006_clc2000_v2020_20u1_raster100m/DATA/U2006_CLC2000_V2020_20u1.tif")
Test5 <- rast("u2000_clc1990_v2020_20u1_raster100m/DATA/U2000_CLC1990_V2020_20u1.tif")

SuperCandidatesWet <- readRDS("CandidatesWet.rds") %>%
  st_as_sf(coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84")

SCW <- terra::project(vect(SuperCandidatesWet), crs(Test))



Test <- Test %>% crop(SCW) %>%  terra::extract(SCW) %>% mutate(LABEL3 = as.character(LABEL3)) %>%
  rename(Landuse2012 = LABEL3)
Test2 <- Test2 %>% crop(SCW) %>%  terra::extract(SCW) %>% mutate(LABEL3 = as.character(LABEL3)) %>%
  rename(Landuse2018 = LABEL3)

Test3 <- Test3 %>% crop(SCW) %>%  terra::extract(SCW) %>% mutate(LABEL3 = as.character(LABEL3)) %>%
  rename(Landuse2006 = LABEL3)

Test4 <- Test4 %>% crop(SCW) %>%  terra::extract(SCW) %>% mutate(LABEL3 = as.character(LABEL3)) %>%
  rename(Landuse2000 = LABEL3)

Test5 <- Test5 %>% crop(SCW) %>%  terra::extract(SCW) %>% mutate(LABEL3 = as.character(LABEL3)) %>%
  rename(Landuse1990 = LABEL3)

Landuse <- list(Test5, Test4, Test3, Test, Test2) %>% purrr::reduce(full_join) %>%
  mutate(Tempcont = case_when(Landuse2018 != Landuse2012 ~ 0,
                              Landuse2018 == Landuse2012 & Landuse2018 != Landuse2006 ~ 6,
                              Landuse2018 == Landuse2012 & Landuse2018 == Landuse2006 & Landuse2018 != Landuse2000 ~ 12,
                              Landuse2018 == Landuse2012 & Landuse2018 == Landuse2006 & Landuse2018 == Landuse2000 & Landuse2018 != Landuse1990 ~ 18,
                              Landuse2018 == Landuse2012 & Landuse2018 == Landuse2006 & Landuse2018 == Landuse2000 & Landuse2018 == Landuse1990 ~ 28)) %>%
  dplyr::select(Tempcont)


CandidatesWetCont <- readRDS("CandidatesWet.rds") %>%
  cbind(Landuse)

saveRDS(CandidatesWetCont, "CandidatesWetCont.rds")


########

SuperCandidates <- readRDS("Candidates.rds") %>%
  st_as_sf(coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84")

Test <- rast("u2018_clc2012_v2020_20u1_raster100m/DATA/U2018_CLC2012_V2020_20u1.tif")
Test2 <- rast("u2018_clc2018_v2020_20u1_raster100m/DATA/U2018_CLC2018_V2020_20u1.tif")
Test3 <- rast("u2012_clc2006_v2020_20u1_raster100m/DATA/U2012_CLC2006_V2020_20u1.tif")
Test4 <- rast("u2006_clc2000_v2020_20u1_raster100m/DATA/U2006_CLC2000_V2020_20u1.tif")
Test5 <- rast("u2000_clc1990_v2020_20u1_raster100m/DATA/U2000_CLC1990_V2020_20u1.tif")

SCW <- terra::project(vect(SuperCandidates), crs(Test))


Test <- Test %>% crop(SCW) %>%  terra::extract(SCW) %>% mutate(LABEL3 = as.character(LABEL3)) %>%
  rename(Landuse2012 = LABEL3)
Test2 <- Test2 %>% crop(SCW) %>%  terra::extract(SCW) %>% mutate(LABEL3 = as.character(LABEL3)) %>%
  rename(Landuse2018 = LABEL3)

Test3 <- Test3 %>% crop(SCW) %>%  terra::extract(SCW) %>% mutate(LABEL3 = as.character(LABEL3)) %>%
  rename(Landuse2006 = LABEL3)

Test4 <- Test4 %>% crop(SCW) %>%  terra::extract(SCW) %>% mutate(LABEL3 = as.character(LABEL3)) %>%
  rename(Landuse2000 = LABEL3)

Test5 <- Test5 %>% crop(SCW) %>%  terra::extract(SCW) %>% mutate(LABEL3 = as.character(LABEL3)) %>%
  rename(Landuse1990 = LABEL3)

Landuse <- list(Test5, Test4, Test3, Test, Test2) %>% purrr::reduce(full_join) %>%
  mutate(Tempcont = case_when(Landuse2018 != Landuse2012 ~ 0,
                              Landuse2018 == Landuse2012 & Landuse2018 != Landuse2006 ~ 6,
                              Landuse2018 == Landuse2012 & Landuse2018 == Landuse2006 & Landuse2018 != Landuse2000 ~ 12,
                              Landuse2018 == Landuse2012 & Landuse2018 == Landuse2006 & Landuse2018 == Landuse2000 & Landuse2018 != Landuse1990 ~ 18,
                              Landuse2018 == Landuse2012 & Landuse2018 == Landuse2006 & Landuse2018 == Landuse2000 & Landuse2018 == Landuse1990 ~ 28)) %>%
  dplyr::select(Tempcont)


CandidatesCont <- readRDS("Candidates.rds") %>%
  cbind(Landuse)

saveRDS(CandidatesCont, "CandidatesCont.rds")
