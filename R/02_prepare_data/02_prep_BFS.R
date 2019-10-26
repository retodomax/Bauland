#' ---
#' project: Bauland    ####################################################
#' title:   Data unification
#' author:  "[Reto Zihlmann](http://www.n.ethz.ch/~retoz)"
#' date:    2019-10-18 15:53:24
#' output:  github_document   #############################################
#' ---


# packages ----------------------------------------------------------------

# devtools::install_git("https://github.com/retodomax/FunRZ")
library(magrittr, warn.conflicts = F); library(FunRZ);   library(tidyverse)
Sys.setlocale("LC_ALL", "English");                   Sys.setenv(LANG="EN")
library(readxl)



bfs_nr_kt <- read_csv("Data/BFS_nr/kt.csv", locale = locale(encoding = "Latin1"))



## general procedere:
## 1) read data
## 2) rename variables (necessary ones)
## 3) make new variable (necessary ones)
## 4) add "reso" variable
## 5) join with bfs_nr_kt to add kanton nr
## 6) fill year
## 7) select necessary variables
## 8) gather to long table



# 02_pg_age20to39 ---------------------------------------------------------

path <- "Data/00_Raw_Data/02_pg_age20to39/px-x-0102010000_101.xlsx"
ages_pg <- read_xlsx(path = path, skip = 2)
alt_anz_20bis39 <- ages_pg[, which(colnames(ages_pg) == "20 Jahre"):
                       which(colnames(ages_pg) == "39 Jahre")] %>% 
  apply(1, sum, na.rm = T)
ages_pg <- ages_pg %>% 
  rename(bev_anz_einwohner = `Alter - Total`) %>% 
  mutate(alt_anz_20bis39 = alt_anz_20bis39,
         alt_ant_20bis39 = alt_anz_20bis39/bev_anz_einwohner) %>% 
  mutate(res_code = stringr::str_sub(name, 1,1)) %>% 
  mutate(reso = ifelse(res_code == "S",
                       "Schweiz",
                       ifelse(res_code == "-",
                              "Kanton",
                              ifelse(res_code == ">",
                                     "Bezirk", "Gemeinde")))) %>% 
  left_join(bfs_nr_kt, by = c("nr" = "kt_kurzel")) %>% 
  mutate(nr = ifelse(is.na(kt_nr), nr, kt_nr)) %>% 
  fill(year) %>% 
  select(reso, nr, name, year, bev_anz_einwohner,
         alt_anz_20bis39, alt_ant_20bis39) %>% 
  gather(bev_anz_einwohner, alt_anz_20bis39, alt_ant_20bis39,
         key = "target", value = "value") %>% 
  mutate_at(c("nr", "year", "value"), list(as.numeric))




# 03_pg_birth_by_age ------------------------------------------------------

path <- "Data/00_Raw_Data/03_pg_birth_by_age/px-x-0102020204_102.xlsx"
birth_pg <- read_xlsx(path = path, skip = 2)
birth_pg <- birth_pg %>% 
  rename(geb_anz_geb = `Altersklasse der Mutter - Total`,
         unter25 = `Unter 25 Jahren`) %>% 
  mutate(ueber35 = `30-34 Jahre` + `35-39 Jahre` + `40 Jahre und mehr`,
         geb_ant_geb_unt25 = unter25/geb_anz_geb,
         geb_ant_geb_ueb35 = ueber35/geb_anz_geb) %>% 
  mutate(res_code = stringr::str_sub(name, 1,1)) %>% 
  mutate(reso = ifelse(res_code == "S",
                       "Schweiz",
                       ifelse(res_code == "-",
                              "Kanton",
                              ifelse(res_code == ">",
                                     "Bezirk", "Gemeinde")))) %>% 
  left_join(bfs_nr_kt, by = c("nr" = "kt_kurzel")) %>% 
  mutate(nr = ifelse(is.na(kt_nr), nr, kt_nr)) %>% 
  fill(year) %>%
  select(reso, nr, name, year, geb_anz_geb,
         geb_ant_geb_unt25, geb_ant_geb_ueb35) %>% 
  gather(geb_anz_geb, geb_ant_geb_unt25, geb_ant_geb_ueb35,
         key = "target", value = "value") %>% 
  mutate_at(c("nr", "year", "value"), list(as.numeric))
  


# 04_pg_civil_status ------------------------------------------------------

path <- "Data/00_Raw_Data/04_pg_civil_status/civil_status.xlsx"
ledig_pg <- read_xlsx(path = path, skip = 2)
ledig_pg <- ledig_pg %>%
  fill(year, nr, name, staendig) %>% 
  select(year, nr, name, staendig, civil_status,
         `20-24 Jahre`, `25-29 Jahre`, `30-34 Jahre`) %>% 
  mutate(year20to24 = as.numeric(`20-24 Jahre`),
         year25to29 = as.numeric(`25-29 Jahre`),
         year30to34 = as.numeric(`30-34 Jahre`)) %>% 
  group_by(year, nr, name, civil_status) %>% 
  summarise(year20to24 = sum(year20to24, na.rm = T),
            year25to29 = sum(year25to29, na.rm = T),
            year30to34 = sum(year30to34, na.rm = T)) %>% 
  group_by(year, nr, name) %>% 
  mutate(ziv_ant_ledig_20bis24 = year20to24/
           year20to24[civil_status == "Zivilstand - Total"],
         ziv_ant_ledig_25bis29 = year25to29/
           year25to29[civil_status == "Zivilstand - Total"],
         ziv_ant_ledig_30bis34 = year30to34/
           year30to34[civil_status == "Zivilstand - Total"]) %>% 
  ungroup() %>% 
  filter(civil_status == "Ledig") %>% 
  select(year, nr, name, ziv_ant_ledig_20bis24,
         ziv_ant_ledig_25bis29, ziv_ant_ledig_30bis34) %>% 
  gather(ziv_ant_ledig_20bis24, ziv_ant_ledig_25bis29,
         ziv_ant_ledig_30bis34,
         key = "target", value = "value") %>% 
  mutate(res_code = stringr::str_sub(name, 1,1)) %>% 
  mutate(reso = ifelse(res_code == "S",
                       "Schweiz",
                       ifelse(res_code == "-",
                              "Kanton",
                              ifelse(res_code == ">",
                                     "Bezirk", "Gemeinde")))) %>% 
  left_join(bfs_nr_kt, by = c("nr" = "kt_kurzel")) %>% 
  mutate(nr = ifelse(is.na(kt_nr), nr, kt_nr)) %>% 
  select(reso, nr, name, year, target, value) %>% 
  mutate_at(c("nr", "year", "value"), list(as.numeric))



# 05_pg_haushaltsgroese ---------------------------------------------------

cat_names <- tibble(target = c("Haushaltsgrösse - Total",
                               "1 Person",
                               "2 Personen",
                               "3 Personen",
                               "4 Personen",
                               "5 Personen",
                               "6 oder mehr Personen"),
       new_target = c("woh_anz_wohnungen",
                      "woh_ant_1pers",
                      "woh_ant_2pers",
                      "woh_ant_3pers",
                      "woh_ant_4pers",
                      "woh_ant_5pers",
                      "woh_ant_mehr6"))
path <- "Data/00_Raw_Data/05_pg_haushaltsgroese/px-x-0102020000_402.xlsx"
haus_pg <- read_xlsx(path = path, skip = 2)
haus_pg <- haus_pg %>% 
  fill(nr, name) %>% 
  gather(`2012`:`2018`, key = "year", value = "value") %>% 
  group_by(nr, name, year) %>% 
  mutate(value = ifelse(target == "Haushaltsgrösse - Total",
                        value,
                        value/value[target == "Haushaltsgrösse - Total"])) %>% 
  ungroup %>% 
  mutate(res_code = stringr::str_sub(name, 1,1)) %>% 
  mutate(reso = ifelse(res_code == "S",
                       "Schweiz",
                       ifelse(res_code == "-",
                              "Kanton",
                              ifelse(res_code == ">",
                                     "Bezirk", "Gemeinde")))) %>% 
  left_join(bfs_nr_kt, by = c("nr" = "kt_kurzel")) %>% 
  mutate(nr = ifelse(is.na(kt_nr), nr, kt_nr)) %>% 
  left_join(cat_names) %>% 
  mutate(target = new_target) %>% 
  select(reso, nr, name, year, target, value) %>% 
  mutate_at(c("nr", "year", "value"), list(as.numeric))



# 06_pg_pendler -----------------------------------------------------------
path <- "Data/00_Raw_Data/06_pg_pendler/Pendler_pro_Gemeinde_Jahr_2000.xlsx"
pendler_pg <- read_xlsx(path = path)
pendler_pg <- pendler_pg %>% 
  mutate(reso = c("Schweiz", rep("Gemeinde", nrow(.)-1)),
         year = 2000) %>% 
  gather(mob_pendler_saldo, mob_ant_pendler_saldo,
         key = "target", value = "value") %>% 
  select(reso, nr, name, year, target, value) %>% 
  mutate_at(c("nr", "year", "value"), list(as.numeric))


# 07_pg_regional_portrait -------------------------------------------------
# path <- "Data/00_Raw_Data/07_pg_regional_portrait/Regionalportrats_2014to2018.xlsx"
# portrait_pg <- read_xlsx(path = path, skip = 4)



# 99_combine all tables ---------------------------------------------------

## Anzahl Geburten pro Kopf und Jahr
ages_pg <- ages_pg %>% 
  filter(target == "bev_anz_einwohner") %>%
  rename(anz_einwohner = value) %>% 
  left_join(birth_pg %>% filter(target == "geb_anz_geb"),
            by = c("reso", "nr", "name", "year")) %>% 
  mutate(target = "geb_ant_geb",
         value = value/anz_einwohner) %>% 
  select(reso, nr, name, year, target, value) %>% 
  rbind(ages_pg)



data <- rbind(ages_pg, birth_pg, ledig_pg, haus_pg, pendler_pg) %>% 
  filter(!is.na(reso),
         !is.na(nr),
         !is.na(target)) %>% 
  filter(!is.na(year),
         !is.na(value))


# predict -----------------------------------------------------------------


# out <- data %>% 
#   group_by(reso, target, nr) %>% 
#   do(fit = lm(value ~ year, data = .))
# 
# fit_coef <- broom::tidy(out, fit)



## old way
# for(i in unique(data$reso)){
#   for(j in unique(data$target[data$reso == i])){
#     for(k in unique(data$nr[data$reso == i & data$target == j])){
#       fit <- lm()
#     }
#   }
# }


# export ------------------------------------------------------------------

saveRDS(data, "Data/02_Unif_Data/unif_data.RDS")
# saveRDS(out, "Data/02_Unif_Data/all_lin_reg.RDS")
# saveRDS(fit_coef, "Data/02_Unif_Data/all_coef.RDS")


## have a look again at warning messages at civil stand !!!!!


