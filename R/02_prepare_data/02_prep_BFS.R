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




# 02_pg_age20to39 ---------------------------------------------------------

ages_pg <- read_xlsx(path = "Data/00_Raw_Data/02_pg_age20to39/px-x-0102010000_101.xlsx", skip = 2)
age20to39 <- ages_pg[, which(colnames(ages_pg) == "20 Jahre"):
                       which(colnames(ages_pg) == "39 Jahre")] %>% 
  apply(1, sum, na.rm = T)
ages_pg <- ages_pg %>% 
  rename(total_pop = `Alter - Total`) %>% 
  mutate(age20to39 = age20to39,
         share_20to39 = age20to39/total_pop) %>% 
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
  select(reso, nr, name, year, total_pop, age20to39, share_20to39) %>% 
  gather(total_pop, age20to39, share_20to39, key = "target", value = "value")





# 03_pg_birth_by_age ------------------------------------------------------

birth_pg <- read_xlsx(path = "Data/00_Raw_Data/03_pg_birth_by_age/px-x-0102020204_102.xlsx", skip = 2)
birth_pg %>% 
  rename(total_geburten = `Altersklasse der Mutter - Total`,
         unter25 = `Unter 25 Jahren`) %>% 
  mutate(ueber35 = `30-34 Jahre` + `35-39 Jahre` + `40 Jahre und mehr`,
         anteil_unter25 = unter25/total_geburten,
         anteil_ueber35 = ueber35/total_geburten) %>% 
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
  select(reso, nr, name, year, total_geburten, anteil_unter25, anteil_ueber35) %>% 
  gather(total_geburten, anteil_unter25, anteil_ueber35, key = "target", value = "value")
  


## general procedere:
## 1) read data
## 2) rename variables (necessary ones)
## 3) make new variable (necessary ones)
## 4) add "reso" variable
## 5) join with bfs_nr_kt to add kanton nr
## 6) fill year
## 7) select necessary variables
## 8) gather to long table



# 04_pg_civil_status ------------------------------------------------------

ledig_pg <- read_xlsx(path = "Data/00_Raw_Data/04_pg_civil_status/civil_status.xlsx", skip = 2)
ledig_pg <- ledig_pg %>%
  fill(year, nr, name, staendig) %>% 
  select(year, nr, name, staendig, civil_status, `20-24 Jahre`, `25-29 Jahre`, `30-34 Jahre`) %>% 
  mutate(year20to24 = as.numeric(`20-24 Jahre`),
         year25to29 = as.numeric(`25-29 Jahre`),
         year30to34 = as.numeric(`30-34 Jahre`)) %>% 
  group_by(year, nr, name, civil_status) %>% 
  summarise(year20to24 = sum(year20to24, na.rm = T),
            year25to29 = sum(year25to29, na.rm = T),
            year30to34 = sum(year30to34, na.rm = T)) %>% 
  group_by(year, nr, name) %>% 
  mutate(year20to24_share = year20to24/year20to24[civil_status == "Zivilstand - Total"],
         year25to29_share = year25to29/year25to29[civil_status == "Zivilstand - Total"],
         year30to34_share = year30to34/year30to34[civil_status == "Zivilstand - Total"]) %>% 
  ungroup() %>% 
  filter(civil_status == "Ledig") %>% 
  select(year, nr, name, year20to24_share, year25to29_share, year30to34_share) %>% 
  gather(year20to24_share, year25to29_share, year30to34_share, key = "target", value = "value") %>% 
  mutate(res_code = stringr::str_sub(name, 1,1)) %>% 
  mutate(reso = ifelse(res_code == "S",
                       "Schweiz",
                       ifelse(res_code == "-",
                              "Kanton",
                              ifelse(res_code == ">",
                                     "Bezirk", "Gemeinde")))) %>% 
  left_join(bfs_nr_kt, by = c("nr" = "kt_kurzel")) %>% 
  mutate(nr = ifelse(is.na(kt_nr), nr, kt_nr)) %>% 
  select(reso, nr, name, year, target, value)



# 05_pg_haushaltsgroese ---------------------------------------------------

haus_pg <- read_xlsx(path = "Data/00_Raw_Data/05_pg_haushaltsgroese/px-x-0102020000_402.xlsx", skip = 2)
haus_pg %>% 
  fill(nr, name) %>% 
  gather(`2012`:`2018`, key = "year", value = "value") %>% 
  group_by(nr, name, year) %>% 
  mutate(value = value/value[target == "Haushaltsgrösse - Total"]) %>% 
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
  select(reso, nr, name, year, target, value)



# 06_pg_pendler -----------------------------------------------------------

## general procedere:
## 1) read data
## 2) rename variables (necessary ones)
## 3) make new variable (necessary ones)
## 4) add "reso" variable
## 5) join with bfs_nr_kt to add kanton nr
## 6) fill year
## 7) select necessary variables
## 8) gather to long table


pendler_pg <- read_xlsx(path = "Data/00_Raw_Data/06_pg_pendler/Pendler_pro_Gemeinde_Jahr_2000.xlsx")
pendler_pg %>% 
  mutate(reso = c("Schweiz", rep("Gemeinde", nrow(.)-1))) %>% 
  gather(pendlersaldo_person, pendlersaldo_percent_worker,
         key = "target", value = "value")


# 07_pg_regional_portrait -------------------------------------------------

portrait_pg <- read_xlsx(path = "Data/00_Raw_Data/07_pg_regional_portrait/Regionalportrats_2014to2018.xlsx",
                         skip = 4)
portrait_pg

