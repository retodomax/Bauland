###########################################################################
## Project:  Bauland
## Content:  Prepare Data for Shiny
## Date:     2019-10-08 10:11:12
## Author:   Reto Zihlmann <retozihlmann@outlook.com>
###########################################################################


# packages ----------------------------------------------------------------

library(magrittr, warn.conflicts = F);                   library(tidyverse)
Sys.setlocale("LC_ALL", "English");                   Sys.setenv(LANG="EN")
library(readxl)
library(stringr)
library(leaflet)



# import data -------------------------------------------------------------


myfiles <- list.files("Data/canton_age", full.names = T)
mytib <- vector("list", length = length(myfiles))

i <- 1
for (i in 1:length(myfiles)) {
  myfile <- read_xlsx(myfiles[i])
  mytib[[i]] <- tibble(cant_id = unlist(unname(myfile[6:31,1])),
                       cant_name = unlist(unname(myfile[6:31,2])),
                       year = str_sub(names(myfile)[1], -4),
                       pop_y_nr = unlist(unname(myfile[6:31,3])),
                       pop_y_pc = unlist(unname(myfile[6:31,4]))
  )
}

cant_y <- do.call("rbind", mytib)
cant_y$cant_id <- as.integer(cant_y$cant_id)
cant_y$year <- as.integer(cant_y$year)
cant_y$pop_y_nr <- as.numeric(cant_y$pop_y_nr)
cant_y$pop_y_pc <- as.numeric(cant_y$pop_y_pc)


# link with CH identifier -------------------------------------------------

ch <- readRDS(file = "Data/gadm-swiss-maps/gadm36_CHE_1_sp.rds")
cant_y <- tibble(NAME_1 = ch$NAME_1,
       cant_id = c(19, 15, 16, 13, 12, 2, 10, 25,
                   8, 18, 26, 3, 24, 7, 6, 17, 14,
                   5, 11, 20, 21, 4, 23, 22, 9, 1)) %>% 
  left_join(cant_y)



# predict future years ----------------------------------------------------

mycant <- unique(cant_y$cant_name)
out <- vector("list", length = length(mycant))
out_mod <- vector("list", length = length(mycant))
for(i in 1:length(mycant)){
  cant_y_temp <- cant_y %>% 
    filter(cant_name == mycant[i]) %>% 
    filter(year > 2009)
  fit_nr <- lm(pop_y_nr ~ year, data = cant_y_temp)
  fit_pc <- lm(pop_y_pc ~ year, data = cant_y_temp)
  pred_year <- 2019:2030
  out[[i]] <- tibble(cant_name = mycant[i],
                     year = pred_year,
                     pop_y_nr = predict(fit_nr, newdata = list(year = pred_year)),
                     pop_y_pc = predict(fit_pc, newdata = list(year = pred_year)))
  out_mod[[i]] <- fit_pc
}
names(out_mod) <- unique(cant_y$NAME_1)

cant_futur <- do.call("rbind", out)
cant_futur <- cant_y %>%
  select(NAME_1, cant_id, cant_name) %>% 
  unique() %>% 
  left_join(cant_futur)

cant_y <- rbind(cant_y, cant_futur) %>% 
  arrange(NAME_1, year)





# export data -------------------------------------------------------------

save(list = c("cant_y","out_mod"), file = "Data/canton_20to39.RData")


# plot trend --------------------------------------------------------------

canton_name <- "Zürich"
plotdata <- cant_y %>% 
  filter(cant_name == canton_name) %>% 
  filter(year > 2008) %>% 
  arrange(year)

plot(pop_y_pc ~ year, data = plotdata, type = "n",
     xlab = "Jahr", ylab = "Altersgruppe 20-39 [%]")
points(plotdata$year[plotdata$year < 2019], plotdata$pop_y_pc[plotdata$year < 2019], pch = 16)
abline(out_mod[[canton_name]])

