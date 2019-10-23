## Read in files from Bundesamt website

library(tidyverse)
library(magrittr)
library(readxl)
library(stringr)
library(leaflet)



# import age tables -------------------------------------------------------

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


# combine age table with sp Polygon ---------------------------------------

link_tab <- cant_y %>% 
  select(cant_id, cant_name) %>% 
  unique()


ch <- readRDS(file = "Data/gadm-swiss-maps/gadm36_CHE_1_sp.rds")
link_tab <- tibble(NAME_1 = ch$NAME_1,
       cant_id = c(19, 15, 16, 13, 12, 2, 10, 25,
                   8, 18, 26, 3, 24, 7, 6, 17, 14,
                   5, 11, 20, 21, 4, 23, 22, 9, 1)) %>% 
  full_join(link_tab)

# write.csv(link_tab[,c("NAME_1", "cant_name")], file = "Data/link_tab.csv")


cant_y <- cant_y %>% 
  select(-cant_id)




# next steps --------------------------------------------------------------

# make wide cant_y with year as columns
# add cant_y to ch as attributs
# plot value of one year to leaflet
# predict trend



# combine with leaflet ----------------------------------------------------

ch$new <- link_tab %>% 
  left_join(cant_y) %>% 
  filter(year == 2018) %$% 
  pop_y_pc

pal <- colorNumeric("viridis", NULL)
leaflet(ch) %>%
  # addTiles() %>%
  addPolygons(color = "#444444", opacity = 1, stroke = T, weight = 1, smoothFactor = 2,
              fillOpacity = 0.7,
              fillColor = ~pal(new), 
              label = ~paste0(NAME_1, ": ", formatC(new)),
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = TRUE)) %>% 
  addLegend(pal = pal, values = ~new, opacity = 1, title = "Altersgruppe 20-39 [%]")



# predict generation y ----------------------------------------------------


cant_y %>% 
  filter(cant_name == "Zürich") %>% 
  filter(year > 1980) %$% 
  plot(pop_y_pc ~ year, xlim = c(1990, 2030))



x <- cant_y %>% 
  filter(cant_name == "Bern") %>% 
  filter(year > 2009)

# x_ts <- ts(x$pop_y_pc, start = 2010, frequency = 1)
# fit <- HoltWinters(x_ts, beta = FALSE, gamma = FALSE)
# 
# lines(predict(fit, n.ahead=2), col="blue", lty=3)
# 
# 
# ## loess
# 
# fit <- loess(x$pop_y_pc ~ x$year)
# lines(x$year, fit$fitted)
# predict(fit, newdata = 2019:2030)

fit <- lm(x$pop_y_pc ~ x$year)

mycant <- unique(cant_y$cant_name)
out <- vector("list", length = length(mycant))
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
}


cant_futur <- do.call("rbind", out)
cant_y <- rbind(cant_y, cant_futur)



# shiny -------------------------------------------------------------------


sh_tiles <- F
sh_res <- "cant"
sh_var <- "age20_39"
sh_year <- 2030
sh_cant <- "Zürich"


ch$new <- link_tab %>% 
  left_join(cant_y) %>% 
  filter(year == sh_year) %$% 
  pop_y_pc

pal <- colorNumeric("viridis", NULL)
leaflet(ch) %>%
  # addTiles() %>%
  addPolygons(color = "#444444", opacity = 1, stroke = T, weight = 1, smoothFactor = 2,
              fillOpacity = 0.7,
              fillColor = ~pal(new), 
              label = ~paste0(NAME_1, ": ", formatC(new)),
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = TRUE)) %>% 
  addLegend(pal = pal, values = ~new, opacity = 1, title = "Altersgruppe 20-39 [%]")



cant_y %>% 
  filter(cant_name == sh_cant) %>% 
  filter(year > 2009) %$% 
  plot(pop_y_pc ~ year, xlim = c(2009, 2030), xlab = "Jahr", ylab = "Altersgruppe 20-39 [%]")