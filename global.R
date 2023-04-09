

library(shinyWidgets)
library(shiny)
library(shinybusy)
library(bslib)
library(mapboxer)
library(echarts4r)
library(dplyr, warn.conflicts = T)
library(dtplyr)
library(sever)
library(shinybusy)

data <- vroom::vroom(paste0("data/",list.files("data")[1]))
countries_polygon <- sf::read_sf("shapefile/World_Countries/World_Countries.shp")


SOURCE_ID <- "world"


set_color <- function(region = "Western Europe", year = unique(data$iyear), crime = unique(data$attacktype1_txt)){
  
  tryCatch({
    countries_polygon |> 
      left_join(
        data |> 
          select(iyear, imonth, country_txt, region_txt,attacktype1_txt, city, latitude, longitude ) |> 
          filter(iyear %in% year, region_txt %in% region, attacktype1_txt %in% crime) |> 
          group_by(country_txt) |> 
          summarise(
            total = n()
          ), by = c("COUNTRY" = "country_txt")
      ) |> 
      mutate(color = scales::col_numeric("magma", total,na.color = "white" )(total))
  }, error = function(e){
    
    countries_polygon |> 
      left_join(
        data |> 
          select(iyear, imonth, country_txt, region_txt,attacktype1_txt, city, latitude, longitude ) |> 
          filter(iyear %in% year, region_txt %in% region, attacktype1_txt %in% crime) |> 
          group_by(country_txt) |> 
          summarise(
            total = n()
          ), by = c("COUNTRY" = "country_txt")
      ) |> 
      mutate(color = "white")
    
  })
}


disconnected <- tagList(
  h1("Espera..."),
  p("¡Parece que te has desconectado del servidor!"),
  reload_button("Recargar página", class = "warning")
)

texto <- list(fontFamily = "Roboto Condensed", 
              color = "gray",
              fontSize = 10)

opts <- list(
  `actions-box` = TRUE,
  `live-search`=TRUE)

