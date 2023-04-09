



function(input, output, session){
  
  sever(html = disconnected, bg_color = "white", color = "black")
  
  
  output$map_country <- renderMapboxer({
    world_selection <- set_color()
    mapboxer(
             style = "mapbox://styles/jorgehdez1998/ckviz4pms2thz14pi5zgu0iox",
             token = "pk.eyJ1IjoiYWFyb242NjYiLCJhIjoiY2o2M2NmZmJ4MWc0ZDJxbnI3dmZ5OXB2MSJ9.T3trJ5Tu66Kw-w-1ELpzBA"
             )  |> 
      set_view_state(lat = 53, lng = 8, zoom = 3) |>
      add_source(as_mapbox_source(world_selection), SOURCE_ID)  |> 
      add_fill_layer(
        fill_color = c("get", "color"),
        fill_opacity = 0.9,
        source = SOURCE_ID,
        fill_outline_color = "black"
      )
  })
  
 
  
  output$texto_fin <- renderText({
    
    atack <- 
      data |> 
      select(iyear, imonth, country_txt, region_txt,attacktype1_txt, city, latitude, longitude ) |> 
      filter(iyear %in% input$year, region_txt %in% input$region, attacktype1_txt %in% input$crimen) |> 
      count(attacktype1_txt)
    
    ciud_max<-
      data |> 
      select(iyear, imonth, country_txt, region_txt,attacktype1_txt, city, latitude, longitude ) |> 
      filter(iyear %in% input$year, region_txt %in% input$region, attacktype1_txt %in% input$crimen) |> 
      count(city) |> 
      filter(city != "Unknown")
    
    country_max <-
      data |> 
      select(iyear, imonth, country_txt, region_txt,attacktype1_txt, city, latitude, longitude ) |> 
      filter(iyear %in% input$year, region_txt %in% input$region, attacktype1_txt %in% input$crimen) |> 
      count(country_txt)
    
    
    paste0(
      "Dada la selección actual, el ataque/crimen más común registrado fue de '",
      atack$attacktype1_txt[atack$n ==max(atack$n, na.rm = T)],"', mientras que el país con un mayor registro de casos de crimen fue ",
      country_max$country_txt[country_max$n == max(country_max$n)],", con un total de ",
      scales::comma(max(country_max$n)), " casos de crimen. Respectivamente, la ciudad con un mayor número de casos de crimen fue ",
      ciud_max$city[ciud_max$n == max(ciud_max$n, na.rm = T)], ", con un total de ",
      scales::comma(max(ciud_max$n, na.rm = T)), " casos."
    )
    
  })
  
  
  output$top_country <- renderEcharts4r({
    
    data |> 
      filter(iyear %in% input$year, region_txt %in% input$region, attacktype1_txt %in% input$crimen) |> 
      group_by(country_txt) |> 
      summarise(
        total = n()
      ) |> 
      arrange(total) |> 
      mutate(
        color = scales::col_numeric("magma", total)(total)
      ) |> 
      e_charts(country_txt, dispose = F) |> 
      e_bar(total) |> 
      e_flip_coords() |> 
      e_add("itemStyle", color) |> 
      e_legend(F) |> 
      e_tooltip(trigger = "axis") |> 
      e_theme("auritus") |> 
      e_x_axis(show = F) |> 
      e_labels(show = T, position = "right")
    
  })
  
  
  output$pal <- renderEcharts4r({
    
    x <-
      countries_polygon |> 
      left_join(
        data |> 
          select(iyear, imonth, country_txt, region_txt,attacktype1_txt, city, latitude, longitude ) |> 
          filter( region_txt %in% input$region, attacktype1_txt %in% input$crimen) |> 
          group_by(country_txt) |> 
          summarise(
            total = n()
          ), by = c("COUNTRY" = "country_txt")
      ) |> 
      mutate(color = scales::col_numeric("magma", total,na.color = "white" )(total)) 
    
    x |> 
      mutate(val = "x") |> 
      group_by(color) |> 
      e_charts(val, dispose = F) |> 
      e_bar(total, stack = "x", name = "Valor") |> 
      e_add("itemStyle", color) |> 
      e_legend(FALSE) |> 
      e_flip_coords() |> 
      e_theme("auritus") |> 
      e_y_axis(show = F) |> 
      e_x_axis(show = F, max = sum(x$total,na.rm = T))
    
    
  })
  
  output$trend <- renderEcharts4r({
    
    data |> 
      select(iyear, imonth, country_txt, region_txt,attacktype1_txt, city, latitude, longitude ) |> 
      mutate(date =lubridate::make_date(year = iyear, month = imonth, day = 1)) |> 
      filter(iyear %in% input$year, region_txt %in% input$region, attacktype1_txt %in% input$crimen) |> 
      group_by(date) |> 
      summarise(
        Total = n()
      ) |> 
      e_charts(date, dispose = F) |> 
      e_line(Total, symbol = "none") |> 
      e_theme("auritus") |> 
      e_tooltip(trigger = "axis") |> 
      e_y_axis(show = F) |> 
      e_color(color = "#F1605DFF")
    
    
  })
  
  output$filtros <- renderUI({
    
   if (input$trend_show %in% T){
     column(width = 12,echarts4rOutput("top_country", height =600, width = 600),
            echarts4rOutput("trend", height =250, width = 500)
     )
   }else{
     column(width = 12,echarts4rOutput("top_country", height =850, width = 600)
     )
   }
    
  })
  
  observeEvent(list(input$region, input$crimen, input$year), {
    world_selection <- set_color(region =input$region, year = input$year, crime = input$crimen)
    mapboxer_proxy("map_country") |> 
      set_data(world_selection, SOURCE_ID) |> 
      update_mapboxer()
  })
  
  observeEvent(input$region,{
    
    val <- set_color(region = input$region)
    
    mapboxer_proxy("map_country") |> 
      fit_bounds(bounds = sf::st_bbox(countries_polygon$geometry[countries_polygon$COUNTRY %in% unique(data$country_txt[data$region_txt %in% input$region])])) |> 
      update_mapboxer()
    
  })
  
  
  
  
  
  
}