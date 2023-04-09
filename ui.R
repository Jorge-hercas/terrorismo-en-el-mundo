



fluidPage(
  tags$head(HTML("<title>Terrorismo en el mundo</title>")),
  add_busy_bar(color = "#FF0000"),
  useSever(),
  gfonts::use_pkg_gfont("oswald"),
   column(width = 4, 
          column(width = 12,
                 titlePanel("Filtros"),
                 div(style="display: inline-block;vertical-align:top; width: 180px;",
                     pickerInput("region", "Region", choices =unique(na.omit(data$region_txt)),width = 180, options = opts, selected = "Western Europe")),
                 div(style="display: inline-block;vertical-align:top; width: 180px;",
                     pickerInput("crimen", "crimen", choices =unique(na.omit(data$attacktype1_txt)),selected = unique(na.omit(data$attacktype1_txt)),multiple = T, width = 180, options = opts )),
                 div(style="display: inline-block;vertical-align:top; width:80px;",
                     pickerInput("year", "Año", choices = unique(data$iyear), selected = unique(data$iyear), multiple = T, options = opts) ),
                 div(style="display: inline-block;vertical-align:top; width: 220px;",
                     checkboxInput("trend_show", label = "¿Mostrar tendencia?", value = F) )
                 ),
          uiOutput("filtros")),
   column(width = 8,
          column(align = "center", width = 12, titlePanel(span("Terrorismo en el mundo: Acerca de las principales causas de crimen por país", style = "color: gray; font-size: 42px;"))),
          column(width = 12,mapboxerOutput("map_country", height = 700, width = "100%"),
                 echarts4rOutput("pal", height = 100, width = 1000)),
          span(textOutput("texto_fin"), style = "color: gray; font-size: 12px;"),br(),
          span("Creador: Jorge Valente Hernández Castelán. mira otras aplicaciones en ",a("R-conomics",href="https://r-conomics.netlify.app/course/"), style = "color: gray; font-size: 12px;")
          )
)





