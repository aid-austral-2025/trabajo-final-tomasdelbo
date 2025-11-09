##### app.R ####
suppressPackageStartupMessages({
if (!require(shinyjs)) install.packages("shinyjs", repos = "https://cloud.r-project.org"); library(shinyjs)
if (!require(shinydashboard)) install.packages("shinydashboard", repos = "https://cloud.r-project.org"); library(shinydashboard)
if (!require(shinyWidgets)) install.packages("shinyWidgets", repos = "https://cloud.r-project.org"); library(shinyWidgets)
if (!require(DT)) install.packages("DT", repos = "https://cloud.r-project.org"); library(DT)
})
  
#interfaz del usuario con dashboard
interfaz <-  dashboardPage(
  dashboardHeader(title = "Carga de horas - I&D"),
  #Defino panel lateral con pestañas
  dashboardSidebar(
    sidebarMenu(
      menuItem("Carga de horas", tabName = "carga", icon = icon("clock")), #Carga de horas
      menuItem("Tablero", tabName = "tablero", icon = icon("chart-pie")), #Visualizacion
      menuItem("Maestros", tabName = "maestros", icon = icon("table")) #Control y tablas
    )
  ),
  #Defino layout de cada pestaña
  dashboardBody(
    tabItem(tabName = "carga"),
    tabItem(tabName = "tablero"),
    tabItem(tabName = "maestros")

  )
  

)


#funcion server
servidor <- function(input, output){
  

  
}

#publicacion
shinyApp(ui = interfaz, server = servidor)
