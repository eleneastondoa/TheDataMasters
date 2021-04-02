Sys.setlocale('LC_ALL', locale = 'Spanish')
library(shiny)
library(shinydashboard)
# Create the UI using the header, sidebar, and body
ui <- dashboardPage(
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem(text = 'España', tabName = 'prueba1'),
      menuItem(text = '¿Estará bien?', tabName = 'prueba2')
    )
  ),
  body = dashboardBody(),
  header = dashboardHeader()
)

server <- function(input, output) {}

shinyApp(ui, server)



