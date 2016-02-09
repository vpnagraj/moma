library(shiny)
library(networkD3)
library(dplyr)
library(readr)
library(lubridate)

# https://github.com/MuseumofModernArt/collection

moma <- read_csv("https://raw.githubusercontent.com/MuseumofModernArt/collection/master/Artworks.csv")
moma$YearAcquired <- year(moma$DateAcquired)

ui <- fluidPage(
    titlePanel(title = "Museum of Modern Art (MOMA) Acquisitions"),
    selectInput("year", "Year", selected = 1945, choices = sort(unique(moma$YearAcquired))),
    simpleNetworkOutput("netplot"),
    HTML("<div align ='center'><em>click and drag a node to center</em><br><em>double click to zoom</em></div>"),
    DT::dataTableOutput("table"),
    HTML("<div align ='left'><br><a href = 'https://github.com/MuseumofModernArt/collection'>data</a> + <a href = 'https://github.com/vpnagraj/moma'>code</a></div>")
)

server <- function(input, output) {
    
    dat <- reactive ({
    
            moma %>%
            # filter a year
            filter(YearAcquired == input$year) %>%
            # get rid of 'various' artist pieces
            filter(!grepl("Various", Artist)) %>%
            select(Artist, Department) %>%
            group_by(Artist, Department) %>%
            summarise(N=n()) %>%
            ungroup() %>%
            arrange(desc(N))
        
    })
    
   output$netplot <-  renderSimpleNetwork({
        
       simpleNetwork(dat(), 
                     Source = "Artist", 
                     Target = "Department",  
                     nodeColour = "gray",
                     textColour = "steelblue", 
                     charge = -75,
                     opacity = .5, 
                     zoom = TRUE)
       
   })
    
   output$table <-  DT::renderDataTable({
       
       dat()
       
   })
}

shinyApp(ui = ui, server = server)