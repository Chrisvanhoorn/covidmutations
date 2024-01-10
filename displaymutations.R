# Shiny program to map covid mutations on a few selected structures
# this program was written to later be complemented with a country-based list of prevalent covid mutations.
# future options: show mutations with different colours based on their prevalence.

# make sure the following packages are installed:

#install.packages("r3dmol")
#install.packages("shiny")
#install.packages("colourpicker")

library(shiny)
library(r3dmol)
library(colourpicker)
ui = shinyUI(fluidPage(
  
  titlePanel("Mutations mapped on structures"
  ),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("structure", h3("SARS COV-2 Structures"), 
                  choices = list("Spike pre-fusion" = "6zwv.cif.pdb",
                                 "Nsp15 endoribonuclease" = "6wlc.pdb",
                                 "Spike post-fusion" = "6xra.pdb"), 
                  multiple = FALSE, selected = 1),
      
      selectInput("countries",h3("Select countries"), 
                  choices = list("US" = 1,"UK" = 2), 
                  multiple = TRUE, selected = 1),
      
      radioButtons('toggle',h2("toggle surface"), choices = list("surface representation" = 1,"cartoon representation" = 2), selected = 2),
      width=3           
      ),
    mainPanel(
      r3dmolOutput(outputId = "r3dmol", height = "700px"),
      textOutput("selected_var"),
      width=8
      
             )
              )
  
))

# Define server logic ----
server <- function(input, output, session) {
  
  output$r3dmol <- renderR3dmol({
    structure <- input$structure
    mutantres <- list(resi = list("501","570","681","716","982","1118"))
    if (input$toggle == 1) {
      return(
        r3dmol(
          cartoonQuality = 10,
          lowerZoomLimit = 50,
          upperZoomLimit = 800,
          backgroundColor = "0xeeeeee"
          ) %>%
          m_add_model(
            data = structure,
            format = "pdb"
            ) %>%
          m_set_style(
            style = list(cartoon = list())
            ) %>%
          m_add_surface(type = "VDW", allsel = TRUE, style = list(color = "lightgrey", isoval = -0.1, opacity = 0.70) 
                        ) %>%
          m_add_surface(type = "VDW", atomsel = mutantres, allsel = FALSE, style = list(color = "red")
                        ) %>%
          m_add_res_labels(
            sel = mutantres,
            style = list(
              font = "Helvetica",
              fontColor = "black",
              fontSize = 20,
              #inFront = TRUE,
              backgroundColor = "white",
              showBackground = FALSE
            )
          ) %>%
          m_zoom_to()
      )}
    else {
      r3dmol(
        cartoonQuality = 10,
        lowerZoomLimit = 50,
        upperZoomLimit = 800,
        backgroundColor = "0xeeeeee"
      ) %>%
        m_add_model(
          data = structure,
          format = "pdb"
        ) %>%
        m_set_style(
          style = list(cartoon = list())
        ) %>%
        m_set_style(sel = mutantres, style = list(cartoon = list(color = "red"))
        ) %>%
        m_add_res_labels(
          sel = mutantres,
          style = list(
            font = "Helvetica",
            fontColor = "black",
            fontSize = 20,
            #inFront = TRUE,
            backgroundColor = "white",
            showBackground = FALSE
          )
        ) %>%
        m_zoom_to()
      
    }
  })
  
output$selected_var <- renderText({ 
    paste(input$structure)})

}

# Run the app ----
shinyApp(ui=ui, server=server)