#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(googlesheets)

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Old Faithful Geyser Data"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "award",
        "Category:",
        c(
          "Actor in a Leading Role", "Actress in a Leading Role", "Actor in a Supporting Role",
          "Actress in a Supporting Role", "Animated Feature Film", "Cinematography", "Costume Design",
          "Directing", "Documentary (Feature)", "Documentary (Short)", "Film Editing",
          "Foreign Language Film", "Makeup & Hairstyling", "Music (Original Score)",
          "Music (Original Song)", "Production Design", "Short Film (Animated)", "Short Film (Live Action)",
          "Sound Editing", "Sound Mixing", "Visual Effects", "Writing (Adapted Screenplay)",
          "Writing (Original Screenplay)", "Best Picture"
        )
      ),
      conditionalPanel(
        condition = "input.award == 'Actor in a Leading Role'",
        radioButtons(
          "lead_actor",
          "And the winner is:",
          c(
            "Not Announced" = NA,
            "Casey Affleck, Manchester by the Sea",
            "Andrew Garfield, Hacksaw Ridge",
            "Ryan Gosling, La La Land",
            "Viggo Mortensen, Captain Fantastic",
            "Denzel Washington, Fences"
          )
        )
      ),
      conditionalPanel(
        condition = "input.award== 'Actress in a Leading Role'",
        radioButtons(
          "lead_actress",
          "And the winner is:",
          c(
            "Not Announced" = NA,
            "Isabelle Huppert, Elle",
            "Ruth Negga, Loving",
            "Natalie Portman, Jackie",
            "Emma Stone, La La Land",
            "Meryl Streep, Florence Foster Jenkins"
          )
        )
      ),
      conditionalPanel(
        condition = "input.award == 'Actor in a Supporting Role'",
        radioButtons(
          "support_actor",
          "And the winner is:",
          c(
            "Not Announced" = NA,
            "Mahershala Ali, Moonlight",
            "Jeff Bridges, Hell or High Water",
            "Lucas Hedges, Manchester by the Sea",
            "Dev Patel, Lion",
            "Michael Shannon, Nocturl Animals"
          )
        )
      ),
      conditionalPanel(
        condition = "input.award == 'Actress in a Supporting Role'",
        radioButtons(
          "support_actress",
          "And the winner is:",
          c(
            "Not Announced"= NA,
            "Viola Davis, Fences",
            "Naomie Harris, Moonlight",
            "Nicole Kidman, Lion",
            "Octavia Spencer, Hidden Figures",
            "Michelle Williams, Manchester by the Sea"
          )
        )
      ),
      actionButton("do", "Update Results")
      ),
    # Show a plot of the generated distribution
    mainPanel(
      tableOutput("table")
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  df_votes <- gs_key("1Wprct0LtNbqVlSjHcC7KeuHqkP5wNklc4IGLUdpPWJg") %>%
    gs_read() %>% select(1:6)
  
  
  awards <- c("lead_actor", "lead_actress", "support_actor", "support_actress"
              # , 
              # "animated_feature", "cinematography", "costume", "directing", 
              # "doc_feature", "doc_short", "editing", "foreign_feature", 
              # "makeup", "music_score", "music_song", "design", "animated_short", 
              # "live_short", "sound_edit", "sound_mix", "effects", 
              # "writing_adapt", "writing_orig", "picture"
              )
  
  names(df_votes) <- c("Timestamp", "team", awards)

  df_results <- df_votes %>%
    select(-Timestamp) %>%
    mutate_at(
      names(.)[-1],
      .funs = function(x) {
        rep(0, nrow(.))
      }
    )
  
  counting_wins <- function(award) {
    # Takes the award name, returns boolean vector with 
    # Win or loss for each team
    result <- data.frame()
    result[[award]]<-grepl(input[[award]], df_votes[[award]])
    result
  }
  
  
  observeEvent(input$do, 
               {
                 df_results[,2:5] <- map_dfr(awards, counting_wins)
               })


  output$table <- renderTable({
    data.frame(Team = df_results[1],
               Score = rowSums(df_results[,-1]))
  })
}

# Run the application
shinyApp(ui = ui, server = server)