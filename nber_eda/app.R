library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(showtext)
library(glue)
library(ggtext)

theme_set(theme_light())

font_add_google(family = "patua-one", "Patua One")
showtext_auto()


nber_all_data <- readRDS("nber_all_data.RDS")

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "NBER Eda"),
  dashboardSidebar(
    selectInput("v_program_name", "Program Name",
                choices = nber_all_data %>% distinct(program_desc))
  ),
  dashboardBody(
    fluidRow(box(plotOutput("author_hist"), width = 12))
  ),
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  

    output$author_hist <- renderPlot({
      
      
      top_authors <- nber_all_data %>% 
        filter(program_desc == input$v_program_name) %>% 
        count(name, sort = TRUE) %>% 
        top_n(20, wt = n) %>%
        mutate(name = fct_reorder(name, n)) 
      
      
      this_title = reactive(glue("<br><span style='color: #0000FF'>{ top_authors %>% slice_max(n) %>% pull(name) }</span> wrote the most papers in the <span style='color: #0000FF'>{ input$v_program_name }</span> category"))
      
      top_authors %>% 
        ggplot(aes(n, name)) +
        geom_col()+
        theme(
          axis.ticks = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.title = element_textbox_simple(
            family = "patua-one",
            size = 30,
            lineheight = 1,
            color = "#000000",
            margin = margin(b = 10)),
          panel.border = element_blank(),
          axis.line.y = element_line(color = "black", size = 1),
          axis.line.x = element_line(color = "black", size = 1)
        ) +
        labs(
          x = "# of papers",
          y = "",
          title = this_title()
        ) +
        scale_x_continuous(
          expand = expansion(add = 0, mult = c(0, 0.1))
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
