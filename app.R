library(shiny)
library(readr)
library(tidyverse)
library(ggplot2)
library(gganimate)
library(gapminder)
library(scales)

#importing world development indicators
WDIData <- read_csv("data/WDIData.csv")

#wrangling data
WDIDataTest <- WDIData |>
  pivot_longer(
    cols = -c(
      "Country Name",
      "Country Code",
      "Indicator Name",
      "Indicator Code"
    ),
    names_to = "year",
    names_transform = parse_number,
    values_to = "values"
  ) |>
  
  #snake_case and factoring data
  rename(indicator_id = "Indicator Code",
         country_name = "Country Name") |>
  mutate(country_name = as.factor(country_name),
         indicator_id = as.factor(indicator_id)) |>
  select(country_name, indicator_id, year, values) |>
  
  #filtering for gdp per capita
  filter(indicator_id == 'NY.GDP.PCAP.CD') |>
  
  
  #converting year column to get it ready for gganimate
  mutate(year = as.integer(year)) |>
  filter(
    country_name %in% c(
      "Canada",
      "France",
      "Germany",
      "Italy",
      "Japan",
      "United States",
      "United Kingdom"
    )
  ) |>
  rename(value = "values") |>
  drop_na(year) |>
  mutate(
    value = replace_na(value, 0)
  ) |>
  group_by(year) |>
  top_n(n = 7, wt = value) |>
  mutate(
    rank = rank(-value)
  ) |>
  ungroup()

rerender <- TRUE

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  # App title ----
  titlePanel("Elegant Starmie Final Project"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    sidebarPanel(
      h3("Data Input"),
      fileInput("file", h5("Choose your data file")),
      h3("Customization"),
      textInput("title_input", h5("Chart Title"), placeholder = "Input your chart title"),
      textInput("subtitle_input", h5("Chart Subtitle"), placeholder = "Input your chart subtitle"),
      textInput("xaxis_input", h5("X-axis Label"), placeholder = "Input your x-axis label"),
      textInput("yaxis_input", h5("Y-axis Label"), placeholder = "Input your y-axis label"),
      selectInput(
        "color_select",
        h5("Choose your color palette"),
        choices = list(
          "Default" = "default",
          "Viridis" = "viridis",
          "Brewer" = "brewer"
        )
      ),
      radioButtons(
        "show_legend_select",
        h5("Show Legend?"),
        choices = list("Yes" = "yes",
                       "No" = "no")
      )
    ),
    mainPanel(
      h3("Bar Chart Preview"),
      plotOutput(outputId = "staticGraph"),
      actionButton(inputId = "showAnimated", "Refresh animated chart"),
      imageOutput(outputId = "animatedGraph")
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  
  output$staticGraph <- renderPlot({
    show_l <- switch(input$show_legend_select,
                     yes = "legend",
                     no = "none",
                     "legend")
    
    fill_palette <- switch(input$color_select,
                            default = scale_fill_hue(name = "Country", guide = show_l), 
                            viridis = scale_fill_viridis_d(name = "Country", guide = show_l),
                            brewer = scale_fill_brewer(name = "Country", guide = show_l),
                            scale_fill_hue(name = "Country", guide = show_l))
    
    color_palette <- switch(input$color_select,
                            default = scale_color_hue(name = "Country"), 
                            viridis = scale_color_viridis_d(name = "Country"),
                            brewer = scale_color_brewer(name = "Country"),
                            scale_color_hue(name = "Country"))
    
    WDIDataTest |>
      filter(year == 2022) |>
      ggplot(aes(group = country_name,
                 fill = as.factor(country_name)), height = 960) +
      geom_col(aes(x = rank, y = value),
               position = position_dodge(),
               alpha = 0.9) +
      fill_palette +
      geom_text(
        aes(
          x = rank,
          y = 0,
          label = paste(country_name, " "),
          col = as.factor(country_name)
        ),
        vjust = 0.2,
        hjust = 1,
        show.legend = FALSE
      ) +
      color_palette +
      geom_text(
        x = 0,
        y = 0,
        aes(label = as.character(year)),
        size = 15,
        alpha = 0.8,
        col = "grey50",
        vjust = 0.2,
        hjust = 1
      ) +
      coord_flip(clip = "off", expand = FALSE) +
      scale_y_continuous(labels = scales::label_dollar(scale_cut = scales::cut_short_scale())) +
      scale_x_reverse()  +
      labs(
        title = input$title_input,
        subtitle = input$subtitle_input,
        y = input$xaxis_input,
        x = input$yaxis_input
      ) +
      theme_minimal() +
      theme(
        axis.text.y  = element_blank(),
        legend.position = "bottom",
        plot.margin = margin(2, 2, 2, 4, "cm"),
        plot.title = element_text(
          size = 25,
          hjust = 0.5,
          face = "bold"
        ),
        panel.grid.major.x = element_line(size = .1, color = "grey"),
        panel.grid.minor.x = element_line(size = .1, color = "grey"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
      )
    
  })
  
  observeEvent(input$showAnimated, {
    rerender <- TRUE
  })
  
  observeEvent(input$showAnimated, {
    if (rerender) {
      rerender <- FALSE
      output$animatedGraph <- renderImage({
        show_l <- switch(input$show_legend_select,
                         yes = "legend",
                         no = "none",
                         "legend")
        
        fill_palette <- switch(
          input$color_select,
          default = scale_fill_hue(name = "Country", guide = show_l),
          viridis = scale_fill_viridis_d(name = "Country", guide = show_l),
          brewer = scale_fill_brewer(name = "Country", guide = show_l),
          scale_fill_hue(name = "Country", guide = show_l)
        )
        
        color_palette <- switch(
          input$color_select,
          default = scale_color_hue(name = "Country"),
          viridis = scale_color_viridis_d(name = "Country"),
          brewer = scale_color_brewer(name = "Country"),
          scale_color_hue(name = "Country")
        )
        
        static_barchart <- WDIDataTest |>
          ggplot(aes(group = country_name,
                     fill = as.factor(country_name)), height = 960) +
          geom_col(aes(x = rank, y = value),
                   position = position_dodge(),
                   alpha = 0.9) +
          fill_palette +
          geom_text(
            aes(
              x = rank,
              y = 0,
              label = paste(country_name, " "),
              col = as.factor(country_name)
            ),
            vjust = 0.2,
            hjust = 1,
            show.legend = FALSE
          ) +
          color_palette +
          geom_text(
            x = 0,
            y = 0,
            aes(label = as.character(year)),
            size = 15,
            alpha = 0.8,
            col = "grey50",
            vjust = 0.2,
            hjust = 1
          ) +
          coord_flip(clip = "off", expand = FALSE) +
          scale_y_continuous(labels = scales::label_dollar(scale_cut = scales::cut_short_scale())) +
          scale_x_reverse()  +
          labs(
            title = input$title_input,
            subtitle = input$subtitle_input,
            y = input$xaxis_input,
            x = input$yaxis_input
          ) +
          theme_minimal() +
          theme(
            axis.text.y  = element_blank(),
            legend.position = "bottom",
            plot.margin = margin(2, 2, 2, 4, "cm"),
            plot.title = element_text(
              size = 25,
              hjust = 0.5,
              face = "bold"
            ),
            panel.grid.major.x = element_line(size = .1, color = "grey"),
            panel.grid.minor.x = element_line(size = .1, color = "grey"),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
          )
        
        animated_barchart <- static_barchart +
          transition_states(year,
                            transition_length = 4,
                            state_length = 0.5) +
          view_follow(fixed_x = TRUE)
        
        anim_save("www/gganim.gif",
                  animate(
                    animated_barchart,
                    200,
                    fps = 10,
                    width = 600,
                    height = 500,
                  ))
        list(src = "www/gganim.gif")
      }, deleteFile = FALSE)
    }
  })
  
  
 
  

}

# Create Shiny app ----
shinyApp(ui = ui, server = server)