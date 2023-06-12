
#install.packages("shinyjs")

library(shinyjs)

model <- 
  readRDS("furniture_finalized_model.rds")

TABLES <- 
  readRDS("furniture_tables.rds")

data_table <- TABLES[[1]]
corr_table <- TABLES[[2]]

model$pre$mold$predictors %>% 
  colnames() %>% 
  as_tibble()

myTitlePanel <- function (title, windowTitle = title, color = "coral") {
  css <- paste(paste0("background-color:", color),
               "padding-left: 15px",
               "margin-left: -15px",
               "margin-right: -15px",
               sep = ";")
  tagList(tags$head(tags$title(windowTitle)), 
          h1(title, style = css))
}

# model_rf <- 
#   readRDS("finalized_model_rent_ALT.rds")
# 
# model_rf$pre$mold$predictors %>% 
#   colnames() %>% 
#   as_tibble()

# User Interface ----

ui <- 
  dashboardPage(skin = "blue", # Page
                dashboardHeader(title = "IKEA Furniture Price Prediction App",
                                titleWidth = 320), # Header
                dashboardSidebar( # Sidebar
                  sidebarMenu(
                    menuItem(
                      "Cleaned Raw Data",
                      tabName = "Data_tab",
                      icon = icon("database")
                      ),
                    menuItem(
                      "Correlation Matrix",
                      tabName = "Corr_Matrix_tab",
                      icon = icon("list-alt")
                      ),
                    menuItem(
                      "Furniture Price Prediction",
                      tabName = "furniture_tab",
                      icon = icon("question-circle")
                      )
                    )
                  ), 
                dashboardBody(
                  tabItems(
                    tabItem(
                      tabName = "furniture_tab",
                      # Box containing the prediction results
                      box(valueBoxOutput("price_prediction"), 
                          width = 320
                          ),
                      box(valueBoxOutput("box1"), width = 640
                          ),
                      
                      # Box containing the inputs; 
                      # (a) selector (for categorical inputs); 
                      # (b) slider (for numerical inputs)
                      # box(selectInput("gender",
                      #                  label = "Gender",
                      #                  choices = c("Male", "Female", "Other")
                      #                  )
                      #      ),
                      box(selectInput("name",
                                      label = "Category of furniture",
                                      choices = c("BESTÅ",
                                                  "EKET",
                                                  "ELVARLI",
                                                  "GRÖNLID",
                                                  "HAVSTA",
                                                  "HEMNES",
                                                  "IVAR",
                                                  "JONAXEL",
                                                  "KALLAX",
                                                  "LIDHULT",
                                                  "NORDLI",
                                                  "PAX",
                                                  "PLATSA",
                                                  "STUVA / FRITIDS",
                                                  "VALLENTUNA",
                                                  "VIMLE",
                                                  "other")
                                      )
                          ),
                      box(selectInput("category",
                                      label = "Category of furniture",
                                      choices = c("Beds",
                                                  "Chairs",
                                                  "Chests of drawers & drawer units",
                                                  "Children's furniture",
                                                  "Nursery furniture",
                                                  "Outdoor furniture",
                                                  "Sofas & armchairs",
                                                  "Tables & desks",
                                                  "TV & media furniture",
                                                  "Wardrobes",
                                                  "Other")
                                      )
                          ),
                      # Slider
                      box(sliderInput("depth",
                                      label = "Depth of furniture",
                                      min = 1.0,
                                      max = 257.00,
                                      value = 47.00)
                          ),
                      box(sliderInput("height",
                                      label = "Height of furniture",
                                      min = 1.0,
                                      max = 700.0,
                                      value = 83.0)
                          ),
                      box(sliderInput("width",
                                      label = "Width of furniture",
                                      min = 1.0,
                                      max = 420.0,
                                      value = 80.0)
                          ),
                      box(sliderInput("Price",
                                      label = "Price of furniture",
                                      min = 3.0,
                                      max = 9585.0,
                                      value = 544.7)
                          ),
                      
                      ),
                    tabItem(
                      tabName = "Corr_Matrix_tab",
                      h2("Correlation Matrix"),
                      fluidRow(
                        box(DT::dataTableOutput("table1"),
                            width = "100%")
                      )
                      # fluidRow(
                      #   box(tableOutput("table1"))
                      # )
                      ), # Body
                    tabItem(
                      tabName = "Data_tab",
                      h2("Cleaned Raw Data of IKEA Furniture Prices"),
                      fluidRow(
                        box(DT::dataTableOutput("table2"),
                            width = "100%")
                        )
                      )
                    ),
                  tags$head(tags$style(HTML(
                    '.myClass { 
                    font-size: 20px;
                    line-height: 50px;
                    text-align: left;
                    font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
                    padding: 0 15px;
                    overflow: hidden;
                    color: white;
                    }
                    ')
                    )
                    ),
                  tags$script(HTML('
                  $(document).ready(function() {
                  $("header").find("nav").append(\'<span class="myClass"> IKEA </span>\');
                  })
                                   ')
                              )
                  )
  ) 

# Server ----

server <- function(input, output)
{
  output$price_prediction <- 
    renderValueBox({
      
      prediction <- 
        predict(
          model,
          tibble("name" = input$name,
                 "category" = input$category,
                 "depth" = input$depth,
                 "height" = input$height,
                 "width" = input$width
                 )
          )
      
       # prediction_prob <-
       #   predict(
       #     model,
       #     tibble("name" = input$name,
       #       "category" = input$category,
       #       "depth" = input$depth,
       #       "height" = input$height,
       #       "width" = input$width
       #       ),
       #     type = "prob"
       #     ) %>%
       #   select(.pred_1)
       # 
       prediction_statement <-
         if_else((10^prediction$.pred) > input$Price,
                 "Overestimate", if_else((10^prediction$.pred) < input$Price,
                                         "Underestimate", "The prediction works!")
                 )

       prediction_visual <-
         if_else((10^prediction$.pred) > input$Price,
                 "red", if_else((10^prediction$.pred) < input$Price,
                                         "yellow", "green")
                 )
       # 
       # valueBox(
       #   value = paste0(round(prediction_prob$.pred_1*100, 1), "%"),
        #   subtitle = paste0("Will this individual have stroke? ",
        #                     prediction_statement),
        #   color = prediction_visual
        # )
      
      for_prediction_statement <- prediction$.pred
      
      valueBox(
        value = paste0(("$"),round(10^for_prediction_statement)
                       ),
        subtitle = paste0("How much will be the median price of this furniture?")
        )
      
      }
    )
  
  output$box1 <- renderValueBox({
    
    prediction <- 
      predict(
        model,
        tibble("name" = input$name,
               "category" = input$category,
               "depth" = input$depth,
               "height" = input$height,
               "width" = input$width
        )
      )
    
    prediction_statement <-
      if_else((10^prediction$.pred) > input$Price,
              "Overestimate", if_else((10^prediction$.pred) < input$Price,
                                      "Underestimate", "The prediction works!")
      )
    
    prediction_visual <-
      if_else((10^prediction$.pred) > input$Price,
              "red", if_else((10^prediction$.pred) < input$Price,
                             "yellow", "green")
      )
    
    
    valueBox(value =  prediction_statement,
             subtitle = "Does the prediction overestimate or underestimate price? ",
             color = prediction_visual,
             width = 640) 
  })
  
  
  
  output$table1 <-
    DT::renderDataTable(
      {
      datatable(corr_table)
        }
      )
  
  output$table2 <- DT::renderDataTable({
    DT::datatable(data_table, options = list(orderClasses = TRUE,
                                             pageLength = 50,
                                             rowGroup = list(dataSrc = 0),
                                             initComplete = JS(
                                               "function(settings, json) {",
                                               "$(this.api().table().header()).css({'background-color': '#B33620', 'color': 'white'});",
                                               "$('table.dataTable.display tbody tr:odd').css('background-color', 'white');",
                                               "$('table.dataTable.display tbody tr:even').css('background-color', '#FFC5B5');",
                                               "}")
                                             )
                  ) 
      # formatStyle('log10_Price',
      #   backgroundColor = styleInterval(3.4, c('gray', 'yellow'))
      # )
    })
  
  # output$table2 <- renderTable({
  #   knitr::kable(data_table) %>%
  #     kableExtra::kable_styling("striped", 
  #                   full_width = F)
  #     # column_spec(c(2, 4, 6, 8, 10, 12), 
  #     #             bold = T, 
  #     #             color = "white", 
  #     #             background = "#ff6347")
  # })
  
  # output$table1 <- renderTable({
  #   corr_table
  # })  
  


}

# Run ----

shinyApp(ui, server)
