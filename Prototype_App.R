#libraries that we need
library(dplyr)
library(ggplot2)
library(forcats)
library(vroom)
library(shiny)

#Check to see if the data exists in the environment
if (!exists("injuries")) { # "If 'injuries' does not exist in your workspace already, read in the injuries, products, and population datasets"
  injuries <- vroom::vroom("neiss/injuries.tsv.gz")
  products <- vroom::vroom("neiss/products.tsv")
  population <- vroom::vroom("neiss/population.tsv")
}

# code to get the products for filtering
prod_codes <- setNames(products$prod_code, products$title)

# useful factor lumping function (that is automated for any variable)...used to truncate the output tables
#"I convert the variable to a factor, order by the frequency of the levels, and then lump together all levels after the top 5."
count_top <- function(df, var, n = 5) {
  df %>%
    mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
    group_by({{ var }}) %>%
    summarise(n = as.integer(sum(weight)))
}

# The UI
#Here I decided to have one row for the inputs (accepting that I’m probably going to add more inputs before this app is done), one row for all three tables (giving each table 4 columns, 1/3 of the 12 column width), and then one row for the plot:
ui <- fluidPage(
  
  fluidRow(
    #user input to select the product
    column(8, #this box is "8" wide, longer than the next selection box (see below)
           selectInput("code", "Product",
                       choices = setNames(products$prod_code, products$title),
                       width = "100%"
           )
    ),
    #user input to select the variable to plot
    column(2, selectInput("y", "Y axis", c("rate", "count"))) #this box is "2" wide, shorter than the previous selection box
  ),
  
  fluidRow(
    #output from server as tables (one for each of diagnosis, body part, and location)
    column(4, tableOutput("diag")),
    column(4, tableOutput("body_part")),
    column(4, tableOutput("location"))
  ),
  fluidRow(
    #output from server as a plot
    column(12, plotOutput("age_sex"))
  ),
  fluidRow(
    column(2, actionButton("story", "Tell me a story")), #add the action button
    column(10, textOutput("narrative"))
  )
)


# The Server...basically repeating our code from our prepping/understanding the data R script, but we are using reactive programming instead of regular variables
server <- function(input, output, session) {
 #make a reactive to select a product code
   selected <- reactive(injuries %>% filter(prod_code == input$code)) # only use the data that the user gives us as an input...making "selected" dataset reactive so you only have to code it/tie it to the UI once
  
  #one of the output tables (1/3) for the diagnosis
  output$diag <- renderTable(count_top(selected(), diag), width = "100%") # "I forced all tables to take up the maximum width (i.e. fill the column that they appear in)"...this improves the aesthetics of the app
  #this is the original code for the output table, but it made ugly tables:
  #output$diag <- renderTable(
  #  selected() %>% count(diag, wt = weight, sort = TRUE))
  
  #one of the output tables (2/3) for the body part
  output$body_part <- renderTable(count_top(selected(), body_part), width = "100%")
  
  #one of the output tables (3/3) for the location
  output$location <- renderTable(count_top(selected(), location), width = "100%")
  
  
#Reactive for getting rate of injury (normalized per 10,000 people in each age group) by age and sex -- and raw number (from count())
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })
  
#Output a plot of injury rate patterns by age and sex (make it a condition of the user input)
  output$age_sex <- renderPlot({
    if (input$y == "count") { #if the user input is "count"...
      summary() %>%
        ggplot(aes(age, n, colour = sex)) + #...then plot "n" 
        geom_line() +
        labs(y = "Estimated number of injuries")#...and change the y axis label
    } else { #if the user input is anything else (in this case "rate")
      summary() %>%
        ggplot(aes(age, rate, colour = sex)) +#...then plot "rate"
        geom_line(na.rm = TRUE) +
        labs(y = "Injuries per 10,000 people") #...and change the y axis label
    }
  }, res = 96)
  
  #tell a story based on an action button
  narrative_sample <- eventReactive(
    list(input$story, selected()),
    selected() %>% pull(narrative) %>% sample(1)
  )
  output$narrative <- renderText(narrative_sample())
}

shinyApp(ui, server)