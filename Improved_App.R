#libraries that we need
library(dplyr)
library(ggplot2)
library(forcats)
library(vroom)
library(shiny)

#In this assignment, I will complete Exercises 3 and 4 from Chapter 4 of the Mastering Shiny textbook:
#Exercise 3: Add an input control that lets the user decide how many rows to show in the summary tables.

#Exercise 4: Provide a way to step through every narrative systematically with forward and backward buttons.


#Check to see if the data exists in the environment
if (!exists("injuries")) { # "If 'injuries' does not exist in your workspace already, read in the injuries, products, and population datasets"
  injuries <- vroom::vroom("neiss/injuries.tsv.gz")
  products <- vroom::vroom("neiss/products.tsv")
  population <- vroom::vroom("neiss/population.tsv")
}

# code to get the products for filtering
prod_codes <- setNames(products$prod_code, products$title)

# useful factor lumping function (that is automated for any variable)...used to truncate the output tables
#I convert the variable to a factor, order by the frequency of the levels, and then lump together all levels after the number of rows the user selects (made n a variable in this function with "{{n}}")
count_top <- function(df, var, n) {
  df %>%
    mutate({{ var }} := fct_lump(fct_infreq({{ var }}), {{n}},
                                 other_level="Sum of All Other Categories")) %>%
    group_by({{ var }}) %>%
    summarise(n = as.integer(sum(weight)))
}

# The UI
ui <- fluidPage(
  h1("Introduction"),
  p("This app lets you interactively explore data from the National Electronic Injury Surveillance System (NEISS). This is a long term study that recorded all accidents seen in a representative sample of hospitals in the US from 2013-2017.  This app lets you explore data from the year 2017. You can find out more about this dataset at:"),
  a(href="https://github.com/hadley/neiss", "https://github.com/hadley/neiss"),
  
  hr(), #horizontal line separating the previous row from this one
  p("To begin, first select a product code.  This is the primary method of injury (e.g. 'knives' or 'baseball)"),
  #First row! Select the product codes
  fluidRow(
    #user input to select the product
    column(8, #this box is "8" wide
           selectInput("code", "Product Code",
                       choices = setNames(products$prod_code, products$title),
                       width = "100%")
    )),
    
    hr(), #New section (horizontal line separation)
    h1("Summarize"), #new header for summarizing the data
    p("Next, we will summarize the data.  First, select the number of rows you would like to display on the tables, then have a look at the numbers!"),
    fluidRow(
      column(1),
      #User input (slider) to select the number of rows to display on the tables:
      column(4, sliderInput(inputId="nRows", label="Number of unique rows",
                       min=2, max=20, step=1, value=5))
      ),
  
    #New row! The tables
    fluidRow(
      #output from server as tables (one for each of diagnosis, body part, and location)
      column(4, tableOutput("diag")),
      column(4, tableOutput("body_part")),
      column(4, tableOutput("location"))
    ),
    
 hr(), #New section (horizontal line separation)
 h1("Plot by age and sex"),
 p("Next, you can explore the overall patterns of specific injuries by age and sex for the product code you selected.  You can choose what to view on the y-axis: either the total number of injury counts or the normalized number of injury counts per 10,000 people."),
 
    #New row!  The plot
 fluidRow(
    #user input to select the variable to plot (I changed the options for y-axis label to be more intuitive for the user):
      column(8, 
             selectInput("y", "Y-axis",
                         choices= c("Injuries per 10,000 people", "Estimated number of injuries"), width = "100%")),
    #output from server as a plot
    column(12, plotOutput("age_sex"))
  ),
  
    hr(), #New section (horizontal line separation)
    h1("Narratives"),
    p("Each injury has a narrative associated with it, that tells you more about how that specific injury happened.  Click the buttons below to explore different narratives for your selected product code! Either generate a narrative at random:"),
  fluidRow(
    column(2, actionButton("story", "Random Narrative")), #add the action button
    column(10, textOutput("narrative"))
  ),
 p("Or scroll sequentially through the narratives:"),
 fluidRow(
   column(2, actionButton("prevNarrative", "Previous Narrative")),
   column(5, textOutput("sequentialnarrative")),
   column(2,actionButton("nextNarrative", "Next Narrative"),
   column(5, textOutput("sequentialnarrativeIndex")))
 )
)
#end of fluidPage()


# The Server...using reactive programming
server <- function(input, output, session) {
  
  selected <- reactive(injuries %>% filter(prod_code == input$code)) # only use the data that the user gives us as an input...making "selected" dataset reactive so you only have to code it/tie it to the UI once
  
  #one of the output tables (1/3) for the diagnosis...for all 3 of the following tables, I changed the code to reflect the user input "nRows" and I made the column headings more understandable
  output$diag <- renderTable({
    diagTable <- count_top(df= selected(), var=diag, n=input$nRows-1) #subtracting 1 from the nRows input so that one of the rows shown is the "Other" row
    colnames(diagTable) <- c("Diagnosis", "Number")
    diagTable
    }, width = "100%") # "I forced all tables to take up the maximum width (i.e. fill the column that they appear in)"...this improves the aesthetics of the app
 
  #one of the output tables (2/3) for the body part
  output$body_part <- renderTable({
    bodyTable <- count_top(df= selected(), var=body_part, n=input$nRows - 1)
    colnames(bodyTable) <- c("Body part injured", "Number")
    bodyTable
    }, width = "100%")
  
  #one of the output tables (3/3) for the location
  output$location <- renderTable({
    locationTable <- count_top(df=selected(), var=location, n=input$nRows - 1)
    colnames(locationTable) <- c("Location of injury", "Number")
    locationTable
    }, width = "100%")
  
  
  #Reactive expression for getting rate of injury (normalized per 10,000 people in each age group) by age and sex -- and raw number (from count())
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })
  
  #Output a plot of injury rate patterns by age and sex (make it a condition of the user input)
  output$age_sex <- renderPlot({
    if (input$y == "Estimated number of injuries") { #if the user input is "Estimated number of injuries"...
      summary() %>%
        ggplot(aes(age, n, colour = sex)) + #...then plot "n" 
        geom_line() +
        labs(y = "Estimated number of injuries")#...and change the y axis label
    } else { #if the user input is anything else (in this case "Injuries per 10,000 people")
      summary() %>%
        ggplot(aes(age, rate, colour = sex)) +#...then plot "rate"
        geom_line(na.rm = TRUE) +
        labs(y = "Injuries per 10,000 people") #...and change the y axis label
    }
  }, res = 96)
  
  #tell a story based on an action button...
  narrative_sample <- eventReactive(
    list(input$story, selected()),
   selected() %>% pull(narrative) %>% sample(1))
   output$narrative <- renderText(narrative_sample())
  
  
  #...or scroll through the buttons to see sequential narratives
  res <- reactiveValues(narrativeIndex = 1)
  
  #look in the selected() function and filter out only the narratives...creating a new reactive expression that ties the product code to its associated narratives. It needs to be in a list format (use the pull function instead of the select function...the select function will lead to errors down the line)
  selectedNarratives <- reactive(selected() %>% pull(narrative))

  #this observeEvent() function will run the server code as soon as the user clicke the nextNarrative action button (defined in the UI)
  observeEvent(input$nextNarrative,{
    # the if statement is to keep the letterIndex in bounds (so it doesn't go past the length of the selecteNarratives dataset)
    if(res$narrativeIndex > length(selectedNarratives())) res$narrativeIndex <- 1
    else res$narrativeIndex <- res$narrativeIndex + 1
  })
  
  #this observeEvent() function will run the server code as soon as the user clicke the previousNarrative action button (defined in the UI)
  observeEvent(input$prevNarrative,{
    if(res$narrativeIndex < 2) res$narrativeIndex <- length(selectedNarratives())
    else res$narrativeIndex <- res$narrativeIndex - 1
  })
  
  
  output$sequentialnarrativeIndex <- renderText({
    paste0(res$narrativeIndex," of ",length(selectedNarratives()))
  })
  
  output$sequentialnarrative <- renderText({
    theNarratives <- selectedNarratives()
    theNarratives[res$narrativeIndex]
  })
  
  
}

shinyApp(ui, server)
