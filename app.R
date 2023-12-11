library(tidyverse)
library(shiny)
library(shinythemes)
library(DT)
library(plotly)

shinyLink <- function(to, label) {
  tags$a(
    href = to,
    label
  )
}

df <- read_delim("final.csv")
df2 <- read_delim("unemployment.csv")
df3 <- read_delim("reasons.csv")
df4 <- read_delim("industry.csv")
df5 <- read_delim("employment.csv")

# Define UI 
ui <- navbarPage(
  theme = shinytheme("readable"), 
  
  #INTRODUCTION PAGE
  "Data Story",
  tabPanel(
    "Introductory",
    div(
      style="height: 100px; background-color: #535356; width: 100%; position: absolute; right:0;",
      h1(HTML("<b>US Unemployment</b>"), style="text-align:center;color:white"),
      br(),
      br(),
      
      div(
        style = "width: 70%; margin: auto; font-size: 20px",
        h3(HTML("Sotry Pitch"), align = "left"),
        br(),
        tags$p(
          "In the TV series",
          shinyLink(to="https://en.wikipedia.org/wiki/House_of_Cards_(American_TV_series)", label="House of Cards"),
          "the concept of ",
          shinyLink(to="https://houseofcards.fandom.com/wiki/America_Works", label="AmWorks"),
          "was introduced as part of the show's political narrative.",
          "AmWorks, a fictional landmark jobs program, ",
          "was aimed to address unemployment issues in the United States by creating jobs through a large-scale public works project.",
          "In the context of the series, it was proposed by Frank Underwood, the show's central character, who is a ruthless, manipulative and ambitious politician."
        ),
        br(),
        tags$p(
          "In the world of “House of Cards,” the legislation is a comprehensive jobs program that calls for a substantial investment in infrastructure projects, 
          such as the construction of bridges, roads, and public buildings. Apart from the intention of creating employment opportunities, stimulating the economy, 
          another significant cause is potentially improvement of Frank Underwood's approval ratings in the next presidential election. Actually, 
          the legislation is the only chip for Underwood to win the campaign, so he even threatened a government official, allowing him to use funds from the 
          FEMA Disaster Relief Fund to finance this program. Underwood made every effort to make the program pass, which clearly shows how American people Concerned about employment issues."
        )
      ),
      
      br(),
      hr(),
      
      div(
        img(src="https://s2.loli.net/2023/12/10/MNB4m1C3ZcrQHyu.png", style = "width: 670px; display: block; margin: 0 auto;")
      ),
      h6("Frank Underwood in the House of Card 3", align = "center"),
      
      hr(),
      br(),
      
      div(
        style = "width: 70%; margin: auto; font-size: 20px",
        h3(HTML("Why should we care about <b>US Unemployment</b>?"), align = "left"),
        br(),
        tags$p(
          "The issue of unemployment in the USA among different demographic groups is 
          not just an abstract concern; it's a very real and immediate part of our lives. 
          Many of us are currently preparing to enter the job market or are already 
          navigating the challenges of finding employment in our chosen fields. 
          Understanding how average hourly wages of workers in the USAs vary among 
          educational backgrounds, genders, and races provides us with critical insights 
          into the obstacles and opportunities we might face upon graduation. 
          It's not just a theoretical issue; it directly affects our future prospects 
          and well-being. This project can serve as a valuable resource for our 
          academic research, class projects, and future development."
        )
      ),
      
      br(),
      
      hr(style = "border-top: 1px solid #000000;"),
      
      div(
        style = "width: 70%; margin: auto; font-size: 15px; text-align: right; color: #535356",
        h5("Built by Rstudio and Shiny"),
        h5("R Package: tidyverse, shiny, shinythemes, DT, plotly")
      ),
      
      br(),
    )
  ),
  
  #PAGE 1, unemployment
  tabPanel(
    "USA Unemployment Rate",
    br(),
    br(),
    sidebarPanel(
      h4(HTML("US Unemployment Rates Since 1940")),
      br(),
      h5(HTML("First, let's see overall unemployment rate of US in the hisotry. We could get some lessons on how the 
              economic big environment, historical context or other uncontrollable factors influence the market.")),
      br(),
      h5(HTML("The blue line represent the average unemployment rate through these years")),
      br(),
      hr(),
      h6(shinyLink(to="https://datahub.io/core/employment-us#r", label="Source of the dataset")),
    ),
    mainPanel(
      plotlyOutput("unemployment")
    ),
    sidebarPanel(
      h5(HTML("Check out what happened in the years that have higher unemployment rate than average to 
              see the uncontrollable elements in these hard time")),
      br(),
      selectInput(
        inputId = "Year",
        label = "Select a year",
        choices = df3$year
      ),
      br()
    ),
      mainPanel(
      br(),
      h3(HTML("Background and potential reasons")),
      br(),
      htmlOutput(outputId = "reasons")
    )
  ),
  
 
  #PAGE 2, wage
  tabPanel(
    "USA Wage Level by Educational Level",
    br(),
    br(),
    sidebarPanel(
      br(),
      h5(HTML("After having awareness of hisotry. Let's see an element that are more closely related to us - EDUCATIONAL LEVEL.
              The graph clearly shows that higher educational level always has higher wage, demonstrating the siginficance of
              a better diploma.")),
      br(),
      h5(HTML("The two plots below are sorted more detailed, by gender and race. Choose the different classifications to see 
         related situations.")),
      br(),
      hr(),
      h6(shinyLink(to="https://www.kaggle.com/datasets/asaniczka/wages-by-education-in-the-usa-1973-2022", label="Source of the dataset"))
      
    ),
    mainPanel(
      
      plotOutput("overall")
    ),
  
    sidebarPanel(
      width = 6,
      selectInput(
        inputId = "gender",
        label = "Gender",
        choices = list("Male", "Female")
      ),
      plotOutput("gender")
    ),
    sidebarPanel(
      width = 6,
      selectInput(
        inputId = "race",
        label = "Race",
        choices = list("White", "Black", "Hispanic")
      ),
      plotOutput("race")
    )
  
  ),
  
  
  #PAGE 3
  tabPanel(
    "US Employment and Output by Major Industry Sector",
    br(),
    br(),
    sidebarPanel(
      br(),
      h5("At last, let's explore the employment and output in different industries, to know some popular areas."),
      br(),
      checkboxGroupInput(
        inputId = "industry",
        label = "Choose any industries to make a comparison",
        choices = df4$IndustrySector,
        selected = "All industries"
      ),
      h6("Reference data only, not for technical specifications."),
      br(),
      h5(em("There might be most profitable area, but we need to choose a most suitable industry based on our own actual situation.")),
      br(),
      hr(),
      h6(shinyLink(to = "https://www.bls.gov/emp/tables/employment-by-major-industry-sector.htm#1", label = "Source of the dataset"))
    ),
    mainPanel(
      h3("Employment by major industry sector", align = "center"),
      h6("Employment is in thousands of jobs", align = "center"),
      tableOutput(outputId = "employ"),
      h3("Output by major industry sector", align = "center"),
      h6("Output is in billions of dollars", align = "center"),
      tableOutput(outputId = "output"),
    )
  ),
)

# Define Server
server <- function(input, output) {
  
  
  output$unemployment <- renderPlotly({
    p <- ggplot(df2, aes(year, unemployed_percent)) +
      geom_line() +
      geom_hline(yintercept = mean(df2$unemployed_percent), color = "blue") +
      labs(x = "Year",
           y = "Unemployment Rate")
    
    p <- ggplotly(p, tooltip = c("year", "unemployed_percent"))
    return(p)
  })
  
  output$reasons <- renderUI({
    r <- function(time){
      str <- pull(filter(df3, year == time), reason)
      return(h5(str))
    }
    r(input$Year)
  })

  output$overall <- renderPlot({
    p <- ggplot(df, aes(x = year)) +
      geom_line(aes(y = less_than_hs, color = "less_than_hs")) +
      geom_line(aes(y = high_school, color = "high_school")) +
      geom_line(aes(y = some_college, color = "some_college")) +
      geom_line(aes(y = bachelors_degree, color = "bachelors_degree")) +
      geom_line(aes(y = advanced_degree, color = "advanced_degree")) +
      labs(x = "Year",
           y = "Average Hourly Wages of Workers in the USA",
           title = "Average hourly wages of workers in the USA based on educational level from 1973 to 2022")
    
    return(p)
  })
  
  output$gender <- renderPlot({
    
    if(input$gender == "Male"){
      g <- ggplot(df, aes(x = year)) +
        geom_line(aes(y = men_less_than_hs, color = "men_less_than_hs")) +
        geom_line(aes(y = men_high_school, color = "men_high_school")) +
        geom_line(aes(y = men_some_college, color = "men_some_college")) +
        geom_line(aes(y = men_bachelors_degree, color = "men_bachelors_degree")) +
        geom_line(aes(y = men_advanced_degree, color = "men_advanced_degree")) +
        labs(x = "Year",
             y = "average hourly wages of workers in the USA")
    } else if(input$gender == "Female"){
    g <- ggplot(df, aes(x = year)) +
      geom_line(aes(y = women_less_than_hs, color = "women_less_than_hs")) +
      geom_line(aes(y = women_high_school, color = "women_high_school")) +
      geom_line(aes(y = women_some_college, color = "women_some_college")) +
      geom_line(aes(y = women_bachelors_degree, color = "women_bachelors_degree")) +
      geom_line(aes(y = women_advanced_degree, color = "women_advanced_degree")) +
      labs(x = "Year",
           y = "average hourly wages of workers in the USA")
  }
    return(g)
  })
  
  output$race <- renderPlot({
    if(input$race == "White"){
      r <- ggplot(df, aes(x = year)) +
        geom_line(aes(y = white_less_than_hs, color = "white_less_than_hs")) +
        geom_line(aes(y = white_high_school, color = "white_high_school")) +
        geom_line(aes(y = white_some_college, color = "white_some_college")) +
        geom_line(aes(y = white_bachelors_degree, color = "white_bachelors_degree")) +
        geom_line(aes(y = white_advanced_degree, color = "white_advanced_degree")) +
        labs(x = "Year",
             y = "average hourly wages of workers in the USA")
    } else if(input$race == "Black"){
      r <- ggplot(df, aes(x = year)) +
        geom_line(aes(y = black_less_than_hs, color = "black_less_than_hs")) +
        geom_line(aes(y = black_high_school, color = "black_high_school")) +
        geom_line(aes(y = black_some_college, color = "black_some_college")) +
        geom_line(aes(y = black_bachelors_degree, color = "black_bachelors_degree")) +
        geom_line(aes(y = black_advanced_degree, color = "black_advanced_degree")) +
        labs(x = "Year",
             y = "average hourly wages of workers in the USA")
    } else if(input$race == "Hispanic"){
      r <- ggplot(df, aes(x = year)) +
        geom_line(aes(y = hispanic_less_than_hs, color = "hispanic_less_than_hs")) +
        geom_line(aes(y = hispanic_high_school, color = "hispanic_high_school")) +
        geom_line(aes(y = hispanic_some_college, color = "hispanic_some_college")) +
        geom_line(aes(y = hispanic_bachelors_degree, color = "hispanic_bachelors_degree")) +
        geom_line(aes(y = hispanic_advanced_degree, color = "hispanic_advanced_degree")) +
        labs(x = "Year",
             y = "average hourly wages of workers in the USA")
    }
    
    return(r)
  })
  
  filtered_df4 <- reactive({
    filter(df4, IndustrySector %in% input$industry)
  })
  
  output$output <- renderTable({
    filtered_df4()
  })
  
  filtered_df5 <- reactive({
    filter(df5, IndustrySector %in% input$industry)
  })
  
  output$employ <- renderTable({
    filtered_df5()
  })
  
}


# Run the App
shinyApp(ui, server)