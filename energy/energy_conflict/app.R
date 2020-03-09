#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(tm)
library(tidytext)
library(dplyr)
library(rsconnect)
library(ISOcodes)
library(forcats)
library(scales)
#library(shinyWidgets)

# Load data
energy_data <- read_csv("energy_clean.csv")

#Set locale
Sys.setlocale(locale = "de_DE.UTF-8")

# Read text files into a corpus called articles
articles <- VCorpus(DirSource("all_clean", encoding = "UTF-8"),
                    readerControl = list(language = "german"))

#Clean up dates and newspaper names
parsed_date <- parse_date_time(energy_data$date_clean, "mdy")

#Sort by filename
energy_data <- energy_data[order(energy_data$filename),]

# Clean up newspaper names
energy_data <- energy_data %>% 
    mutate(newspaper = fct_recode(newspaper, "Süddeutsche Zeitung" = "s|ddeutsche.de", "Süddeutsche Zeitung" = "Sueddeutsche Zeitung", "Süddeutsche Zeitung" = "süddeutsche.de", "Süddeutsche Zeitung" = "Süddeutsche Zeitung (inkl. Regionalausgaben)")) 


#Make all text lowercase
articles <- tm_map(articles, content_transformer(tolower))
articles <- tm_map(articles, removePunctuation) 
articles <- tm_map(articles, removeWords, stopwords("de"))
articles <- tm_map(articles, stripWhitespace)
articles <- tm_map(articles, removeNumbers)

#had to finagle this list a little bit to get the right words for stemming
all_terms <- tolower(c("Atomkraftwerk", 
                       "Demonstration", 
                       "Jahrestag", 
                       "Kernkraftwerk",
                       "Mahnwach",
                       "WAA",
                       "Wackersdorf",
                       "Kalkar",
                       "Wyhl",
                       "Waldspaziergang",
                       "Wiederaufarbeitungsanlag"))

facilities <- tolower(c("Atomkraftwerk",
                        "Kernkraftwerk",
                        "Schneller Brüter",
                        "Schneller Brueter",
                        "Wiederaufarbeitungsanlag"))

memorial <- tolower(c("Demonstration",
                      "Mahnwach",
                      "Jahrestag",
                      "Waldspaziergang"))

cities <- tolower(c("Wyhl",
                    "Wackersdorf",
                    "Kalkar"))

term_labels <- c("Atomkraftwerk", 
                           "Demonstration",
                           "Jahrestag",
                           "Kernkraftwerk",
                           "Mahnwache",
                           "WAA",
                           "Wackersdorf",
                           "Kalkar",
                           "Wyhl",
                           "Waldspaziergang",
                           "Wiederaufarbeitungsanlage")


#There are two spellings of this in the corpus
bigrams <- tolower(c("Schneller Brüter",
                     "Schneller Brueter"))

#Creates a bigram tokenizer that can also handle 1-word ngrams (1:2)
bigram_tokenizer <- function(x) {
    unlist(lapply(ngrams(words(x), 1:2), paste, collapse = " "), use.names = FALSE)
}

#Count the number of times each word above appears in each article in the corpus
terms_data_frame <- (DocumentTermMatrix(articles, list(dictionary = all_terms, stemming = TRUE)))

#Count the number of time each bigram appears in each article in the corpus, includes appearances of word in title
terms_data_frame2 <- (DocumentTermMatrix(articles, control = list(tokenizer = bigram_tokenizer, dictionary = bigrams)))

#Convert the dataframes into matrices and write to csv files
terms_data <- as.matrix(terms_data_frame)
terms_data2 <- as.matrix(terms_data_frame2)
write.csv(terms_data, "terms_data.csv")
write.csv(terms_data2, "terms_data2.csv")

#Load in new terms data matrix - necessary because it includes the observation in the first column (filename)
terms <- read_csv("~/Documents/projects/energy/terms_data.csv")
terms2 <- read_csv("~/Documents/projects/energy/terms_data2.csv")

#Rename blank column (for filename observations) from original csv file 
names(terms)[names(terms) == "X1"] <- "filename" 
names(terms2)[names(terms2) == "X1"] <- "filename" 

#Join terms data with energy data by the filename column
energy_data <- inner_join(energy_data, terms, by = "filename") %>% 
    ungroup()
energy_data <- inner_join(energy_data, terms2, by = "filename")

#Remove whitespace in column names
names(energy_data)<-str_replace_all(names(energy_data), c(" " = ""))

#Sum the two Schneller Brueter columns as a new column called "sb", and move them to the end (to make selecting column ranges easier in future functions)
energy_data <- energy_data %>% 
    mutate(sb = schnellerbrüter + schnellerbrueter) %>% 
    select(-schnellerbrüter,schnellerbrüter) %>% 
    select(-schnellerbrueter,schnellerbrueter)

# I LEFT OFF HERE!!

# #Gather spread data
terms_gather <- energy_data %>%
    pivot_longer(cols = atomkraftwerk:wyhl, names_to = 'term', values_to = 'count') %>%
    select(months, years, newspaper, title, term, count) %>%
    filter(count != 0) %>%
    group_by(months, years, newspaper, term) %>%
    summarise(title = paste(title, collapse = " <br/> "), count = sum(count))

#This line omits "implicit na" warning caused by factoring newspapers. There are no missing newspaper values. 
terms_gather2 <- na.omit(terms_gather)

term_choices <- unique(sort(all_terms))

# Define UI for application that draws a line chart
ui <- fluidPage(

    # Application title
    titlePanel("Energy Conflict in Germany"),
    

    # Sidebar with checkbox options for terms 
    sidebarLayout(
        sidebarPanel(

            # Could be useful when writing nested checkbox: https://stackoverflow.com/questions/30592873/shiny-hierarchical-nested-checkboxgroupinput
            checkboxGroupInput(
                        inputId = "term",
                        h3("Terms"),
                        #choices = terms_gather$term,
                        #choices = term_choices, 
                        choiceNames = term_labels, 
                        choiceValues = term_choices,
                        selected = c("wyhl", "waa")
                        ),
            actionButton("all", "Select All"),
            actionButton("none", "Select None"),
        ),

        # Show a plot of selected terms over time
        mainPanel(
            div(
                style = "position: relative",
                plotOutput("plot",
                           hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce")),
                uiOutput("hover_info")
            ),
            
            div(
                style = "position: relative",
                sliderInput("obs", "Date:",
                            min = as.Date("1994-01-01"), max = as.Date("2019-12-31"), value = as.Date("2011"),
                            plotOutput("distPlot")
                            )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    output$plot <- renderPlot({
        ggplot(data = terms_gather[terms_gather$term %in% input$term,],
               aes(x = years, y = count, colour=term)) +
            geom_line(aes(linetype=term)) +
            geom_point() + 
            ylim(0, 50) +
            facet_grid(newspaper ~ .)
    })

    #Select All button
   observeEvent(input$all, {
       updateCheckboxGroupInput(
           session, "term", choiceNames = term_labels, choiceValues = term_choices,
           selected = term_choices
       )
   })
   
   #Select none if "Select None" button is clicked
   observeEvent(input$none, {
       updateCheckboxGroupInput(
           session, "term", choiceNames = term_labels, choiceValues = term_choices,
           selected = NULL
       )
   })
   
   # Slider
   output$distPlot <- renderPlot({
       hist(rnorm(input$obs))
   })
   
    #Set up the hover boxes
    output$hover_info <- renderUI({
        hover <- input$plot_hover
        point <- nearPoints(terms_gather, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
        if (nrow(point) == 0) return(NULL)
        
        # calculate point position INSIDE the image as percent of total dimensions
        # from left (horizontal) and from top (vertical)
        left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
        top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
        
        # calculate distance from left and bottom side of the picture in pixels
        left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
        top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
        
        # create style property fot tooltip
        # background color is set so tooltip is a bit transparent
        # z-index is set so we are sure are tooltip will be on top
        style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                        "left:", left_px + 2, "px; top:", top_px + 2, "px;")
        
        #actual toolitp created as wellPanel
        wellPanel(
            style = style,
            p(HTML(paste0("<b>Term: </b>", point$term, "</br>",
                          "<b>Month: </b>", point$months, "</br>",
                          "<b>Year: </b>", point$years, "</br>",
                          "<b>Number: </b>", point$count, "</br>",
                          "<b>Articles: </b></br>", point$title, "</br>"
                          
                          )))
        )
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
