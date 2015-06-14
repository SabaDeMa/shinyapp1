library(shiny)
library(ggplot2)
library(dplyr)
data <- readRDS("data/data.rds")
data <- data %>% group_by(Year) %>%
    mutate( TotalP = sum(Profits, na.rm = T), TotalR = sum(Revenue) )
data$Company <- gsub("_", " ", data$Company)
scelt <- sort(unique(data$Company))


navbarPage("Top 500 Fortune Companies",
           tabPanel("Explore",
                    sidebarPanel("Write the name of a Fortune 500 Company in the box below",
                                 selectizeInput("azienda", "", choices = scelt),
                                 
                                 textOutput("bp"),
                                 textOutput("bestp"),
                                 textOutput("br"),
                                 textOutput("bestr"),
                                 textOutput("wr"),
                                 textOutput("worsr"),
                                 textOutput("wp"),
                                 textOutput("worsp"),
                                 p(em("All values are in millions of US dollars"))
                                 
                                 
                                 ),
                    mainPanel(plotOutput("gra1"),
                              plotOutput("gra2")
                              
                             
                              )
                    ),
           tabPanel("About",
                    mainPanel(p("This is the project for the Coursera course 'Developing Data products' by Johns Hopkins University."),
                              p("The data used in this web app are available", a("here",
                                                                                 href = "http://wiki.stat.ucla.edu/socr/index.php/SOCR_Data_Fortune500_1955_2008"),
                                "."),
                              p("This project is been develope by", a("Sabato De Maio", href = "http://sabadema.github.io") ),
                              h2("Usage"),
                              p("To use this web app just click on the field, type", em("Delete (on Mac OS)"), "or", em("Backspace (on Windows)"),
                                "and write the name of a US company."),
                              p("Do not worry if you do not know the exact name, this webb app use the", a("selectize javascript", href = "https://github.com/brianreavis/selectize.js")),
                              p("The code of this shiny app can be found", a("here", href = "https://github.com/SabaDeMa/shinyapp1"))
                              )
                    
                    )

           
    )