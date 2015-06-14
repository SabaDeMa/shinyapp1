library(shiny)
library(ggplot2)
library(dplyr)
data <- readRDS("data/data.rds")
data <- data %>% group_by(Year) %>%
    mutate( TotalP = sum(Profits, na.rm = T), TotalR = sum(Revenue) )
data$Company <- gsub("_", " ", data$Company)
scelt <- sort(unique(data$Company))


source("func.R")

shinyServer(function(input, output) {
    

    
    output$gra1 <- renderPlot({ compa <- data %>% filter(Company == input$azienda) %>%
                                                    mutate(pratio = Profits / Revenue)
    
                                pr <- compa$Revenue / lag(compa$Revenue) -1
                                pp <- compa$Profits / lag(compa$Profits) -1
                                compa <- tbl_df(cbind(compa, pr, pp))
                                compa_omi <- na.omit(compa)
    

                                azg <- ggplot(compa_omi, aes(Year, pp))
                                rott <- compa_omi$Year
                                if(nrow(compa) >= 5){
                                azg + geom_line(size = 1.5, aes(color = "Profits")) +
                                      geom_line(aes(y = pr, color = "Revenues"), size = 1.5) +
                                      {if(length(rott) <= 30 ){scale_x_continuous(breaks = rott)} else{scale_x_continuous()} } +
                                      theme( axis.text.x = element_text(size = 13, angle = 45, hjust = 1 ),
                                             axis.text.y = element_text(size = 13, vjust = 1),
                                             legend.title = element_text(size = 14) ) +
                                        scale_color_discrete(name="Percentage\nchanges of:") +
                                        scale_y_continuous(name = "Percentage changes") +
                                        scale_x_continuous(name = "Years")
                                       
                                }
                                else{ azg + geom_point(size = 5, color = "coral2") }
        
                                })
    
    output$gra2 <- renderPlot({ compa2 <- data %>% filter(Company == input$azienda) %>%
                                mutate(pratio = Profits / Revenue)
    
                                praa <- ggplot(compa2, aes(Year, pratio))
                                if(nrow(compa2) >= 5){
                                praa + geom_line(size = 1.5) +
                                        theme( axis.text.x = element_text(size = 13, angle = 45, hjust = 1 ),
                                               axis.text.y = element_text(size = 13, vjust = 1),
                                               legend.title = element_text(size = 14) ) +
                                               scale_x_continuous(name = "Years") + 
                                               scale_y_continuous(name = "Profit/Revenue Ratio") }
                                else{ praa + geom_point(size = 5, color = "coral2") } 
                              
                                    })
    output$worsr <- renderText({compa <- data %>% filter(Company == input$azienda);worsr <- min(compa$Revenue)})
    output$worsp <- renderText({compa <- data %>% filter(Company == input$azienda);worsp <- min(compa$Profits)})
    output$bestp <- renderText({compa <- data %>% filter(Company == input$azienda);bestp <- max(compa$Profits)})
    output$bestr <- renderText({compa <- data %>% filter(Company == input$azienda);bestr <- max(compa$Revenue)})
    output$bp <- renderText("The best profit is:")
    output$br <- renderText("The best revenues are:")
    output$wp <- renderText("The worst profits is:")
    output$wr <- renderText("The worst revenues are:")


})