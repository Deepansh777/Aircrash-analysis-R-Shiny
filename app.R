library(shiny)
library(shinythemes)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(leaflet)
library(htmltools)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(memoise)



aircrash <-read.csv("aircrash_clean_data.csv", header = T)
aircrash1<-aircrash[, -c(1,25)]
aircrash_geocoded <- read.csv("aircrash_geocoded.csv", header=T)
text <- read.csv("summary.csv", header = T)
docs <- Corpus(VectorSource(text))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
dd<-read.csv("dd.csv", header=T)

common_phrases <- read.csv("common phrases.csv", header=T)




# Define UI for application that draws a histogram
ui <- fluidPage(
     navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
                           "Aircrash Data Analysis", id="nav",
                 tabPanel("Timeline",
                         sidebarLayout(
                             sidebarPanel(tags$div(
                                 tags$strong("Aircrash Timeline"), br(),
                                 "Aviation safety means the state of an aviation system or organization in which risks associated with aviation activities, related to, or in direct support of the operation of aircraft, are reduced and controlled to an acceptable level. Earlier tragedies investigations and improved engineering has allowed many safety improvements that have allowed an increasing safer aviation. With the help of the slider below, user can understand the aircrash statistics for any given year(s) for both Civilian, & Military aircrafts.",
                                 br(), br()),
    sliderInput("yearInput", "Year", min=1908, max=2020, 
                value=c(1908, 2020), sep=""),
    checkboxGroupInput("aircrafttypeInput", "Select the Aircraft Type:",
                       choices = c("Civilian",
                                   "Military"),
                       selected = c("Civilian", "Military"))
    
                                        
),
mainPanel(
    plotlyOutput("Aircrafttypecount"),
    br(), br(),
    plotlyOutput("fatalitiesplot"),
    br(), br(),
    plotlyOutput("survivalplot"),
    br(), br(),
    plotlyOutput("DonutCivil"),
    br(), br(),
    plotlyOutput("DonutMilitary")
)
)
),

tabPanel("Operators",
         sidebarLayout(
             sidebarPanel(tags$div(
                 tags$strong("Aircrashes by Operator"), br(),
                 "Aircrash history plays a vital role in determining the future of any airline. There are many airlines around the globe which have an impeccable flight safety record whereas others have had multiple aircrashes within the same year. The user can adjust the number of operators they want to see on the bar plot. These plots will help the user to understand the aircrash history of both Civilian, & Military operators.", 
                 br(), br()),
                 sliderInput("numberInput", "Operators with highest number of crashes:", min=1, max=30, 
                             value=5)
),
mainPanel(
    plotlyOutput("CivilianOperators"),
    br(), br(),
    plotlyOutput("MilitaryOperators")
)
)
),

tabPanel("Aircrash Mapper", 
         leafletOutput('map',width = "100%", height = 900),

             absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                           draggable = TRUE, top = 60, left = "auto",
                           width = 340, height = "auto",
                           tags$div(
                               tags$strong("Locating the Crash Site"), br(),
                               "Geocoding was done to map the locations of the crash. Geocoding is the process of taking a text-based description of a location, such as an address or the name of a place, and returning geographic coordinates. The slider will allow the user to switch between years. The blue markers represents the crash site of a particular aircrash. Clicking on the blue markers will give you the location, aircraft type and summary of the aircrash.",
                              br(), br()),
                 sliderInput("YrsInput", "Year:", min=min(aircrash_geocoded$crash_year), max=max(aircrash_geocoded$crash_year), 
                             value=2017)
                 
             )
         ),

tabPanel("Word Cloud",
         sidebarLayout(
             sidebarPanel( tags$div(
                 tags$strong("About Word Cloud"), br(),
                 "A word cloud (weighted list in visual design) is a novelty visual representation of text data. It depicts the words which occured more frequently in the aircrash summary. The importance of each tag is shown with font size and color. The user has the ability to change the number of words they want to see in the word cloud by adjusting the slider below.",
                 br(), br()),
                 sliderInput("numberInput2", "Maximum Number of Words:", min=1, max=105, 
                             value=35)
                 
             ),
             mainPanel(plotOutput("WordCloudPlot"),
                 br(),
                 plotlyOutput("WordCloudBarPlot"),
                 br(),
                 plotlyOutput("AircrashPhrases")
             )
         )
),




tabPanel("Data Table",
         fluidRow(
         column(4,
                selectInput("crashyear",
                            "Crash Year:",
                            c("All",
                              unique(as.character(aircrash$crash_year))))
         ),
         column(4,
                selectInput("crashreason",
                            "Crash Reason:",
                            c("All",
                              unique(as.character(aircrash$crash_reason))))
         ),
         column(4,
                selectInput("crashoprtype",
                            "Crash Operator Type:",
                            c("All",
                              unique(as.character(aircrash$crash_opr_type))))
         )
),
hr(),
DT::dataTableOutput("table")


),

tabPanel("About this site",
         tags$div(
             tags$h4("Background"), 
             "Historically there have been several instance of air plane crashes. The application has been developed with an attempt to explore the possible causes of such air crashes, and to determine if air travel is a safe option. The objective of this application is to perform an Exploratory Data Analysis to determine the common cause/reason of airplane crash, countries with maximum/minimum airplane crashes, and any other interesting relationships. A systematic data analysis was undertaken to answer the objectives.", 
             tags$br(),tags$br(),
             tags$h4("Data Source and Tools used"),
             "For this data analysis, two data sources were used. The primary source was",
             tags$a(href="https://www.kaggle.com/saurograndi/airplane-crashes-since-1908", "Kaggle"),
             " and the secondary source was ",
             tags$a(href="http://www.planecrashinfo.com/database.htm", "PlaneCrashInfo."),
             " Kaggle provided data only from 1908 to 2009. The remaining data (i.e. from 2010 - 2020) was acquired using the PlaneCrashInfo database. Web scrapping using rvest package was done to extract the data from the secondary source. The primary tool used to develop this application is R. The packages used are", tags$b("R Shiny"),",  ", tags$b("leaflet"), ",  ",tags$b("rvest"),",  ", tags$b("Plotly"),",  ", tags$b("ggplot"),", and  ", tags$b("ggmap"), ". Geocoding was done using ggmap package to determine the approximate coordinates of the crash site.",
             tags$br(),tags$br(),
             tags$h4("Author's Opinion"),
             "As the common adage goes, 'a picture is worth a thousand words'. Once the data was cleaned and composed in a tidy format, it was ready for visualizations. Data visualization helps in determining possible relationship between variables. Some notable observations made from this analysis are: ",
             tags$li("The worst period in the aviation history was from 1967 - 2000. About",tags$b("11,000 people lost their lives due to civilian aircrashes in 1976"),". This number is higher than the current population of Vatican City."),
             tags$li("The number of military aircrashes was higher than civilian aircrashes in 1945."),
             tags$li("Civilian operator with maximum aircrashes is Aeroflot followed by Airfrance, and Lufthansa."),
             tags$li("The most common phrase mentioned in the aircrash summary is", tags$b("crashed during takeoff")," followed by", tags$b("crashed while en route"),", and", tags$b("crashed into the sea.")),
             tags$li("About 520 civilian aircrashes happened due to the technical failures. Their was a significant decline in the civilian aircrashes due to technical failures from 2000-2020. This could be due to advancement in the aviation technology."), 
                     tags$li("Their is a huge decline in the number of people died due to civilian aircrashes from about 7,600 in 2000 to about 1,100 in 2020."),
                     tags$li("Their is a 4% increase in military aircrashes due to technical failures in 2000 - 2020 than 1980 - 2000."),
             tags$br(),
             
             
             tags$hr(),
             tags$h4("About the Author"), 
             img(src='unnamed.jpg', align = "right", width = "200", height="200"),
                p("                           
                   Hello! My name is Deepansh Arora. Currently, I am pursuing my Master of Science in Industrial Engineering (Concentration: Data Analytics). I, currently, work at National Institute for Aviation Research (NIAR), Wichita, Kansas. I am a highly analytical, detail-focused, and data-driven Analyst with in-depth knowledge of database types, research methodologies, visualization, and lab material. If you have any questions, you can", tags$a(href="mailto: dxarora@shockers.wichita.edu", "email me"), "or connect with me on ", tags$a(href=" https://www.linkedin.com/in/deepansh-arora-423661168", "LinkedIn"), ".") 
           

)) 
        ))        
             
# Define server logic required to draw a histogram
server <- function(input, output) {

    
    d1 <- reactive({
            aircrash %>%
            filter(crash_opr_type %in% input$aircrafttypeInput,
                   crash_year >= input$yearInput[1],
                   crash_year <= input$yearInput[2])
        
    }) 
    
    output$Aircrafttypecount <-renderPlotly({
       
        a1<-ggplot(data=d1(), aes(x= crash_year, y=stat(count), color =crash_opr_type ))+
            geom_line(stat = "count")+theme_bw()+
            theme(plot.title = element_text(color = "black", size = 20, face = "bold", hjust = 0.5), 
                  axis.title.x =element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
                  axis.title.y = element_text(color = "black", size = 14, face = "bold", hjust = 0.5))+
            labs(y = "Number of Aircrashes",x="Year",title = "Number of Crashes per Year",color = "Aircraft Type")+
            expand_limits(y=c(0,100)) + 
            scale_y_continuous(breaks=seq(0, 100, 20))+theme(legend.title = element_blank())
        ggplotly(a1, source = "select", tooltip = c("crash_year","count"))
        
    })
    
    d2 <- reactive({
        d1() %>%
            group_by(crash_opr_type, crash_year) %>%
            summarise(fatalities = sum(fatalities,na.rm = TRUE))
    }) 
    
    output$fatalitiesplot <-renderPlotly({
        
        a2<-ggplot(data=d2(), aes(x= crash_year, y=fatalities, color=crash_opr_type))+
            geom_line() +theme_bw()+
            theme(plot.title = element_text(color = "black", size = 20, face = "bold", hjust = 0.5), 
                  axis.title.x =element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
                  axis.title.y = element_text(color = "black", size = 14, face = "bold", hjust = 0.5))+
            labs(y = "Fatalities",x="Year",title = "Fatalities per Year",color = "Aircraft Type")+
            theme(legend.title = element_blank())
        ggplotly(a2, source = "select", tooltip = c("crash_year","fatalities"))
        
    })
    
    d3 <- reactive({
        d1() %>%
            group_by(crash_opr_type, crash_year) %>%
            summarise(survived = sum(survived,na.rm = TRUE))
    }) 
    
    output$survivalplot <-renderPlotly({
        
        a3<-ggplot(data=d3(), aes(x= crash_year, y=survived, color=crash_opr_type) )+
            geom_line() +theme_bw()+
            theme(plot.title = element_text(color = "black", size = 20, face = "bold", hjust = 0.5), 
                  axis.title.x =element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
                  axis.title.y = element_text(color = "black", size = 14, face = "bold", hjust = 0.5))+
            labs(y = "Survival",x="Year",title = "Survival per Year",color = "Aircraft Type")+
            theme(legend.title = element_blank())
        ggplotly(a3, source = "select", tooltip = c("crash_year","survived"))
        
    })
    
    d3_c <- reactive({
        aircrash %>% 
        filter(crash_year >= input$yearInput[1],
                    crash_year <= input$yearInput[2],
               crash_opr_type=="Civilian") %>%
            select(crash_reason) %>%
            count(crash_reason) 
        
    }) 
    
    output$DonutCivil <-renderPlotly({
        plot_ly(data= d3_c(),labels = ~crash_reason, values = ~n) %>%
        add_pie(hole = 0.6) %>%
        layout(title = "<b>Quantifying reasons for the Civilian Aircrashes</b>", showlegend = T, font=list(size = 18, color = "#000"),margin=m, 
                              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE), height = 500,
                              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    })
    
    d3_m <- reactive({
        aircrash %>% 
            filter(crash_year >= input$yearInput[1],
                   crash_year <= input$yearInput[2],
                   crash_opr_type=="Military") %>%
            select(crash_reason) %>%
            count(crash_reason) 
        
    }) 
    
    output$DonutMilitary <-renderPlotly({
        plot_ly(data= d3_m(),labels = ~crash_reason, values = ~n) %>%
            add_pie(hole = 0.6) %>%
            layout(title = "<b>Quantifying reasons for the Military Aircrashes</b>", showlegend = T, font=list(size = 18, color = "#000"),margin=m, 
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE), height = 500,
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        
    })
    
    d4 <- reactive({
        aircrash %>%
            dplyr::filter(crash_opr_type=="Civilian") %>%
            dplyr::select(operator) %>%
            dplyr::group_by(operator) %>%
            dplyr::summarise(count=n()) %>%
            dplyr::arrange(desc(count)) %>%
            dplyr::top_n(input$numberInput)
        
    }) 
    
    output$CivilianOperators <-renderPlotly({
        
        a4 <- ggplot(d4(), aes(x=reorder(operator, -count), y=count))+
            geom_bar(stat="identity", fill='steelblue')+
            theme_bw()+
            labs(y = "Number of Aircrashes",x="Cvilian Operators",title = "Number of Aircrashes by Civilian Operators")+
            theme(plot.title = element_text(color = "black", size = 20, face = "bold", hjust = 0.5), 
                  axis.title.x =element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
                  axis.title.y = element_text(color = "black", size = 14, face = "bold", hjust = 0.5))+
            expand_limits(y=c(0,200))+ 
            scale_y_continuous(breaks=seq(0, 200, 20))+
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
        ggplotly(a4, source = "select", tooltip = "count")
    })
    
    d5 <- reactive({
        aircrash %>%
            dplyr::filter(crash_opr_type=="Military") %>%
            dplyr::select(operator) %>%
            dplyr::group_by(operator) %>%
            dplyr::summarise(count=n()) %>%
            dplyr::arrange(desc(count)) %>%
            dplyr::top_n(input$numberInput)
        
    })
    
    output$MilitaryOperators <-renderPlotly({
        
        a5 <- ggplot(d5(), aes(x=reorder(operator, -count), y=count))+
            geom_bar(stat="identity", fill='steelblue')+
            theme_bw()+
            labs(y = "Number of Aircrashes",x="Military Operators",title = "Number of Aircrashes by Military Operators")+
            theme(plot.title = element_text(color = "black", size = 20, face = "bold", hjust = -0.35), 
                  axis.title.x =element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
                  axis.title.y = element_text(color = "black", size = 14, face = "bold", hjust = 0.5))+
            expand_limits(y=c(0,200))+ 
            scale_y_continuous(breaks=seq(0, 200, 20))+
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
        ggplotly(a5, source = "select", tooltip = "count")
    })
    
    d6<- reactive({
        aircrash_geocoded %>%
            filter(crash_year == input$YrsInput)
        
    })
    
    output$map <- renderLeaflet({ 
        leaflet() %>% 
            addTiles() %>% 
            addCircles(data = d6(), lng = ~lon, lat = ~lat,
                       popup  = paste("<b>Crash Country:</b>", d6()$crash_country,"<br>",
                                     "<b>Crash Area:</b>", d6()$crash_area, "<br>",
                                     "<b>Aircraft Type:</b>", d6()$type, "<br>",
                                     "<b>Summary:</b>", d6()$summary), weight = 15, radius = 30)
    })
    
    
    wordcloud_rep <- repeatable(wordcloud)
    output$WordCloudPlot <- renderPlot({par(mar = rep(0, 4))
        wordcloud_rep(names(v), v, scale=c(4.5,1),
                      min.freq = 20, max.words=input$numberInput2,
                      colors=brewer.pal(8, "Dark2"))
        }, width = 900, height = 350)
    
    d7<- reactive({
        dd %>%
            dplyr::top_n(input$numberInput2)
    })
        output$WordCloudBarPlot <-renderPlotly({
    a7 <- ggplot(d7(), aes(x=reorder(word, -frequency), y=frequency))+
        geom_bar(stat="identity", fill='steelblue')+theme_bw()+
        theme(plot.title = element_text(color = "black", size = 20, face = "bold", hjust = 0.5), 
              axis.title.x =element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
              axis.title.y = element_text(color = "black", size = 14, face = "bold", hjust = 0.5))+
        labs(y = "Frequency",x="Words",title = "Word Cloud Frequency")+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(a7, source = "select", tooltip = "frequency")
})
        
        d8<- reactive({
            req(input$numberInput2)
            if (input$numberInput2 <= 17) {
                common_phrases %>%
                    dplyr::top_n(input$numberInput2)
            }
            else {
                common_phrases %>%
                    dplyr::top_n(17)
            }
        })
        
        output$AircrashPhrases <-renderPlotly({
            a8 <- ggplot(d8(), aes(x=reorder(word, -frequency), y=frequency))+
                geom_bar(stat="identity", fill='steelblue')+theme_bw()+
                theme(plot.title = element_text(color = "black", size = 20, face = "bold", hjust = 0.5), 
                      axis.title.x =element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
                      axis.title.y = element_text(color = "black", size = 14, face = "bold", hjust = 0.5))+
                labs(y = "Frequency",x="Words",title = "Common Phrases Frequency")+
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
            ggplotly(a8, source = "select", tooltip = "frequency")  
            
        })
        

    
    output$table <- DT::renderDataTable(DT::datatable({
        data <- aircrash1
        if (input$crashyear != "All") {
            data <- data[data$crash_year == input$crashyear,]
        }
        if (input$crashreason != "All") {
            data <- data[data$crash_reason == input$crashreason,]
        }
        if (input$crashoprtype != "All") {
            data <- data[data$crash_opr_type == input$crashoprtype,]
        }
        data
    }, options = list(
        autoWidth = TRUE,
        columnDefs = list(list( targets = 19, width = '600px')),
        scrollX = TRUE
    )))
}
    
    



# Run the application 
shinyApp(ui = ui, server = server)
