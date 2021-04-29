
library(shiny)
#install.packages("shinydashboard")
library(shinydashboard)
library(plotly)
#install.packages("semantic.dashboard")
#library(semantic.dashboard)
library(sf)
library(spData)
library(lubridate)
#install.packages("recipes")
library(recipes)
library(stargazer)

#https://www.freecodecamp.org/news/build-your-first-web-app-dashboard-using-shiny-and-r-ec33c9f3f6c/

ui <- fluidPage(
  headerPanel("Solving Uttar Pradesh's Growth Paradox"),
  tabsetPanel(tabPanel(title="Trends Over Time",
                       dashboardPage(  dashboardHeader(title="Data Dashboard"),
                                       dashboardSidebar(
                                         sidebarMenu(
                                           menuItem("Dashboard",
                                                    tabName="dashboard",
                                                    icon=icon("dashboard")),
                                           menuItem(sliderInput(inputId = "Time",
                                                                label = "Timeline",
                                                                min=2004,
                                                                max=2020,
                                                                value=2017,
                                                                step=1)),
                                           menuItem(selectInput("option",
                                                                label="Choose the Indicator",
                                                                choices=list("Industry",
                                                                             "Price and Wages","State Domestic Product"),selected="Industry")))),
                                       dashboardBody(
                                         fluidRow(column(width=12,
                                                         plotlyOutput("plot1")))))),
              
              tabPanel(title="IMF's Sentiments",
                       fluidRow(
                         column(width=4,
                                sliderInput( inputId = "Year_Sentiments",
                                             label = "Timeline",
                                             min=year(as.Date("2010", "%Y")),
                                             max=year(as.Date("2019", "%Y")),
                                             value=year(as.Date("2019", "%Y")),
                                             sep="")),
                         column(width=8,
                                plotOutput("sentiments")
                         )
                       )),
              
              tabPanel(title="Is it Infrastructure?", 
                       fluidRow(
                         column(width=6,
                                verbatimTextOutput(outputId = "reg_tab")),
                         column(width=6,
                                img(src='cap_power.png', align = "center")))
                         )
                       )
  )



server<- function(input, output, session){
  
  #load state data
  #state_data<-read.csv(paste0(getwd(),"/all_state_final.csv"))
  state_data<-read.csv("all_state_final.csv")
  
  #load shape file and add 
  
  #state_shape<- st_read(paste0(getwd(),"/IND_adm1.shp" ))
  state_shape<-st_read("IND_adm1.shp")
  
  
  state_data_sf<-st_sf(inner_join(state_shape,state_data, 
                                  by=c("NAME_1"="state_ut")))
  

  #ind_shape<-st_read(paste0(getwd(),"/Ind shape/IND_adm0.shp"))
  ind_shape<-st_read("IND_adm0.shp")
  
  
  #change state data based on time
  year_state_data<- reactive({
    
    sd<-state_data%>%
      filter(year==input$Time)
    sd
  })
  
  
  #Analysis of Demography and other indicators     
  
  output$plot1 <- renderPlotly({ 
    if (input$option=="Industry") {
      
      plt_1<-ggplot()+
        geom_point(data=subset(year_state_data(),!is.na(state.wise.number.of.factories)),
                   aes(x=state.wise.number.of.factories,
                       y=state_ut))+
        geom_point(data=subset(year_state_data(),!is.na(state.wise.number.of.factories.1)),
                   aes(x=state.wise.number.of.factories.1,
                       y=state_ut))+
        labs(x="State",y="Number Of Factories",
             title="A Comparison of Number of Factories Across India")+
        theme_minimal()
      
      ggplotly(plt_1)
    } 
    else if (input$option=="State Domestic Product") 
    {   
      plt_1<-ggplot()+
        geom_point(data=subset(year_state_data(),!is.na(net.state.value.added.by.economic.activity.manufacturing.1)),
                   aes(x=as.numeric(net.state.value.added.by.economic.activity.manufacturing.1),y=state_ut))+
        geom_point(data=subset(year_state_data(),!is.na(net.state.value.added.by.economic.activity.manufacturing.2)),
                   aes(x=as.numeric(net.state.value.added.by.economic.activity.manufacturing.2),y=state_ut))+
        geom_point(data=subset(year_state_data(),!is.na(net.state.value.added.by.economic.activity.manufacturing.3)),
                   aes(x=as.numeric(net.state.value.added.by.economic.activity.manufacturing.3),y=state_ut))+
        geom_point(data=subset(year_state_data(),!is.na(net.state.value.added.by.economic.activity.manufacturing.4)),
                   aes(x=as.numeric(net.state.value.added.by.economic.activity.manufacturing.4),y=state_ut))+
        geom_point(data=subset(year_state_data(),!is.na(net.state.value.added.by.economic.activity.manufacturing.5)),
                   aes(x=as.numeric(net.state.value.added.by.economic.activity.manufacturing.5),y=state_ut))+
        labs(x="Economic Value Added BY Manufacturing",y="State",
             title="A Comparison of Economic Valued Added By Manufacturing Across India")+
        theme_minimal()
      
      
      ggplotly(plt_1)
    }
    else if(input$option=="Price and Wages"){
      
      plt_1<-ggplot()+
        geom_point(data=subset(year_state_data(),!is.na(state.wise.average.daily.wage.rates.in.rural.india)),
                   aes(x=as.numeric(state.wise.average.daily.wage.rates.in.rural.india),
                       y=state_ut))+
        geom_point(data=subset(year_state_data(),!is.na(state.wise.average.daily.wage.rates.in.rural.india.1)),
                   aes(x=as.numeric(state.wise.average.daily.wage.rates.in.rural.india.1),
                       y=state_ut))+
        geom_point(data=subset(year_state_data(),!is.na(state.wise.average.daily.wage.rates.in.rural.india.2)),
                   aes(x=as.numeric(state.wise.average.daily.wage.rates.in.rural.india.2),
                       y=state_ut))+
        geom_point(data=subset(year_state_data(),!is.na(state.wise.average.daily.wage.rates.in.rural.india.3)),
                   aes(x=as.numeric(state.wise.average.daily.wage.rates.in.rural.india.3),
                       y=state_ut))+
        labs(x="Daily Rural Wage Rate",y="State",title="A Comparsion of Rural Wage Rates across India")+
        theme_minimal()
      
      ggplotly(plt_1)
    }
    
  })
  
  #Sentiment Analysis      
  
  plot_figs<- reactive({
    
    data<- 
      read.csv(paste0(getwd(),"/comp_sentiment.csv"))%>%
      filter(doc==as.numeric(input$Year_Sentiments))%>%
      select(sentiment)
    
    d<-with(density(data$sentiment), data.frame(x,y))
    
    d
  })
  
  output$sentiments<- renderPlot({
    
    ggplot(plot_figs(), aes(x=x, y=y))+
      geom_line()+
      geom_area(mapping=aes(x=ifelse(x>=0, x,0)), fill="dark green")+
      geom_area(mapping=aes(x=ifelse(x<=0, x,0)), fill="dark red")+
      scale_y_continuous(limits = c(0,1.5)) +
      #scale_x_continuous(breaks=seq(-2,2,0.25)) +
      theme_minimal(base_size = 12) +
      xlim(-2,2)+
      labs(x = "Sentiment", 
           y = "", 
           title = paste0("Article IV IMF Report for India: ","Sentiment Distribution"))+
      theme(plot.title = element_text(hjust = 0.25), 
            axis.text.y=element_blank())
    
    
    #https://medium.com/@ODSC/an-introduction-to-sentence-level-sentiment-analysis-with-sentimentr-ac556bd7f75a
    
  })
  
  #Regressions
  plot_reg<- reactive({
    d<-state_data%>%
      select(state_ut,year,input$independent_var,input$dep_var)%>%
      filter(year==2019)
    d
  })
  
  up_infra<-read.csv(paste0(getwd(),"/up_infra.csv"))
  
  diff_in_power=up_infra$power_requirement-up_infra$availability_of_power
  
  
  output$reg_tab=
    renderPrint({summary(lm(value_added_manufacturing~
                              state_highway_length+installed_capacity_of_power+diff_in_power,data=up_infra)
                         
    )})
  
}

shinyApp(ui, server)

#https://pranjalchandra.shinyapps.io/final-project-pranjal/
  