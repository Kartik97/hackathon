library(shiny)
library(shinydashboard)
library(tseries)
library(reshape)
library(dplyr)
library(forecast)
read<-read.csv("rainfall.csv",stringsAsFactors = FALSE)
read<-read[1:36,2]
read<-as.character(read)
crop_data<-read.csv("rainmonth.csv",stringsAsFactors = F)
crop_name<-as.character(crop_data$crop)
ui<-dashboardPage(
  
  
  dashboardHeader(title="Rainfall Prediction"),
  
  
  dashboardSidebar(
    
    sidebarMenu(
              id="sidebarmenu",
      menuItem("Welcome",tabName="nodata"),        
      menuItem("plot",tabName = "Plot",icon=icon("line-chart")),
      conditionalPanel("input.sidebarmenu==='Plot'",
                       selectInput(inputId="region",label = "Select Region",read),
                       selectInput(inputId="year",label="Select Year",2010:2014),
                       selectInput(inputId="season",label="Select the Season",c("SUMMER","SPRING","WINTER","NONE"),selected = "NONE")
      ),
      menuItem("Crop according to season",tabName ="CSeason",icon=icon("line-chart")),
      conditionalPanel("input.sidebarmenu==='CSeason'",
                       selectInput(inputId="regioncrop",label="Select Region",c("WEST UTTAR PRADESH")),
                       selectInput(inputId = "seasoncrop",label="Select Season",c("SUMMER","SPRING","WINTER","NONE"))
      ),
      
      menuItem("Season According to crops",tabName="SCrops",icon=icon("file-text-o")),
                 
                conditionalPanel("input.sidebarmenu==='SCrops'",
                                  selectInput(inputId="cropname",label="Select the Crop",crop_name)                             
                                 ),
      menuItem("Suitable Crops",tabName = "Suitable",icon = icon("file-text-o")),
                conditionalPanel("input.sidebarmenu==='Suitable'",
                                  selectInput(inputId = "cropName",label = "Select the Crop",crop_name)
                                 )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Plot",fluidRow(box(plotOutput("RainPlot"),title = "Predicted Plot"),box(tableOutput("RainData"),title = "Predicted rainfall data")),fluidRow(box(plotOutput("RainPlot1"),title = "Actual Plot"),box(tableOutput("RainData1"),title = "Actual rainfall data"))),
      tabItem(tabName = "nodata",fluidPage(h1("Welcome to the project"))),
      tabItem(tabName="CSeason",fluidPage(tableOutput("scrop"))),
      tabItem(tabName = "SCrops",fluidPage(tableOutput("cseason"))),
      tabItem(tabName="Suitable",fluidPage(tableOutput("")))
    )
  )
)

server<-function(input, output){
  
  data<-read.csv("rainfall.csv",stringsAsFactors = FALSE)
  output$RainPlot<-renderPlot({data<-filter(data,SD_Name==input$region)
  
  ####   JAN-APR
  
  testjan<-filter(data,YEAR<=input$year)
  trainjan<-filter(data,YEAR<(input$year))
  trainjan<-select(trainjan,YEAR,JAN:APR)
  testjan<-select(testjan,YEAR,JAN:APR)
  trainjan<-melt(trainjan,c("YEAR"))
  testjan<-melt(testjan,c("YEAR"))
  trainjan<-arrange(trainjan,YEAR)
  testjan<-arrange(testjan,YEAR)
  trainjan<-ts(trainjan$value,frequency=4)
  testjan<-ts(testjan$value,frequency=4)
  #output$RainPlot<-renderPlot(plot(decompose(trainjan)))
  jantrend<-decompose(trainjan)$trend
  #jantrendtest<-decompose(testjan)$trend
  janseasonal<-decompose(trainjan)$seasonal
  janrandom<-decompose(trainjan)$random
  jantrend<-na.omit(jantrend)
  #jantrendtest<-na.omit(jantrendtest)
  janrandom<-na.omit(janrandom)
  jantrend<-ts(jantrend,frequency=4)
  #jantrendtest<-ts(jantrendtest,frequency = 4)
  janseasonal<-ts(janseasonal,frequency=4)
  janrandom<-ts(janrandom,frequency = 4)
  #output$RainPlot<-renderPlot(plot(janrandom))
  modjantrend<-auto.arima(jantrend)
  modjanrandom<-auto.arima(janrandom)
  #modjantrend<-Arima(jantrend,order=c(3,0,0),seasonal=list(order=c(2,0,0),period=4))
  #modjanrandom<-Arima(janrandom,order=c(3,0,0),seasonal=list(order=c(2,0,0),period=4))
  
  forjantrend<-forecast(modjantrend,h=4)
  forjanrandom<-forecast(modjanrandom,h=4)
  forjantrend<-as.data.frame(forjantrend)
  janseas<-head(janseasonal,4)
  janseas<-as.data.frame(janseas)
  forjanrandom<-as.data.frame(forjanrandom)
  janseas<-as.data.frame(janseas)
  
  janfinal<-janseas$x+forjantrend$`Point Forecast`+forjanrandom$`Point Forecast`
  #output$RainPlot<-renderPlot(plot(janfinal))
  
  
  testjan1<-filter(data,YEAR==input$year)
  testjan1<-filter(testjan1,SD_Name==input$region)
  testjan1<-select(testjan1,YEAR,JAN:APR)
  testjan1<-melt(testjan1,c("YEAR"))
  testjan1<-ts(testjan1$value,frequency=4)
  janfinal<-ifelse(janfinal>0,janfinal,0)
  
  #MAY-OCT
  
  testmay<-filter(data,YEAR<=input$year)
  trainmay<-filter(data,YEAR<(input$year))
  trainmay<-select(trainmay,YEAR,MAY:OCT)
  testmay<-select(testmay,YEAR,MAY:OCT)
  trainmay<-melt(trainmay,c("YEAR"))
  testmay<-melt(testmay,c("YEAR"))
  trainmay<-arrange(trainmay,YEAR)
  testmay<-arrange(testmay,YEAR)
  trainmay<-ts(trainmay$value,frequency=6)
  testmay<-ts(testmay$value,frequency=6)
  #output$RainPlot<-renderPlot(plot(decompose(trainmay)))
  maytrend<-decompose(trainmay)$trend
  #maytrendtest<-decompose(testmay)$trend
  mayseasonal<-decompose(trainmay)$seasonal
  mayrandom<-decompose(trainmay)$random
  maytrend<-na.omit(maytrend)
  #maytrendtest<-na.omit(maytrendtest)
  mayrandom<-na.omit(mayrandom)
  maytrend<-ts(maytrend,frequency=6)
  #maytrendtest<-ts(maytrendtest,frequency = 4)
  mayseasonal<-ts(mayseasonal,frequency=6)
  mayrandom<-ts(mayrandom,frequency = 6)
  #output$RainPlot<-renderPlot(plot(mayrandom))
  modmaytrend<-auto.arima(maytrend)
  modmayrandom<-auto.arima(mayrandom)
  #modmaytrend<-Arima(maytrend,order=c(3,0,0),seasonal=list(order=c(2,0,0),period=4))
  #modmayrandom<-Arima(mayrandom,order=c(3,0,0),seasonal=list(order=c(2,0,0),period=4))
  
  formaytrend<-forecast(modmaytrend,h=6)
  formayrandom<-forecast(modmayrandom,h=6)
  formaytrend<-as.data.frame(formaytrend)
  mayseas<-head(mayseasonal,6)
  mayseas<-as.data.frame(mayseas)
  formayrandom<-as.data.frame(formayrandom)
  mayseas<-as.data.frame(mayseas)
  
  mayfinal<-mayseas$x+formaytrend$`Point Forecast`+formayrandom$`Point Forecast`
  #output$RainPlot<-renderPlot(plot(mayfinal))
  
  
  testmay1<-filter(data,YEAR==input$year)
  testmay1<-filter(testmay1,SD_Name==input$region)
  testmay1<-select(testmay1,YEAR,MAY:OCT)
  testmay1<-melt(testmay1,c("YEAR"))
  testmay1<-ts(testmay1$value,frequency=4)
  mayfinal<-ifelse(mayfinal>0,mayfinal,0)
  
  ##NOV-DEC
  
  testnov<-filter(data,YEAR<=input$year)
  trainnov<-filter(data,YEAR<(input$year))
  trainnov<-select(trainnov,YEAR,MAY:OCT)
  testnov<-select(testnov,YEAR,MAY:OCT)
  trainnov<-melt(trainnov,c("YEAR"))
  testnov<-melt(testnov,c("YEAR"))
  trainnov<-arrange(trainnov,YEAR)
  testnov<-arrange(testnov,YEAR)
  trainnov<-ts(trainnov$value,frequency=2)
  testnov<-ts(testnov$value,frequency=2)
  #output$RainPlot<-renderPlot(plot(decompose(trainnov)))
  novtrend<-decompose(trainnov)$trend
  #novtrendtest<-decompose(testnov)$trend
  novseasonal<-decompose(trainnov)$seasonal
  novrandom<-decompose(trainnov)$random
  novtrend<-na.omit(novtrend)
  #novtrendtest<-na.omit(novtrendtest)
  novrandom<-na.omit(novrandom)
  novtrend<-ts(novtrend,frequency=2)
  #novtrendtest<-ts(novtrendtest,frequency = 4)
  novseasonal<-ts(novseasonal,frequency=2)
  novrandom<-ts(novrandom,frequency = 2)
  #output$RainPlot<-renderPlot(plot(novrandom))
  modnovtrend<-auto.arima(novtrend)
  modnovrandom<-auto.arima(novrandom)
  #modnovtrend<-Arima(novtrend,order=c(3,0,0),seasonal=list(order=c(2,0,0),period=4))
  #modnovrandom<-Arima(novrandom,order=c(3,0,0),seasonal=list(order=c(2,0,0),period=4))
  
  fornovtrend<-forecast(modnovtrend,h=2)
  fornovrandom<-forecast(modnovrandom,h=2)
  fornovtrend<-as.data.frame(fornovtrend)
  novseas<-head(novseasonal,2)
  novseas<-as.data.frame(novseas)
  fornovrandom<-as.data.frame(fornovrandom)
  novseas<-as.data.frame(novseas)
  
  novfinal<-novseas$x+fornovtrend$`Point Forecast`+fornovrandom$`Point Forecast`
  #output$RainPlot<-renderPlot(plot(novfinal))
  
  
  testnov1<-filter(data,YEAR==input$year)
  testnov1<-filter(testnov1,SD_Name==input$region)
  testnov1<-select(testnov1,YEAR,MAY:OCT)
  testnov1<-melt(testnov1,c("YEAR"))
  testnov1<-ts(testnov1$value,frequency=4)
  novfinal<-ifelse(novfinal>0,novfinal,0)
  
  
  janfinal<-as.data.frame(janfinal)
  mayfinal<-as.data.frame(mayfinal)
  novfinal<-as.data.frame(novfinal)
  jan<-as.numeric(janfinal$x)
  jan<-as.data.frame(jan)
  jan<-rename(jan,rain=jan)
  
  may<-as.numeric(mayfinal$x)
  may<-as.data.frame(may)
  may<-rename(may,rain=may)
  
  nov<-as.numeric(novfinal$x)
  nov<-as.data.frame(nov)
  nov<-rename(nov,rain=nov)
  if(input$season=="NONE"){
  final<-rbind(jan,may,nov)
  final_data<<-final
    final<-ts(final$rain,frequency=12)
  }
  else if(input$season=="SPRING"){
    final<-rbind(jan)
    final_data<<-final
  final<-ts(final$rain,frequency=4)
  }
  else if(input$season=="SUMMER"){
    final<-rbind(may)
    final_data<<-final
    final<-ts(final$rain,frequency=6)
  }
  else{
    final<-rbind(nov)
    final_data<<-final
    final<-ts(final$rain,frequency=2)
  }
    
  #output$RainPlot<-renderPlot(plot(final,col='red'))
  plot(final)
  })
  output$RainData<-renderTable({final_data})
  output$RainPlot1<-renderPlot({
    finaltest<-filter(data,YEAR==input$year)
    finaltest<-filter(finaltest,SD_Name==input$region)
    if(input$season=="NONE"){
      finaltest<-select(finaltest,YEAR,JAN:DEC)
      finaltest<-melt(finaltest,c("YEAR"))
      finaltestData<<-finaltest$value
      finaltest<-ts(finaltest$value,frequency=12)
    }
    else if(input$season=="SPRING"){
      finaltest<-select(finaltest,YEAR,JAN:APR)
      finaltest<-melt(finaltest,c("YEAR"))
      finaltestData<<-finaltest$value
      finaltest<-ts(finaltest$value,frequency=4)
    }
    else if(input$season=="SUMMER"){
      finaltest<-select(finaltest,YEAR,APR:OCT)
      finaltest<-melt(finaltest,c("YEAR"))
      finaltestData<<-finaltest$value
      finaltest<-ts(finaltest$value,frequency=6)
    }
    else{
      finaltest<-select(finaltest,YEAR,NOV:DEC)
      finaltest<-melt(finaltest,c("YEAR"))
      finaltestData<<-finaltest$value
      finaltest<-ts(finaltest$value,frequency=2)
    }
    
  
    plot(finaltest)
    })
  output$RainData1<-renderTable({finaltestData})
  #output$RainPlot1<-renderPlot( plot(finaltest))
  output$scrop<-renderTable({
        if(input$seasoncrop=="SPRING")
            crops_data<-filter(crop_data,(crop_data$startmonth=="JAN"|crop_data$startmonth=="FEB"|crop_data$startmonth=="MAR"))
        
        else  if(input$seasoncrop=="SUMMER")
          crops_data<-filter(crop_data,(crop_data$startmonth=="APR"|crop_data$startmonth=="MAY"|crop_data$startmonth=="JUN"|crop_data$startmonth=="JUL"|crop_data$startmonth=="OCT"))
        else
          crops_data<-filter(crop_data,(crop_data$startmonth=="NOV"|crop_data$startmonth=="DEC"))
        crops_data
  })
  output$cseason<-renderTable({
      crop_data[crop_data$crop==input$cropname,]
  })
  output$nextCrop<-renderTable({
      
  })
}
shinyApp(ui,server)
