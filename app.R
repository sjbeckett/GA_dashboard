library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("COVID-19 in Georgia"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

	  
		# Input: Slider for the number of bins ----
		sliderInput(inputId = "Windowlength",
                  label = "Number of prior days to show:",
                  min = 30,
                  max = as.double(diff(c(as.Date("2020-03-04"),Sys.Date()))),
				  step = 1,
                  value = 90),
				  
		sliderInput(inputId = "Ascertainment",
                  label = "Ascertainment bias:",
                  min = 1,
                  max = 15,
                  value = c(5,10)),


		helpText("Ascertainment bias is the expected number of unrecorded cases for every recorded case and is the difference between our ability to measure those who are infectious and those who are actually infectious."),

		helpText(tags$h3("Acknowledgements"),"This page shows COVID-19 related data for Georgia collected by ",tags$a(href="http://www.covidtracking.com","The COVID Tracking Project")," from local health resources. Code for this project is available at",tags$a(href="http://github.com/sjbeckett/GA_dashboard","http://github.com/sjbeckett/GA_dashboard")," and was developed by ",tags$a(href="http://sjbeckett.github.io", "Dr. Stephen Beckett"),"a research scientist at Georgia Tech.",br(),br(),tags$h4("Data notes"),"(1) Prior to 28 August 2020, current hospitalizations are confirmed hospitalized cases of COVID-19. Following this date, this data stream also includes probable hospitalized cases of COVID-19.",br(),"(2) Note that the data collection is based on the reported dates for data, not the date of sympton onset.",br(),"(3) The risk that one (or more) people in a group are infectious is derived from the binomial distribution, assuming an infectious period of ten days and that we are only able to detect a subset of infectious individuals via testing. More detailed analysis of risk at regional levels for multiple countries (including US counties) is available at the",tags$a(href="https://covid19risk.biosci.gatech.edu/","COVID-19 Event Risk Assessment Planning Tool"),"website.",br(),"(4) Measuring the ascertainment bias is difficult. In the early stages of the epidemic this appears to be close to 10; it is likely to be closer to 5 now. Work on trying to quantify the ascertainment bias is ongoing."),

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Histogram ----
      plotOutput(outputId = "distPlot")

    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {

COVIDTRACKER<- read.csv("https://covidtracking.com/data/download/georgia-history.csv")
 output$distPlot <- renderPlot({

Probdeath = rev(COVIDTRACKER$deathProbable)
recordedDeaths =  rev(COVIDTRACKER$death)
recordedDeaths[!is.na(Probdeath)] = recordedDeaths[!is.na(Probdeath)] - Probdeath[!is.na(Probdeath)]
recordedCases= rev(COVIDTRACKER$positive)
recordedHospitalised = rev(COVIDTRACKER$hospitalized)
currentHospitalised = rev(COVIDTRACKER$hospitalizedCurrently)
PCRTotToday =  rev(COVIDTRACKER$totalTestsViralIncrease)
PCRposToday = c(NA,diff(rev(COVIDTRACKER$positiveTestsViral)))
PCRposToday[c(167,168)]=c(1953,2506)  #correct these values (one of them is negative!). Correction made via DPH records.
PCRpercentToday = 100*PCRposToday/PCRTotToday
PCRcumulative = rev(COVIDTRACKER$totalTestsViral)
Dates = as.Date(rev(COVIDTRACKER$date))


LREC = length(recordedCases)

#calculate cases per day and deaths per day
recordedCPD = recordedCases[2:LREC] - recordedCases[1:(LREC-1)]
recordedDPD = recordedDeaths[2:LREC] - recordedDeaths[1:(LREC-1)]
recordedHPD = recordedHospitalised[2:LREC] - recordedHospitalised[1:(LREC-1)]


#7day rolling AVG

RA_PT =c()
RA_PTot=c()
RA_P =c()
RA_C=c()
RA_D=c()
RA_H=c()
RA_T =c()
WINDOW = 7

for(aa in 1:(length(recordedCPD)-WINDOW)){
	RA_C[aa] = mean(recordedCPD[aa:(aa+WINDOW)])
	RA_D[aa] = mean(recordedDPD[aa:(aa+WINDOW)])
	RA_H[aa]  = mean(recordedHPD[aa:(aa+WINDOW)])
	RA_T[aa] = max(Dates[(aa+1):(aa+WINDOW+1)])
	RA_P[aa] = mean(PCRpercentToday[aa:(aa+WINDOW)])
	RA_PTot[aa] = mean(PCRTotToday[aa:(aa+WINDOW)])
	RA_PT[aa] = max(Dates[(aa+1):(aa+WINDOW+1)])
}


TODAY = Dates[length(Dates)]
WINDOWSIZE = input$Windowlength  #let this be a slider.

XLIM = c(as.Date(TODAY) - WINDOWSIZE ,as.Date(TODAY)+1)
TIMES = sort(as.Date(c(paste0("2020-",1:12,"-",rep("01",12)),paste0("2020-",1:12,"-",rep("15",12)))))
TIMES = c(TIMES, sort(as.Date(c(paste0("2021-",1:12,"-",rep("01",12)),paste0("2021-",1:12,"-",rep("15",12))))), Dates[length(Dates)], XLIM[1])
TIMES = sort(TIMES)



#dev.new(width=12, height=10, unit="in")

COL="royalblue"
AXCEX = 1.45	
AXCEX2 = 1.9
par(mfrow=c(4,3),mar= c(3.8,6.1,1,1),oma = c(3,3,3,3))#,pin = c(2,2.4))

plot(0,0,xaxt="n",yaxt="n",col=NA,axes="F",ylab="",xlab="")
text(0,0.4,paste("COVID-19 status for GA\nover the last",WINDOWSIZE,"days"),font=2,cex=2.5)
text(0,-0.2,paste("Reported today (",TODAY,"):\n","+",recordedCPD[LREC-1]," cases   +",recordedHPD[LREC-1]," hospitalizations   \n+",recordedDPD[LREC-1]," deaths",sep=""),font=2,cex=1.5,col="red4")

text(0,-0.7,paste("Total reported:\n",format(recordedCases[LREC],big.mark=",")," cases   ",format(recordedHospitalised[LREC],big.mark=",")," hospitalizations   \n",format(recordedDeaths[LREC],big.mark=",")," deaths",sep=""),font=2,cex=1.5,col="black")

text(0,-1.,paste("Data: covidtracking.com; Figure: @BeckettStephen"),font=2,cex=1,col="black")



#CASES
#per day
plot(Dates[2:(LREC)],recordedCPD,ylab="New recorded\ncases per day",xlab="",pch =19,ylim=c(0,5000),xaxt="n",col=NA,xlim=XLIM,xaxs="i",cex.lab=AXCEX2,cex.axis=AXCEX)
axis.Date(1,at=TIMES,format="%e %b",las=2,cex.axis=AXCEX)
#WEEKENDS
SAT = Dates[4]
SUN = Dates[5]
for (aa in 1:50){
	polygon(c(SAT - 0.5, SUN+0.5,SUN+0.5,SAT-0.5), c(-5000,-5000,500000,500000),col="grey",border=NA)
	SAT = SAT+7
	SUN= SUN+7
}
legend("topleft",c("7 day moving average"),col=COL,lty=1,lwd=5, box.col = "lightblue", bg = "lightblue",cex=1.4)#box.col = rgb(1,0,0,0.5), bg = rgb(1,0,0,0.5))
lines(RA_T,RA_C,lwd=5,col=COL)
points(Dates[2:(LREC)],recordedCPD,pch=19)
box()
#cumulative
plot(Dates,recordedCases,ylab="Cumulative cases",xaxt="n",col=NA,xlab="",xlim=XLIM,xaxs="i",cex.lab=AXCEX2,cex.axis=AXCEX)
axis.Date(1,at=TIMES,format="%e %b",las=2,cex.axis=AXCEX)
#WEEKENDS
SAT = Dates[4]
SUN = Dates[5]
for (aa in 1:50){
	polygon(c(SAT - 0.5, SUN+0.5,SUN+0.5,SAT-0.5), c(-10000,-100000,500000,500000),col="grey",border=NA)
	SAT = SAT+7
	SUN= SUN+7
}
points(Dates,recordedCases,pch=19)
box()




#RISK
ActiveInf = c()
AI_T=c()
Ascertainty = input$Ascertainment
GAPOP = 10617423
WINDOW=14
for(aa in 1:(length(recordedCases)-WINDOW)){
	ActiveInf[aa] = (recordedCases[aa+WINDOW] - recordedCases[aa])*(10/14)
}
AI_T = c(Dates[(1+WINDOW):length(Dates)])
AI_T[length(AI_T)] = AI_T[length(AI_T)]+0.5
pH = Ascertainty[2]*ActiveInf/GAPOP
pL = Ascertainty[1]*ActiveInf/GAPOP

n = 10
n2 = 30
n3 = 75

plot(AI_T,1-(1-pL)^n,col=NA,ylim=c(0,100),ylab="Risk one or more in a\ngroup are infectious (%)",xaxt="n",xaxs="i",xlab="",xlim=XLIM,cex.lab=AXCEX2,cex.axis=AXCEX)
axis.Date(1,at=TIMES,format="%e %b",las=2,cex.axis=AXCEX)
#WEEKENDS
SAT = Dates[4]
SUN = Dates[5]
for (aa in 1:50){
	polygon(c(SAT - 0.5, SUN+0.5,SUN+0.5,SAT-0.5), c(-100,-100,500000,500000),col="grey",border=NA)
	SAT = SAT+7
	SUN= SUN+7
}
abline(h = 0,lty=3,col=rgb(0.6,0.6,0.6))
abline(h = 20,lty=3,col=rgb(0.6,0.6,0.6))
abline(h = 40,lty=3,col=rgb(0.6,0.6,0.6))
abline(h = 60,lty=3,col=rgb(0.6,0.6,0.6))
abline(h = 80,lty=3,col=rgb(0.6,0.6,0.6))
abline(h = 100,lty=3,col=rgb(0.6,0.6,0.6))

XLIMdiff = XLIM[2]-XLIM[1]

polygon(c(AI_T, rev(AI_T)), c(100*(1-(1-pL)^n), rev(100*(1-(1-pH)^n))),col=rgb(1.0, 0, 0, 0.25),border=NA)#,col = 'indianred2',border = NA)
polygon(c(AI_T, rev(AI_T)), c(100*(1-(1-pL)^n2), rev(100*(1-(1-pH)^n2))),col=rgb(1.0, 0, 0, 0.5),border=NA)#,col = 'tomato2',border = NA)
polygon(c(AI_T, rev(AI_T)), c(100*(1-(1-pL)^n3), rev(100*(1-(1-pH)^n3))),col=rgb(1, 0, 0, 0.75),border=NA)#,col = 'red4',border = NA)
rect(XLIM[1],75,XLIM[1]+XLIMdiff*0.45,105,col="lightblue",border=NA)
text(XLIM[1]+XLIMdiff*0.225,87.5, paste("Uncertainty assumes\nbetween",Ascertainty[1],"and",Ascertainty[2],"\nunrecorded cases for\nevery recorded case\n"),cex=1.4)



XIND = length(pL)-20
text( XLIM[2]-XLIMdiff*0.2,mean(c(100*(1-(1-pL[XIND])^n),100*(1-(1-pH[XIND])^n))),paste("Group of",n),cex=1.6,font=2)
text( XLIM[2]-XLIMdiff*0.2,mean(c(100*(1-(1-pL[XIND])^n2),100*(1-(1-pH[XIND])^n2))),paste("Group of",n2),cex=1.6,font=2)
text( XLIM[2]-XLIMdiff*0.2,mean(c(100*(1-(1-pL[XIND])^n3),100*(1-(1-pH[XIND])^n3))),paste("Group of",n3),cex=1.6,font=2)
box()


#DEATHS
#per day
plot(Dates[2:(LREC)],recordedDPD,ylab="New recorded\ndeaths per day",xlab="",pch=19,xaxt="n",col=NA,xlim=XLIM,xaxs="i",cex.lab=AXCEX2,cex.axis=AXCEX)
axis.Date(1,at=TIMES,format="%e %b",las=2,cex.axis=AXCEX)
#WEEKENDS
SAT = Dates[4]
SUN = Dates[5]
for (aa in 1:50){
	polygon(c(SAT - 0.5, SUN+0.5,SUN+0.5,SAT-0.5), c(-100,-100,500000,500000),col="grey",border=NA)
	SAT = SAT+7
	SUN= SUN+7
}
legend("topleft",c("7 day moving average"),col=COL,lty=1,lwd=5, box.col = "lightblue", bg = "lightblue",cex=1.4)# box.col = rgb(1,0,0,0.5), bg = rgb(1,0,0,0.5))
lines(RA_T,RA_D,lwd=5,col=COL)
points(Dates[2:(LREC)],recordedDPD,pch=19)
box()
#cumulative
plot(Dates,recordedDeaths,ylab="Cumulative deaths",xaxt="n",col=NA,xlab="",xlim=XLIM,xaxs="i",cex.lab=AXCEX2,cex.axis=AXCEX)
axis.Date(1,at=TIMES,format="%e %b",las=2,cex.axis=AXCEX)
#WEEKENDS
SAT = Dates[4]
SUN = Dates[5]
for (aa in 1:50){
	polygon(c(SAT - 0.5, SUN+0.5,SUN+0.5,SAT-0.5), c(-10000,-10000,500000,500000),col="grey",border=NA)
	SAT = SAT+7
	SUN= SUN+7
}
points(Dates,recordedDeaths,pch=19)
box()


#CURR H
plot(Dates,currentHospitalised,xlim=XLIM,col=NA,xlab="",xaxt="n",xaxs="i",ylab="Currently hospitalized\nwith COVID-19",cex.lab=AXCEX2,cex.axis=AXCEX,ylim=c(0,max(currentHospitalised,na.rm=TRUE)+(max(currentHospitalised,na.rm=TRUE)-min(currentHospitalised,na.rm=TRUE))*0.04))
axis.Date(1,at=TIMES,format="%e %b",las=2,cex.axis=AXCEX)
#WEEKENDS
SAT = Dates[4]
SUN = Dates[5]
for (aa in 1:50){
	polygon(c(SAT - 0.5, SUN+0.5,SUN+0.5,SAT-0.5), c(-5000,-5000,500000,500000),col="grey",border=NA)
	SAT = SAT+7
	SUN= SUN+7
}

DataChange = which(Dates=="2020-08-28")
abline(v=Dates[DataChange],lwd=1,col="green")
text(Dates[DataChange],200,"Data source now\nincludes probable\nCOVID-19 cases",cex=1.4,pos=4)


points(Dates,currentHospitalised,pch=19)
box()

#HOSPS
#per day
plot(Dates[2:(LREC)],recordedHPD,ylab="New recorded \nhospitalizations per day",xlab="",pch=19,xaxt="n",col=NA,xlim=XLIM,xaxs="i",cex.lab=AXCEX2,cex.axis=AXCEX)
axis.Date(1,at=TIMES,format="%e %b",las=3,cex.axis=AXCEX)
#WEEKENDS
SAT = Dates[4]
SUN = Dates[5]
for (aa in 1:50){
	polygon(c(SAT - 0.5, SUN+0.5,SUN+0.5,SAT-0.5), c(-100,-100,500000,500000),col="grey",border=NA)
	SAT = SAT+7
	SUN= SUN+7
}
legend("topleft",c("7 day moving average"),col=COL,lty=1,lwd=5,inset=-0.01, box.col = "lightblue", bg = "lightblue",cex=1.4)# box.col = rgb(1,0,0,0.5), bg = rgb(1,0,0,0.5))
lines(RA_T,RA_H,lwd=5,col=COL)
points(Dates[2:(LREC)],recordedHPD,pch=19)
box()
#cumulative
plot(Dates,recordedHospitalised,ylab="Cumulative hospitalizations",xaxt="n",col=NA,xlab="",xlim=XLIM,xaxs="i",cex.lab=AXCEX2,cex.axis=AXCEX)
axis.Date(1,at=TIMES,format="%e %b",las=2,cex.axis=AXCEX)
#WEEKENDS
SAT = Dates[4]
SUN = Dates[5]
for (aa in 1:50){
	polygon(c(SAT - 0.5, SUN+0.5,SUN+0.5,SAT-0.5), c(-10000,-10000,500000,500000),col="grey",border=NA)
	SAT = SAT+7
	SUN= SUN+7
}
points(Dates,recordedHospitalised,pch=19)
box()

 
#Testing
#Positivity
 
plot(Dates, PCRpercentToday, col=NA, ylab="% PCR tests positive per day",xlab="",xlim=XLIM,xaxt="n",xaxs="i",cex.lab=AXCEX2,cex.axis=AXCEX)
axis.Date(1,at=TIMES,format="%e %b",las=2,cex.axis=AXCEX)
# #WEEKENDS
SAT = Dates[4]
SUN = Dates[5]
for (aa in 1:50){
 polygon(c(SAT - 0.5, SUN+0.5,SUN+0.5,SAT-0.5), c(-5000,-5000,500000,500000),col="grey",border=NA)
 SAT = SAT+7
 SUN= SUN+7
}
points(Dates, PCRpercentToday,pch=19)
lines(RA_PT,RA_P,col=COL,lwd=5)
legend("topleft",c("7 day moving average"),col=COL,lty=1,lwd=5, box.col = "lightblue", bg = "lightblue",cex=1.4)
box()
 
#Tests per day
 
YL = range(PCRTotToday)
YL[1]= 0
YL[2]=YL[2]*1.1
 
 plot(Dates,PCRTotToday,col=NA,ylab="New PCR tests per day",xlab="",xlim=XLIM,xaxt="n",xaxs="i",cex.lab=AXCEX2,cex.axis=AXCEX,ylim=YL)
 axis.Date(1,at=TIMES,format="%e %b",las=2,cex.axis=AXCEX)
#WEEKENDS
SAT = Dates[4]
SUN = Dates[5]
for (aa in 1:50){
 polygon(c(SAT - 0.5, SUN+0.5,SUN+0.5,SAT-0.5), c(-5000,-5000,500000,500000),col="grey",border=NA)
 SAT = SAT+7
 SUN= SUN+7
}
points(Dates,PCRTotToday,pch=19)
lines(RA_PT,RA_PTot,col=COL,lwd=5)


legend("topleft",c("7 day moving average"),col=COL,lty=1,lwd=5, box.col = "lightblue", bg = "lightblue",cex=1.4)
box()

 
#Cumulative tests
plot(Dates,PCRcumulative,ylab="Cumulative PCR tests",xaxt="n",col=NA,xlab="",xlim=XLIM,xaxs="i",cex.lab=AXCEX2,cex.axis=AXCEX)
axis.Date(1,at=TIMES,format="%e %b",las=2,cex.axis=AXCEX)
 #WEEKENDS
 SAT = Dates[4]
SUN = Dates[5]
for (aa in 1:50){
 polygon(c(SAT - 0.5, SUN+0.5,SUN+0.5,SAT-0.5), c(-500000,-500000,50000000,50000000),col="grey",border=NA)
 SAT = SAT+7
 SUN= SUN+7
}
points(Dates,PCRcumulative,pch=19)


box()
 
 
 
 },width=1152*(0.9),height=1280*(0.9))#960*(0.9))

}

# Create Shiny app ----
shinyApp(ui = ui, server = server)