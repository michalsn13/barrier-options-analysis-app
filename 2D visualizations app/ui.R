shinyUI(fluidPage(
  headerPanel(title="Wycena opcji metodą Finite Difference-porównanie rodzajów opcji"),
  fluidRow(
    sliderInput("moment","Moment czasu",0,0.75,step = 0.025,value=0,width='50%',animate=animationOptions(interval = 3000))
  ),
  fluidRow(
    column(1, offset = 0,
           div(style = "font-size: 11px;
                   padding: 10px 0px;
                   margin:10%;
                   width:200%",
               fluidRow(
                 radioButtons('freeze','Ładowanie:',choices=c('Go','Stop'),selected="Go",inline=T),
                 radioButtons('divtype','Rodzaj dywidendy:',choices=c("%","zł"),selected="%",inline=T),
                 radioButtons("type","Typ opcji:",choices=c("call","put"),selected="call",inline=T)
               ),
               fluidRow(
                 sliderInput("strike","Cena wykonania:",50,3000,step = 50,value=2150,width='85%')
               ),
               fluidRow(
                 sliderInput("Time","Okres zapadalności:",0.25,2,step = 0.25,value=0.75,width='85%')
               ),
               fluidRow(
                 sliderInput("barrier","Bariera:",50,3000,step = 50,value=2400,width='85%')
               ),
               fluidRow(
                 sliderInput("r","Stopa wolna od ryzyka:",0,0.2,step = 0.001,value=0.03,width='85%')
               ),
               fluidRow(
                 sliderInput("div","Dywidenda (%):",0,30,step = 5,value=10,width='85%')
               ),
               fluidRow(
                 sliderInput("sigma_l","Zmienność-próg dolny:",0,0.5,step = 0.05,value=0.15,width='85%')
               ),
               fluidRow(
                 sliderInput("sigma_p","Zmienność-próg górny:",0.15,0.35,step = 0.05,value=0.25,width='85%')
               ),
               fluidRow(
                 sliderInput("limit","Ograniczenie wykresu",0,3500,step = 100,value=0,width='85%')
               )
           )
    ),
    mainPanel(column(10,offset=2,plotOutput("plot",width = "155%",height="600px")))
)
))
