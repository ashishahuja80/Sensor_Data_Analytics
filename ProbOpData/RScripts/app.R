############version_02#################

###########clear workspace##############
rm(list=ls())
gc()

###########Load Packages###############
library(shiny)
# library(shinyjs)
library(ggplot2)
library(KernSmooth)
library(ash)
library(geometry)
library(dygraphs)
library(xts)
# library(sp)
# library(dplyr)
# library(rgeos)
# library(plotly)


print(Sys.getenv('R_LIBS_USER'))

#########Set Paths###############

in_path<-"C:/data/work/Data_Analytics/ProbOpData_Local_Repo/z0026h4e/Data_WinTS_version"
# in_path<-"C:/Shiny/Data"
# in_path<-"file://Md1k6d2c/e$/2015/Probabilistic_Design/TOOLS/Shiny/Data"
# in_path<-"//Md1k6d2c/e$/2015/Probabilistic_Design/TOOLS/Shiny/Data"
# in_path<-"//Md1k6d2c/u$/Shiny/Data"


out_path<-"C:/data/work/Data_Analytics/ProbOpData_Local_Repo/z0026h4e/Output/"
# out_path<-"C:/Shiny/Output/"
# out_path<-"//Md1k6d2c/e$/2015/Probabilistic_Design/TOOLS/Shiny/Output/"
# out_path<-"//Md1k6d2c/u$/Shiny/Output/"

# scripts_path<-"Y:/Rscripts"
# scripts_path<-"C:/Shiny/Rscripts"
scripts_path<-"C:/data/work/Data_Analytics/ProbOpData_Local_Repo/z0026h4e/ProbOpData/RScripts"
# scripts_path<-"C:/Shiny/Code/rscripts"
# scripts_path<-"//Md1k6d2c/e$/2015/Probabilistic_Design/TOOLS/Shiny/Rscripts"
# scripts_path<-"E:/2015/Probabilistic_Design/TOOLS/Shiny/Rscripts"
# scripts_path<-"//Md1k6d2c/u$/Shiny/Rscripts"

####Load Projects for in_path#########

dr<-list.dirs(paste0(in_path), recursive=FALSE, full.name=FALSE) # directories inside in_path
d<-data.frame(dr)
de<-list.files(in_path, pattern="*.txt", recursive=TRUE) #files inside in_path
# current_project<-""

#####Define USER INTERFACE FOR SHINY###############


ui<-fluidPage(
	
	titlePanel("ProbOpData v2.0 -- PROBABILISTIC DESIGN & DATA ANALYTICS"),
	sidebarLayout(
		sidebarPanel(width=4,
			selectInput(inputId="sel1",label="Select Project",choices=unique(as.character(d$dr))),
			selectInput(inputId="sel2",label="Select Plant", choices=list()),
			
			fluidRow(column(width=6,radioButtons("load_data",label=("Select Source"), 
			                                     choices= list("Last Analysed Output" = 1, 
			                                                   "New Analysis-All Data"=2,"Event Data - NONE"=3), selected=1)
			                # checkboxInput("event_data",label="Event Data")
			                ),
			         column(width=6,
			                # checkboxGroupInput("extent", label =("Data in Extent : NO"),choiceNames=list("1D Filter","2D Filter","nD Filter"),
			                #                    choiceValues=list(1,2,3),selected = NULL))
			                radioButtons("extent", label =("Data in Extent : NO"),choices=list("No Filter"=0,"1D Filter"=1,"2D Filter"=2,"nD Filter"=3),
			                             selected = 0))
			        ),
			conditionalPanel(condition="input.load_data==1",
			                 selectInput(inputId="last_ext",label="Select Last Analysis", choices=list("No Selection"="No Selection"),selected="No Selection")
			                 ),
			fluidRow(column(width=7,h4("Select Signals")),
			         column(width = 5,actionButton("gen_plot", label = "GENERATE PLOT",width="100%"))),
			selectInput(inputId="sig1", label="Signal 1",choices=list()),
			
			selectInput(inputId="sig2", label="Signal 2",choices=list())
			
			#selectInput(inputId="sig3", label="Signal 3",choices=list())
			
		),
		
		mainPanel(navbarPage(title="Statistics",id="stat",
		# mainPanel(navbarPage(id="stat",
		    tabPanel("Data Processing",
		             tabsetPanel(id="datapro_tabs",
		               tabPanel("General Information",
		                        htmlOutput("Plant"),
		                        # tableOutput("gen_info"),
		                        hr(),
		                        fluidRow(column(width=3,selectInput(inputId = "rcodefile",label="RCode Creation",choices=c("Enabled"=1,"Disabled"=2),selected=2))),
		                        conditionalPanel(condition="input.rcodefile==1",
		                                         hr(),
		                                         fluidRow(column(width=4,selectInput(inputId = "rcodeproject",label="Select Project",choices=list("No Selection"="No Selection"),
		                                                     selected="No Selection")),
		                                                  column(width=4,selectInput(inputId = "rcodeplant",label="Select Plant",choices=list("No Selection"="No Selection"),
		                                                                             selected="No Selection")),
		                                                  column(width=2,"CONTROLS",actionButton(inputId = "rcodecreate",label="CREATE"))))
		                        ),
		               tabPanel("Boundary Conditions",
                                     fluidRow(
                                       column(width=10,
                                              fluidRow(column(width=9,textInput(inputId = "des_cdn",label=h5("Description"), value="", width="100%")),
                                                       column(width=3,selectInput(inputId="der_sig",label=h5("Derived Signal"),choices=c(Yes="Yes",No="No"),selected="No"))
                                              ),
                                              fluidRow(column(width=6,selectInput(inputId="sig1_cdn",label=h5("Signal-1"),width="100%",choices=list("No Selection"="No Selection"))),
                                                       column(width=3,selectInput(inputId="op_cdn",label=h5("Operator"),choices=list(" "= " ", "("= "(",")"= ")","==","==", "<"="<","<="="<=",">"=">",">="=">=",
                                                                                                                                     "+"="+","-"="-","*"="*","/"="/","^"="^"))),
                                                       column(width=3,numericInput("val_cdn", label = h5("Value"), value = ""))
                                              ),
                                              fluidRow(column(width=10,textOutput("cdn_bld")))
                                       ),
                                       column(width=2, h4("Controls"),
                                              fluidRow(column(width =10 ,actionButton("add_cdn", label = "Add",width="100%"))
                                              ),
                                              br(),
                                              fluidRow(column(width=10,actionButton("rem_cdn", label = "Rem",width="100%"))),
                                              br(),
                                              fluidRow(column(width =10 ,actionButton("end_cdn", label = "Add->List",width="100%"))),
                                              br(),
                                              fluidRow(column(width =10 ,actionButton("rem_end_cdn", label = "Del<-List",width="100%")))
                                              # ))
                                              # column(width =3 ,actionButton("app_cdn", label = "Apply",width="100%"))
                                       )
                                     ),
		                        conditionalPanel(condition="input.der_sig=='No'",
  		                        conditionalPanel(condition="input.op_cdns=='EDIT'",
  		                                         fluidRow(column(width=7,verbatimTextOutput("edit_cdn"),tags$style(type="text/css", "#edit_cdn {white-space: pre-wrap;}")),
  		                                                  column(width=5,fluidRow(column(width=3,actionButton("prev_cdn",label="Prev")),
  		                                                                          column(width=3,actionButton("nxt_cdn",label="Next"))),
  		                                                                 br(),
  		                                                                 fluidRow(column(width=3,actionButton("upd_cdn",label="Upd")))
  		                                                         )
  		                                                  )
  		                                         ),
  		                        
  		                        hr(),
  
  		                        fluidRow(column(width=10,
  		                                  fluidRow(column(width=6,selectInput(inputId="cdns",label="Added Conditions - Select",width="100%",choices=list())),
  		                                           column(width=3,selectInput(inputId="op_cdns",label="Operator",width="100%",choices=list(" "= " ",
  		                                                                                                                                   "("="(",")"=")","& - AND"="&","| - OR"="|","! - NOT"="!","EDIT"="EDIT"
  		                                                                                                                                  # "&* - AND_ALL"="&*","|* - OR_ALL"="|*", 
  		                                                                                                                                   # "Custom"="Custom"
  		                                                                                                                                   )),selected=" "),
  		                                           column(width = 3,
  		                                                  fluidRow(column(width =6 ,actionButton("add_cdns", label = "Add",width="100%")),
  		                                                           column(width =6,actionButton("rem_cdns", label = "Rem",width="100%"))),
  		                                                  br(),
  		                                                  fluidRow(column(width =6,actionButton("add_LS", label = "LS+",width="100%")),
  		                                                           column(width =6,actionButton("rem_LS", label = "LS-",width="100%")))
  		                                                  # helpText("LS+ :Add to Logical String"),
  		                                                  # helpText("LS- : Remove from Logical String")
  		                                           )
  		    
  		                                           ),
  		                                  
  		                                  textOutput("show_cdn"),
  		                                  tags$style(type="text/css", "#show_cdn {white-space: pre-wrap;}"),
  		                                  verbatimTextOutput("cdn_test"),
  		                                  tags$style(type="text/css", "#cdn_test {white-space: pre-wrap;}")
  		                                  ),
  		                                 column(width = 2,
    		                                fluidRow(column(width =10 ,actionButton("preview_cdns", label = "Preview",width="100%"))),
    		                                br(),
    		                                fluidRow(column(width =10,actionButton("app_cdns", label = "Apply",width="100%")))
    		                                # br(),
    		                                # fluidRow(column(width =10,actionButton("save_cdns", label = "Save",width="100%"))),
  		                                 )
  		                        ),
  		                        
  		                        br(),
  		                        
  		                        conditionalPanel(condition="input.op_LS=='EDIT'",
  		                                         fluidRow(column(width=7,verbatimTextOutput("edit_LS"),tags$style(type="text/css", "#edit_LS {white-space: pre-wrap;}")),
  		                                                  column(width=5,fluidRow(column(width=3,actionButton("prev_LS",label="Prev")),
  		                                                                          column(width=3,actionButton("nxt_LS",label="Next"))),
  		                                                         br(),
  		                                                         fluidRow(column(width=3,actionButton("upd_LS",label="Upd")))
  		                                                  )
  		                                         )
  		                        ),
  		                        hr(),
  		                        h4("Logical String"),
  		                        textOutput("logical_string"),
  		                        br(),
  		                        fluidRow(column(width=10,
  		                                        fluidRow(column(width=6,selectInput(inputId="LS",label="Logical Strings - Select",width="100%",choices=list("No Selection"="No Selection"),
  		                                                                            selectize = FALSE)
  		                                                        ),
  		                                                 column(width=3,selectInput(inputId="op_LS",label="Operator",width="100%",choices=list(" "=" ","EDIT"="EDIT","DELETE"="DELETE")))
  		                                                 ),
  		                                        verbatimTextOutput("LS_test"),
  		                                        tags$style(type="text/css", "#LS_test {white-space: pre-wrap;}")),
  		                                 column(width = 2,
  		                                        fluidRow(column(width = 10 ,actionButton("preview_LS", label = "Preview",width="100%"))),
  		                                        br(),
  		                                        fluidRow(column(width =10,actionButton("app_LS", label = "Apply",width="100%"))),
  		                                        br(),
  		                                        fluidRow(column(width =10,actionButton("save_LS", label = "Save",width="100%")))
  		                                 )),
  		                        
  		                        verbatimTextOutput("LS_list"),
  		                        htmlOutput("LS_list_delCdn"),
  		                        tags$style(type="text/css", "#LS_list_delCdn {white-space: pre-wrap;}")
		                        
		                        
		                        ),
		                        conditionalPanel(condition="input.der_sig=='Yes'",
		                                         fluidRow(column(width=5,textInput(inputId = "der_sig_name",label=h5("Derived Signal Name"), value="", width="100%")),
		                                                  column(width=3,textInput(inputId = "der_sig_unit",label=h5("Derived Signal Unit"), value="", width="100%"))),
		                                         hr(),
                                             fluidRow(column(width=10,
                                                             selectInput(inputId="sigs",label="Derived Signals - Select",choices=list()),
                                                             textOutput("show_sig"),
                                                             tags$style(type="text/css", "#show_sig {white-space: pre-wrap;}")),
                                                      column(width=2,
                                                             fluidRow(column(width=10,actionButton("sig_cal",label = "APPLY",width="100%"))),
                                                             br(),
                                                             fluidRow(column(width=10,actionButton("sig_save",label = "SAVE",width="100%")))))
		                                         
                                             
		                        )
		                ),
		               tabPanel("Event Detection", 
		                        fluidRow(column(width=10,fluidRow(column(width=4, selectInput("evt_cdn",label=h5("Conditions"),choices=list("No Selection"="No Selection"),selected = "No Selection")),
		                                                          column(width=2, selectInput("evt_op",label=h5("Operator"),choices=list(" "=" ","<="="<=",
		                                                                                                                                 "<"="<",">="=">=",">"=">","="="=="),selected = "No Selection")),
		                                                          column(width=2, numericInput("evt_dur", label=h5("Duration"),value=NULL)),
		                                                          column(width=2, selectInput("evt_dur_unit", label=h5("Unit"),choices=list(" "=" ","secs"="secs",
		                                                                                                                                    "mins"="mins","hours"="hours")),selected=" ")),
		                                        textOutput("evt_cdn_sel"),
		                                        tags$style(type="text/css", "#evt_cdn_sel {white-space: pre-wrap;}")),
		                                 column(width=2,h4("Controls"),
		                                        fluidRow(column(width=10,actionButton("evt_add", label="ADD",width="100%"))),
		                                        br(),
		                                        fluidRow(column(width=10,actionButton("evt_rem", label="REM",width="100%"))),
		                                        br(),
		                                        fluidRow(column(width=10,actionButton("evt_plus", label="EVT+",width="100%"))),
		                                        br(),
		                                        fluidRow(column(width=10,actionButton("evt_minus", label="EVT-",width="100%"))))),
		                        
		                        # fluidRow(column(width=8,textOutput("evt_cdn_sel"),
		                        #                 tags$style(type="text/css", "#evt_cdn_sel {white-space: pre-wrap;}"))
		                                 
		                                 #        )
		                                 
		                                 # column(width=2,actionButton("evt_rem", label="REM",width="80%"))
		                                 # ),
		                                 
		                        verbatimTextOutput("evt_def"),
		                        tags$style(type="text/css", "#evt_def {white-space: pre-wrap;}"),
		                        hr(),
		                        fluidRow(column(width = 6,selectInput("evts",label="Events",choices=list("No Selection"="No Selection"),
		                                                              selected = "No Selection")),
		                                 column(width=4,checkboxInput("time_seq",label="Common Time Stamps")),
		                                 
		                                 column(width=2,fluidRow(column(width=10,actionButton("evt_app",label="APPLY",width="100%"))))),
		                        
		                        fluidRow(column(width=6,textOutput("all_events"),
		                                                tags$style(type="text/css", "#all_events {white-space: pre-wrap;}")),
		                                 column(width=6,textOutput("event_outcome"),
		                                        tags$style(type="text/css", "#event_outcome {white-space: pre-wrap;}"))),
		                        
		                        hr(),
		                        fluidRow(column(width = 6,textInput('evt_txt_name',label='Event Name',width = '100%'))),
		                        fluidRow(
		                          # column(width=4,checkboxInput("success_evt",label="Common Time Stamps")),
		                          # column(width=4,checkboxInput("failed_evt",label="Common Time Stamps")),
		                          column(width=6,radioButtons("list_evt_sel",label="Download Event Data",inline=TRUE,choices = list("Successful Events"="Successful Events",
		                                                                                                                            "Failed Events"="Failed Events",
		                                                                                                                            "All Events"="All Events"))),
		                          column(width=4,checkboxInput('inc_sig_data_evt',label='Include Signal Data')),
		                          column(width=2,fluidRow(column(width=10,actionButton("evt_list",label="EXPORT",width="100%")))))
		                        
		                        
		               ),
		               # tabPanel("Data Quality Summary"),
		               # tabPanel("R-Script Generator"),
		               tabPanel("Define Evaluations", tableOutput("table"))
		             )
		             ),
    		
				tabPanel("Time Series",
				         fluidRow(column(width=4,selectInput("ts_data",label="SELECT DATA SOURCE:",choices=c("No Selection"=1,"All Data"=2,"Event Data"=3),
				                                             selected=2)),
				                  column(width=4,selectInput(inputId="time_series_sig", label=("Select Signal for Time Series Plot"),choices=list())),
				                  column(width=2,  # h4("Controls"),
				                         fluidRow(column(width=6,actionButton(inputId="time_series_add",offset=1, label=h5("ADD"),width="130%")),
				                                  column(width=6,actionButton(inputId="time_series_rem", label=h5("REM"),width="130%"))
				                         )),
				                  column(width = 2,checkboxInput("tszoom","Zoom ALL",value=TRUE))),
				         conditionalPanel(condition = "input.ts_data==2",
				                          fluidRow(
				                            column(width = 4,dateRangeInput('dateRange',label = 'Date range: yyyy-mm-dd',start = NA, end = NA))
				                            # column(width=6,sliderInput("time_range","Time Range:",min=0,max=1,value = c(0,1),width="100%")),
				                            
				                            ),
				                          hr(),
				                          div(id = 'placeholder1_TS_alldata')
				                          # plotOutput("plot_ts_alldata",click="plot_ts_alldata_click",dblclick = "plot_ts_alldata_dblclick",brush = brushOpts(id = "plot_ts_alldata_brush",resetOnNew = TRUE),height = "400px",width="100%")
				         ),
				         conditionalPanel(condition = "input.ts_data==3",
				                          fluidRow(
				                            column(width=4,selectInput(inputId="ts_event", label=("Select Event for Time Series Plot"),choices=list())),
				                            # column(width=2,textInput("time_series_low", label = h5("Time Frame Low"), value = "")),
				                            column(width=4,sliderInput("ts_event_step",label="Event Counter",min=0,max=1,value=0,
				                                                       animate=animationOptions(interval=2000,loop=FALSE))),
				                            column(width=4,div(strong("Event Date-Time Range:",verbatimTextOutput("ts_event_datetime",placeholder = TRUE),
				                                                      tags$style(type="text/css", "#ts_event_datetime {white-space: pre-wrap;}"))))
				                            # column(width=4,textInput("time_series_low", label = h5("Time Frame Low"), value = ""),
				                            #        textInput("time_series_high", label = h5("Time Frame High"), value = "")
				                            # )
				                          ),
				                          checkboxInput("show_evt_cdn",label="Show Event Condtions",value=FALSE),
				                          # conditionalPanel(condition='input.show_evt_cdn==TRUE',
				                          textOutput("cdn_evt_disp"),tags$style(type="text/css", "#cdn_evt_disp {white-space: pre-wrap;}"),
				                                           # ),
				                          
				                          hr(),
				                          div(id = 'placeholder1_TS_evtdata')
				                          # fluidRow(column(width=10,dygraphOutput("ts_plot")),
				                          #          column(width=2,checkboxGroupInput("ts_signal","Plotted Signals:",choices=NULL)))
				                          # ),
				         #hr(),
				         ),
				         # fluidRow(
				         #   column(width=6, h5('Time Event Plot'),
				         #          plotOutput("time_event_plot",click="time_event_plot_click",dblclick = "time_event_plot_dblclick",brush = brushOpts(id = "time_event_plot_brush",resetOnNew = TRUE),height="300px"),
				         #          div(id = 'placeholder1_time_series')),
				         #   column(width=6,div(id = 'placeholder2_time_series'))
				         # ),
				         
				         hr(),
				         downloadButton("plot1d_time_series_d","Download Plot")
				         
				),
				navbarMenu(title="1D Statistics",
          tabPanel("VIOLIN / BOX PLOTS",
                    h4("VIOLIN / BOX PLOTS"),
                   fluidRow(column(width=6,
                                          selectInput(inputId="vio_box", label=h5("Select Plot Type"),choices=list("No Selection"="No Selection",
                                                                                          "BoxPlot24hours" ="BoxPlot24hours",
                                                                                          "BoxPlotWeekDays"="BoxPlotWeekDays",
                                                                                          "BoxPlotMonths"="BoxPlotMonths",
                                                                                          "BoxPlotCalenderWeeks"= "BoxPlotCalenderWeeks",
                                                                                          "BoxPlotBusinessDaysWeekends"="BoxPlotBusinessDaysWeekends",
                                                                                          "BoxPlotYears"="BoxPlotYears",
                                                                                          "BoxPlotSeasons"="BoxPlotSeasons",
                                                                                          "violinBoxPlot24hours"="violinBoxPlot24hours",
                                                                                          "violinBoxPlotMonths"="violinBoxPlotMonths",
                                                                                          "violinBoxPlotWeekDays"="violinBoxPlotWeekDays",
                                                                                          "violinBoxPlotCalendarWeeks"="violinBoxPlotCalendarWeeks",
                                                                                          "violinBoxPlotBusinessDaysWeekends"="violinBoxPlotBusinessDaysWeekends",
                                                                                          "violinBoxPlotYears"="violinBoxPlotYears",
                                                                                          "violinBoxPlotSeasons"="violinBoxPlotSeasons"
                                                                                          ), selected="No Selection")),
                            column(width=4,textOutput("vioboxDatasource"),tags$style(type="text/css", "#vioboxDatasource {white-space: pre-wrap;}"))),
                                   
                    plotOutput("plot1d_vio_box",click="plot1d_vio_box_click",dblclick = "plot1d_vio_box_dblclick",brush = brushOpts(id = "plot1d_vio_box_brush",resetOnNew = TRUE),height = "600px",width="120%"),
                    downloadButton("plot1d_vio_box_d","Download Plot")
                    #verbatimTextOutput("test")
           ),
					# tabPanel("DENSITY",
					# 	h4("DENSITY"),
					# 	conditionalPanel(condition="input.load_data=='2'",
					# 	                 fluidRow(column(width=6,selectInput(inputId="den_brk", label=h5("Select Breaks:"),choices=list())),
					# 	                          column(width=4,textOutput("denDatasource"),tags$style(type="text/css", "#denDatasource {white-space: pre-wrap;}")))
					# 	                 ),
					# 	
					# 	div(
					# 	  style = "position:relative",
					# 	  plotOutput("plot1d_den",click="plot1d_den_click",dblclick = "plot1d_den_dblclick",
					# 	             brush = brushOpts(id = "plot1d_den_brush",resetOnNew = TRUE), height = "600px"),
					# 	  uiOutput("Prob_1D_Density")
					# 	),
					# 	
					# 	downloadButton("plot1d_den_d","Download Plot")
					# 	# verbatimTextOutput("test")
					# ),
					tabPanel("FREQUENCY",
					         conditionalPanel(condition="input.load_data==1",h4("FREQUENCY")
					                          ),
					         conditionalPanel(condition="input.load_data!=1",
					                          fluidRow(column(width=3,h4("FREQUENCY")),
					                                   column(width=6,fluidRow(column(width=6,selectInput(inputId="freq_brk", label=("Select Breaks:"),
					                                                                                      choices=list("No Selection"="No Selection"),selected = "No Selection")),
					                                                           column(width=6,selectInput("filter_1d",label="Data Filtering",choices=c("Enabled"=1,"Disabled"=2),selected=2)))),
					                                   column(width = 3,textOutput("freqDatasource"),tags$style(type="text/css", "#freqDatasource {white-space: pre-wrap;}")))
					                          ),
					         
					         hr(),
					         div(
					           style = "position:relative",
					           plotOutput("plot1d_freq",height ="600px",click="plot1d_freq_click",dblclick = "plot1d_freq_dblclick",
					                      brush = brushOpts(id = "plot1d_freq_brush",resetOnNew = TRUE),
                                hover = hoverOpts(id = "plot1d_freq_hover")),
					           uiOutput("Val_1D_freq"),uiOutput("Prob_1D_Density")
					         ),
					         
					         conditionalPanel(condition="input.filter_1d==1",
					                          hr(),
					                          fluidRow(column(width=4,verbatimTextOutput("filter_1d_range",placeholder = TRUE),tags$style(type="text/css", "#filter_1d_range {white-space: pre-wrap;}"),
					                                          fluidRow(column(width = 6,numericInput("filter_1d_xmin",value=NULL,label="Xmin")),
					                                                   column(width = 6,numericInput("filter_1d_xmax",value=NULL,label="Xmax")))
					                                     
					                                          ),
					                                   column(width=2,radioButtons("filter_1d_option",label="Data in Extent",choices=c("Include"=1,"Exclude"=2),selected=1),
					                                                  actionButton("filter_1d_transfer",label="TRANSFER")),
					                                   column(width=4,fluidRow(column(width=6,actionButton("filter_1d_sel",label="SELECT",width="100%")),
					                                                           column(width=6,actionButton("filter_1d_clear",label="CLEAR",width="100%"))),
					                                                  br(),
					                                                  fluidRow(column(width=6,actionButton("filter_1d_app",label="APPLY",width="100%")),
					                                                           column(width=6,actionButton("filter_1d_reset",label="RESET",width="100%")))
					                                          )),
					                          textOutput("filter_1d_cdns"),tags$style(type="text/css", "#filter_1d_cdns {white-space: pre-wrap;}")
					                          
					                          ),
					         hr(),
					         fluidRow(column(width=3,downloadButton("plot1d_freq_d","Download Plot")),
					                  column(width = 9,radioButtons("download_freq",label="Download Options",inline=TRUE,
					                                                choices=list("Frequency"=1,"Density"=2,"Combined"=3),selected=3))),
					         hr()
					         # verbatimTextOutput("test")
					),
	         # tabPanel("PROBABILITY",
	         #          h4("PROBABILITY"),
	         #          fluidRow(column(width=6,selectInput(inputId="prob_brk", label=h5("Select Breaks:"),choices=list("No Selection"="No Selection"),selected = "No Selection")),
	         #                   column(width=4,textOutput("probDatasource"),tags$style(type="text/css", "#probDatasource {white-space: pre-wrap;}"))),
	         #          plotOutput("plot1d_prob",height="600px",click="plot1d_prob_click",dblclick = "plot1d_prob_dblclick",brush = brushOpts(id = "plot1d_prob_brush",resetOnNew = TRUE)),
	         #          downloadButton("plot1d_prob_d","Download Plot")
	         #          #verbatimTextOutput("test")
	         # ),
	         # 
					
					tabPanel("ECDF",
					         fluidRow(
					           column(width=7,h4("Empirical Cumulative Distribution Function"),
					                  selectInput(inputId="ecdf_check", label=h4("Select Plot Type"),choices=list("No Selection"="No Selection",
					                                                                                              "ECDF"="ECDF",
					                                                                                              "1-ECDF"="1-ECDF"),selected="ECDF")),
					           # column(width=7,checkboxInput(inputId="ecdf_check", label=h4("1 - ECDF")))
					           column(width=4,textOutput("ecdfDatasource"),tags$style(type="text/css", "#ecdfDatasource {white-space: pre-wrap;}"))
					         ),
					         plotOutput("plot1d_ecdf",height="600px",click="plot1d_ecdf_click",dblclick = "plot1d_ecdf_dblclick",brush = brushOpts(id = "plot1d_ecdf_brush",resetOnNew = TRUE)),
					         downloadButton("plot1d_ecdf_d","Download Plot")
					),
					tabPanel("FREQUENCY_SENSITIVITY",
					         fluidRow(column(width=4,h4("FREQUENCY SENSITIVITY"),hr()),
					                  column(width=2,numericInput("freq_sens_low", label = ("Low (Manual)"), value = "",width="100%")),
					                  column(width=2,numericInput("freq_sens_high", label = ("High (Manual)"), value = "",width="100%")),
					                  column(width=4,fluidRow(column(width=4,numericInput("freq_interval", label = ("Intervals"), value = 100)
					                                                 # checkboxInput("freqsens_manual",label="Manual",value=FALSE)
					                                                 # actionButton("freqsens_manual",label="APPLY")
					                                                 ),
					                                          column(width=8,textOutput("freqsensDatasource"),
					                                                 tags$style(type="text/css", "#freqsensDatasource {white-space: pre-wrap;}")
					                                                 # helpText("Set Manual Signal Range")
					                                                 ))
					                        )
					                  ),
					         fluidRow(column(width=4,selectInput(inputId="freq_sens", label=("Select Signal for Frequency Plot"),choices=list()),
					                                 fluidRow(column(width=6,actionButton(inputId="freq_add",offset=1, label=("ADD"),width="100%")),
					                                          column(width=6,actionButton(inputId="freq_rem", label=("REM"),width="100%"))
					                                          )
					                        ),
					           column(width=8,fluidRow(# column(width=2,numericInput("freq_interval", label = ("Intervals"), value = 100)),
					                                   column(width=2,h4("Low"),verbatimTextOutput("freqsenslow_txt",placeholder=TRUE)),
					                                   # column(width=2,h4("Low"),verbatimTextOutput("freq_sens_low", placeholder = TRUE)),
					                                   column(width=6,sliderInput("freq_sens_step",label=("Select Interval for Signal 1"),min=0,max=1,value=0,
					                                                              animate=animationOptions(interval=2000,loop=FALSE))),
					                                   column(width=2,h4("High"),verbatimTextOutput("freqsenshigh_txt", placeholder = TRUE)))
					                          # helpText("Clear Low (Manual) & High (Manual) before starting animation on slider.")
					                  )
          		     ),
					         hr(),
					         fluidRow(
					           column(width=6,div(id = 'placeholder1')),
					           column(width=6,div(id = 'placeholder2'))
					         ),
					         # verbatimTextOutput("sens_check"),
					         # plotOutput("plot1d_freq_sens",click="plot1d_freq_sens_click",dblclick = "plot1d_freq_sens_dblclick",brush = brushOpts(id = "plot1d_freq_sens_brush",resetOnNew = TRUE)),
					         hr(),
					         downloadButton("plot1d_freq_sens_d","Download Plot")
					         # verbatimTextOutput("test")
					),
					
					tabPanel("ECDF_SENSITIVITY",
					         fluidRow(column(width=4,selectInput(inputId="ecdf_sens_check", label=("Select Plot Type"),
					                                             choices=list("No Selection"="No Selection","ECDF"="ECDF","1-ECDF"="1-ECDF"))),
					           # column(width=4,h4("ECDF/1-ECDF SENSITIVITY"),hr()),
					                  column(width=2,numericInput("ecdf_sens_low", label = ("Low (Manual)"), value = "",width="100%")),
					                  column(width=2,numericInput("ecdf_sens_high", label = ("High (Manual)"), value = "",width="100%")),
					                  column(width=4,fluidRow(column(width=4,numericInput("ecdf_interval", label = ("Intervals"), value = 100)),
					                                          column(width=8,textOutput("ecdfsensDatasource"),
					                                          tags$style(type="text/css", "#ecdfsensDatasource {white-space: pre-wrap;}"))))),
					         fluidRow(
					           column(width=4,selectInput(inputId="ecdf_sens", label=("Select Signal for ECDF Plot"),choices=list()),
					                         fluidRow(column(width=6,actionButton(inputId="ecdf_add",offset=1, label=("ADD"),width="100%")),
					                                  column(width=6,actionButton(inputId="ecdf_rem", label=("REM"),width="100%")))),
					                  column(width=8,fluidRow(column(width=2,h4("Low"),verbatimTextOutput("ecdfsenslow_txt",placeholder=TRUE)),
					                                          column(width=6,sliderInput("ecdf_sens_step",label=("Select Interval for Signal 1"),min=0,max=1,value=0,
					                                                                      animate=animationOptions(interval=2000,loop=FALSE))),
					                                          column(width=2,h4("High"),verbatimTextOutput("ecdfsenshigh_txt", placeholder = TRUE))))),
					         # fluidRow(column(width=4,selectInput(inputId="ecdf_sens_check", label=h5("Select Plot Type"),
					         #                             choices=list("No Selection"="No Selection","ECDF"="ECDF","1-ECDF"="1-ECDF")))),
					         # helpText("Clear Low (Manual) & High (Manual) before starting animation on slider.")
					         hr(),
					         fluidRow(column(width=6,div(id = 'ecdf_placeholder1')),
					           column(width=6,div(id = 'ecdf_placeholder2'))),
					         hr(),
					         downloadButton("plot1d_ecdf_sens_d","Download Plot")
					),
					tabPanel("OPERATING_HOURS",
					         h4("OPERATING HOURS"),
					         fluidRow(column(width=6,selectInput(inputId="op_hours_plot", label=h5("Select Plot Type"), choices=list("No Selection"="No Selection",
					                                                                                         "Operating Hours Accumulate"="Operating Hours Accumulate",
					                                                                                         "Operating Hours Remaining" = "Operating Hours Remaining"
					                                                                                         ),selected="Operating Hours Accumulate")),
					                  column(width=4,textOutput("ophoursDatasource"),tags$style(type="text/css", "#ophoursDatasource {white-space: pre-wrap;}"))),
					         plotOutput("plot1d_op_hours",click="plot1d_op_hours_click",dblclick = "plot1d_op_hours_dblclick",brush = brushOpts(id = "plot1d_op_hours_brush",resetOnNew = TRUE)),
					         downloadButton("plot1d_op_hours_d","Download Plot")
					         # verbatimTextOutput("test")
					),
					tabPanel("HOURS_DISTRIBUTION",
					         fluidRow(column(width=4,h4("HOURS DISTRIBUTION"),hr()),
					                  column(width=4,textOutput("hoursdistDatasource"),
					                         tags$style(type="text/css", "#hoursdistDatasource {white-space: pre-wrap;}"))),
					                  
					         conditionalPanel(condition='input.load_data!=1',
					                          fluidRow(column(width=6,selectInput(inputId="hours_dist_brk", label=h5("Select Breaks:"),choices=list("No Selection"="No Selection"),
					                                                              selected = "No Selection")))
					                          ),
					                  
					         plotOutput("plot1d_hours_dist",click="plot1d_hours_dist_click",dblclick = "plot1d_hours_dist_dblclick",brush = brushOpts(id = "plot1d_hours_dist_brush",resetOnNew = TRUE),height="600px"),
					         downloadButton("plot1d_hours_dist_d","Download Plot")
					         # verbatimTextOutput("test")
					)
				),
				navbarMenu(title="2D Statistics",
					tabPanel("X_Y_GRAPHS",
					         fluidRow(column(width=6,h4("X_Y_GRAPHS"),hr()),
					                  column(width=4,textOutput("xygraphDatasource"),tags$style(type="text/css", "#xygraphDatasource {white-space: pre-wrap;}"))),
					         plotOutput("plot2D_x_y_graph",width="100%",height="600px",
					                    click="plot2D_x_y_graph_click",
					                    dblclick = "plot2D_x_y_graph_dblclick",
					                    brush = brushOpts(
					                      id = "plot2D_x_y_graph_brush",resetOnNew = TRUE
					                    )
					         ),
					         downloadButton("plot2D_x_y_graph_d","Download Plot")
					         # verbatimTextOutput("test")
					),
					tabPanel("X_Y_KERNEL DENSITY",
					         fluidRow(column(width=4,h4("X_Y_KERNEL_DENSITY"),
					                         hr(),
					                         selectInput("Kdata_filter",label="DATA FILTERING",choices=c("ENABLED"=1,"DISABLED"=2),selected=2)
					                         # h5("Calculated Non Zero Kernel Density"),
					                         # fluidRow(column(width=6,numericInput("ker_cal_low",label="Min Value",value="")),
					                         #          column(width=6,numericInput("ker_cal_high",label="Max Value",value="")))
					                         
					                         ),
					                  column(width=4,h4("X_Y_PROBABILITY"),
					                         hr(),
					                         selectInput("probCalc",label="PROBABILITY CALCULATION",choices=c("ENABLED"=1,"DISABLED"=2),selected=2)
					 
					                         
					                         ),
					                  column(width=4,textOutput("Kdata_source"),
					                         tags$style(type="text/css", "#Kdata_source {white-space: pre-wrap;}"))
					         ),
					         
					         hr(),
					         
					         div(
					           style = "position:relative",

                  plotOutput("plot2D_x_y_ker_den2",width="100%",height="600px",
                             click="plot2D_x_y_ker_den2_click",
                             dblclick = "plot2D_x_y_ker_den2_dblclick",
                             brush = brushOpts(
                               id = "plot2D_x_y_ker_den2_brush",resetOnNew = TRUE
                             ),
                             hover = hoverOpts(
                               id = "plot_hover_den2"
                             )
                  ),uiOutput("hover_info")
					           ),
                  downloadButton("plot2D_x_y_ker_den2_d","Download Plot"),
                  conditionalPanel(condition = "input.Kdata_filter==1",
                           hr(),
                           fluidRow(column(width=6,verbatimTextOutput("Kdata_range",placeholder = TRUE),tags$style(type="text/css", "#Kdata_range {white-space: pre-wrap;}"),
                                           fluidRow(column(width=3,numericInput("Kdata_xmin",label="Xmin",value="NA")),
                                                    column(width=3,numericInput("Kdata_xmax",label="Xmax",value="NA")),
                                                    column(width=3,numericInput("Kdata_ymin",label="Ymin",value="NA")),
                                                    column(width=3,numericInput("Kdata_ymax",label="Ymax",value="NA")))),
                                    column(width=2,
                                           radioButtons("Kdata_sel",label="Data in Extent",choices=c("Include"=1,"Exclude"=2),selected=1),
                                           actionButton("transfer_bnd",label="TRANSFER")),
                                    column(width=3,fluidRow(column(width=6,actionButton("Kdata_range_sel",label="SELECT",width="100%")),
                                           column(width=6,actionButton("Kdata_range_clear",label="CLEAR",width="100%"))),
                                           br(),
                                           fluidRow(column(width=6,actionButton("Kdata_range_app",label="APPLY",width="100%")),
                                           column(width=6,actionButton("Kdata_range_reset",label="RESET",width="100%"))))
                                          
                           ),
                           textOutput("Kdata_cdns"),tags$style(type="text/css", "#Kdata_cdns {white-space: pre-wrap;}"),
                           fluidRow(column(width=6,verbatimTextOutput("Ktest")))
                  ),
                  hr(), 
					        # div(
					        #    style = "position:relative",
                    plotOutput("plot2D_x_y_ker_den1",width="100%",height="600px",
    		                      click="plot2D_x_y_ker_den1_click",
    		                      dblclick = "plot2D_x_y_ker_den1_dblclick",
    		                      brush = brushOpts(
    		                        id = "plot2D_x_y_ker_den1_brush",resetOnNew = TRUE
    		                      ),
    		                      hover = hoverOpts(
    		                        id = "plot_hover"
    		                      )
    		              ),
					           # uiOutput("hover_info")
					         # ),
					         
  		            downloadButton("plot2D_x_y_ker_den1_d","Download Plot")
					        
  		           
                  
					),
					tabPanel("X_Y_KERNEL_DENSITY_SENSITIVITY",
					         fluidRow(column(width=4,h4("KERNEL_DENSITY SENSITIVITY"),hr()),
					                  column(width=2,numericInput("xykden_sens_low", label = ("Low (Manual)"), value = NA,width="100%")),
					                  column(width=2,numericInput("xykden_sens_high", label = ("High (Manual)"), value =NA,width="100%")),
					                  column(width=4,fluidRow(column(width=4,numericInput("xykden_interval", label = ("Intervals"), value = 100,width="100%")),
					                                                 # checkboxInput("freqsens_manual",label="Manual",value=FALSE)
					                                                 # actionButton("freqsens_manual",label="APPLY")
					                                          
                        					                  column(width=8,textOutput("xykdensensDatasource"),
                        					                         tags$style(type="text/css", "#xykdensensDatasource {white-space: pre-wrap;}"))))
					         ),
					         fluidRow(column(width=4,selectInput(inputId="xykden_sens", label=("Select Signal for Intervals"),choices=list()),
					                         selectInput(inputId="xykden_sens_prob_enable", label=("Probability Calculation"),choices=list("Disabled"=1,"Enabled"=2),
					                                     selected=1)),
					                  # column(width=8,fluidRow(column(width=2,h4("Low"),verbatimTextOutput("xykdensenslow_txt",placeholder=TRUE)),
					                  #                         column(width=8,sliderInput("xykden_sens_step",label=("Select Interval"),min=0,max=1,value=1,
					                  #                                                     animate=animationOptions(interval=2000,loop=FALSE))),
					                  #                         column(width=2,h4("High"),verbatimTextOutput("xykdensenshigh_txt", placeholder = TRUE)))
					                  #                         # helpText("Clear Low (Manual) & High (Manual) before starting animation on slider.")
					                  #        )
					                  column(width=2,h4("Low"),verbatimTextOutput("xykdensenslow_txt",placeholder=TRUE)),
					                  column(width=4,sliderInput("xykden_sens_step",label=("Select Interval"),min=0,max=1,value=1,
					                                                                     animate=animationOptions(interval=2000,loop=FALSE))),
					                  column(width=2,h4("High"),verbatimTextOutput("xykdensenshigh_txt", placeholder = TRUE))
					                         # helpText("Clear Low (Manual) & High (Manual) before starting animation on slider.")
					                  
					                ),
					         hr(),
					         div(style = "position:relative",
					            plotOutput("plot2D_xykden_sens",height="400px",click="plot2D_xykden_sens_click",
					                     dblclick = "plot2D_xykden_sens_dblclick",brush = brushOpts(id = "plot2D_xykden_sens_brush",resetOnNew = TRUE)),
					            uiOutput("xykden_sens_prob")
					         ),
					         hr(),
					         downloadButton("plot2D_xykden_sens_d","Download Plot")
					)
				)
			)
		)
	)
)

server<-function(input,output, session){
  id<-NULL
  id<-showNotification(paste("Loading Application...."),duration=0,type="message")
  
  ##############Load R-Kernel###########
  source(paste0(scripts_path,"/STEP_01_02_PROJECT_TEMPLATE_for_analysis_of_Power_Plants_EXTENDED_26E.R"))
  
  current_project<-reactiveValues(prj="") # Current selected project
  # ct_pr<-reactiveValues(prj="start")
  data_load<-reactiveValues(dl=NULL) # stores for which plant data is loaded in RAM
  # st<-reactiveValues(cst="start")
  username<-Sys.getenv("USERNAME")  #gets username of the current user
  
  caller<-NULL #used to specify which input variable is calling function updatefromLoad_Data (eg. input$last_ext, input$sel2)
  
  ##########################################
  # INPUT VARIABLES LINKED TO GUI - Stored in default list called 'input' and are referenced through $ operator.
  # 1. input$sel1 - input variable representing selection on GUI for project
  # 2. input$stat - input variable representing selection on GUI for current evaluation/tab (eg 1D statistics - Frequency)
  # 3. input$sel2 - input variable representing selection on GUI for plant.
  # 4. input$sig1 - input variable representing selection on GUI for Signal 1
  # 5. input$sig2 - input variable representing selection on GUI for Signal 2
  # 6. input$sig1_cdn - input variable on Boundary Condition GUI for selection of signal for defining logical condition.
  # 7. input$time_series_sig - input variable representing signal selected on Time Series GUI for plotting.
  # 8. input$freq_sens - input variable representing signal selected on Frequency Sensitivity GUI for plotting.
  # 9. input$ecdf_sens - input variable representing signal selected on ECDF Sensitivity GUI for plotting.
  # 10. input$xykden_sens - input variable representing signal selected on XY Kernel Density GUI as conditioned signal.
  # 11. input$last_ext - input variable representing signal selected on main GUI for visualizing last analysed output.
  # 12. input$load_data - input variable representing selection on GUI for data source (last analysed table, all data or event data)
  # 13. input$rcodefile - input variable representing whether Rcode creation functionality is enabled or disabled
  # 14. input$rcodeproject - input variable representing selected project for R code creation
  # 15. input$rcodeplant - input variable representing selected plant for R code creation
  # 16. input$gen_plot - input variable representing button click of 'GENERATE PLOT' button
  # 17. input$extent - input variable representing filter selection on main GUI
  # 18. input$rcodecreate - input variable representing button click of RCode create functionality
  # 19. input$vio_box - input variable representing selection of type of plot in violin/box plot
  # 20. input$ecdf_check - input variable representing selection ECDF / 1- ECDF
  # 21. input$op_hours_plot - input variable representing selection of operating hours plot - Operating Hours Accumulate/ Operating Hours Remaining
  ############################################
  
  
  ###############
  #path_to_tables - Function to get path to output tables for different statistics
  ###############
  path_to_tables<-function(){
    pa<-""
    if ((input$sel1!="No Selection")&(input$sel2!="No Selection")){
      # pa<-paste0(out_path,input$sel1,"/",input$sel2)
      pa<-paste0(out_path,username,"/",input$sel1,"/",input$sel2)
      if (input$stat=="X_Y_KERNEL DENSITY"){
        pa=paste0(pa,"/2D_STATISTICS_TABLES/02_X_Y_KERNEL_DENSITY/")
      }else if (input$stat=="X_Y_ECDF"){
        pa=paste0(pa,"/2D_STATISTICS_TABLES/02_X_Y_ECDF/")
      }else if (input$stat=="X_Y_PROBABILITY"){
        pa=paste0(pa,"/2D_STATISTICS_TABLES/02_X_Y_PROBABILITY/")
      }else if (input$stat=="X_Y_COUNT"){
        pa=paste0(pa,"/2D_STATISTICS_TABLES/02_X_Y_COUNT/")
      }else if (input$stat=="X_Y_GRAPHS"){
        pa=paste0(pa,"/2D_STATISTICS_TABLES/01_X_Y_GRAPHS/")
      }else if (input$stat=="FREQUENCY"){
        pa=paste0(pa,"/1D_STATISTICS_TABLES/FREQUENCY/")
      }else if (input$stat=="ECDF"){
        if (input$ecdf_check=="1-ECDF"){
          pa=paste0(pa,"/1D_STATISTICS_TABLES/1-ECDF/")
        }else if (input$ecdf_check=="ECDF"){
          pa=paste0(pa,"/1D_STATISTICS_TABLES/ECDF/")  
        }else if(input$ecdf_check=="No Selection"){
          pa=""
          showNotification("Please select plot type.",duration=2)
        }
      }else if (input$stat=="DENSITY"){
        pa=paste0(pa,"/1D_STATISTICS_TABLES/DENSITY/")
      }else if (input$stat=="OPERATING_HOURS"){
        if (input$op_hours_plot=="Operating Hours Accumulate"){
          pa=paste0(pa,"/1D_STATISTICS_TABLES/OP_HOURS_ACCUM/")  
        }else if (input$op_hours_plot=="Operating Hours Remaining"){
          pa=paste0(pa,"/1D_STATISTICS_TABLES/OP_HOURS_REMAINING/")  
        }else if (input$op_hours_plot=="No Selection"){
          pa=""
          showNotification("Please select plot type.",duration=2)
        }
      }else if (input$stat=="HOURS_DISTRIBUTION"){
        pa=paste0(pa,"/1D_STATISTICS_TABLES/OP_HOURS_DISTRIBUTION/")
      }else if (input$stat=="PROBABILITY"){
        pa=paste0(pa,"/1D_STATISTICS_TABLES/PROBABILITY/")
      }else if (input$stat=="VIOLIN / BOX PLOTS"){
        if (input$vio_box=="No Selection"){
          pa=""
          showNotification("Please select plot type.",duration=2)
        }else{
          pa=paste0(pa,"/1D_STATISTICS_TABLES/",input$vio_box,"/")
        }
      }
    }
    return(pa)
  }
  
  ##################
  #selectInputUpdate - Function to update different input variables (drop down menus) on GUI with signal list in dataset through argument p1.
  ##################
  selectInputUpdate<-function(p1){
    updateSelectInput(session,"sig1",choices=p1)
    updateSelectInput(session,"sig2",choices=p1)
    #updateSelectInput(session,"sig3",choices=p1)
    updateSelectInput(session,"sig1_cdn",choices=p1)
    # updateSelectInput(session,"sig2_cdn",choices=p1)
    # updateSelectInput(session,"evt_cdn",choices=p1)
    updateSelectInput(session,"time_series_sig",choices=p1)
    updateSelectInput(session,"freq_sens",choices=p1)
    updateSelectInput(session,"ecdf_sens",choices=p1)
    updateSelectInput(session,"xykden_sens",choices=p1)
    updateSelectInput(session,"last_ext",choices=p1)
    
  }
  
  ##################
  #updatefromSel1 - Function to update drop down menu for plant (input$sel2) based on the selected project (input$se1l)
  ##################
  updatefromSel1<-function(){
    if (input$sel1!="No Selection"){#project selection
      
      p<-list("No Selection"="No Selection")
      p1<-list.dirs(paste0(in_path,"/",input$sel1), recursive=FALSE, full.name=FALSE) # get list of plants for selected project
      if (length(p1)==0){
        if (!is.null(id))
          removeNotification(id)
        id <<- NULL
        showNotification("Plant data not present.",duration=2)
        updateSelectInput(session,"sel2",choices = list("No Selection"="No Selection"),
                          selected="No Selection")
        return()
      }
      for (i in 1:length(p1)) {
        p[[p1[i]]]<-p1[i]
      }
      updateSelectInput(session,"sel2",choices = p,selected="No Selection") #update drop down menu with plant list
      
      p1<-c("No Selection"="No Selection")
      # sig<-data.frame(p1)
      selectInputUpdate(p1) 
      updateNumericInput(session,"freq_interval",value=NA)
      
      if (!is.null(id))
        removeNotification(id)
      id <<- NULL
      # if ((input$sel2=="No Selection")&((caller=="sel2")|(caller=="stat")))
      showNotification("Please select Plant.", duration = 2,type="message")
      
      return()
    } 
  }
  
  #################
  #updatefromSel2 - Function to load RCode file, update signal list in Signal 1, Signal 2 etc based on selected plant (input$sel2)
  #################
  updatefromSel2<-function(){
    if ((input$sel2!="No Selection")&(input$sel2!="")){
      updateSelectInput(session,"last_ext",choices=list("No Selection"="No Selection"), selected="No Selection")
      
      txt<-paste0(input$sel2,".R")  # RCode file for the selected plant
      fl<-list.files(paste0(in_path,"/",input$sel1,"/",input$sel2),pattern="*.R", recursive=FALSE)
      
      if (length(fl[grepl(txt,fl,fixed=TRUE)])>0){
        source(paste0(in_path,"/",input$sel1,"/",input$sel2,"/",txt))  #loads RCode file
        BASIC_Project_path<<-paste0(out_path,Sys.getenv("USERNAME"),"/",input$sel1,"/",input$sel2,"/") #sets user specific output path
        source(paste0(scripts_path,"/General_Settings.R")) #loads General Settings used for plotting
        
        p1<-NULL
        p1<-INPUT_VECTOR_VXX_Title_Type_Text #gets signal list from input vector list defined through RCode file
        p1<-c("No Selection",p1)
        # sig<-data.frame(p1)
        selectInputUpdate(p1) 
        
        updateNumericInput(session,"freq_interval",value=INPUT_NUMBER_OF_INTERVALLS_FOR_1D_SENSITIVITY_ANALYSIS)
        # if (current_project$prj!=input$sel2){
        current_project$prj<-input$sel2 # stores current loaded project
        data_load$dl<-NULL
        # }  
      }else{
        showNotification("RCode file not found.",duration=2)
        updateSelectInput(session,"sel2",choices=list("No Selection"="No Selection"))
      }
    }else if (input$sel2=="No Selection"){
      
      selectInputUpdate(list("No Selection"="No Selection"))
    }
  }
  
  ###############
  #updatefromLoad_Data - Function to load last analysed table or complete dataset, depending on the selection on GUI.
  ###############
  updatefromLoad_Data<-function(){
    
    if (input$load_data==3){#event data
      if (is.null(appEvt)){
        showNotification("Event data not present.", duration=2)
        return()
      }
    }else if (input$load_data==2){#all data
      updateSelectInput(session,"last_ext",choices=list("No Selection"="No Selection"), selected="No Selection")
      if ((input$sel1!="No Selection")&(input$sel2!="No Selection")&(is.null(data_load$dl))){
        id<<-NULL
        id<<-showNotification("Loading Plant Data.",duration=0,type="message")
        source(paste0(scripts_path,"/Load_Data.R")) #loads dataset from txt file to RAM
        loadCdnRfile() #loads existing EBCs to shiny interface
        data_load$dl="DONE"
        updateDateRangeInput(session,"dateRange",start=as.Date(PROJECT_DATA_VXX$MY_DATE[1]),
                             end=as.Date(PROJECT_DATA_VXX$MY_DATE[dim(PROJECT_DATA_VXX)[1]]))
      }
      else if ((input$sel1!="No Selection")&(input$sel2!="No Selection")&(!is.null(data_load$dl))&(caller=="load_data")){
        showNotification("Data already loaded.",duration=2)
      }
    }else if (input$load_data==1){#last output table
      if ((caller=="stat")|(caller=="sel2")|(caller=="load_data")){
        pa<-path_to_tables()
        if (pa!=""){
          fl<-list()
          file_out<-NULL
          if (input$stat=="X_Y_KERNEL DENSITY"){
            file_out<-list.files(pa,pattern="*.txt", recursive=TRUE)
            # showNotification("Please select last analysis file",duration=2)
          }else if (input$stat=="X_Y_GRAPHS"){
            file_out<-list.files(pa,pattern="*.txt", recursive=TRUE)
            # showNotification("Please select last analysis file",duration=2)
          }else if (input$stat=="FREQUENCY"){
            file_out<-list.files(pa,pattern="*.txt", recursive=TRUE)
            # showNotification("Please select last analysis file",duration=2)
          }else if (input$stat=="ECDF"){
            file_out<-list.files(pa,pattern="*.txt", recursive=TRUE)
            # showNotification("Please select last analysis file",duration=2)
          }else if (input$stat=="OPERATING_HOURS"){
            file_out<-list.files(pa,pattern="*.txt", recursive=TRUE)
            # showNotification("Please select last analysis file",duration=2)
          }else if (input$stat=="HOURS_DISTRIBUTION"){
            file_out<-list.files(pa,pattern="*.txt", recursive=TRUE)
            # showNotification("Please select last analysis file",duration=2)
            # updateSelectInput(session,"hours_dist_brk",choices=list("No Selection"="No Selection"), selected="No Selection")
          }else if (input$stat=="VIOLIN / BOX PLOTS"){
            file_out<-list.files(pa,pattern="*.txt", recursive=TRUE)
            # showNotification("Please select plot type and last analysis file",duration=2)
          }else if (input$stat=="FREQUENCY_SENSITIVITY"){
            showNotification("'New Analysis - All Data' Option Required", duration = 2)
            updateRadioButtons(session,"load_data",selected="2")
          }else if (input$stat=="ECDF_SENSITIVITY"){
            showNotification("'New Analysis - All Data' Option Required", duration = 2)
            # updateRadioButtons(session,"load_data",selected="2")
          }else if (input$stat=="X_Y_GRAPH_COUNT_SENSITIVITY"){
            showNotification("'New Analysis - All Data' Option Required", duration = 2)
            updateRadioButtons(session,"load_data",selected="2")
          }
          if (!is.null(file_out)){
            file_out<-c("No Selection", file_out)
            for (i in 1:length(file_out)){
              fl[[file_out[i]]]=file_out[i]
            }
            if ((input$last_ext=="No Selection")|(caller!="load_data")){
              updateSelectInput(session,"last_ext",choices=fl, selected="No Selection")
              showNotification("Please select last analysis file",duration=2)
            }
          }
          # else{
          #   updateSelectInput(session,"last_ext",choices=list("No Selection"="No Selection"), selected="No Selection")
          # }
        }
        
      }else if (caller=="last_ext"){
        if (input$last_ext!="No Selection"){
          pa<-path_to_tables()
          head <- read.table(paste0(pa,input$last_ext), nrows=1,header=FALSE,
                             skip = 0,
                             dec=".",
                             na.strings=c("NULL", "ERROR", "-1.#INF", "1.#INF", "NA"),sep=";" )
          if((input$stat=="FREQUENCY")||(input$stat=="DENSITY")||(input$stat=="PROBABILITY")||(input$stat=="HOURS_DISTRIBUTION")){
            updateSelectInput(session,"sig1",selected=substr(as.character(head$V1),3,nchar(as.character(head$V1))))
          }else if(input$stat=="ECDF"){
            t3<-regexpr("SIGNAL",as.character(head$V1))
            updateSelectInput(session,"sig1",selected=substr(as.character(head$V1),t3+6,nchar(as.character(head$V1))))
          }else if(input$stat=="OPERATING_HOURS"){
            updateSelectInput(session,"sig1",selected=head$V1)
          }else{
            updateSelectInput(session,"sig1",selected=head$V1)
            updateSelectInput(session,"sig2",selected=head$V2)  
          }
        }
      }
    }
    if ((caller=="load_data")|(caller=="sel2")){
      if (!is.null(id)){
        removeNotification(id)
        id<<-NULL
        showNotification("Ready to use.",duration=2)
      }
    }
  }
  
  
  ##############
  #clear_sens - Function removes sensitivity plots when called
  ##############
  clear_sens<-function(){
    if (length(inserted)>0){
      for (i in 1:length(inserted)){
        selector = paste0('#', inserted[length(inserted)])
        removeUI(selector=selector)  
        selector = paste0('#plot1d_freq_sens_', inserted[length(inserted)])
        removeUI(selector=selector)
        inserted <<- inserted[-length(inserted)]  
      }
    }
    
    
    
    
    if (length(inserted_TSall)>0){
      for (i in 1:length(inserted_TSall)){
        selector = paste0('#', inserted_TSall[length(inserted_TSall)])
        removeUI(selector=selector)  
        selector = paste0('#plot1d_time_series_', inserted_TSall[length(inserted_TSall)])
        removeUI(selector=selector)
        inserted_TSall <<- inserted_TSall[-length(inserted_TSall)]
      }
    }
    if (length(inserted_TSevt)>0){
      for (i in 1:length(inserted_TSevt)){
        selector = paste0('#', inserted_TSevt[length(inserted_TSevt)])
        removeUI(selector=selector)  
        selector = paste0('#plot1d_time_series_', inserted_TSevt[length(inserted_TSevt)])
        removeUI(selector=selector)
        inserted_TSevt <<- inserted_TSevt[-length(inserted_TSevt)]
      }
    }
    
    ranges1d_time_seriesx<-reactiveValues()
    ranges1d_time_seriesy<-reactiveValues()
    time_series_plts<-reactiveValues()
    # time_series_plts<-list()
    PROJECT_DATA_VXX_COPY<<-NULL
    # if (length(time_series_insert)>0){
    #   for (i in 1:length(time_series_insert)){
    #     selector = paste0('#', time_series_insert[length(time_series_insert)])
    #     removeUI(selector=selector)  
    #     selector = paste0('#time_series_', time_series_insert[length(time_series_insert)])
    #     removeUI(selector=selector)
    #     time_series_insert <<- time_series_insert[-length(time_series_insert)]  
    #   }
    # }
    # if (length(den_inserted)>0){
    #   for (i in 1:length(den_inserted)){
    #     selector = paste0('#', den_inserted[length(den_inserted)])
    #     removeUI(selector=selector)  
    #     selector = paste0('#plot1d_den_sens_', den_inserted[length(den_inserted)])
    #     removeUI(selector=selector)
    #     den_inserted <<- den_inserted[-length(den_inserted)]  
    #   }
    # }
    if (length(ecdf_inserted)>0){
      for (i in 1:length(ecdf_inserted)){
        selector = paste0('#', ecdf_inserted[length(ecdf_inserted)])
        removeUI(selector=selector)  
        selector = paste0('#plot1d_ecdf_sens_', ecdf_inserted[length(ecdf_inserted)])
        removeUI(selector=selector)
        ecdf_inserted <<- ecdf_inserted[-length(ecdf_inserted)]  
      }
    }
    # if (length(prob_inserted)>0){
    #   for (i in 1:length(prob_inserted)){
    #     selector = paste0('#', prob_inserted[length(prob_inserted)])
    #     removeUI(selector=selector)  
    #     selector = paste0('#plot1d_prob_sens_', prob_inserted[length(prob_inserted)])
    #     removeUI(selector=selector)
    #     prob_inserted <<- prob_inserted[-length(prob_inserted)]  
    #   }
    # }
    
  }
  
  #############
  #clear_all - Function resets all variables and plots
  #############
  clear_all<-function(){
    #resetting all plots
    {
      denPlt<<-NULL
      freqPlt<<-NULL
      # plts$prob<-NULL
      # plts$graph<-NULL
      plts$kernel1<-NULL
      plts$kernel2<-NULL
      plts$counts<-NULL
      plts$counts1<-NULL
      plts$ecdf<-NULL
      # plts$xy_count_sens<-NULL
      # plts$xy_graph_sens<-NULL
      plts$xykden_sens<-NULL
      plts1d$freq_sens<-NULL
      plts1d$vio_box<-NULL
      plts1d$freq<-NULL
      # plts1d$den<-NULL
      # plts1d$prob<-NULL
      plts1d$ecdf<-NULL
      plts1d$op_hours<-NULL
      plts1d$hours_dist<-NULL
      
      # plt_xygraph$y<-"Yes"
      # plt_xygraph$y<-"No"
      
      # plt_xycount$y<-"Yes"
      # plt_xycount$y<-"No"
      
      plt_xykerden$y<-"Yes"
      plt_xykerden$y<-"No"  
      
      # plt_xyprob$y<-"Yes"
      # plt_xyprob$y<-"No"
      # 
      # plt_xyecdf$y<-"Yes"
      # plt_xyecdf$y<-"No"
      
      # plt_xy_sens$y<-"Yes"  
      # plt_xy_sens$y<-"No"
      
      plt_freq$y<-"Yes"
      plt_freq$y<-"No"
      
      # plt_den$y<-"Yes"
      # plt_den$y<-"No"
      # 
      # plt_prob$y<-"Yes"
      # plt_prob$y<-"No"
      
      plt_ecdf$y<-"Yes"
      plt_ecdf$y<-"No"
      
      plt_freq_sens$y<-"Yes"
      plt_freq_sens$y<-"No"
      
      plt_ecdf_sens$y<-"Yes"
      plt_ecdf_sens$y<-"No"
      # 
      # plt_prob_sens$y<-"Yes"
      # plt_prob_sens$y<-"No"	   
      
      plt_op_hours$y<-"Yes"
      plt_op_hours$y<-"No"
      
      plt_vio_box$y<-"Yes"
      plt_vio_box$y<-"No"
      
      plt_hours_dist$y<-"Yes"  
      plt_hours_dist$y<-"No"
      
    }
    
    # DataFilter Reset
    {
      PROJECT_DATA_VXX_FILTERED_2D<<-NULL
      dataFilter2D<<-NULL
      # PROJECT_DATA_VXX_FILTERED_nD<<-NULL
      
      cdnReset$val<<-"NA"
      EBCs2d<<-c()
      LogicalString2d<<-NULL
      LSList<<-list()
      lsTxt<<-NULL
      cdnAllList<<-NULL
      cdnCounter<<-0
      cdnPartCounter<<-0
      cdnString<<-NULL
      cdnCompleteString<<-NULL
      cdnList<<-NULL
      cdnListShow$val<<-NULL
      cdnCompleteString<<-NULL
      updPlot$val<<-"NA"
      
      PROJECT_DATA_VXX_FILTERED_1D<<-NULL
      dataFilter1D<<-NULL
      
      cdnCompleteString1d<<-NULL
      cdnCounter1d<<-0
      
      cdnReset1d$val<<-"NA"
      updPlot1d$val<<-"NA"
      EBCs1d<<-c()
      LogicalString1d<<-NULL
      # LSList1d<<-list()
      lsTxt1d<<-NULL
      # cdnAllList1d<<-NULL
      cdnCounter1d<<-0
      cdnPartCounter1d<<-0
      cdnString1d<<-NULL
      cdnCompleteString1d<<-NULL
      cdnList1d<<-c()
      # cdnListShow1d$val<<-NULL
      
      PROJECT_DATA_VXX_FILTERED_nD<<-NULL
      dataFilternD<<-NULL
      updateExtentStatus(0)
    } 
    
    #Boundary condition variable resetting
    {
      
      cdn_hist<<-c()
      cdn_hist_vis<<-c()
      cdn_hist_v<<-c()
      cdn_last<<-c("start")
      cdn_last_cnt<<-c()
      
      bracketClose_cnt<<-0
      bracketOpen_cnt<<-0
      bracketClose<<-c()
      bracketOpen<<-c()
      
      cdnAll<<-list("No Selection"="No Selection")
      # cdnAllVis<<-list("No Selection"="No Selection", "SelectAll"="SelectAll")
      # cdnAllV<<-list("No Selection"="No Selection", "SelectAll"="SelectAll")
      # cdnDes<<-list("No Selection"="No Selection", "SelectAll"="SelectAll")
      cdnAllVis<<-list("No Selection"="No Selection")
      cdnAllV<<-list("No Selection"="No Selection")
      cdnDes<<-list("No Selection"="No Selection")
      cdnSource<<-c("NA","NA")
      cdnNum<<-0
      delCdn<-NULL
      
      op_match<<-c()
      itemSel<<-NULL
      itemCdn<<-c()
      itemCdnCode<<-c()
      itemCdnV<<-c()
      cdnSelNum<<-NULL
      cdnEdit<<-NULL
      cdnEditCode<<-NULL
      cdnEditV<<-NULL
      
      def_cdn_string<<-NULL
      
      cdn_string_hist_vis<<-c()
      cdn_string_hist<<-c()
      
      
      cdn$val<-NULL
      cdn_vis$val<-NULL
      cdn_v$val<<-NULL
      def_cdn_string_vis$val<-NULL
      cdnEditItem$val<-NULL
      cdnCnt$val<-NULL
      cdn_string_vis$val<-NULL
      cdn_string$val<-NULL
      
      cdn_string_last<<-c("start")
      cdn_string_last_cnt<<-c()
      LS_bracketClose_cnt<<-0
      LS_bracketOpen_cnt<<-0
      LS_bracketClose<<-c()
      LS_bracketOpen<<-c()
      
      lsCnt$val<<-NULL
      isLogicalStringInvalidate<<-FALSE
      
      itemLS<<-NULL
      lsEdit<<-NULL
      lsSelNum<<-NULL
      allLS<<-list("No Selection"="No Selection","LS1(Default)"=" ")
      lsStringAll<<-list("No Selection"="No Selection")
      lsEditItem$val<<-NULL
      showLs$val<-NULL
      
      updateSelectInput(session,"cdns",choices=cdnAllVis,selected="No Selection")
      updateSelectInput(session,"LS",choices=allLS,selected="No Selection")
      
      sigNum<<-0
      sigCnt$val<-NULL
      sigAll<<-list()
      sigAllVis<<-list()
      sigAllV<<-list()
      sigDes<<-list()
      showSig$val<-NULL
      createdSig<<-c()
      updateSelectInput(session,"sigs",choices=c("No Selection"="No Selection"),selected="No Selection")
      
    }
    
    #Event Detection variable resetting
    {
      dataFilterEvents<<-NULL
      PROJECT_DATA_VXX_EVENTS<<-NULL
      appEvt<<-NULL
      appEvtName<<-NULL
      evtOutput<<-NULL
      allEvts<<-list("No Selection"="No Selection")
      evtStrend<<-FALSE
      evtStringpart<<-c()
      evtNum<<-0
      evtCnt<<-NULL
      showCdn$val<-NULL
      evtDefinition$val<-NULL
      showEvt$val<-NULL
      evtCount$val<-NULL
      evt_status$val<-NULL
      startDate<<-NULL
      endDate<<-NULL
      allEvts<-list("No Selection"="No Selection")
      updateSelectInput(session,"evts",choices=allEvts,selected="No Selection")
      updateSelectInput(session,"ts_event",choices=allEvts,selected="No Selection")
      updateTextInput(session,'evt_txt_name',value=NA)
    }
    
    #Time Series Variable resetting
    {
      evtCnt<<-NULL
      dataTimeSeries<<-NULL
      meanDataFull<<-NULL
      inserted_TSall<<-c()
      inserted_TSevt<<-c()
      ranges1d_time_seriesx<-reactiveValues()
      ranges1d_time_seriesy<-reactiveValues()
      time_series_plts<-reactiveValues()
      PROJECT_DATA_VXX_COPY<<-NULL
      startDate<<-NULL
      endDate<<-NULL
    }
    
    #frequency sensitivity variable resetting
    {
      inserted <<- c()
      freq_sens_plts<<-reactiveValues()
      freq_steps<<-1
      freq_sens_upd<<-"Yes"
      freq_sens_dat<<-list()
      freq_sens_left_x<<-list()
      freq_sens_right_x<<-list()
      freq_sens_x_unit<<-list()
      freq_sens_title<<-list()
      # freq_sens_xmin<-reactiveValues()
      # freq_sens_xmax<-reactiveValues()
      freq_sens_brks03<<-list()
      ranges1d_freq_sensx<<-reactiveValues()
      ranges1d_freq_sensy<<-reactiveValues()
      freq_high_val<<-NULL
      freq_low_val<<-NULL
      PROJECT_DATA_VXX___FILTER_RESULT_RECOMPUTED<<-NULL
      freq_sens_brks<<-list()
      freq_sens_xmin<<-list()
      freq_sens_xmax<<-list()
      freq_sens_ylim<<-list()
      
      freqIntrange$low<-NULL
      freqIntrange$high<-NULL
      freqStepval<<-NULL
      boundMin<<-NULL
      boundMax<<-NULL
      freqSteps<<-NULL
      dataManualFreqsens<<-NULL
      datasplitFreqsens<<-NULL
      freqsensStepRange<<-list()
      
      freqsensdataFilter<-reactiveValues(val=paste0("Data Source : FULL \n","Filtering Source : NONE"))
      updateSliderInput(session,"freq_sens_step",value=0,min=0,max=1,step=1)
    }
    
    #ECDF sensitivity variable resetting
    {
      # inserted <<- c()
      ecdf_sens_plts<<-reactiveValues()
      ecdf_steps<<-1
      ecdf_sens_upd<<-"Yes"
      ecdf_sens_dat<<-list()
      ecdf_sens_left_x<<-list()
      ecdf_sens_right_x<<-list()
      ecdf_sens_x_unit<<-list()
      ecdf_sens_title<<-list()
      # freq_sens_xmin<-reactiveValues()
      # freq_sens_xmax<-reactiveValues()
      ecdf_sens_brks03<<-list()
      ranges1d_ecdf_sensx<<-reactiveValues()
      ranges1d_ecdf_sensy<<-reactiveValues()
      ecdf_high_val<<-NULL
      ecdf_low_val<<-NULL
      PROJECT_DATA_VXX___FILTER_RESULT_RECOMPUTED<<-NULL
      ecdf_sens_brks<<-list()
      ecdf_sens_xmin<<-list()
      ecdf_sens_xmax<<-list()
      ecdf_sens_ylim<<-list()
      ecdf_sens_xlim<<-list()
      
      ecdfStepval<<-NULL
      ecdfboundMin<<-NULL
      ecdfboundMax<<-NULL
      ecdfSteps<<-NULL
      dataManualecdfsens<<-NULL
      datasplitecdfsens<<-NULL
      ecdfsensStepRange<<-list()
      
      ecdfsensdataFilter<-reactiveValues(val=paste0("Data Source : FULL \n","Filtering Source : NONE"))
      updateSliderInput(session,"ecdf_sens_step",value=0,min=0,max=1,step=1)
    }
    
    # 2D Kernel Density Sensitivity variable resetting
    {
      xykdenStepval<<-NULL
      xykdenboundMin<<-NULL
      xykdenboundMax<<-NULL
      xykdenSteps<<-NULL
      dataManualxykdensens<<-NULL
      datasplitxykdensens<<-NULL
      xykdensensStepRange<<-list()
      xykden_high_val<<-NULL
      xykden_low_val<<-NULL
      upperX<<-NULL
      lowerX<<-NULL
      upperY<<-NULL
      lowerY<<-NULL
      typesigxykden<<-"Binary"
      ranges2D_x_y_kden_sens$x<-NULL
      ranges2D_x_y_kden_sens$y<-NULL
      plts$xykden_sens<-NULL
      xykdensensdataFilter$val<-NULL
      plt_xykden_sens$y<-"No"
    }
    
    #1d variable resetting
    {
      hoursdistdataFilter<<-reactiveValues(val=NULL)
      vioboxdataFilter<<-reactiveValues(val=NULL)
      freqdataFilter<<-reactiveValues(val=paste0("Data Source : COMPLETE \n","Filtering Source : NONE"))
      freqData<<-NULL
      
      ecdfdataFilter<<-reactiveValues(val=NULL)
      ophoursdataFilter<<-reactiveValues(val=NULL)
      dendataFilter<<-reactiveValues(val=paste0("Data Source : COMPLETE \n","Filtering Source : NONE"))
      density1D<<-NULL
      gridSize<<-500
      # probdataFilter<<-reactiveValues(val=paste0("Data Source : COMPLETE \n","Filtering Source : NONE"))
    }
    
    #2d variable resetting
    {
      xygraphDatafilter<<-reactiveValues(val=NULL)
      # xycountDatafilter<<-reactiveValues(val=NULL)
      
      kerneldendataFilter<<-reactiveValues(val=NULL)
      den_breaks<<-reactiveValues(br=NULL)
      probability<<-NULL
      xmin<<-NULL
      xmax<<-NULL
      ymin<<-NULL
      ymax<<-NULL
      appK<<-reactiveValues(val="No")
      returnCode<<-NULL
      
      # xyecdfDatafilter<<-reactiveValues(val=NULL)
      # xyprobDatafilter<<-reactiveValues(val=NULL)
    }
    
    #update select input
    {
      # if (input$load_data==3)
      #   updateSelectInput(session,"load_data",selected=1)
      
      updateSelectInput(session,"ts_event",choices=list("No Selection"="No Selection"),selected = "No Selection")
      updateSelectInput(session,"evt_cdn",choices=list("No Selection"="No Selection"),selected="No Selection")
      updateSelectInput(session,"last_ext",choices=list("No Selection"="No Selection"), selected="No Selection")
      
    }
    
    #update load data input
    val<-input$load_data
    l<-list()
    l[["Last Analysed Output"]]<-1
    l[["New Analysis-All Data"]]<-2
    l[[paste0("Event Data - NONE")]]<-3
    updateRadioButtons(session,"load_data",choices=l,selected = val)
    
  }
  
  
  #statChangeSettings - Function initializes input parameters on GUI for evaluation whenever it selected by user 
  statChangeSettings<-function(){
    if ((input$sel1=="No Selection")|(input$sel2=="No Selection")){ #checks selected project and plant should not be "No Selection"
      return()
    }
    # if (input$stat=="DENSITY"){
    #   if ((input$load_data=="2")&&(input$sig1=="No Selection")){
    #     showNotification("Please select Signal 1",duration=2)
    #   }
    #   if (input$den_brk=="No Selection"){
    #     density1D<<-NULL  
    #     p1<-NULL
    #     i<-which(INPUT_VECTOR_VXX_Title_Type_Text==input$sig1)
    #     p1<-c("No Selection",INPUT_VECTOR_breaks_01[i],INPUT_VECTOR_breaks_02[i],INPUT_VECTOR_breaks_03[i])
    #     sig<-data.frame(p1)
    #     updateSelectInput(session,"den_brk",choices=sig$p1)
    #   }
    # }
    # if (input$stat=="PROBABILITY"){
    #   if ((input$load_data=="2")&&(input$sig1=="No Selection")){
    #     showNotification("Please select Signal 1",duration=2)
    #   }
    #   if (input$prob_brk=="No Selection"){
    #     p1<-NULL
    #     i<-which(INPUT_VECTOR_VXX_Title_Type_Text==input$sig1)
    #     p1<-c("No Selection",INPUT_VECTOR_breaks_01[i],INPUT_VECTOR_breaks_02[i],INPUT_VECTOR_breaks_03[i])
    #     sig<-data.frame(p1)
    #     updateSelectInput(session,"prob_brk",choices=sig$p1)
    #   }
    # }
    if (input$stat=="FREQUENCY"){
      if ((input$load_data=="2")&&(input$sig1=="No Selection")){
        showNotification("Please select Signal 1",duration=2)
      }
      
      if (input$freq_brk=="No Selection"){
        p1<-NULL
        i<-which(INPUT_VECTOR_VXX_Title_Type_Text==input$sig1)
        p1<-c("No Selection",INPUT_VECTOR_breaks_01[i],INPUT_VECTOR_breaks_02[i],INPUT_VECTOR_breaks_03[i])
        sig<-data.frame(p1)
        updateSelectInput(session,"freq_brk",choices=sig$p1,selected=sig$p1[length(p1)])
      }
      
    }
    if (input$stat=="HOURS_DISTRIBUTION"){
      if ((input$load_data=="2")&&(input$sig1=="No Selection")){
        showNotification("Please select Signal 1",duration=2)
      }
      if (input$hours_dist_brk=="No Selection"){
        p1<-NULL
        i<-which(INPUT_VECTOR_VXX_Title_Type_Text==input$sig1)
        p1<-c("No Selection",INPUT_VECTOR_breaks_01[i],INPUT_VECTOR_breaks_02[i],INPUT_VECTOR_breaks_03[i])
        sig<-data.frame(p1)
        updateSelectInput(session,"hours_dist_brk",choices=sig$p1)
      }
    }
    # if (input$stat=="X_Y_GRAPH_COUNT_SENSITIVITY"){
    #   if ((input$sig1=="No Selection")||(input$sig2=="No Selection")){
    #     showNotification("Please select Signals.",duration=2)
    #   }else if((input$sig1!="No Selection")&&(input$sig2!="No Selection")&&(input$sig1==input$sig2)){
    #     showNotification("Selected Signal 1 and Signal 2 are same. Please check.")
    #   }
    #   if ((input$load_data=="2")&&(input$sig1!="No Selection")){
    #     if (input$sig1!=input$sig2){
    #       p1<-NULL
    #       p1<-INPUT_VECTOR_VXX_Title_Type_Text
    #       p1<-c("No Selection",p1)
    #       sig<-data.frame(p1)
    #       updateSelectInput(session,"xy_sens",choices=sig$p1)
    #     }
    #   }
    # }
    if (input$stat=="ECDF_SENSITIVITY"){
      # if (input$sig1=="No Selection"){
      #   showNotification("Please select Signal 1",duration=2)
      # }
      if ((input$load_data=="2")&&(input$sig1!="No Selection")){
        datasplitecdfsens<<-NULL
        # i<-which(INPUT_VECTOR_VXX_Title_Type_Text==input$sig1)
        # my_abs_min_delta_PROJECT_DATA_VXX_i = min(abs(diff(PROJECT_DATA_VXX[[i]])))
        # my_abs_max_delta_PROJECT_DATA_VXX_i = max(abs(diff(PROJECT_DATA_VXX[[i]])))
        # my_min_PROJECT_DATA_VXX_i = min(PROJECT_DATA_VXX[[i]])
        # my_max_PROJECT_DATA_VXX_i = max(PROJECT_DATA_VXX[[i]])
        # 
        # # my_PROJECT_DATA_VXX_i_step = ceiling((my_max_PROJECT_DATA_VXX_i - my_min_PROJECT_DATA_VXX_i) /
        # #                                        INPUT_NUMBER_OF_INTERVALLS_FOR_1D_SENSITIVITY_ANALYSIS)
        # # 
        # # steps<-ceiling((my_max_PROJECT_DATA_VXX_i - my_min_PROJECT_DATA_VXX_i)/my_PROJECT_DATA_VXX_i_step)
        # 
        # my_PROJECT_DATA_VXX_i_step = ((my_max_PROJECT_DATA_VXX_i - my_min_PROJECT_DATA_VXX_i) /
        #                                 INPUT_NUMBER_OF_INTERVALLS_FOR_1D_SENSITIVITY_ANALYSIS)
        # 
        # steps<-((my_max_PROJECT_DATA_VXX_i - my_min_PROJECT_DATA_VXX_i)/my_PROJECT_DATA_VXX_i_step)
        # 
        # updateSliderInput(session,"ecdf_sens_step",value=1,min=1,max=steps,step=1)
        # 
        # low_val<-my_min_PROJECT_DATA_VXX_i
        # high_val<-low_val+my_PROJECT_DATA_VXX_i_step
        # updateNumericInput(session,"ecdf_sens_low", value=low_val)
        # updateNumericInput(session,"ecdf_sens_high", value=high_val)
        # 
        # p1<-NULL
        # p1<-INPUT_VECTOR_VXX_Title_Type_Text
        # p1<-c("No Selection",p1)
        # sig<-data.frame(p1)
        # updateSelectInput(session,"ecdf_sens",choices=sig$p1)
        # 
      }
    }
    if (input$stat=="FREQUENCY_SENSITIVITY"){
      # if (input$sig1=="No Selection"){
      #   showNotification("Please select Signal 1",duration=2)
      # }
      if ((input$load_data=="2")&&(input$sig1!="No Selection")){
        datasplitFreqsens<<-NULL
        # id<<-showNotification("Loading.....")
        # sensStep()
        # 
        # steps<-freqSteps
        # updateSliderInput(session,"freq_sens_step",value=1,min=1,max=steps,step=1)
        # updateNumericInput(session,"freq_interval",value=INPUT_NUMBER_OF_INTERVALLS_FOR_1D_SENSITIVITY_ANALYSIS)
        # 
        # # low_val<-my_min_PROJECT_DATA_VXX_i+(input$freq_sens_step-1)*my_PROJECT_DATA_VXX_i_step
        # low_val<-boundMin
        # high_val<-low_val+freqStepval
        # updateNumericInput(session,"freq_sens_low", value=low_val)
        # updateNumericInput(session,"freq_sens_high", value=high_val)
        # 
        # freqsensState<<-c(low_val,1,high_val)
        # datasplitFreqsens<<-list()
        # p1<-NULL
        # p1<-INPUT_VECTOR_VXX_Title_Type_Text
        # p1<-c("No Selection",p1)
        # sig<-data.frame(p1)
        # updateSelectInput(session,"freq_sens",choices=sig$p1)
        
      }
    }
    # if (input$stat=="DENSITY_SENSITIVITY"){
    #   if (input$sig1=="No Selection"){
    #     showNotification("Please select Signal 1",duration=2)
    #   }
    #   if ((input$load_data=="2")&&(input$sig1!="No Selection")){
    #     
    #     i<-which(INPUT_VECTOR_VXX_Title_Type_Text==input$sig1)
    #     my_abs_min_delta_PROJECT_DATA_VXX_i = min(abs(diff(PROJECT_DATA_VXX[[i]])))
    #     my_abs_max_delta_PROJECT_DATA_VXX_i = max(abs(diff(PROJECT_DATA_VXX[[i]])))
    #     # my_min_PROJECT_DATA_VXX_i = min(PROJECT_DATA_VXX[[i]])
    #     # my_max_PROJECT_DATA_VXX_i = max(PROJECT_DATA_VXX[[i]])
    #     my_min_PROJECT_DATA_VXX_i = INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE[i]
    #     my_max_PROJECT_DATA_VXX_i = INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE[i]
    #     
    #     # my_PROJECT_DATA_VXX_i_step = ceiling((my_max_PROJECT_DATA_VXX_i - my_min_PROJECT_DATA_VXX_i) /
    #     #                                        INPUT_NUMBER_OF_INTERVALLS_FOR_1D_SENSITIVITY_ANALYSIS)
    #     # 
    #     # steps<-ceiling((my_max_PROJECT_DATA_VXX_i - my_min_PROJECT_DATA_VXX_i)/my_PROJECT_DATA_VXX_i_step)
    #     
    #     my_PROJECT_DATA_VXX_i_step = ((my_max_PROJECT_DATA_VXX_i - my_min_PROJECT_DATA_VXX_i) /
    #                                     INPUT_NUMBER_OF_INTERVALLS_FOR_1D_SENSITIVITY_ANALYSIS)
    #     
    #     steps<-((my_max_PROJECT_DATA_VXX_i - my_min_PROJECT_DATA_VXX_i)/my_PROJECT_DATA_VXX_i_step)
    #     
    #     updateSliderInput(session,"den_sens_step",value=1,min=1,max=steps,step=1)
    #     
    #     # low_val<-my_min_PROJECT_DATA_VXX_i+(input$freq_sens_step-1)*my_PROJECT_DATA_VXX_i_step
    #     low_val<-my_min_PROJECT_DATA_VXX_i
    #     high_val<-low_val+my_PROJECT_DATA_VXX_i_step
    #     updateNumericInput(session,"den_sens_low", value=low_val)
    #     updateNumericInput(session,"den_sens_high", value=high_val)
    #     
    #     p1<-NULL
    #     p1<-INPUT_VECTOR_VXX_Title_Type_Text
    #     p1<-c("No Selection",p1)
    #     sig<-data.frame(p1)
    #     updateSelectInput(session,"den_sens",choices=sig$p1)
    #     
    #   }
    # }
    # if (input$stat=="PROBABILITY_SENSITIVITY"){
    #   if (input$sig1=="No Selection"){
    #     showNotification("Please select Signal 1",duration=2)
    #   }
    #   if ((input$load_data=="2")&&(input$sig1!="No Selection")){
    #     
    #     i<-which(INPUT_VECTOR_VXX_Title_Type_Text==input$sig1)
    #     my_abs_min_delta_PROJECT_DATA_VXX_i = min(abs(diff(PROJECT_DATA_VXX[[i]])))
    #     my_abs_max_delta_PROJECT_DATA_VXX_i = max(abs(diff(PROJECT_DATA_VXX[[i]])))
    #     # my_min_PROJECT_DATA_VXX_i = min(PROJECT_DATA_VXX[[i]])
    #     # my_max_PROJECT_DATA_VXX_i = max(PROJECT_DATA_VXX[[i]])
    #     my_min_PROJECT_DATA_VXX_i = INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE[i]
    #     my_max_PROJECT_DATA_VXX_i = INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE[i]
    #     
    #     # my_PROJECT_DATA_VXX_i_step = ceiling((my_max_PROJECT_DATA_VXX_i - my_min_PROJECT_DATA_VXX_i) /
    #     #                                        INPUT_NUMBER_OF_INTERVALLS_FOR_1D_SENSITIVITY_ANALYSIS)
    #     # 
    #     # steps<-ceiling((my_max_PROJECT_DATA_VXX_i - my_min_PROJECT_DATA_VXX_i)/my_PROJECT_DATA_VXX_i_step)
    #     
    #     my_PROJECT_DATA_VXX_i_step = ((my_max_PROJECT_DATA_VXX_i - my_min_PROJECT_DATA_VXX_i) /
    #                                     INPUT_NUMBER_OF_INTERVALLS_FOR_1D_SENSITIVITY_ANALYSIS)
    #     
    #     steps<-((my_max_PROJECT_DATA_VXX_i - my_min_PROJECT_DATA_VXX_i)/my_PROJECT_DATA_VXX_i_step)
    #     
    #     updateSliderInput(session,"prob_sens_step",value=1,min=1,max=steps,step=1)
    #     
    #     # low_val<-my_min_PROJECT_DATA_VXX_i+(input$freq_sens_step-1)*my_PROJECT_DATA_VXX_i_step
    #     low_val<-my_min_PROJECT_DATA_VXX_i
    #     high_val<-low_val+my_PROJECT_DATA_VXX_i_step
    #     updateNumericInput(session,"prob_sens_low", value=low_val)
    #     updateNumericInput(session,"prob_sens_high", value=high_val)
    #     
    #     p1<-NULL
    #     p1<-INPUT_VECTOR_VXX_Title_Type_Text
    #     p1<-c("No Selection",p1)
    #     sig<-data.frame(p1)
    #     updateSelectInput(session,"prob_sens",choices=sig$p1)
    #     
    #   }
    # }
  }
  
  #Rcode_Generation - Function to create RCode file for the selected project(input$rcodeporject) and plant (input$rcodeplant) on GUI
  Rcode_Generation<-function(project,plant){
    
    # # project<-input$rcodeproject
    # # plant<-input$rcodeplant
    # 
    # INPUT_NUMBER_OF_LINES_SKIPPED = 2
    # INPUT_MY_FREQUENCY = 30
    
    fl<-list.files(path=paste0(in_path,"/",project,"/",plant),pattern="*.txt", recursive=FALSE)
    input_txt<-paste0(plant,".txt")
    if (length(fl[grepl(input_txt,fl,fixed=TRUE)])==0){
      if (!is.null(id)){
        removeNotification(id)
      }
      showNotification("Project data file not present",duration=2)
      return()
    }else{
      INPUT_PROJECT_INPUT_TXT_FILE<-paste0(in_path,"/",project,"/",plant,"/",plant,".txt")
    }
    
    interface_txt<-paste0("R_CODE_INTERFACE_FILE_",plant,".txt")
    interface_file_found<-FALSE
    if (length(fl[grepl(interface_txt,fl,fixed=TRUE)])>0){
      interface_file_found<-TRUE
      interfacefile<-paste0(in_path,"/",project,"/",plant,"/R_CODE_INTERFACE_FILE_",plant,".txt")
      signal_info <- fread(interfacefile,skip = 0,na.strings=c("NULL", "ERROR", "-1.#INF", "1.#INF", "NA"),blank.lines.skip=TRUE)
    }
    
    
    header_info <- fread(INPUT_PROJECT_INPUT_TXT_FILE,
                         header=TRUE,
                         nrows=10,
                         skip = 0,
                         dec=".",
                         na.strings=c("NULL", "ERROR", "-1.#INF", "1.#INF", "NA", "-1.#INF0", "1.#INF0"),blank.lines.skip=TRUE)
    
    data_source_row<-2
    if (length(which(header_info[[1]]!=""))>0){
      data_source_row<-which(header_info[[1]]!="")[1]
    }
    
    txt_file_source<-NULL
    time_check_tde<-length(which(colnames(header_info)=="Time:"))
    date_check_tde<-length(which(colnames(header_info)=="Date:"))
    time_check_probopdata<-length(which(colnames(header_info)=="Time"))
    if ((time_check_tde>0)&(date_check_tde>0)){
      txt_file_source<-"TDE"
      INPUT_NUMBER_OF_LINES_SKIPPED<-data_source_row
      # INPUT_NUMBER_OF_LINES_SKIPPED<-2
    }else if(time_check_probopdata>0){
      txt_file_source<-"ProbOpData"
      INPUT_NUMBER_OF_LINES_SKIPPED<-1
    }else{
      if (!is.null(id)){
        removeNotification(id)
      }
      showNotification("Unrecognized txt file.",duration=2)
      return()
    }
    if (interface_file_found){
      temp_data <- fread(INPUT_PROJECT_INPUT_TXT_FILE,
                         # header=TRUE,
                         nrows=100,
                         skip = INPUT_NUMBER_OF_LINES_SKIPPED,
                         dec=".",
                         na.strings=c("NULL", "ERROR", "-1.#INF", "1.#INF", "NA", "-1.#INF0", "1.#INF0"),blank.lines.skip=TRUE)

    }else{
      temp_data <- fread(INPUT_PROJECT_INPUT_TXT_FILE,
                         # header=TRUE,
                         # nrows=100,
                         skip = INPUT_NUMBER_OF_LINES_SKIPPED,
                         dec=".",
                         na.strings=c("NULL", "ERROR", "-1.#INF", "1.#INF", "NA", "-1.#INF0", "1.#INF0"),blank.lines.skip=TRUE)
      
    }
    
    if (txt_file_source=="TDE"){
      dataColumn <- ymd_hms(paste0(temp_data$V1,temp_data$V2))
    }else if(txt_file_source=="ProbOpData"){
      dataColumn <- ymd_hms(temp_data$V1)
    }
    INPUT_MY_FREQUENCY <- difftime(dataColumn[2],dataColumn[1],units = "secs")
    
    filename<-paste0(in_path,"/",project,"/",plant,"/",plant, ".R")
    
    template<-paste0(scripts_path,"/RCodeBlank.R")
    
    rl<-readLines(con=template)
    
    i<-which(grepl("Input_Turbine_DATA_Explorer_CSV_Path",rl,fixed=TRUE))[1]
    
    rl[i]<-paste0("Input_Turbine_DATA_Explorer_CSV_Path = '",in_path,"/",project,"/",plant,"/'")
    
    i<-which(grepl("INPUT_PROJECT_INPUT_TXT_FILE",rl,fixed=TRUE))[1]
    
    rl[i]<-paste0("INPUT_PROJECT_INPUT_TXT_FILE = '", plant,".txt'")
    
    i<-which(grepl("INPUT_NUMBER_OF_LINES_SKIPPED",rl,fixed=TRUE))[1]
    
    if (txt_file_source=="TDE"){
      rl[i]<-paste0("INPUT_NUMBER_OF_LINES_SKIPPED = ", data_source_row)  
    }else{
      rl[i]<-"INPUT_NUMBER_OF_LINES_SKIPPED = 1"
    }
    
    
    i<-which(grepl("INPUT_MY_FREQUENCY",rl,fixed=TRUE))[1]
    
    rl[i]<-paste0("INPUT_MY_FREQUENCY = ",INPUT_MY_FREQUENCY)
    
    i<-which(grepl("INPUT_Project_Name",rl,fixed=TRUE))[1]
    
    rl[i]<-paste0("INPUT_Project_Name = '",project,"'")
    
    i<-which(grepl("INPUT_Analysis_Title",rl,fixed=TRUE))[1]
    
    rl[i]<-paste0("INPUT_Analysis_Title = '",project,"-",plant,"'")
    
    i<-which(grepl("INPUT_Analysis_Title_underscored",rl,fixed=TRUE))[1]
    
    rl[i]<-paste0("INPUT_Analysis_Title_underscored = '",project,"_",plant,"'")
    
    i<-which(grepl("BASIC_Project_path",rl,fixed=TRUE))[1]
    
    rl[i]<-paste0("BASIC_Project_path = '",out_path,project,"/",plant,"/'")
    
    if (txt_file_source=="TDE"){
      
      numberofColumns<-length(colnames(temp_data))-1 #last column in TDE data is all NA
      startofColumns<-3 #skipping Date and Time column
      notusedColumns<-2
    }else if(txt_file_source=="ProbOpData"){
      numberofColumns<-length(colnames(temp_data))
      startofColumns<-2 #skipping Time column
      notusedColumns<-1
    }
        
    # i<-which(grepl("INPUT_MY_NUMBER_OF_INPUTS",rl,fixed=TRUE))[1]
    # print(i)
    # rl[i]<-paste0("INPUT_MY_NUMBER_OF_INPUTS = ",(l-2))
    
    bdc<-NULL
    r<-NULL
    r1<-NULL
    nay<-NULL
    nayn<-NULL
    minv<-NULL
    maxv<-NULL
    minval<-NULL
    maxval<-NULL
    vtext<-NULL
    vtext1<-NULL
    
    for (colmn in startofColumns:numberofColumns){
      mv<-NULL
      mav<-NULL
      # coln<-colnames(temp_data)[i]
      r<-c(r,paste0("INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_TYPE_V",(colmn-notusedColumns),"= 'GET'"))
      r1<-c(r1,paste0("INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_TYPE_V",(colmn-notusedColumns),"= 'LET'"))
      bdc<-paste0(bdc,"BDC",(colmn-notusedColumns)," & ")
      nay<-c(nay,paste0("#INPUT_VECTOR_REMOVE_NA_V", (colmn-notusedColumns)," = 'YES'"))
      nayn<-c(nayn,paste0("INPUT_VECTOR_REMOVE_NA_V", (colmn-notusedColumns)," = 'NO'"))
      
      found<-FALSE
      if (interface_file_found){
        if (txt_file_source=="TDE"){
          ii<-which(signal_info$SignalName==colnames(header_info)[colmn])
        }else if(txt_file_source=="ProbOpData"){
          ii<-which(signal_info$Info==colnames(header_info)[colmn])  
        }
        
        if (length(ii)>0){
          minv<-c(minv,paste0("INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE_V",(colmn-notusedColumns)," = " ,signal_info$Scalmin[ii]))
          maxv<-c(maxv,paste0("INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE_V",(colmn-notusedColumns)," = ", signal_info$Scalmax[ii]))
          found<-TRUE
        }
      }
      
      if (!found){
        if (interface_file_found){
          showNotification("Signal not found in interface file. ", duration =2)
          return()
        }
        # mv<-min(temp_data[[colmn]][2:dim(temp_data)[1]],na.rm=TRUE)
        # mav<-max(temp_data[[colmn]][2:dim(temp_data)[1]],na.rm=TRUE)
        # mv<-min(temp_data[[colmn]],na.rm=TRUE)
        # mav<-max(temp_data[[colmn]],na.rm=TRUE)
        mv<-min(temp_data[[colmn]],na.rm=TRUE)
        mav<-max(temp_data[[colmn]],na.rm=TRUE)
        minval<-c(minval,as.numeric(mv))
        maxval<-c(maxval,as.numeric(mav))
        if (mv>0)
          minv<-c(minv,paste0("INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE_V",(colmn-notusedColumns)," = " ,floor(0.9*as.numeric(mv))))
        else
          minv<-c(minv,paste0("INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE_V",(colmn-notusedColumns)," = " ,floor(1.1*as.numeric(mv))))
        
        if (mav>0)
          maxv<-c(maxv,paste0("INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE_V",(colmn-notusedColumns)," = ", ceiling(1.1*as.numeric(mav))))
        else
          maxv<-c(maxv,paste0("INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE_V",(colmn-notusedColumns)," = ", ceiling(0.9*as.numeric(mav))))
      }
      
      coln<-colnames(header_info)[colmn]
      if (found){
        unt<-signal_info$SignalUnit[ii]
      }else{
        # unt<-temp_data[[i]][1]
        unt<-header_info[[colmn]][1]
      }
      x1<-substr(rl[1],2,nchar(rl[1]))
      unt<-gsub(x1,"perc",unt,fixed = TRUE)
      x1<-substr(rl[2],2,nchar(rl[1]))
      unt<-gsub(x1,"deg",unt,fixed = TRUE)
      unt<-gsub("/","_",unt,fixed = TRUE)
      unt<-gsub(" ","_",unt,fixed = TRUE)
      
      if (grepl("@",coln,fixed=TRUE)){
        signame<-paste0(substr(coln,2,nchar(coln)),"_",unt)
        signame<-gsub("(","_",signame,fixed=TRUE)
        signame<-gsub(")","_",signame,fixed=TRUE)
        vtext<-c(vtext,paste0("INPUT_VECTOR_VXX_Title_Type_Text_V",(colmn-notusedColumns),"= '",signame,"'"))
      }else{
        signame<-paste0(coln,"_",unt)
        signame<-gsub("(","_",signame,fixed=TRUE)
        signame<-gsub(")","_",signame,fixed=TRUE)
        vtext<-c(vtext,paste0("INPUT_VECTOR_VXX_Title_Type_Text_V",(colmn-notusedColumns),"= '",signame,"'"))
      }
      
      if (grepl("@",coln,fixed=TRUE)){
        signame<-paste0(substr(coln,2,nchar(coln)),"_",unt)
        signame<-gsub("(","_",signame,fixed=TRUE)
        signame<-gsub(")","_",signame,fixed=TRUE)
        vtext1<-c(vtext1,paste0("INPUT_VECTOR_VXX_Title_Type_Underscored_TEXT_V",(colmn-notusedColumns),"= '",signame,"'"))
      }else{
        signame<-paste0(coln,"_",unt)
        signame<-gsub("(","_",signame,fixed=TRUE)
        signame<-gsub(")","_",signame,fixed=TRUE)
        vtext1<-c(vtext1,paste0("INPUT_VECTOR_VXX_Title_Type_Underscored_TEXT_V",(colmn-notusedColumns),"= '",signame,"'"))
      }
    }
    
    bdc<-substr(bdc,1,(nchar(bdc)-3))
    
    i<-which(grepl("INPUT_MY_NUMBER_OF_INPUTS",rl,fixed=TRUE))[1]
    
    rl[i]<-paste0("INPUT_MY_NUMBER_OF_INPUTS = ",(numberofColumns-notusedColumns))
    
    rlNew<-rl[1:i]
    rlNew<-c(rlNew,vtext,"##########",vtext1)
    rlNew<-c(rlNew,rl[(i+1):length(rl)])
    rl<-rlNew
    
    i<-which(grepl("# DEFINE DOMAIN_BASED_BOUNDARY_CONDITIONS",rl,fixed=TRUE))[1]
    
    i<-i+4
    rlNew<-rl[1:(i)]
    rlNew<-c(rlNew,minv,"###########",maxv)
    rlNew<-c(rlNew,rl[(i+1):length(rl)])
    rl<-rlNew
    
    i<-which(grepl("# DEFINE DOMAIN_BASED_CONDITION_TYPES",rl,fixed=TRUE))[1]
    
    i<-i+12
    rlNew<-rl[1:i]
    rlNew<-c(rlNew,r,"##########",r1)
    rlNew<-c(rlNew,rl[(i+1):length(rl)])
    rl<-rlNew
    
    i<-which(grepl("INPUT_LOGICAL_BDC_STRING",rl,fixed=TRUE))[1]
    
    # rl[i]<-paste0("INPUT_LOGICAL_BDC_STRING = '",bdc,"'")
    rl[i]<-paste0("INPUT_LOGICAL_BDC_STRING = ''")
    
    i<-which(grepl("# define for which Parameters NA shall be REMOVED",rl,fixed=TRUE))[1]
    
    i<-i+10
    rlNew<-rl[1:i]
    rlNew<-c(rlNew,nay,"#########",nayn)
    rlNew<-c(rlNew,rl[(i+1):length(rl)])
    rl<-rlNew
    
    writeLines(rl,con=filename)
    
    if (!is.null(id)){
      removeNotification(id)
      showNotification("RCode File created.",duration=2)
    }
    
    rm(temp_data)
    gc()
  }
  
  #############################
  #Global Controls - GUI elements on the main page
  
  #observeEvent is Shiny function to capture changes on the input controls (GUI elements) by user and trigger some funciton or code in response.
  
  #############################
  {
  
  observeEvent(input$rcodefile,ignoreInit = TRUE,{
    if (input$rcodefile==1){ # 1 = enable Rcode creation
      dr<-list.dirs(paste0(in_path), recursive=FALSE, full.name=FALSE)
      d<-data.frame(c("No Selection",dr))
      colnames(d)<-"projects"
      updateSelectInput(session,"rcodeproject",choices = unique(as.character(d$projects)),selected="No Selection")
    }
  })
  
  observeEvent(input$rcodeproject,ignoreInit = TRUE,{
    if ((input$rcodeproject!="")&(input$rcodeproject!="No Selection")){
      p<-list("No Selection"="No Selection")
      p1<-list.dirs(paste0(in_path,"/",input$rcodeproject), recursive=FALSE, full.name=FALSE)
      if (length(p1)==0){
        # if (!is.null(id))
        #   removeNotification(id)
        # id <<- NULL
        showNotification("Plant data not present.",duration=2)
        updateSelectInput(session,"rcodeplant",choices = list("No Selection"="No Selection"),
                          selected="No Selection")
        return()
      }
      for (i in 1:length(p1)) {
        p[[p1[i]]]<-p1[i]
      }
      updateSelectInput(session,"rcodeplant",choices = p,selected="No Selection")
    }
  })
  
  observeEvent(input$rcodecreate,ignoreInit = TRUE,{
    if ((input$rcodeproject!="No Selection")&(input$rcodeplant!="No Selection")){
      id<<-showNotification("Creating RCode File...",duration=NULL)
      Rcode_Generation(project=input$rcodeproject,plant=input$rcodeplant)
      # if (!is.null(id)){
      #   removeNotification(id)
      #   showNotification("RCode File created.",duration=2)
      # }
    }
  })
  
  observeEvent(input$sel1,{
    caller<<-"sel1"
    updatefromSel1()
  })
  
  observeEvent(input$sel2,ignoreInit = TRUE,{
    req(input$sel2)
    caller<<-"sel2"
    updatefromSel2()
    clear_sens()
    clear_all()
    updatefromLoad_Data()
    # conditions_check()
  })
  
  observeEvent(input$stat,{
    caller<<-"stat"
    # updateSelectInput(session,"last_ext",choices=list("No Selection"="No Selection"), selected="No Selection")
    updatefromLoad_Data()
    statChangeSettings()
    # visualfilter1D2Dupdate()
  })
  
  observeEvent(input$last_ext,{
    caller<<-"last_ext"
    updatefromLoad_Data()
  })
  
  observeEvent(input$gen_plot,{
    if (checkExtent()){
      updatePlotCaller<<-"GEN_PLOT"
      updatePlots()  
    }
  })
  
  observeEvent(input$load_data,{
    # if ((input$load_data==3)&(is.null(appEvt))){
    #   showNotification("Event Data not available.",duration=2)
    #   return()
    # }else if (input$load_data!=3){
    caller<<-"load_data"
    # updateSelectInput(session,"last_ext",choices=list("No Selection"="No Selection"), selected="No Selection")
    # conditions_check()
    updatefromLoad_Data()
    # }
    
  })
  
  observeEvent(input$sig1,{
    req(input$sig1)
    statChangeSettings()
  })
  
  observeEvent(input$extent,{
    req(input$extent)
    
    if (input$stat=="FREQUENCY_SENSITIVITY"){
      datasplitFreqsens<<-NULL
    }else if (input$stat=="ECDF_SENSITIVITY"){
      datasplitecdfsens<<-NULL
    }else if (input$stat=="DENSITY"){
      density1D<<-NULL
    }
    # if (checkExtent()){
      if (input$stat!="Data Processing"){
        # id<<-showNotification("Generating plot.....",duration=NULL)
        updatePlotCaller<<-"EXTENT"
        callerFilter<<-"Gen_Plot"
        # updatePlots()
      }
    #   # 
    # }
    
    # updatePlots()
  })
  
  #Local Controls - GUI elements inside evaluations eg. drop down menu for type of violin/box plot
  
  observeEvent(input$vio_box,ignoreInit = TRUE,{
    fl<-list()
    req(input$vio_box)
    file_out<-NULL
    pa=""
    if (input$load_data=="1"){
      pa<-path_to_tables()
      if (input$stat=="VIOLIN / BOX PLOTS"){
        file_out<-list.files(pa,pattern="*.txt", recursive=TRUE)
      }
      if (!is.null(file_out)){
        file_out<-c("No Selection", file_out)
        for (i in 1:length(file_out)){
          fl[[file_out[i]]]=file_out[i]
        }
      }
      
      updateSelectInput(session,"last_ext",choices=fl, selected="No Selection")
      showNotification("Please select last analysis file",duration=2)
    }
    
  })
  
  observeEvent(input$ecdf_check,ignoreInit = TRUE,{
    fl<-list()
    file_out<-NULL
    pa=""
    if (input$load_data=="1"){
      pa<-path_to_tables()
      # if (input$ecdf_check==TRUE){
      file_out<-list.files(pa,pattern="*.txt", recursive=TRUE)
      if (!is.null(file_out)){
        file_out<-c("No Selection", file_out)
        for (i in 1:length(file_out)){
          fl[[file_out[i]]]=file_out[i]
        }
        # }
        updateSelectInput(session,"last_ext",choices=fl, selected="No Selection")
        showNotification("Please select last analysis file",duration=2)
      }
    }
  })
  
  observeEvent(input$op_hours_plot,ignoreInit = TRUE,{
    file_out<-NULL
    fl<-list()
    pa<-path_to_tables()
    if ((input$load_data=="1")&&(input$stat=="OPERATING_HOURS")){
      file_out<-list.files(pa,pattern="*.txt", recursive=TRUE)

      if (!is.null(file_out)){
        file_out<-c("No Selection", file_out)
        for (i in 1:length(file_out)){
          fl[[file_out[i]]]=file_out[i]
        }
        updateSelectInput(session,"last_ext",choices=fl, selected="No Selection")
        showNotification("Please select last analysis file",duration=2)
      }
    }
  })
  
  }
  ################
  #messages indicating progress of plot creation
  # observe is a shiny function that monitors change in the value of reactiveValues. For each evaluation, the generated plots are stored
  # in a corresponding reactive value (eg. plts1d$freq) and after the plot is loaded in this reactive value, observe function is called to execute its code.
  ################
  {
  observe(
    if (input$stat=="X_Y_GRAPHS"){
      if (!is.null(plts$graph)){
        if (!is.null(id)){
          removeNotification(id)
          id<<-NULL
          showNotification("XY Graph created.",duration=2)
        }
      }
    }
    # else if (input$stat=="X_Y_COUNT"){
    #   if (!is.null(plts$counts)){
    #     if (!is.null(id)){
    #       removeNotification(id)
    #       id<<-NULL
    #       showNotification("XY Count plots created.",duration=2)
    #     }
    #   }
    # }
    else if (input$stat=="X_Y_KERNEL DENSITY"){
      if ((!is.null(plts$kernel1))&&(!is.null(plts$kernel2))){
        if (!is.null(id)){
          removeNotification(id)
          id<<-NULL
          showNotification("XY Kernel Density Plots created.",duration=2)
        }
      }
    }
    # else if (input$stat=="X_Y_PROBABILITY"){
    #   if (!is.null(plts$prob)){
    #     if (!is.null(id)){
    #       removeNotification(id)
    #       id<<-NULL
    #       showNotification("XY Probability Plots created.",duration=2)
    #     }
    #   }
    # }else if (input$stat=="X_Y_ECDF"){
    #   if (!is.null(plts$ecdf)){
    #     if (!is.null(id)){
    #       removeNotification(id)
    #       id<<-NULL
    #       showNotification("XY ECDF Plots created.",duration=2)
    #     }
    #   }
    # }
    else if (input$stat=="VIOLIN / BOX PLOTS"){
      if (!is.null(plts1d$vio_box)){
        if (!is.null(id)){
          removeNotification(id)
          id<<-NULL
          showNotification("Plots created.",duration=2)
        }
      }
    }else if (input$stat=="FREQUENCY"){
      if (!is.null(plts1d$freq)){
        if (!is.null(id)){
          removeNotification(id)
          id<<-NULL
          showNotification("Plots created.",duration=2)
        }
      }
    }
    # else if (input$stat=="DENSITY"){
    #   if (!is.null(plts1d$den)){
    #     if (!is.null(id)){
    #       removeNotification(id)
    #       id<<-NULL
    #       showNotification("Plots created.",duration=2)
    #     }
    #   }
    # }else if (input$stat=="PROBABILITY"){
    #   if (!is.null(plts1d$prob)){
    #     if (!is.null(id)){
    #       removeNotification(id)
    #       id<<-NULL
    #       showNotification("Plots created.",duration=2)
    #     }
    #   }
    # }
    else if (input$stat=="ECDF"){
      if (!is.null(plts1d$ecdf)){
        if (!is.null(id)){
          removeNotification(id)
          id<<-NULL
          showNotification("Plots created.",duration=2)
        }
      }
    }else if (input$stat=="OPERATING_HOURS"){
      if (!is.null(plts1d$op_hours)){
        if (!is.null(id)){
          removeNotification(id)
          id<<-NULL
          showNotification("Plots created.",duration=2)
        }
      }
    }else if (input$stat=="HOURS_DISTRIBUTION"){
      if (!is.null(plts1d$hours_dist)){
        if (!is.null(id)){
          removeNotification(id)
          id<<-NULL
          showNotification("Plots created.",duration=2)
        }
      }
    }
    
  )
  }
  
  ######################
  # plot & Zoom variables - define and initialize variables used in zooming of plots. 
  # ranges1d_**** and ranges2d_**** are reactive Values that carry extent of rectangular region selected on corresponding plot.
  # plts and plt1d are reactive values that store plots created on screen for different evaluations to be used for downloading as pdf.
  # plt***** are reactive values used for triggering creation of respective plot on 'GENERATE BUTTON' click on main GUI
  #####################
  {
  # ranges1d_den<-reactiveValues(x=NULL,y=NULL)
  ranges1d_ecdf<-reactiveValues(x=NULL,y=NULL)
  # ranges1d_ecdf_sens<-reactiveValues(x=NULL,y=NULL)
  ranges1d_freq<-reactiveValues(x=NULL,y=NULL)
  # ranges1d_freq_sens<-reactiveValues(x=NULL,y=NULL)
  ranges1d_prob<-reactiveValues(x=NULL,y=NULL)
  ranges1d_op_hours<-reactiveValues(x=NULL,y=NULL)
  ranges1d_hours_dist<-reactiveValues(x=NULL,y=NULL)
  ranges1d_vio_box<-reactiveValues(x=NULL,y=NULL)
  
  # ranges2D_x_y_count<-reactiveValues(x=NULL,y=NULL)
  # ranges2D_y_x_count<-reactiveValues(x=NULL,y=NULL)
  ranges2D_x_y_ker_den<-reactiveValues(x=NULL,y=NULL)
  # ranges2D_x_y_prob<-reactiveValues(x=NULL,y=NULL)
  # ranges2D_x_y_ecdf<-reactiveValues(x=NULL,y=NULL)
  ranges2D_x_y_graph<-reactiveValues(x=NULL,y=NULL)
  
  signal<-reactiveValues(sig1=NULL,sig2=NULL)
  plot_update<-reactiveValues(pl=NULL)
  plts<-reactiveValues(prob=NULL, graph=NULL, kernel1=NULL,kernel2=NULL, counts=NULL,counts1=NULL, 
                       ecdf=NULL,xy_count_sens=NULL,xy_graph_sens=NULL,xykden_sens=NULL)
  plts1d<-reactiveValues(freq_sens=NULL,vio_box=NULL,freq=NULL,den=NULL,prob=NULL,ecdf=NULL,op_hours=NULL,hours_dist=NULL)
  

	plt_xygraph<-reactiveValues(y="No")
	# plt_xycount<-reactiveValues(y="No")
	plt_xykerden<-reactiveValues(y="No")
	# plt_xyprob<-reactiveValues(y="No")
	# plt_xyecdf<-reactiveValues(y="No")
	plt_freq<-reactiveValues(y="No")
	plt_den<-reactiveValues(y="No")
	plt_ecdf<-reactiveValues(y="No")
	plt_prob<-reactiveValues(y="No")
	plt_vio_box<-reactiveValues(y="No")
	# plt_xy_mov<-reactiveValues(y="No")
	# plt_xy_sens<-reactiveValues(y="No")
	plt_xykden_sens<-reactiveValues(y="No")
	plt_ecdf_sens<-reactiveValues(y="No")
	plt_freq_sens<-reactiveValues(y="No")
	plt_den_sens<-reactiveValues(y="No")
	# plt_prob_sens<-reactiveValues(y="No")
	plt_op_hours<-reactiveValues(y="No")
	# plt_freq_mov<-reactiveValues(y="No")
	# plt_den_mov<-reactiveValues(y="No")
	# plt_prob_mov<-reactiveValues(y="No")
	plt_ecdf_mov<-reactiveValues(y="No")
	plt_hours_dist<-reactiveValues(y="No")
	# plt_time_series_mov<-reactiveValues(y="No")
	# plt_time_series_plot<-reactiveValues(y="No")
	
  }
  
  
  # Function updatePlots is called to update plot for selected evaluation on click of 'GENERATE PLOT' button. Respective reactive values
  # are set for each evaluation
  updatePlots<-function(){
    filterSelection<-input$extent
    callerFilter<<-"Gen_Plot"
    if((input$load_data==3)&(is.null(appEvt))){
      showNotification("Events not defined.",duration=2)
      return()
    }
    if (input$stat=="X_Y_GRAPHS"){
      # if (((input$load_data==2)|(input$load_data==3)) &&((input$sig1=="No Selection")||(input$sig2=="No Selection"))){
      #   showNotification("Please select Signal 1 and Signal 2.")
      # }else if (input$sig1==input$sig2){
      #   showNotification("Selected Signal 1 and Signal 2 are same. Please check.")
      # }else if((input$load_data==1) &&(input$last_ext=="No Selection")){
      #   showNotification("Please select last analysis.")
      # }else{
        # id<<-NULL
        # id<<-showNotification("Generating XY Graph.",duration=0)
        plt_xygraph$y<-"No"
        plt_xygraph$y<-"Yes"
      # }
    }
    # if (input$stat=="X_Y_COUNT"){
    #   if (((input$load_data==2)|(input$load_data==3)) &&((input$sig1=="No Selection")||(input$sig2=="No Selection"))){
    #     showNotification("Please select Signal 1 and Signal 2.")
    #   }else if (input$sig1==input$sig2){
    #     showNotification("Selected Signal 1 and Signal 2 are same. Please check.")
    #   }else if((input$load_data==1) &&(input$last_ext=="No Selection")){
    #     showNotification("Please select last analysis.")
    #   }else{
    #     id<<-NULL
    #     id<<-showNotification("Generating XY Count Plots.",duration=0)
    #     plt_xycount$y<-"No"
    #     plt_xycount$y<-"Yes"
    #   }
    # }
    if (input$stat=="X_Y_KERNEL DENSITY"){
      # if (((input$load_data==2)|(input$load_data==3)) &((input$sig1=="No Selection")|(input$sig2=="No Selection"))){
      #   showNotification("Please select Signal 1 and Signal 2.")
      # }else if (input$sig1==input$sig2){
      #   showNotification("Selected Signal 1 and Signal 2 are same. Please check.")
      # }else if((input$load_data==1) &&(input$last_ext=="No Selection")){
      #   showNotification("Please select last analysis.")
      # }else{
      #   ct1<<-NULL
      #   ct_den<<-NULL
      #   ct_den_levels<<-NULL
      #   min_den<<-NULL
      #   max_den<<-NULL
      #   den_breaks<-reactiveValues(br=NULL)
      #   id<<-NULL
      #   density_data<<-NULL
      #   id<<-showNotification("Generating XY Kernel Density Plots.",duration=0)
        
        plt_xykerden$y<-"No"  
        plt_xykerden$y<-"Yes"  
      # }
    }
    # if (input$stat=="X_Y_PROBABILITY"){
    #   if (((input$load_data==2)|(input$load_data==3)) &&((input$sig1=="No Selection")||(input$sig2=="No Selection"))){
    #     showNotification("Please select Signal 1 and Signal 2.")
    #   }else if (input$sig1==input$sig2){
    #     showNotification("Selected Signal 1 and Signal 2 are same. Please check.")
    #   }else if((input$load_data==1) &&(input$last_ext=="No Selection")){
    #     showNotification("Please select last analysis.")
    #   }else{
    #     id<<-NULL
    #     id<<-showNotification("Generating XY Probability Plots.",duration=0)
    #     plt_xyprob$y<-"No"
    #     plt_xyprob$y<-"Yes"
    #   }
    # }
    # if (input$stat=="X_Y_ECDF"){
    #   if (((input$load_data==2)|(input$load_data==3)) &&((input$sig1=="No Selection")||(input$sig2=="No Selection"))){
    #     showNotification("Please select Signal 1 and Signal 2.")
    #   }else if (input$sig1==input$sig2){
    #     showNotification("Selected Signal 1 and Signal 2 are same. Please check.")
    #   }else if((input$load_data==1) &&(input$last_ext=="No Selection")){
    #     showNotification("Please select last analysis.")
    #   }else{
    #     id<<-NULL
    #     id<<-showNotification("Generating XY ECDF Plots.",duration=0)
    #     plt_xyecdf$y<-"No"
    #     plt_xyecdf$y<-"Yes"
    #   }
    # }
    if (input$stat=="FREQUENCY"){
      # if (((input$load_data==2)|(input$load_data==3)) &&(input$sig1=="No Selection")){
      #   showNotification("Please select Signal 1.")
      # }else if (((input$load_data==2)|(input$load_data==3)) &&(input$freq_brk=="No Selection")){
      #   showNotification("Please select breaks.")
      # }
      # else if((input$load_data==1) &&(input$last_ext=="No Selection")){
      #   showNotification("Please select last analysis.")
      # }else{
        # id<<-NULL
        # id<<-showNotification("Generating Frequency Plots.",duration=0)
        
        plt_freq$y<-"No"
        plt_freq$y<-"Yes"
        
      # }
    }
    # if (input$stat=="DENSITY"){
    #   if (((input$load_data==2)|(input$load_data==3)) &&(input$sig1=="No Selection")){
    #     showNotification("Please select Signal 1.")
    #   }else if (((input$load_data==2)|(input$load_data==3)) &&(input$den_brk=="No Selection")){
    #     showNotification("Please select breaks.")
    #   } else if((input$load_data==1) &&(input$last_ext=="No Selection")){
    #     showNotification("Please select last analysis.")
    #   }else{
    #     id<<-NULL
    #     id<<-showNotification("Generating Density Plots.",duration=0)
    #     plt_den$y<-"No"
    #     plt_den$y<-"Yes"
    #   }
    # }
    # if (input$stat=="PROBABILITY"){
    #   if (((input$load_data==2)|(input$load_data==3)) &&(input$sig1=="No Selection")){
    #     showNotification("Please select Signal 1.")
    #   }else if (((input$load_data==2)|(input$load_data==3)) &&(input$prob_brk=="No Selection")){
    #     showNotification("Please select breaks.")
    #   }
    #   else if((input$load_data==1) &&(input$last_ext=="No Selection")){
    #     showNotification("Please select last analysis.")
    #   }else{
    #     id<<-NULL
    #     id<<-showNotification("Generating Probability Plots.",duration=0)
    #     plt_prob$y<-"No"
    #     plt_prob$y<-"Yes"
    #   }
    # }
    # if (input$stat=="X_Y_GRAPH_COUNT_SENSITIVITY"){
    #   if (((input$load_data==2)|(input$load_data==3)) &&((input$sig1=="No Selection")||(input$sig2=="No Selection"))){
    #     showNotification("Please select Signal 1 and Signal 2.")
    #   }else if (input$sig1==input$sig2){
    #     showNotification("Selected Signal 1 and Signal 2 are same. Please check.")
    #   }else if (input$xy_sens=="No Selection"){
    #     showNotification("Please select signal for sensitivity.")
    #   }else{
    #     plt_xy_sens$y<-"No"  
    #     plt_xy_sens$y<-"Yes"  
    #   }
    #   
    #   # plt_xycountsens$y<-"Yes"
    # }
    if (input$stat=="X_Y_KERNEL_DENSITY_SENSITIVITY"){
      if (((input$load_data==2)|(input$load_data==3)) &&((input$sig1=="No Selection")||(input$sig2=="No Selection"))){
        showNotification("Please select Signal 1 and Signal 2.")
      }else if (input$sig1==input$sig2){
        showNotification("Selected Signal 1 and Signal 2 are same. Please check.")
      }else if (input$xykden_sens=="No Selection"){
        showNotification("Please select signal for sensitivity.")
      }else{
        plt_xykden_sens$y<-"No"  
        plt_xykden_sens$y<-"Yes"  
      }
      
      # plt_xycountsens$y<-"Yes"
    }
    if (input$stat=="ECDF"){
      # if (((input$load_data==2)|(input$load_data==3))&&(input$sig1=="No Selection")){
      #   showNotification("Please select Signal 1.")
      # }else if((input$load_data==1) &&(input$last_ext=="No Selection")){
      #   showNotification("Please select last analysis.")
      # }else if (input$ecdf_check=="No Selection"){
      #   showNotification("Please select plot type.")
      # }else{
        # id<<-NULL
        # id<<-showNotification("Generating ECDF Plot.",duration=0)
        plt_ecdf$y<-"No"
        plt_ecdf$y<-"Yes"
      # }
    }
    if (input$stat=="ECDF_SENSITIVITY"){
      if (input$sig1=="No Selection"){
        showNotification("Please select Signal 1.",duration=2)
      }else{
        # showNotification("Creating plot files.",duration=2)
        plt_ecdf_sens$y<-"No"
        plt_ecdf_sens$y<-"Yes"
        # data1d_ecdf_sens()
      }
    }
    if (input$stat=="FREQUENCY_SENSITIVITY"){
      if (input$sig1=="No Selection"){
        showNotification("Please select Signal 1.",duration=2)
      }else{
        # showNotification("Creating plot files.",duration=2)
        # data1d_freq_sens()
        plt_freq_sens$y<-"No"  
        plt_freq_sens$y<-"Yes"  
      }
      
    }
    # if (input$stat=="DENSITY_SENSITIVITY"){
    #   if (input$sig1=="No Selection"){
    #     showNotification("Please select Signal 1.",duration=2)
    #   }else{
    #     showNotification("Creating plot files.",duration=2)
    #     data1d_den_sens()
    #     plt_den_sens$y<-"No"	   
    #     plt_den_sens$y<-"Yes"	   
    #   }
    # }
    # if (input$stat=="PROBABILITY_SENSITIVITY"){
    #   if (input$sig1=="No Selection"){
    #     showNotification("Please select Signal 1.",duration=2)
    #   }else{
    #     showNotification("Creating plot files.",duration=2)
    #     data1d_prob_sens()
    #     plt_prob_sens$y<-"No"	   
    #     plt_prob_sens$y<-"Yes"	   
    #   }
    # }
    if (input$stat=="OPERATING_HOURS"){
      # if ((input$load_data==1)&&(input$last_ext=="No Selection")){
      #   showNotification("Please select last analysis output",duration=2,type="message")
      # } else if ((input$load_data==1)&&(input$op_hours_plot=="No Selection")){
      #   showNotification("Please select plot type",duration=2,type="message")
      # } else if (((input$load_data==2)|(input$load_data==3))&&(input$sig1=="No Selection")){
      #   showNotification("Please select Signal 1 for Analysis",duration=2,type="message")
      # } else if (((input$load_data==2)|(input$load_data==3))&&(input$op_hours_plot=="No Selection")){
      #   showNotification("Please select plot type",duration=2,type="message")
      # } else {
        # id<<-NULL
        # id<<-showNotification("Generating Operating Hours Plot.",duration=0)
        plt_op_hours$y<-"No"
        plt_op_hours$y<-"Yes"
      # }
    }
    if (input$stat=="VIOLIN / BOX PLOTS"){
      # if (((input$load_data==2)|(input$load_data==3)) &&(input$sig1=="No Selection")){
      #   showNotification("Please select Signal 1.")
      # }else if((input$load_data==1) &&(input$last_ext=="No Selection")){
      #   showNotification("Please select last analysis.")
      # }else{
        # id<<-NULL
        # id<<-showNotification("Generating VIOLIN / BOX Plot.",duration=0)
        plt_vio_box$y<-"No"
        plt_vio_box$y<-"Yes"
      # }
    }
    if (input$stat=="HOURS_DISTRIBUTION"){
      # if ((input$load_data==1)&&(input$last_ext=="No Selection")){
      #   showNotification("Please select last analysis output",duration=2,type="message")
      # } else if (input$sig1=="No Selection"){
      #   showNotification("Please select Signal 1 for Analysis",duration=2,type="message")
      # } else if ((input$load_data!=1)&(input$hours_dist_brk=="No Selection")){
      #   showNotification("Please select breaks for Analysis",duration=2,type="message")
      # } else {
        # id<<-NULL
        # id<<-showNotification("Generating Hours Distribution Plot.",duration=0)
        plt_hours_dist$y<-"No"  
        plt_hours_dist$y<-"Yes"  
      # }
      
    }
  }
  
  # Function to update data filtering control (input$extent) on main GUI when data filter is created in 1D, 2D or nD. 
  updateExtentStatus<-function(sel=0){
  
    
    if ((is.null(dataFilter1D))&(is.null(dataFilter2D))&(is.null(dataFilternD))){ #000
      # updateCheckboxGroupInput(session,"extent",label="Data in Extent : NO",choiceNames = list("1D Filter","2D Filter","nD Filter"),
      #                          choiceValues = list(1,2,3),selected = sel)
      updateRadioButtons(session,"extent",label="Data in Extent : NO",choices = list("No Filter"=0,"1D Filter"=1,"2D Filter"=2,"nD Filter"=3),
                         selected = sel)
    }else if ((is.null(dataFilter1D))&(is.null(dataFilter2D))&(!is.null(dataFilternD))){ #001
      # updateCheckboxGroupInput(session,"extent",label="Data in Extent : YES",choiceNames = list("1D Filter","2D Filter","nD Filter - YES"),
      #                          choiceValues = list(1,2,3),selected = sel)
      updateRadioButtons(session,"extent",label="Data in Extent : YES",choices = list("No Filter"=0,"1D Filter"=1,"2D Filter"=2,"nD Filter - YES"=3),
                         selected = sel)
    }else if ((is.null(dataFilter1D))&(!is.null(dataFilter2D))&(!is.null(dataFilternD))){#011
      # updateCheckboxGroupInput(session,"extent",label="Data in Extent : YES",choiceNames = list("1D Filter","2D Filter - YES","nD Filter - YES"),
      #                          choiceValues = list(1,2,3),selected = sel)
      updateRadioButtons(session,"extent",label="Data in Extent : YES",choices = list("No Filter"=0,"1D Filter"=1,"2D Filter - YES"=2,"nD Filter - YES"=3),
                         selected = sel)
    }else if ((is.null(dataFilter1D))&(!is.null(dataFilter2D))&(is.null(dataFilternD))){#010
      # updateCheckboxGroupInput(session,"extent",label="Data in Extent : YES",choiceNames = list("1D Filter","2D Filter - YES","nD Filter"),
      #                          choiceValues = list(1,2,3),selected = sel)
      updateRadioButtons(session,"extent",label="Data in Extent : YES",choices = list("No Filter"=0,"1D Filter"=1,"2D Filter - YES"=2,"nD Filter"=3),
                         selected = sel)
    }else if ((!is.null(dataFilter1D))&(is.null(dataFilter2D))&(is.null(dataFilternD))){#100
      # updateCheckboxGroupInput(session,"extent",label="Data in Extent : YES",choiceNames = list("1D Filter - YES","2D Filter","nD Filter"),
      #                          choiceValues = list(1,2,3),selected = sel)
      updateRadioButtons(session,"extent",label="Data in Extent : YES",choices = list("No Filter"=0,"1D Filter - YES"=1,"2D Filter"=2,"nD Filter"=3),
                         selected = sel)
    }else if ((!is.null(dataFilter1D))&(is.null(dataFilter2D))&(!is.null(dataFilternD))){#101
      # updateCheckboxGroupInput(session,"extent",label="Data in Extent : YES",choiceNames = list("1D Filter - YES","2D Filter","nD Filter - YES"),
      #                          choiceValues = list(1,2,3),selected = sel)
      updateRadioButtons(session,"extent",label="Data in Extent : YES",choices = list("No Filter"=0,"1D Filter - YES"=1,"2D Filter"=2,"nD Filter - YES"=3),
                         selected = sel)
    }else if ((!is.null(dataFilter1D))&(!is.null(dataFilter2D))&(is.null(dataFilternD))){#110
      # updateCheckboxGroupInput(session,"extent",label="Data in Extent : YES",choiceNames = list("1D Filter - YES","2D Filter - YES","nD Filter"),
      #                          choiceValues = list(1,2,3),selected = sel)
      updateRadioButtons(session,"extent",label="Data in Extent : YES",choices = list("No Filter"=0,"1D Filter - YES"=1,"2D Filter - YES"=2,"nD Filter"=3),
                         selected = sel)
    }else if ((!is.null(dataFilter1D))&(!is.null(dataFilter2D))&(!is.null(dataFilternD))){#111
      # updateCheckboxGroupInput(session,"extent",label="Data in Extent : YES",choiceNames = list("1D Filter - YES","2D Filter - YES","nD Filter - YES"),
      #                          choiceValues = list(1,2,3),selected = sel)
      updateRadioButtons(session,"extent",label="Data in Extent : YES",choices = list("No Filter"=0,"1D Filter - YES"=1,"2D Filter - YES"=2,"nD Filter - YES"=3),
                         selected = sel)
    }
   
  }
	
  ######################################
  # Function dataFilterEventApply is called to apply selected datafilter on the dataset. Options are 1D, 2D, nD and event data
  # Arguments:
  # filterSelection = selection on main GUI (Value of input$extent)
  # dataFilter1D, dataFilter2D, dataFilternD - logical vectors (TRUE/FALSE) of length equal to number of rows in dataset from representing datafilter
  # i, j represent column number in dataset for Signal 1 and Signal 2 selected on main GUI
  # PROJECT_DATA_VXX - complete dataset loaded in application
  # dataFilterEvents - logical vector representing applied event
  # appEvtName - name of the applied event eg. EVT1
  # callerFilter tells the function which control on GUI is calling dataFilterEventApply function. eg. for GENERATE PLOT BUTTON click callerFilter = 'gen_plot'
  ######################################
  
	callerFilter<-"Gen_Plot"
	dataFilterEventApply<-function(filterSelection=0,dataFilter1D=NULL,dataFilter2D=NULL,
	                            dataFilternD=NULL,i=0,j=0,PROJECT_DATA_VXX=PROJECT_DATA_VXX,
	                            dataFilterEvents=NULL,appEvtName,callerFilter="Gen_Plot"){
	  
	  PROJECT_DATA_VXX_COPY<-NULL
	  appFilter<-NULL
	  filterSource<-NULL
	  dataSource<-"FILTERED"
	  if (is.null(callerFilter)){
	    return()
	  }
	  
	  if (callerFilter=="1D_Apply"){
	    if (!is.null(dataFilter1D)){
	      appFilter<- dataFilter1D 
	      filterSource<-"1D Filter"
	    }
	  }else if (callerFilter=="2D_Apply"){
	    if (!is.null(dataFilter2D)){
	      appFilter<-dataFilter2D
	       filterSource<-"2D Filter"
	    }
	  }else if (callerFilter=="Gen_Plot"){
	    if (filterSelection==0){
	      appFilter<-rep(TRUE,dim(PROJECT_DATA_VXX)[1])
	      dataSource<-"COMPLETE"
	      filterSource<-"NONE"
	    }else if (filterSelection==1){
	      if (!is.null(dataFilter1D)){
	        appFilter<- dataFilter1D
	        filterSource<-"1D Filter"  
	      }  
	    }else if (filterSelection==2){
	      if (!is.null(dataFilter2D)){
	        appFilter<- dataFilter2D
	        filterSource<-"2D Filter"  
	      }  
	    }else if (filterSelection==3){
	      if (!is.null(dataFilternD)){
	        appFilter<- dataFilternD
	        filterSource<-"nD Filter"  
	      }  
	    }
	   else if(callerFilter=="Reset"){
	     appFilter<-rep(TRUE,dim(PROJECT_DATA_VXX)[1])
	     dataSource<-"COMPLETE"
	     filterSource<-"NONE"
	   }
	    
	  }
	  
	  if (!is.null(dataFilterEvents)){ #Events 
	    dataSource<-appEvtName
	    appFilter<-appFilter & dataFilterEvents
	  }
	  
	  PROJECT_DATA_VXX_FILTERED<-PROJECT_DATA_VXX[appFilter,]
	  val<-paste0("Data Source : ",dataSource," \n","Filtering Source : ",filterSource)
	  
	  if ((i!=0)&(j!=0)){ #2D plots
	    PROJECT_DATA_VXX_FILTERED<-PROJECT_DATA_VXX_FILTERED[!is.na(PROJECT_DATA_VXX_FILTERED[[i]]) & 
	                                                          !is.na(PROJECT_DATA_VXX_FILTERED[[j]]) ,]  
	  }else if ((i!=0)&(j==0)){ # 1D plots
	    PROJECT_DATA_VXX_FILTERED<-PROJECT_DATA_VXX_FILTERED[!is.na(PROJECT_DATA_VXX_FILTERED[[i]]) ,]  
	  }
	  
	  return(list(PROJECT_DATA_VXX_FILTERED,val))
	}
	
	
	
	#########################################
	# Function EquationBasedDataFilter applies logical conditions created in data filtering to the dataset.
	# Arguments:
	# EBCs - list of generated logical conditions in vector form eg.EBC1 = V1>100 
	# LogicalString - String representing boolean combination of EBCs eg. EBC1 & EBC2
	# PROJECT_DATA - Dataset to apply conditions on (datatable)
	# Return value:
	# Logical vector (TRUE/FALSE) of length equal to number of rows in dataset 
	##########################################
	
	EquationBasedDataFilter<-function(EBCs,LogicalString,PROJECT_DATA){
	  
	  INPUT_NUMBER_OF_EBCs<-length(EBCs)
	  dataFilter<-NULL
	  # my_for_eval_INPUT_VECTOR_EBC = "INPUT_VECTOR_EBC <- c("
	  my_for_eval_analyse_string<-LogicalString
	  e<-list()
	  if (!is.null(EBCs)){
	    # if (length(LSList)>0){
	    for (i in 1:INPUT_NUMBER_OF_EBCs)
	      # for (i in 1:length(LSList))
	    {
	      
	      # eval(parse(text=EBCs[i]))
	      j<-regexpr("=",EBCs[i],fixed=TRUE)
	      ebc_space<-substr(EBCs[i],1,j[1][1]-1)
	      ebc<-trimws(ebc_space)
	      e[[ebc]]<-trimws(substr(EBCs[i],j[1][1]+1,nchar(EBCs[i])))
	      j1<-regexpr(ebc,my_for_eval_analyse_string,fixed=TRUE)
	      if (length(j1[1][1])>0){
	        my_for_eval_analyse_string<-gsub(paste0(" ",ebc," "),trimws(substr(EBCs[i],j[1][1]+1,nchar(EBCs[i]))),my_for_eval_analyse_string)
	      }
	    }
	    expr<-parse(text= my_for_eval_analyse_string)[[1]]
	    # print(expr)
	    dataFilter<-(eval(expr,PROJECT_DATA))
	    return(dataFilter)
	  }
	  
	}
	
	#########################################
	# Function checkEBCs checks EBCs and LogicalStrings created in 1D, 2D and nD filter so that any EBCs is not repeated and LogicalStrings of 
	# two filters are correctly combined. This is called while creating new EBCs and logical string for either 1D, 2D or nD data filter when one of these 
	# data fitlering condition is already applied on the dataset. eg. user has created 1D datafilter conditions already and now with the
	# reduced dataset, user is creating 2D data filter logical conditions to apply on 1D filtered dataset.
	
	# Arguments:
	# caller - numeric value to represent filter that is creating EBCs (1 = 1D, 2 = 2D, 3 = nD)
	# filterSelection - Selection on main GUI for filter (numeric value of input$extent). Represents the filtered that is already applied.
	# EBCs1d - list of EBCs defined using 1D data filtering
	# LogicalString1d - String representing boolean combination of EBCs1d
	# EBCs2d - list of EBCs defined using 2D data filtering
	# LogicalString2d - String representing boolean combination of EBCs2d
	# EBCs - list of EBCs defined using nD data filtering
	# LogicalString - String representing boolean combination of EBCs
	# cdnPartCounter - counter value representing part of logical condition that is being created
	# cdnTxt - String representing logical condition (EBC) created so far
	# lsTxt - String represeting logical string created so far
	
	# Return value:
	# list containing merged EBCs, merged LogicalString and String used for display EBCs on GUI
	##########################################
	
	checkEBCs<-function(caller,filterSelection,EBCs1d,LogicalString1d,EBCs2d,LogicalString2d,
	                    EBCs,LogicalString,cdnPartCounter,cdnTxt,lsTxt){
	  ebc_to<-NULL
	  ebc_from<-NULL
	  LogicalString_from<-NULL
	  LogicalString_to<-NULL
	  if (caller==2){
	    cd<-"2D-"
	    ebc_to<-EBCs2d
	    LogicalString_to<-LogicalString2d
	  }else if(caller==1){
	    cd<-"1D-"
	    ebc_to<-EBCs1d
	    LogicalString_to<-LogicalString1d
	  }else if (caller==3){
	    ebck_to<-EBCs
	    LogicalString_to<-LogicalString
	    cd=" "
	  }
	  
	  if (filterSelection==1){
	    ebc_from<-EBCs1d
	    LogicalString_from<-LogicalString1d
	  }else if(filterSelection==2){
	    ebc_from<-EBCs2d
	    LogicalString_from<-LogicalString2d
	  }else if (filterSelection==3){
	    ebc_from<-EBCs
	    LogicalString_from<-LogicalString
	  }
	  
	  # if (((!is.null(ebc_from))&(!is.null(LogicalString_from)))&((!is.null(ebc_from))&(!is.null(LogicalString_from)))){
	  if (filterSelection!=0){
	    if (caller!=filterSelection){
	      if ((!is.null(LogicalString_from))&(!is.null(ebc_from))){
	        txt<-LogicalString_from
	        for (i in 1:length(ebc_from)){
	          cdnPartCounter<-cdnPartCounter+1
	          st<-regexpr("=",ebc_from[i],fixed=TRUE)[[1]]+1
	          cdn1<-trimws(substr(ebc_from[i],st,nchar(ebc_from[i])))
	          found<-FALSE
	          for (j in 1:length(ebc_to)){
	            st2<-regexpr("=",ebc_to[j],fixed=TRUE)[[1]]+1
	            cdn2<-trimws(substr(ebc_to[j],st2,nchar(ebc_to[j])))
	            
	            if (cdn1==cdn2){
	              found<-TRUE
	              cdnname_from<-trimws(substr(ebc_from[i],1,st-2))
	              cdnname_to<-trimws(substr(ebc_to[j],1,st2-2))
	              txt<-gsub(paste0(" ",cdnname_from," "),paste0(" ",cdnname_to," "),txt,fixed=TRUE) 
	            }
	          }
	          if (!found){
	            cdn<-paste0(" ",cd,"EBC",cdnPartCounter," = ",trimws(substr(ebc_from[i],st,nchar(ebc_from[i]))),"\n")
	            cdnTxt<-paste0(cdnTxt,cdn)
	            ebc_to<-c(ebc_to,cdn)
	            cdnname<-trimws(substr(ebc_from[i],1,st-2))
	            txt<-gsub(paste0(" ",cdnname," "),paste0(" ",cd,"EBC",cdnPartCounter," "),txt,fixed=TRUE) 
	          }
	          
	        }
	        dispcdnTxt<-paste0(cdnTxt,"\n","Logical String: (",txt," & ",lsTxt," )")
	        lsTxt<-paste0("(",txt," & ",lsTxt," )")  
	      }else{
	        dispcdnTxt<-paste0(cdnTxt,"\n","Logical String: ",lsTxt)
	      }
	      
	    }else if(caller==filterSelection){
	      if (!is.null(LogicalString_from)){
	        lsTxt<-paste0("(( ",LogicalString_from,") & (",lsTxt," ))")
	      }
	      dispcdnTxt<-paste0(cdnTxt,"\n","Logical String: ",lsTxt)  
	    }
	    
	  }else{
	    dispcdnTxt<-paste0(cdnTxt,"\n","Logical String: ",lsTxt)  
	  }
	  
	  
	  return(list(ebc_to,LogicalString_to,cdnTxt,dispcdnTxt,lsTxt))
	  
	}
	
	
	###########################################
	# Function checkExtent checks correctness of selection on main GUI for data filtering option (input$extent) with data source (input$load_data).
	# eg. if option other that 'No Filter' is selected with 'Last analysed output', user gets a message to 'Data not Loaded' 
	
	# Argument: NONE
	
	# Return Value: Boolean (TRUE/FALSE)
	###########################################
	
	checkExtent<-function(){
	  ok<-TRUE
	  if (input$load_data==1){
	    if (input$extent!=0){
	      ok<-FALSE
	      showNotification("Data not loaded.",duration=2)
	    }
	  }else if (input$load_data==3){
	    if (is.null(appEvt)){
	      ok<-FALSE
	      showNotification("Event Data not available.",duration=2)
	    }
	  }
	  if (ok){
	    
	    if ((input$extent==1)&(is.null(dataFilter1D))){
	      ok<-FALSE
	      showNotification("Filter not defined.",duration=2)
	    }
	    if ((input$extent==2)&(is.null(dataFilter2D))){
	      ok<-FALSE
	      showNotification("Filter not defined.",duration=2)
	    }
	    if ((input$extent==3)&(is.null(dataFilternD))){
	      ok<-FALSE
	      showNotification("Filter not defined.",duration=2)
	    }  
	  }
	  # if (ok){
	  #   id<<-showNotification("Generating Plot....",duration=NULL)
	  # }
	  
	  return(ok)
	}
	
	
	##############################################################################################
	# Data filtering logic
	# 1D filtering conditions are in EBCs1d vector and LogicalString1d
	
	
	# Update plots
	# Trigger #1 - GEN_PLOT button
	# Trigger #2 - Filter selection - input$extent
	# Trigger #3 - 1d filter application - input$filter_1d_app
	# Trigger #4 - 2d filter application
	###############################################################################################
	
	#Boundary conditions
	{
	  
	dataFilternD<-NULL                                       # stores boolean vector for nD filter
	#PROJECT_DATA_VXX_FILTERED_nD<-NULL
	  
	cdn<-reactiveValues(val=NULL)                            # reactive value for storing partial EBCs in R Code form 
	cdn_hist<-c()                                            # character vector storing parts of EBCs as user builds condition through GUI
	cdn_hist_vis<-c()                                        # character vector storing parts of EBCs with signal names as KKS (used to display on screen)
	cdn_hist_v<-c()                                          # character vector storing parts of EBCs with signal names in vector form (V1, V2 etc)
	cdn_vis<-reactiveValues(val=NULL)                        # reactive value for storing partial EBCs to trigger display of string on screen as user builds condition
	cdn_v<-reactiveValues(val=NULL)                          # reactive value for storing partial EBCs to trigger display of string on screen as user builds condition
	cdn_last<-c("start")                                     # temporary character vector to save sequence of additions made by user while building condition. eg. signal, operator. 'start' is start of EBC
	cdn_last_cnt<-c()
	cdn_last_selection<-c()
	bracketOpen_cnt<-0                                       # counter to keep track of number of opened brackets
	bracketClose_cnt<-0                                      # counter to keep track of number of closed brackets
	bracketOpen<-c()                                         # character vector used to keep track of whether open bracket '(' is added by user, 0 - not added, 1- added
	bracketClose<-c()                                        # character vector used to keep track of whether close bracket ')' is added by user, 0 - not added, 1- added
	
	
	################################
	# Function addCdn is called when user clicks 'Add' button on Boundary Conditions tab while creating logical condition (EBC). It performs checks to ensure correct
	# logical condition is created by the user.
	
	# Argument: NONE
	
	# Return value: NONE. Updates some variables using '<<-' operator defined in its parent scope. 
	################################
	
	addCdn<-function(){
	  last_cnt<-0
	  addsig<-FALSE
	  
	  if (input$sig1_cdn!="No Selection"){
	    if ((cdn_last[length(cdn_last)]=="operator")|(cdn_last[length(cdn_last)]=="start")){
	      j<-which(INPUT_VECTOR_VXX_Title_Type_Text==input$sig1_cdn)
	      cdn$val<-paste0(cdn$val,"PROJECT_DATA_VXX[[",j,"]]")
	      cdn_vis$val<-paste0(cdn_vis$val," ",input$sig1_cdn)
	      cdn_v$val<-paste0(cdn_v$val," V",j)
	      cdn_last<<-c(cdn_last,"signal")  
	      last_cnt<-last_cnt+1
	      addsig<-TRUE
	    }else{
	      showNotification("Cannot add consecutive signals without operator", duration=2)
	    }
	  }
	  
	  bracketOpen<<-c(bracketOpen,0)
	  bracketClose<<-c(bracketClose,0)
	  addop<-FALSE
	  element<-NULL
	  if (input$op_cdn!=" "){
	    if (input$op_cdn=="("){
	      if ((cdn_last[length(cdn_last)]!="operator")&(cdn_last[length(cdn_last)]!="start")){
	        showNotification("Incorrect start of bracket.",duration=2)
	      }else{
	        bracketOpen_cnt<<-bracketOpen_cnt+1
	        bracketOpen[length(bracketOpen)]<<-1  
	        addop<-TRUE
	        element<-"operator"
	      }
	    }else if (input$op_cdn==")"){
	      if (((cdn_last[length(cdn_last)]=="operator")&(cdn_last[length(cdn_last)]!="start"))|(bracketOpen_cnt<=bracketClose_cnt)){
	        showNotification("Incorrect end of bracket.",duration=2)
	      }else{
	        bracketClose_cnt<<-bracketClose_cnt+1
	        bracketClose[length(bracketClose)]<<-1  
	        addop<-TRUE
	        element<-"bracket_end"
	      }
	    }else if ((cdn_last[length(cdn_last)]=="operator")&(cdn_last[length(cdn_last)]!="start")){
	      
	      if ((input$op_cdn=="-")&(cdn_last_selection[length(cdn_last_selection)]=='(')){
	        addop<-TRUE
	        element<-'operator'
	      }else{
	        showNotification("Cannot add consecutive operators", duration=2)  
	      }
	    }else{
	      addop<-TRUE
	      element<-"operator"
	    }
	    if (addop){
	      cdn$val<-paste0(cdn$val,input$op_cdn)  
	      cdn_vis$val<-paste0(cdn_vis$val," ",input$op_cdn)
	      cdn_v$val<-paste0(cdn_v$val," ",input$op_cdn)  
	      cdn_last<<-c(cdn_last,element)
	      cdn_last_selection<<-c(cdn_last_selection,input$op_cdn)
	      last_cnt<-last_cnt+1  
	    }
	  }
	  
	  addval<-FALSE
	  if (!is.na(input$val_cdn)){
	    if (((cdn_last[length(cdn_last)]=="operator")&((input$op_cdn!="(")|(input$op_cdn!=")")))|(cdn_last[length(cdn_last)]=="start")){
  	    cdn$val<-paste0(cdn$val,input$val_cdn)  
  	    cdn_vis$val<-paste0(cdn_vis$val," ",input$val_cdn)
  	    cdn_v$val<-paste0(cdn_v$val," ",input$val_cdn)
  	    cdn_last<<-c(cdn_last,"value")
  	    last_cnt<-last_cnt+1
  	    addval<-TRUE
	    }else{
  	     showNotification("Incorrect use of numeral.",duration=2)
  	   }
	  }
	  cdn_vis$val<-trimws(cdn_vis$val)
	  cdn_v$val<-trimws(cdn_v$val)
	  cdn$val<-trimws(cdn$val)
	  updateSelectInput(session,"sig1_cdn",selected="No Selection")
	  updateSelectInput(session,"op_cdn",selected=" ")
	  updateNumericInput(session,"val_cdn",value=NA)
	  if (addsig | addop | addval){
	    cdn_hist<<-c(cdn_hist,cdn$val)
	    cdn_hist_vis<<-c(cdn_hist_vis,cdn_vis$val)
	    cdn_hist_v<<-c(cdn_hist_v,cdn_v$val)
	    cdn_last_cnt<<-c(cdn_last_cnt,last_cnt)  
	  }else{
	    bracketOpen<<-bracketOpen[-length(bracketOpen)]
	    bracketClose<<-bracketClose[-length(bracketClose)]
	  }
	}
	
	# Function captures 'Add' button click on boundary conditions tab and calls addCdn function.
  observeEvent(input$add_cdn,{
    addCdn()
  })
  
  
  # Function captures 'Rem' button click on boundary conditions tab and removes last added partial entries to logical condition (EBC)
  observeEvent(input$rem_cdn,{
    cdn_hist<<-cdn_hist[-length(cdn_hist)]
    cdn_hist_vis<<-cdn_hist_vis[-length(cdn_hist_vis)]
    cdn_hist_v<<-cdn_hist_v[-length(cdn_hist_v)]
    last_cnt<-cdn_last_cnt[length(cdn_last_cnt)]
    cdn_last<<-cdn_last[1:(length(cdn_last)-last_cnt)]
    cdn_last_cnt<<-cdn_last_cnt[-length(cdn_last_cnt)]
    bracketOpen_cnt<<-bracketOpen_cnt-bracketOpen[length(bracketOpen)]
    bracketClose_cnt<<-bracketClose_cnt-bracketClose[length(bracketClose)]
    
    if (length(cdn_hist)>0){
      cdn$val<-cdn_hist[length(cdn_hist)]
      cdn_vis$val<-cdn_hist_vis[length(cdn_hist_vis)]
      cdn_v$val<-cdn_hist_v[length(cdn_hist_v)]
      
    }else{
      cdn$val<-NULL
      cdn_vis$val<-NULL
      cdn_v$val<-NULL
      cdn_last<<-c("start")
      cdn_last_cnt<<-c()
      bracketClose_cnt<<-0
      bracketOpen_cnt<<-0
      bracketClose<<-c()
      bracketOpen<<-c()
    }
    
  })
  
  cdnAll<-list("No Selection"="No Selection")              # list to store created EBC in R Code form
  cdnAllVis<-list("No Selection"="No Selection")           # list to store created EBC with signal name in KKS form for displaying on screen 
  cdnAllV<-list("No Selection"="No Selection")             # list to store created EBC with signal name in vector form
  cdnDes<-list("No Selection"="No Selection")              # list to save description for created EBCs
  cdnSource<-c("NA","NA")
  cdnNum<-0                                                # counter to store number of created EBCs
  def_cdn_string_vis<-reactiveValues(val=NULL)             # reactive value to trigger display of created EBC on GUI
  def_cdn_string<-NULL
  cdnCnt<-reactiveValues(val=NULL)                         # reactive value to keep track of number of created EBCs  
  
  allLS<-list("No Selection"="No Selection",               # list to store created LogicalStrings 
              "LS1(Default)"=" ")
  lsStringAll<-list("No Selection"="No Selection")
  showCdn<-reactiveValues(val=NULL)                        # reactive value to trigger display of all created EBCs on screen
  
  #derived signal variables
  sigNum<-0                                                # counter to store number of derived signal
  sigCnt<-reactiveValues(val=NULL)                         # reactive value to store number of derived signal.
  sigAll<-list()                                           # list to store derived signal definition in R Code form
  sigAllVis<-list()                                        # list to store derived signal definition with signal name in KKS form for displaying on screen
  sigAllV<-list()                                          # list to store derived signal definition with signal name in vector form
  sigDes<-list()                                           # list to store derived signal description
  showSig<-reactiveValues(val=NULL)                        # reactive value to trigger display of derived signal definition on screen
  createdSig<-c()                                          # 
  opcdn<-c("<","<=",">","<=","==","!")                     # logial operators available
  
  
  ###########################
  # Function extractcdnNum returns the cdnNum last used for numbering EBCs.
  
  # Arguments: list of all EBCs with signal name in KKS form
  
  # Return value: numeric
  ###########################
  extractcdnNum<-function(cdnAllVis){
    l<-lapply(names(cdnAllVis),function(x){
      if (x!="No Selection"){
        st<-regexpr("EBC",x,fixed = TRUE)[[1]]
        return(trimws(substr(x,st+3,nchar(x))))
      }
    })
    l1<-lapply(delCdn,function(x){ # delCdn keeps track of deleted EBCs. 
      if (x!="No Selection"){
        st<-regexpr("EBC",x,fixed = TRUE)[[1]]
        return(trimws(substr(x,st+3,nchar(x))))
      }
    })
    ll<-c(unlist(l),unlist(l1))
    cdnNum<-ifelse(length(ll)>0,max(as.numeric(ll)),0)
    return(cdnNum)
  }
  
  
  # Function called on 'AddtoList' button click on Boundary Conditions tab. Adds the defined EBC or derived signal to its corresponding lists.
  observeEvent(input$end_cdn,{
    
    if (is.null(cdn$val)){
      showNotification("Please create condition first by clicking Add.",duration=2)
      return()
    }
    if(input$des_cdn==""){
      showNotification("Description Required.",duration=2)
      return()
    }
    if (input$der_sig=="No"){
      l<-lapply(opcdn,function(x,cdn)return(as.numeric(grepl(x,cdn,fixed=TRUE))),cdn_vis$val)
      if (sum(unlist(l))==0){
        showNotification("Condition must have a conditional operator.",duration=2)
        return()
      }
      if (bracketOpen_cnt!=bracketClose_cnt){
        showNotification("Brackets incorrect. Please check.",duration=2)
        return()
      }
      
      cdnNum<<-extractcdnNum(cdnAllVis)+1
      
      cdnCnt$val<-cdnNum
      # cdnAll[[input$des_cdn]]<<-cdn$val
      # cdnAllVis[[input$des_cdn]]<<-cdn_vis$val
      cdnAll[[paste0("EBC",cdnNum)]]<<-cdn$val
      cdnAllVis[[paste0("EBC",cdnNum)]]<<-cdn_vis$val
      cdnAllV[[paste0("EBC",cdnNum)]]<<-cdn_v$val
      cdnDes[[paste0("EBC",cdnNum)]]<<-paste0("# ",input$des_cdn)
      updateSelectInput(session,"cdns",choices=cdnAllVis,selected="No Selection")
      showCdn$val<-NULL
      
      if (is.null(def_cdn_string_vis$val)){
      # if (allLS[["LS1(Default)"]]=="( )"){
        def_cdn_string_vis$val<-paste0("EBC",cdnNum)
        def_cdn_string<<-paste0(cdn_v$val)
      }else{
        # def_cdn_string_vis<<-paste0(def_cdn_string_vis," & (",cdn_vis$val,")")
        # def_cdn_string_vis<<-paste0(def_cdn_string_vis," & (",paste0("C",cdnNum),")")
        def_cdn_string_vis$val<-paste0(def_cdn_string_vis$val," & ",paste0("EBC",cdnNum),"")
        def_cdn_string<<-paste0(def_cdn_string, " & ",cdn_v$val)
      }
      
      lsStringAll[["LS1(Default)"]]<<-def_cdn_string
      allLS[["LS1(Default)"]]<<-def_cdn_string_vis$val
      updateSelectInput(session,"LS",choices=allLS,selected=def_cdn_string_vis$val)  
      cdn_last<<-c("start")
      cdn_last_cnt<<-c()
      bracketClose_cnt<<-0
      bracketOpen_cnt<<-0
      bracketClose<<-c()
      bracketOpen<<-c()
    }else if (input$der_sig=="Yes"){
      if (input$der_sig_name==""){
        showNotification("Derived Signal Name required.",duration=2)
        return()
      }else if (input$der_sig_unit==""){
        showNotification("Derived Signal Unit required.",duration=2)
        return()
      }
      sigNum<<-sigNum+1
      sigCnt$val<-sigNum
      # cdnAll[[input$des_cdn]]<<-cdn$val
      # cdnAllVis[[input$des_cdn]]<<-cdn_vis$val
      sigName_underscore <- gsub(" ","_",input$der_sig_name)
      sigUnit_underscore<-gsub(" ","_",input$der_sig_unit)
      sigUnit_underscore<-gsub("/","_",sigUnit_underscore)

      # dersigName<-paste0("DS",sigNum,"_",sigName_underscore,"_",sigUnit_underscore)
      dersigName<-paste0(sigName_underscore,"_",sigUnit_underscore)
      if (length(which(grepl(dersigName,names(sigAllVis))))>0){
        showNotification("Signal with same name already present.",duration=2)
        return()
      }
      sigAll[[dersigName]]<<-cdn$val
      sigAllVis[[dersigName]]<<-cdn_vis$val
      sigAllV[[dersigName]]<<-cdn_v$val
      sigDes[[dersigName]]<<-input$des_cdn
      updateSelectInput(session,"sigs",choices=c("No Selection"="No Selection",sigAllVis),selected="No Selection")
      showSig$val<-NULL
      cdn_last<<-c("start")
      cdn_last_cnt<<-c()
      bracketOpen_cnt<<-0
      bracketClose_cnt<<-0
      bracketOpen<<-c()
      bracketClose<<-c()
      updateTextInput(session,"der_sig_name",value=NA)
      updateTextInput(session,"der_sig_unit",value=NA)
    }
    
    cdn_hist_v<<-NULL
    cdn_hist_vis<<-NULL
    cdn_hist<<-NULL
    cdn$val<-NULL
    cdn_vis$val<-NULL
    cdn_v$val<-NULL
    updateTextInput(session,"des_cdn",value=NA)
    
  })
  
  delCdnCnt<-reactiveValues(val=0)                         # reactive value used to keep track of number of deleted EBCs
  delCdn<-NULL                                             # list storing deleted EBCs
  
  # Function called on 'DelfromList' button click on Boundary Conditions tab for EBC.
  observeEvent(input$rem_end_cdn,{
    if (input$der_sig=="No"){
      if (input$cdns!="No Selection"){
        cn<-0
        for (i in 1:length(cdnAllVis)){
          if (input$cdns==cdnAllVis[[i]]){
            cn<-i
          }
        }
        cn<-names(which(cdnAllVis==input$cdns))
        delCdn<<-c(delCdn,cn)
        delCdnCnt$val<-delCdnCnt$val+1
        
        cdnAll[[cn]]<<-NULL
        cdnAllVis[[cn]]<<-NULL
        cdnAllV[[cn]]<<-NULL
        cdnDes[[cn]]<<-NULL
        
        cdnNum<<-cdnNum-1
        if (cdnNum!=0){
          cdnCnt$val<-cdnNum  
        }else{
          cdnCnt$val<-NULL
        }
        updateSelectInput(session,"cdns",choices=cdnAllVis,selected="No Selection")
        
        def_cdn_string<<-NULL
        def_cdn_string_vis$val<-NULL
        
        showCdn$val<-NULL
        
        if (length(cdnAllV)>1){
          for (i in 2:length(cdnAllVis)){
            if (is.null(def_cdn_string)){
              def_cdn_string<<-paste0(" ",cdnAllV[[i]])
              def_cdn_string_vis$val<-names(cdnAllVis)[i]
            }
            else{
              def_cdn_string<<-paste0(def_cdn_string," & ",cdnAllV[[i]])
              def_cdn_string_vis$val<-paste0(def_cdn_string_vis$val," & ",names(cdnAllVis)[i])
            }
          }
        }
        if (!is.null(def_cdn_string_vis$val)){
          allLS[["LS1(Default)"]]<<-def_cdn_string_vis$val  
        }else{
          allLS[["LS1(Default)"]]<<-"( )"
        }
        
      }else{
        showNotification("No Condition Selected.",duration=2)
      }
    }else if (input$der_sig=="Yes"){
      if (input$sigs!="No Selection"){
        cn<-names(which(sigAllVis==input$sigs))
        sigAll[[cn]]<<-NULL
        sigAllVis[[cn]]<<-NULL
        sigAllV[[cn]]<<-NULL
        sigDes[[cn]]<<-NULL
        
        sigNum<<-sigNum-1
        if (sigNum!=0){
          sigCnt$val<-sigNum  
        }else{
          sigCnt$val<-NULL
        }
        showSig$val<-NULL
        updateSelectInput(session,"sigs",choices=c("No Selection"="No Selection",sigAllVis),selected="No Selection")
        showNotification("Derived signal removed.",duration=2)
      }
    }
  })
  
  
  # Output variable displays partial EBC as it is being created by the user.
  output$cdn_bld<-renderText({
    req(cdn$val)
    req(cdn_vis$val)
    # cdn_vis$val
    # <font color=\"#FF0000\"><b>
    cdn_vis$val
    # paste0("<font color=\"#FF0000\"><b>", cdn_vis$val,"</b></font>")
  })
  # allCdns<-reactiveValues(val=NULL)
  
  #####################
  # Function cdnDisp is called to create a display string from all created EBCs
  
  # Arguments: NONE. Uses variables from parent scope eg. cdnAllvis
  
  # Return: NONE. Updates variable in variable scope. eg. showCdn$val
  ######################
  
  cdnDisp<-function(){
    allCdns<-"Conditions:-"
    allCdnsV<-"\n "
    if (!is.null(cdnCnt$val)){
      if (length(cdnAllV)>1){
        for (i in 2:length(cdnAllV)){
          # allCdns$val<-paste0(allCdns$val," \n ",names(cdnAllVis)[i]," : (",cdnAllVis[[i]],")   #",cdnDes[[(i-2)]])
          if(cdnAllVis[[i]]!=" "){
            allCdns<-paste0(allCdns," \n ",names(cdnAllVis)[i]," : (",cdnAllVis[[i]],")   ",cdnDes[[i]]) 
            allCdnsV<-paste0(allCdnsV," \n ",names(cdnAllV)[i]," : (",cdnAllV[[i]],")   ",cdnDes[[i]])
          }else{
            allCdnsV<-paste0(allCdnsV," \n ",names(cdnAllV)[i]," : ",cdnAllV[[i]])  
          }
        }
      }
      showCdn$val<- paste0(allCdns,allCdnsV)
      # showCdn$val<-length(cdnAllVis)
    }
  }
  
  # Output variable to show all EBCs created by the user on screen
  output$show_cdn<-renderText({
    req(input$cdns)
    if (is.null(showCdn$val)){
      cdnDisp()
    }
    showCdn$val
    
  })
  
  #derived signal
  {
    observeEvent(input$der_sig,{
      if (input$der_sig=="Yes"){
        updateSelectInput(session,"op_cdn",choices=list(" "= " ", "("= "(",")"= ")","+"="+","-"="-","*"="*","/"="/","^"="^"),
                          selected = " ")
        sigCnt$val<-sigNum
        showSig$val<-NULL
      }else if (input$der_sig=="No"){
        updateSelectInput(session,"op_cdn",choices=list(" "= " ", "("= "(",")"= ")","==","==", "<"="<","<="="<=",">"=">",">="=">="),
                                                        # "+"="+","-"="-","*"="*","/"="/","^"="^"),
                          selected = " ")
        cdnCnt$val<-cdnNum
        showCdn$val<-NULL
      }
    })
    
    #show derived signals
    output$show_sig<-renderText({
      req(input$sigs)
      if (is.null(showSig$val)){
        allSigs<-"Derived Signals:-"
        allSigsV<-"\n "
        if (!is.null(sigCnt$val)){
          if (length(sigAllV)>0){
            for (i in 1:length(sigAllV)){
              # allCdns$val<-paste0(allCdns$val," \n ",names(cdnAllVis)[i]," : (",cdnAllVis[[i]],")   #",cdnDes[[(i-2)]])
              if(sigAllVis[[i]]!=" "){
                allSigs<-paste0(allSigs," \n ",names(sigAllVis)[i]," : (",sigAllVis[[i]],")   #",sigDes[[i]]) 
                allSigsV<-paste0(allSigsV," \n ",names(sigAllV)[i]," : (",sigAllV[[i]],")   #",sigDes[[i]])
              }else{
                allSigsV<-paste0(allSigsV," \n ",names(sigAllV)[i]," : ",sigAllV[[i]])  
              }
            }
            showSig$val<- paste0(allSigs,allSigsV)  
          }
        }
      }
      showSig$val
    })
    
    #calculate derived signal
    observeEvent(input$sig_cal,{
      if (input$sigs!="No Selection"){
        cn<-names(which(sigAllVis==input$sigs))
        if (length(cn)>0){
          derived_sig_check<-which(INPUT_VECTOR_VXX_Title_Type_Text==cn)
          if(length(derived_sig_check)>0){
            showNotification('Derived signal exists.',duration=2)
            return()
          }
          id<<-showNotification("Calculating Derived Signal...",duration=NULL)
          expx<-parse(text=paste0(cn,"=",sigAll[[cn]]))[[1]]
          eval(expx)
          datetime<-PROJECT_DATA_VXX$MY_DATE
          PROJECT_DATA_VXX[,MY_DATE:=NULL]
          expx1<-parse(text=paste0("PROJECT_DATA_VXX<<-cbind(PROJECT_DATA_VXX,data.table(",cn,"))"))[[1]]
          eval(expx1)
          
          createdSig<<-c(createdSig,cn)
          cnName<-NULL
          for (i in 1:dim(PROJECT_DATA_VXX)[2]){
            if (colnames(PROJECT_DATA_VXX)[i]!="MY_DATE"){
              cnName<-c(cnName,paste0("V",i))  
            }
          }
          
          PROJECT_DATA_VXX<<-cbind(PROJECT_DATA_VXX,data.table(datetime))
          
          minds<-min(PROJECT_DATA_VXX[[cn]],na.rm = TRUE)
          if (minds<0){
            INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE <<- c(INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE,1.1*minds)  
          }else{
            INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE <<- c(INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE,0.9*minds)
          }
          
          maxds<-max(PROJECT_DATA_VXX[[cn]],na.rm = TRUE)
          if (maxds<0){
            INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE<<-c(INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE,0.9*maxds)  
          }else{
            INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE<<-c(INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE,1.1*maxds)
          }
          
          colnames(PROJECT_DATA_VXX)<<-c(cnName,"MY_DATE")
          INPUT_VECTOR_VXX<<-cnName
          # INPUT_VECTOR_VXX_Title_Type_Text<<-c(input$sigs,INPUT_VECTOR_VXX_Title_Type_Text)
          INPUT_VECTOR_VXX_Title_Type_Text<<-c(INPUT_VECTOR_VXX_Title_Type_Text,cn)
          INPUT_VECTOR_VXX_Title_Type_Underscored_TEXT <<- c(INPUT_VECTOR_VXX_Title_Type_Underscored_TEXT,cn)
          
          INPUT_VECTOR_breaks_01 <<- c(INPUT_VECTOR_breaks_01,20)
          INPUT_VECTOR_breaks_02 <<- c(INPUT_VECTOR_breaks_02,40)
          INPUT_VECTOR_breaks_03 <<- c(INPUT_VECTOR_breaks_03,200)
          updateSelectInput(session,"sig1",choices = c("No Selection"="No Selection",INPUT_VECTOR_VXX_Title_Type_Text),selected = "No Selection")
          updateSelectInput(session,"sig2",choices = c("No Selection"="No Selection",INPUT_VECTOR_VXX_Title_Type_Text),selected = "No Selection")
          updateSelectInput(session,"sig3",choices = c("No Selection"="No Selection",INPUT_VECTOR_VXX_Title_Type_Text),selected = "No Selection")
          updateSelectInput(session,"sig1_cdn",choices = c("No Selection"="No Selection",INPUT_VECTOR_VXX_Title_Type_Text),selected = "No Selection")
          updateSelectInput(session,"time_series_sig",choices = c("No Selection"="No Selection",INPUT_VECTOR_VXX_Title_Type_Text),selected = "No Selection")
          updateSelectInput(session,"freq_sens",choices = c("No Selection"="No Selection",INPUT_VECTOR_VXX_Title_Type_Text),selected = "No Selection")
          updateSelectInput(session,"prob_sens",choices = c("No Selection"="No Selection",INPUT_VECTOR_VXX_Title_Type_Text),selected = "No Selection")
          updateSelectInput(session,"ecdf_sens",choices = c("No Selection"="No Selection",INPUT_VECTOR_VXX_Title_Type_Text),selected = "No Selection")
          updateSelectInput(session,"xy_sens",choices = c("No Selection"="No Selection",INPUT_VECTOR_VXX_Title_Type_Text),selected = "No Selection")
          source(paste0(scripts_path,"/General_Settings.R"))
          
          PROJECT_DATA_VXX_FILTERED_1D<<-NULL
          PROJECT_DATA_VXX_FILTERED_2D<<-NULL
          PROJECT_DATA_VXX_FILTERED_nD<<-NULL
          rm(datetime)
          gc()
          if(!is.null(id)){
            removeNotification(id)
            showNotification("Derived signal added to signals.",duration=2)
          }
          
        }  
      }else{
        showNotification("Derived Signal Definition not selected",duration=2)
      }
      
    })
    
    #save derived signals to R Code file
    observeEvent(input$sig_save,{
      # if ((length(sigAllVis)==0)&(!sigFound)){
      #   showNotification("Derived Signals not defined.",duration=2)
      #   return()
      # }else 
      if (length(sigAllVis)==0){
        sig_vector<-NULL
      }else if (length(sigAllVis)>0){
        sig_vector<-c("# Shiny Derived Signals Start","# DS Visualization")
        for (i in 1:length(sigAllVis)){
          sig_vector<-c(sig_vector,paste0("# ",names(sigAllVis)[i]," = ",sigAllVis[[i]],"#",sigDes[[i]]))
        }
        sig_vector<-c(sig_vector,"# DS Vector form")
        for (i in 1:length(sigAllV)){
          sig_vector<-c(sig_vector,paste0("# ",names(sigAllV)[i]," = ",sigAllV[[i]],"#",sigDes[[i]]))
        }
        sig_vector<-c(sig_vector,"# DS RCode form")
        for (i in 1:length(sigAllV)){
          sig_vector<-c(sig_vector,paste0("# ",names(sigAll)[i]," = ",sigAll[[i]],"#",sigDes[[i]]))
        }
        sig_vector<-c(sig_vector,"# Shiny Derived Signals End")  
      }
      
      
      filename<-paste0(in_path,"/",input$sel1,"/",input$sel2,"/",input$sel2,".R")
      rl<-readLines(con=filename)
      i<-which(rl=="# Shiny Derived Signals Start")
      j<-which(rl=="# Shiny Derived Signals End")
      if ((length(i)!=0)&(length(j)!=0)){
        if (j==length(rl)){
          new_rl<-c(rl[1:(i-1)],sig_vector)  
        }else if (j<length(rl)){
          new_rl<-c(rl[1:(i-1)],sig_vector,rl[(j+1):length(rl)])
        }
        
      }else{
        new_rl<-c(rl,sig_vector)
      }
      # cat("#testing \n #testing again",file=filename,append=TRUE,sep="\n")
      
      writeLines(new_rl,con=filename)
      showNotification("Derived Signals Saved.",duration=2)
    })
    
    
  }
  
  #create Logical Strings
  {
  
  cdn_string_hist_vis<-c()                                  # list to save partial logical string with EBC in detailed form
  cdn_string_hist<-c()                                      # list to save partial logical string with EBCS written as EBC1, EBC2..
  cdn_string_vis<-reactiveValues(val=NULL)                  # reactive value to trigger display of partial logical string as created by user
  cdn_string<-reactiveValues(val=NULL)                      # reactive value to trigger update of list saving partial logical string
  lsCnt<-reactiveValues(val=NULL)                           # reactive value to keep track of count of logical string
  
  cdn_string_last<-c("start")                               # character vector to store elements of logical string in sequence as added by user.
  cdn_string_last_cnt<-c()                                  # character vector keeping track of number of elements added to logical string
  LS_bracketOpen_cnt<-0                                     # counter to keep track of open brackets
  LS_bracketClose_cnt<-0                                    # counter to keep track of closed bracktets
  LS_bracketOpen<-c()                                       # character vector to keep track of whether open bracket is added by user (0 - not added, 1 - added)
  LS_bracketClose<-c()                                      # character vector to keep track of whether close bracket is added by user (0 - not added, 1 - added)
  isLogicalStringInvalidate<-FALSE                          # boolean to represent if any logical string has been invalidate due to deletion of any of its EBCs
  showLs<-reactiveValues(val=NULL)                          # reactive value to trigger display of all logical strings added by user on screen
  
  
  # Function is called on click of 'Add' button on Boundary Conditions tab for logical strings. Performs checks on selection made by user and
  # adds to list if found correct.
  observeEvent(input$add_cdns,{
    req(input$cdns)
    req(input$op_cdns)
    last_cnt<-0
    if (input$LS!="No Selection"){
      updateSelectInput(session,"LS",selected="No Selection")
    }
    # if (!is.null(def_cdn_string)){
    #   lsCnt$val<-1
    #   def_cdn_string<<-NULL
    #   def_cdn_string_vis<<-NULL
    # }
    addebc<-FALSE
    if (input$cdns!="No Selection"){
      
      if ((cdn_string_last[length(cdn_string_last)]=="ebc")&(cdn_string_last[length(cdn_string_last)]!="start")){
        showNotification("Operator required.",duration=2)
      }else{
        cn<-0
        for (i in 1:length(cdnAllVis)){
          if (cdnAllVis[[i]]==input$cdns){
            cn<-i
          }
        }
        cdn_string$val<-paste0(cdn_string$val," ",cdnAllV[[cn]])
        # cdn_string_vis$val<-paste0(cdn_string_vis$val," (",input$cdns,") ")
        cdn_string_vis$val<-paste0(cdn_string_vis$val," ",names(cdnAllV)[cn])
        cdn_string_last<<-c(cdn_string_last,"ebc")
        addebc<-TRUE
        last_cnt<-last_cnt+1
      }
    }
    
    LS_bracketClose<<-c(LS_bracketClose,0)
    LS_bracketOpen<<-c(LS_bracketOpen,0)
    addop<-FALSE
    if (input$op_cdns!=" "){
      
      if (input$op_cdns=="("){
        if (cdn_string_last[length(cdn_string_last)]=="ebc"){
          showNotification("Incorrect selection of open bracket.",duration=2)
        }else{
          LS_bracketOpen_cnt<<-LS_bracketOpen_cnt+1
          LS_bracketOpen[length(LS_bracketOpen)]<<-1  
          addop<-TRUE
        }
      }else if(input$op_cdns==")"){
        if ((cdn_string_last[length(cdn_string_last)]=="operator")|(LS_bracketOpen_cnt<=LS_bracketOpen_cnt)){
          showNotification("Incorrect selection of close bracket.",duration=2)
        }else{
          LS_bracketClose_cnt<<-LS_bracketClose_cnt+1
          LS_bracketClose[length(LS_bracketClose)]<<-1  
          addop<-TRUE
        }
      }else if (input$op_cdns=="!"){
        if (cdn_string_last[length(cdn_string_last)]=="ebc"){
          showNotification("Incorrect selection of NOT operator.",duration=2)
        }else{
          addop<-TRUE
        }
      }
      else if (cdn_string_last[length(cdn_string_last)]!="operator"){
        addop<-TRUE
      }else if (cdn_string_last[length(cdn_string_last)]=="operator"){
        showNotification("Incorrect selection of consecutive operators.",duration=2)
      }
      if (addop){
        cdn_string$val<-paste0(cdn_string$val," ",input$op_cdns)
        cdn_string_vis$val<-paste0(cdn_string_vis$val," ",input$op_cdns)
        cdn_string_last<<-c(cdn_string_last,"operator")  
        last_cnt<-last_cnt+1
      }
    }
    
    updateSelectInput(session,"cdns",selected="No Selection")
    updateSelectInput(session,"op_cdns",selected=" ")
    updateNumericInput(session,"val_cdns",value=NA)
    if (addebc | addop){
      cdn_string_hist<<-c(cdn_string_hist,cdn_string$val)
      cdn_string_hist_vis<<-c(cdn_string_hist_vis,cdn_string_vis$val)  
      cdn_string_last_cnt<<-c(cdn_string_last_cnt,last_cnt)
    }else{
      LS_bracketClose<<-LS_bracketClose[-length(LS_bracketClose)]
      LS_bracketOpen<<-LS_bracketOpen[-length(LS_bracketOpen)]
    }
    
    
  })
  
  # Function is called on click of 'Rem' button on Boundary COnditions tab for logical strings. Deletes last addition made by user.
  observeEvent(input$rem_cdns,{
    if (!is.null(cdn_string$val)){
      cdn_string_hist<<-cdn_string_hist[-length(cdn_string_hist)]
      cdn_string_hist_vis<<-cdn_string_hist_vis[-length(cdn_string_hist_vis)]
      cdn_string_last<<-cdn_string_last[-length(cdn_string_last)]
      last_cnt<-cdn_string_last_cnt[length(cdn_string_last_cnt)]
      cdn_string_last<<-cdn_string_last[1:(length(cdn_string_last)-last_cnt)]
      cdn_string_last_cnt<<-cdn_string_last_cnt[-length(cdn_string_last_cnt)]
      LS_bracketClose_cnt<<-LS_bracketClose_cnt-LS_bracketClose[length(LS_bracketClose)]
      LS_bracketOpen_cnt<<-LS_bracketOpen_cnt-LS_bracketOpen[length(LS_bracketOpen)]
      LS_bracketClose<<-LS_bracketClose[-length(LS_bracketClose)]
      LS_bracketOpen<<-LS_bracketOpen[-length(LS_bracketOpen)]
      
      
      if (length(cdn_string_hist)>0){
        cdn_string$val<-cdn_string_hist[length(cdn_string_hist)]
        cdn_string_vis$val<-cdn_string_hist_vis[length(cdn_string_hist_vis)]  
      }else{
        cdn_string$val<-NULL
        cdn_string_vis$val<-NULL
        cdn_string_last<<-c("start")
        cdn_string_last_cnt<<-c()
        LS_bracketClose_cnt<<-0
        LS_bracketOpen_cnt<<-0
        LS_bracketClose<<-c()
        LS_bracketOpen<<-c()
      }
    }
    
  })
  
  # Output variable displays all logical strings on screen
  output$logical_string<-renderText({
   
    if(!is.null(cdn_string$val)){
      cdn_string_vis$val
    }
    else if(!is.null(def_cdn_string_vis$val)){
      def_cdn_string_vis$val
    }
    
  })
  
  # Function is called on click of 'AddtoList' button on Boundary Conditions tab for logical strings. Adds created logical string to list.
  observeEvent(input$add_LS,{
    if (!is.null(cdn_string_vis$val)){
      lsCnt$val<-NULL
      lsCnt$val<-length(allLS)
      
      allLS[[paste0("LS",lsCnt$val)]]<<-trimws(cdn_string_vis$val)
      lsStringAll[[paste0("LS",lsCnt$val)]]<<-trimws(cdn_string$val)
      updateSelectInput(session,"LS",choices=allLS,selected="No Selection")
      # cdn_string_vis$val<-NULL
      cdn_string_vis$val<-NULL
      
      cdn_string_hist<<-NULL
      cdn_string_hist_vis<<-NULL
      cdn_string_last<<-c("start")
      cdn_string_last_cnt<<-c()
      LS_bracketClose_cnt<<-0
      LS_bracketOpen_cnt<<-0
      LS_bracketClose<<-c()
      LS_bracketOpen<<-c()
      showCdn$val<-NULL
    }
    
  })
  
  observeEvent(input$rem_LS,{
    req(input$LS)
    if (input$LS == "No Selection"){
      showNotification("Please select Logical String.",duration =3)
      return()
    }
    ls_index<-which(allLS==input$LS)
    if (length(ls_index)>0){
      if (names(allLS)[ls_index]=='LS1(Default)'){
        showNotification("Default Logical String cannot be deleted.",duration=3)
        return()
      }
      allLS[[ls_index]]<<-NULL
      lsStringAll[[ls_index]]<<-NULL
      lsCnt$val<-NULL
      lsCnt$val<-length(allLS)-1
      updateSelectInput(session,"LS",choices=allLS,selected="No Selection")
    }
  })
  
  output$LS_list_delCdn<-renderText({
    req(input$LS)
    # lsCnt$val
    # if (!is.null(lsCnt$val)){
      a2<-lsCnt$val
      a<-paste0("Logical Strings:-")
      if (length(allLS)>1){
        for (i in 2:length(allLS)){
          a1<-paste0(names(allLS)[i]," : (",allLS[[i]],")")
          if (delCdnCnt$val>0){
            # a<-delCdn$val
            for (j in 1:delCdnCnt$val){
              if(grepl(delCdn[j],allLS[[i]],fixed=TRUE)){
                a1<-paste0("<font color=\"#FF0000\"><b>",names(allLS)[i]," : (",allLS[[i]],")","</b></font>")
                isLogicalStringInvalidate<<-TRUE
              }
            }
          }else{
            isLogicalStringInvalidate<<-FALSE
          }
          # a<-paste0(a," \n ",names(itemLS)[i]," : (",itemLS[[i]],")")
          a<-paste0(a," \n",a1)
        }
      }
    #   # showLs$val<-a
    # }
    a
  })
  }
  
  observe({
    if ((input$op_cdns=="EDIT")&(input$cdns=="No Selection")){
      showNotification("Please select condition to edit.",duration=2)
    }
    # if((input$LS!="No Selection")&(isLogicalStringInvalidate)){
    #   color<-"red"
    #   runjs(paste0("document.getElementById('LS').style.borderColor ='", color ,"'"))
    #   # showNotification("Invalid Logical String Exists.",duration=2)
    # }
  })
  
  # Edit Conditions (EBC)
  {
  cdnEditItem<-reactiveValues(val=NULL)                    # reactive value to trigger display EBCs parts on screen while editing
  op_match<-c()                                            # character vector to save operators found in EBC
  itemSel<-NULL
  itemCdn<-c()                                             # character vector to store elements of a logical condition (EBC) in vector form eg. signal, operator, numeral
  itemCdnCode<-c()
  itemCdnV<-c()
  cdnSelNum<-NULL                                          # temporary variable to store EBC number eg. EBC1 or EBC2
  cdnEdit<-NULL                                            # temporary variable to store EBC to be edited
  cdnEditCode<-NULL                                        # temporary variable to store EBC to be edited in RCode form
  cdnEditV<-NULL                                           # temporary variable to store EBC to be edited in vector form
  # op1<-c("<=","==",">=")
  # op2<-c("+","-","/","^","*","(",")","<",">")
  
  
  #################################################
  # Function editCdn breaks the logical condition (EBC) into components (signal, operator, numeral) and lets user edit any of these independently
  
  # Arguments: NONE. Uses variables defined in parent scope (eg. cdnAllvis)
  
  # Return: NONE. Updates variables defined in parent scope using '<<-' operator. 
  #################################################
  
  editCdn<-function(){
    cdnEditItem$val<-NULL
    itemCdn<<-c()
    if ((input$cdns!="No Selection")&(input$op_cdns=="EDIT")){
      cdnEdit<<-isolate(input$cdns) # copy of logical condition (EBC) being edited
      for (i in 1:length(cdnAllVis)){
        if (cdnAllVis[[i]]==isolate(input$cdns)){
          cdnSelNum<<-names(cdnAllVis)[i]
        }
      }
      op1<-c("<=","==",">=") # search for these operators in EBC
      op_match1<-c()
      for (i in 1:length(op1)){
        j<-gregexpr(op1[i],cdnEdit,fixed=TRUE)
        if (j[[1]][1]!=-1){
          op_match1<-c(op_match1,j[[1]])
          cdnEdit<<-gsub(op1[i],"  ",cdnEdit,fixed=TRUE)
        }
      }
      op2<-c("+","-","/","^","*","(",")","<",">")
      op_match2<-c()
      for (i in 1:length(op2)){
        j<-gregexpr(op2[i],cdnEdit,fixed=TRUE)
        if (j[[1]][1]!=-1){
          op_match2<-c(op_match2,j[[1]])
          cdnEdit<<-gsub(op2[i]," ",cdnEdit,fixed=TRUE)
        }
      }
      op_match<-sort(c(op_match1,op_match2))
      cdnEdit<<-isolate(input$cdns)
      cdnIter<-1                        # iterate through all found operators in EBC to collect signal name or numeric value between two operators
      if (length(op_match)>0){
        for (i in 1:(length(op_match))){ #extracts signal name before the operator in EBC
          if (op_match[i]>cdnIter){
            st<-cdnIter
            en<-op_match[i]-1
            sig<-substr(cdnEdit,st,en)
            itemCdn<<-c(itemCdn,sig)
          }
          cdnIter<-op_match[length(op_match)]+1
          pos1<-which(op_match1==op_match[i])
          pos2<-which(op_match2==op_match[i])
          if (length(pos1)>0){
            itemCdn<<-c(itemCdn,substr(cdnEdit,op_match[i],(op_match[i]+1)))
            st<-op_match[i]+2
          }else if (length(pos2)>0) {
            itemCdn<<-c(itemCdn,substr(cdnEdit,op_match[i],(op_match[i])))
            st<-op_match[i]+1
          }
          if (i<length(op_match)){
            en<-(op_match[(i+1)]-1)
          }else{
            en<-nchar(cdnEdit)
          }
          # if ((st<=op_match[length(op_match)])&(en<=op_match[length(op_match)])){
          if ((st<=nchar(cdnEdit))&(en<=nchar(cdnEdit))){
            sig<-substr(cdnEdit,st,en)
            itemCdn<<-c(itemCdn,sig)  
          }
          
        }
      }
    }
  }
  
  
  #################################################
  # Function editCdnCode breaks the R Code form of logical condition (EBC) into components (signal, operator, numeral) and 
  # updates it as user edits EBC on screen (through editCdn function)
  
  # Arguments: NONE. Uses variables defined in parent scope (eg. cdnAll)
  
  # Return: NONE. Updates variables defined in parent scope using '<<-' operator. 
  #################################################
  
  editCdnCode<-function(){
    cdnEditItem$val<-NULL
    itemCdnCode<<-c()
    if ((input$cdns!="No Selection")&(input$op_cdns=="EDIT")){
      
      # for (i in 1:length(cdnAllVis)){
      #   if (cdnAllVis[[i]]==isolate(input$cdns)){
      #     cdnSelNum<<-(i-2)
      #   }
      # }
      # cdnEditCode<<-cdnAll[[paste0("C",cdnSelNum)]]
      cdnEditCode<<-cdnAll[[cdnSelNum]]
      op1<-c("<=","==",">=")
      
      op_match1_code<-c()
      for (i in 1:length(op1)){
        j1<-gregexpr(op1[i],cdnEditCode,fixed=TRUE)
        if (j1[[1]][1]!=-1){
          op_match1_code<-c(op_match1_code,j1[[1]])
          cdnEditCode<<-gsub(op1[i],"  ",cdnEditCode,fixed=TRUE)
        }
        
      }
      op2<-c("+","-","/","^","*","(",")","<",">")
      op_match2_code<-c()
      for (i in 1:length(op2)){
        j1<-gregexpr(op2[i],cdnEditCode,fixed=TRUE)
        if (j1[[1]][1]!=-1){
          op_match2_code<-c(op_match2_code,j1[[1]])
          cdnEditCode<<-gsub(op2[i]," ",cdnEditCode,fixed=TRUE)
        }
      }
      op_match<-sort(c(op_match1_code,op_match2_code))
      
      # cdnEditCode<<-cdnAll[[paste0("C",cdnSelNum)]]
      cdnEditCode<<-cdnAll[[cdnSelNum]]
      
      cdnIter<-1
      if (length(op_match)>0){
        for (i in 1:(length(op_match))){
          if (op_match[i]>cdnIter){
            st<-cdnIter
            en<-op_match[i]-1
            sig<-substr(cdnEditCode,st,en)
            itemCdnCode<<-c(itemCdnCode,sig)
          }
          cdnIter<-op_match[length(op_match)]+1
          pos1<-which(op_match1_code==op_match[i])
          pos2<-which(op_match2_code==op_match[i])
          if (length(pos1)>0){
            itemCdnCode<<-c(itemCdnCode,substr(cdnEditCode,op_match[i],(op_match[i]+1)))
            st<-op_match[i]+2
          }else if (length(pos2)>0) {
            itemCdnCode<<-c(itemCdnCode,substr(cdnEditCode,op_match[i],(op_match[i])))
            st<-op_match[i]+1
          }
          if (i<length(op_match)){
            en<-(op_match[(i+1)]-1)
          }else{
            en<-nchar(cdnEditCode)
          }
          # if ((st<=op_match[length(op_match)])&(en<=op_match[length(op_match)])){
          if ((st<=nchar(cdnEditCode))&(en<=nchar(cdnEditCode))){
            sig<-substr(cdnEditCode,st,en)
            itemCdnCode<<-c(itemCdnCode,sig)  
          }
          
        }
      }
    }
  }
  
  #################################################
  # Function editCdnV breaks the vector form of logical condition (EBC) into components (signal, operator, numeral) and 
  # updates it as user edits EBC on screen (through editCdn function)
  
  # Arguments: NONE. Uses variables defined in parent scope (eg. cdnAllV)
  
  # Return: NONE. Updates variables defined in parent scope using '<<-' operator. 
  #################################################
  
  editCdnV<-function(){
    cdnEditItem$val<-NULL
    itemCdnV<<-c()
    if ((input$cdns!="No Selection")&(input$op_cdns=="EDIT")){
      
      # for (i in 1:length(cdnAllVis)){
      #   if (cdnAllVis[[i]]==isolate(input$cdns)){
      #     cdnSelNum<<-(i-2)
      #   }
      # }
      # cdnEditV<<-cdnAllV[[paste0("C",cdnSelNum)]]
      cdnEditV<<-cdnAllV[[cdnSelNum]]
      op1<-c("<=","==",">=")
      
      op_match1_code<-c()
      for (i in 1:length(op1)){
        j1<-gregexpr(op1[i],cdnEditV,fixed=TRUE)
        if (j1[[1]][1]!=-1){
          op_match1_code<-c(op_match1_code,j1[[1]])
          cdnEditV<<-gsub(op1[i],"  ",cdnEditV,fixed=TRUE)
        }
        
      }
      op2<-c("+","-","/","^","*","(",")","<",">")
      op_match2_code<-c()
      for (i in 1:length(op2)){
        j1<-gregexpr(op2[i],cdnEditV,fixed=TRUE)
        if (j1[[1]][1]!=-1){
          op_match2_code<-c(op_match2_code,j1[[1]])
          cdnEditV<<-gsub(op2[i]," ",cdnEditV,fixed=TRUE)
        }
      }
      op_match<-sort(c(op_match1_code,op_match2_code))
      # cdnEditV<<-cdnAllV[[paste0("C",cdnSelNum)]]
      cdnEditV<<-cdnAllV[[cdnSelNum]]
      cdnIter<-1
      if (length(op_match)>0){
        for (i in 1:(length(op_match))){
          if (op_match[i]>cdnIter){
            st<-cdnIter
            en<-op_match[i]-1
            sig<-substr(cdnEditV,st,en)
            itemCdnV<<-c(itemCdnV,sig)
          }
          cdnIter<-op_match[length(op_match)]+1
          pos1<-which(op_match1_code==op_match[i])
          pos2<-which(op_match2_code==op_match[i])
          if (length(pos1)>0){
            itemCdnV<<-c(itemCdnV,substr(cdnEditV,op_match[i],(op_match[i]+1)))
            st<-op_match[i]+2
          }else if (length(pos2)>0) {
            itemCdnV<<-c(itemCdnV,substr(cdnEditV,op_match[i],(op_match[i])))
            st<-op_match[i]+1
          }
          if (i<length(op_match)){
            en<-(op_match[(i+1)]-1)
          }else{
            en<-nchar(cdnEditV)
          }
          # if ((st<=op_match[length(op_match)])&(en<=op_match[length(op_match)])){
          if ((st<=nchar(cdnEditV))&(en<=nchar(cdnEditV))){
            sig<-substr(cdnEditV,st,en)
            itemCdnV<<-c(itemCdnV,sig)  
          }
          
        }
      }
    }
  }
  
  
  #Function call edit condition functions when edit option selected
  observeEvent(input$cdns,{
    editCdn()
    # editCdnCode()
    editCdnV()
  })
  
  #Function call edit condition functions when edit option selected
  observeEvent(input$op_cdns,{
    editCdn()
    # editCdnCode()
    editCdnV()
  })
  
  # Output variable to display EBC being edited on screen with individual components
  output$edit_cdn<-renderText({
      if ((is.null(cdnEditItem$val))&(input$cdns!="No Selection")){
        itemSel<<-1
        # cn<-NULL
        # for (i in 1:length(cdnAllVis)){
        #   if (cdnAllVis[[i]]==input$cdns){
        #     cn<-(i-2)
        #   }
        # }
        cdnEditItem$val<-paste0("Condition to Edit: \n", cdnSelNum," : ",input$cdns, "\n","Selected Item: \n", itemCdn[itemSel],"\n",cdnAllV[[cdnSelNum]], "\n","Selected Item: \n", itemCdnV[itemSel])
        # cdnEditItem$val<-paste0("Condition to Edit: \n", "C",cdnSelNum," : ",input$cdns, "\n","Selected Item: \n", itemCdn[itemSel],"\n",cdnAll[[paste0("C",cdnSelNum)]], "\n","Selected Item: \n", itemCdnCode[itemSel])
        # cdnEditItem$val<-paste0("Condition to Edit: \n", "C",cdnSelNum," : ",cdnAll[[paste0("C",cdnSelNum)]], "\n","Selected Item: \n", itemCdnCode[itemSel])
        # cdnEditItem$val<-itemCdnV

      }
      cdnEditItem$val
    # }
    
  })
  
  # Function called when 'NXT' button clicked while editing EBC. Moves to next EBC part.
  observeEvent(input$nxt_cdn,{
    if (input$cdns=="No Selection"){
      showNotification("Please select condition to edit.",duration=2)
      return()
    }
    if (itemSel<length(itemCdn)){
      itemSel<<-itemSel+1
      
      # for (i in 1:length(cdnAllVis)){
      #   if (cdnAllVis[[i]]==input$cdns){
      #     cdnSelNum<<-(i-2)
      #   }
      # }
      # cdnEditItem$val<-paste0("Condition to Edit: \n", "C",cdnSelNum," : ",input$cdns, "\n","Selected Item: \n", itemCdn[itemSel])
      # cdnEditItem$val<-paste0("Condition to Edit: \n", "C",cdnSelNum," : ",cdnAll[[paste0("C",cdnSelNum)]], "\n","Selected Item: \n", itemCdnCode[itemSel])
      # cdnEditItem$val<-paste0("Condition to Edit: \n", "C",cdnSelNum," : ",cdnEdit, "\n","Selected Item: \n", itemCdn[itemSel],"\n",cdnAll[[paste0("C",cdnSelNum)]], "\n","Selected Item: \n", itemCdnCode[itemSel])
      # cdnEditItem$val<-paste0("Condition to Edit: \n", "C",cdnSelNum," : ",cdnEdit, "\n","Selected Item: \n", itemCdn[itemSel],"\n",cdnAllV[[paste0("C",cdnSelNum)]], "\n","Selected Item: \n", itemCdnV[itemSel])
      cdnEditItem$val<-paste0("Condition to Edit: \n", cdnSelNum," : ",cdnEdit, "\n","Selected Item: \n", itemCdn[itemSel],"\n",cdnAllV[[cdnSelNum]], "\n","Selected Item: \n", itemCdnV[itemSel])
      # cdnEditItem$val<-itemCdn[itemSel]
    }else{
      showNotification("Reached end of condition string.",duration=2)
    }
    
  })
  
  # Function called when 'PRV' button clicked while editing EBC. Moves to previous EBC part.
  observeEvent(input$prev_cdn,{
    if (input$cdns=="No Selection"){
      showNotification("Please select condition to edit.",duration=2)
      return()
    }
    if (itemSel>1){
      itemSel<<-itemSel-1
      # cn<-NULL
      # for (i in 1:length(cdnAllVis)){
      #   if (cdnAllVis[[i]]==input$cdns){
      #     cdnSelNum<<-(i-2)
      #   }
      # }
      # cdnEditItem$val<-paste0("Condition to Edit: \n", "C",cdnSelNum," : ",input$cdns, "\n","Selected Item: \n", itemCdn[itemSel])
      # cdnEditItem$val<-paste0("Condition to Edit: \n", "C",cdnSelNum," : ",cdnAll[[paste0("C",cdnSelNum)]], "\n","Selected Item: \n", itemCdnCode[itemSel])
      # cdnEditItem$val<-paste0("Condition to Edit: \n", "C",cdnSelNum," : ",cdnEdit, "\n","Selected Item: \n", itemCdn[itemSel],"\n",cdnAll[[paste0("C",cdnSelNum)]], "\n","Selected Item: \n", itemCdnCode[itemSel])
      cdnEditItem$val<-paste0("Condition to Edit: \n", cdnSelNum," : ",cdnEdit, "\n","Selected Item: \n", itemCdn[itemSel],"\n",cdnAllV[[cdnSelNum]], "\n","Selected Item: \n", itemCdnV[itemSel])
      # cdnEditItem$val<-itemCdn[itemSel]
    }else{
      showNotification("Reached end of condition string.",duration=2)
    }
    
  })
  
  # Function called when 'UPD' button clicked while editing EBC. Changes selected EBC part.
  observeEvent(input$upd_cdn,{
    if (input$cdns=="No Selection"){
      showNotification("Please select condition to edit.",duration=2)
      return()
    }
    j<-which(INPUT_VECTOR_VXX_Title_Type_Text==trimws(itemCdn[itemSel]))
    op1<-c("<=","==",">=")
    op2<-c("+","-","/","^","*","(",")","<",">")
    j1<-which(op1==itemCdn[itemSel])
    j2<-which(op2==itemCdn[itemSel])
    if (length(j)>0){
      if (input$sig1_cdn=="No Selection"){
        showNotification("Please select signal to update.",duration=2)
      }else{
        itemCdn[itemSel]<<-input$sig1_cdn
        jj<-which(INPUT_VECTOR_VXX_Title_Type_Text==input$sig1_cdn)
        itemCdnCode[itemSel]<<-paste0("PROJECT_DATA_VXX[[",jj,"]]")
        itemCdnV[itemSel]<<-paste0("V",jj)
        cdnNew<-NULL
        for (i in 1:length(itemCdn)){
          cdnNew<-paste0(cdnNew,itemCdn[i])
        }
        
        cdnNewCode<-NULL
        for (i in 1:length(itemCdnCode)){
          cdnNewCode<-paste0(cdnNewCode,itemCdnCode[i])
        }
        cdnNewV<-NULL
        for (i in 1:length(itemCdnV)){
          cdnNewV<-paste0(cdnNewV,itemCdnV[i])
        }
        # cn<-NULL
        # for (i in 1:length(cdnAllVis)){
        #   if (cdnAllVis[[i]]==cdnEdit){
        #     cn<-(i-2)
        #   }
        # }
        # cdnEditItem$val<-paste0("Condition to Edit: \n", "C",cdnSelNum," : ",cdnNew, "\n","Selected Item: \n", itemCdn[itemSel])
        # cdnEditItem$val<-paste0("Condition to Edit: \n", "C",cdnSelNum," : ",cdnNewCode, "\n","Selected Item: \n", itemCdnCode[itemSel])
        # cdnEditItem$val<-paste0("Condition to Edit: \n", "C",cdnSelNum," : ",cdnNew, "\n","Selected Item: \n", itemCdn[itemSel],"\n",cdnNewCode, "\n","Selected Item: \n", itemCdnCode[itemSel])
        cdnEditItem$val<-paste0("Condition to Edit: \n", cdnSelNum," : ",cdnNew, "\n","Selected Item: \n", itemCdn[itemSel],"\n",cdnNewV, "\n","Selected Item: \n", itemCdnV[itemSel])
        cdnAll[[cdnSelNum]]<<-cdnNewCode
        cdnAllVis[[cdnSelNum]]<<-cdnNew
        cdnAllV[[cdnSelNum]]<<-cdnNewV
        
        updateSelectInput(session,"sig1_cdn",selected="No Selection")
        updateSelectInput(session,"cdns",choices=cdnAllVis,selected=cdnNew)
        showCdn$val<-NULL
        cdnEdit<<-cdnNew
        cdnEditCode<<-cdnNewCode
        cdnEditV<<-cdnNewV
        # allCdns$val<-NULL
      }
    }else if ((length(j1)>0)|(length(j2)>0)){
      if (input$op_cdn==" "){
        showNotification("Please select operator.")
      }else{
        itemCdn[itemSel]<<-input$op_cdn
        itemCdnCode[itemSel]<<-input$op_cdn
        itemCdnV[itemSel]<<-input$op_cdn
        cdnNew<-NULL
        for (i in 1:length(itemCdn)){
          cdnNew<-paste0(cdnNew,itemCdn[i])
        }
        
        cdnNewCode<-NULL
        for (i in 1:length(itemCdnCode)){
          cdnNewCode<-paste0(cdnNewCode,itemCdnCode[i])
        }
        
        cdnNewV<-NULL
        for (i in 1:length(itemCdnV)){
          cdnNewV<-paste0(cdnNewV,itemCdnV[i])
        }
        # cdnEditItem$val<-paste0("Condition to Edit: \n", "C",cdnSelNum," : ",cdnNew, "\n","Selected Item: \n", itemCdn[itemSel])
        
        cdnAllV[[cdnSelNum]]<<-cdnNewV
        cdnAllVis[[cdnSelNum]]<<-cdnNew
        cdnAll[[cdnSelNum]]<<-cdnNewCode
        
        # cdnEditItem$val<-paste0("Condition to Edit: \n", "C",cdnSelNum," : ",cdnNew, "\n","Selected Item: \n", itemCdn[itemSel],"\n",cdnNewCode, "\n","Selected Item: \n", itemCdnCode[itemSel])
        cdnEditItem$val<-paste0("Condition to Edit: \n", cdnSelNum," : ",cdnNew, "\n","Selected Item: \n", itemCdn[itemSel],"\n",cdnNewV, "\n","Selected Item: \n", itemCdnV[itemSel])
        updateSelectInput(session,"op_cdn",selected="No Selection")
        updateSelectInput(session,"cdns",choices=cdnAllVis,selected=cdnNew)
        showCdn$val<-NULL
        cdnEdit<<-cdnNew
        cdnEditCode<<-cdnNewCode
        cdnEditV<<-cdnNewV
      }
    }else{
      if (is.na(input$val_cdn)){
        showNotification("Please input value.",duration=2)
      }else{
        itemCdn[itemSel]<<-input$val_cdn
        itemCdnCode[itemSel]<<-input$val_cdn
        itemCdnV[itemSel]<<-input$val_cdn
        cdnNew<-NULL
        
        for (i in 1:length(itemCdn)){
          cdnNew<-paste0(cdnNew,itemCdn[i])
        }
        
        cdnNewCode<-NULL
        for (i in 1:length(itemCdnCode)){
          cdnNewCode<-paste0(cdnNewCode,itemCdnCode[i])
        }
        
        cdnNewV<-NULL
        for (i in 1:length(itemCdnV)){
          cdnNewV<-paste0(cdnNewV,itemCdnV[i])
        }
        # cdnEditItem$val<-paste0("Condition to Edit: \n", "C",cdnSelNum," : ",cdnNew, "\n","Selected Item: \n", itemCdn[itemSel])
        cdnAllV[[cdnSelNum]]<<-cdnNewV
        cdnAllVis[[cdnSelNum]]<<-cdnNew
        cdnAll[[cdnSelNum]]<<-cdnNewCode
        
        # cdnEditItem$val<-paste0("Condition to Edit: \n", "C",cdnSelNum," : ",cdnNew, "\n","Selected Item: \n", itemCdn[itemSel],"\n",cdnNewCode, "\n","Selected Item: \n", itemCdnCode[itemSel])
        cdnEditItem$val<-paste0("Condition to Edit: \n", cdnSelNum," : ",cdnNew, "\n","Selected Item: \n", itemCdn[itemSel],"\n",cdnNewV, "\n","Selected Item: \n", itemCdnV[itemSel])
        updateNumericInput(session,"val_cdn",val="NA")
        updateSelectInput(session,"cdns",choices=cdnAllVis,selected=cdnNew)
        showCdn$val<-NULL
        cdnEdit<<-cdnNew
        cdnEditCode<<-cdnNewCode
        cdnEditV<<-cdnNewV
      }
    }
    
  })
  }
  
  #Edit Logical Statement
  {
  itemLS<-NULL                                           # character vector to store parts of LS being edited
  lsEdit<-NULL                                           # temporary copy of LS being edited
  lsSelNum<-NULL                                         # stores identifier number of LS. eg. LS1
  lsEditItem<-reactiveValues(val=NULL)                   # reactive value to trigger display of LS items on screen 
  
  #################################################
  # Function editLS breaks the logical string into components (EBCs, boolean operatorl) and lets user change any of these independently
  
  # Arguments: NONE. Uses variables defined in parent scope (eg. allLS)
  
  # Return: NONE. Updates variables defined in parent scope using '<<-' operator. 
  #################################################
  
  editLS<-function(){
    
    lsEditItem$val<-NULL
    itemLS<<-c()
    if (input$LS==allLS[[2]]){
      showNotification("Default string cannot be modified.",duration=2)
      return()
    }
    if ((input$LS!="No Selection")&(input$op_LS=="EDIT")){
      lsEdit<<-isolate(input$LS)
      for (i in 1:length(allLS)){
        if (allLS[[i]]==input$LS){
          lsSelNum<<-names(allLS)[i]
        }
      }
      op1<-c("XOR")
      op_match1<-c()
      for (i in 1:length(op1)){
        j<-gregexpr(op1[i],lsEdit,fixed=TRUE)
        if (j[[1]][1]!=-1){
          op_match1<-c(op_match1,j[[1]])
          lsEdit<<-gsub(op1[i],"  ",lsEdit,fixed=TRUE)
        }
      }
      op2<-c("&","!","|","(",")")
      op_match2<-c()
      for (i in 1:length(op2)){
        j<-gregexpr(op2[i],lsEdit,fixed=TRUE)
        if (j[[1]][1]!=-1){
          op_match2<-c(op_match2,j[[1]])
          lsEdit<<-gsub(op2[i]," ",lsEdit,fixed=TRUE)
        }
      }
      op_match<-sort(c(op_match1,op_match2))
      
      lsEdit<<-input$LS
      
      lsIter<-1
      if (length(op_match)>0){
        for (i in 1:(length(op_match))){
          if (op_match[i]>lsIter){
            st<-lsIter
            en<-op_match[i]-1
            sig<-substr(lsEdit,st,en)
            if (sig!=""){
              itemLS<<-c(itemLS,trimws(sig))  
            }
            
          }
          lsIter<-op_match[length(op_match)]+1
          pos1<-which(op_match1==op_match[i])
          pos2<-which(op_match2==op_match[i])
          if (length(pos1)>0){
            itemLS<<-c(itemLS,substr(lsEdit,op_match[i],(op_match[i]+2)))
            st<-op_match[i]+2
          }else if (length(pos2)>0) {
            itemLS<<-c(itemLS,substr(lsEdit,op_match[i],(op_match[i])))
            st<-op_match[i]+1
          }
          if (i<length(op_match)){
            en<-(op_match[(i+1)]-1)
          }else{
            en<-nchar(lsEdit)
          }
          # if ((st<=op_match[length(op_match)])&(en<=op_match[length(op_match)])){
          if ((st<=nchar(lsEdit))&(en<=nchar(lsEdit))){
            sig<-trimws(substr(lsEdit,st,en))
            if (sig!=""){
              itemLS<<-c(itemLS,sig)  
            }
          }
        }
      }
    }
  }
  
  # Function calls editLS when editing of logical string is selected 
  observeEvent(input$LS,{
    if ((input$LS!="No Selection")&(input$op_LS=="EDIT")){
      editLS()
    }
  })
  
  # Function calls editLS when editing of logical string is selected 
  observeEvent(input$op_LS,{
    if ((input$LS!="No Selection")&(input$op_LS=="EDIT")){
      editLS()
    }
  })
  
  # Output variable to display parts of LS being edited on screen
  output$edit_LS<-renderText({
  # output$edit_LS<-renderPrint({
    if (is.null(lsEditItem$val)){
      if (is.null(itemSel)){
        itemSel<<-1  
      }
      lsEditItem$val<-paste0("Logical String to Edit: \n", lsSelNum," : ",input$LS, "\n","Selected Item: \n", itemLS[itemSel])
      
      # lsEditItem$val<-itemLS

    }
    lsEditItem$val
  })
  
  # Moves to next part of LS being edited
  observeEvent(input$nxt_LS,{
    if (input$LS=="No Selection"){
      showNotification("Please select Logical String to edit.",duration=2)
      return()
    }
    if (itemSel<length(itemLS)){
      itemSel<<-itemSel+1
      
      lsEditItem$val<-paste0("Logical String to Edit: \n", lsSelNum," : ",lsEdit, "\n","Selected Item: \n", itemLS[itemSel])
      # cdnEditItem$val<-itemCdn[itemSel]
    }else{
      showNotification("Reached end of Logical string.",duration=2)
    }
    
  })
  
  # Moves to previous part of LS being edited
  observeEvent(input$prev_LS,{
    if (input$LS=="No Selection"){
      showNotification("Please select Logical String to edit.",duration=2)
      return()
    }
    if (itemSel>1){
      itemSel<<-itemSel-1
      lsEditItem$val<-paste0("Logical String to Edit: \n", lsSelNum," : ",lsEdit, "\n","Selected Item: \n", itemLS[itemSel])
    }else{
      showNotification("Reached end of Logical string.",duration=2)
    }
    
  })
  
  # Updates the selected part of LS being edited
  observeEvent(input$upd_LS,{
    if (input$LS=="No Selection"){
      showNotification("Please select Logical String to edit.",duration=2)
      return()
    }
    op1<-c("XOR")
    op2<-c("&","!","|","(",")")
    j<-which(names(cdnAllV)==itemLS[itemSel])
    jj<-which(delCdn==trimws(itemLS[itemSel]))
    j1<-which(op1==itemLS[itemSel])
    j2<-which(op2==itemLS[itemSel])
    if ((length(j)>0)|(length(jj)>0)){
      if ((input$cdns=="No Selection")|(input$cdns=="SelectAll")){
        showNotification("Please select condition to update.",duration=2)
      }else{
        itemLS[itemSel]<<-names(which(cdnAllVis==input$cdns))
        
        lsNew<-NULL
        for (i in 1:length(itemLS)){
          lsNew<-paste0(lsNew," ",itemLS[i])
        }
        lsNew<-trimws(lsNew)
        lsEditItem$val<-paste0("Logical String to Edit: \n", lsSelNum," : ",lsNew, "\n","Selected Item: \n", itemLS[itemSel])
        allLS[[lsSelNum]]<<-trimws(lsNew)
        
        updateSelectInput(session,"cdns",selected="No Selection")
        updateSelectInput(session,"LS",choices=allLS,selected=lsNew)
        showLs$val<-NULL
        lsEdit<<-lsNew
      }
    }else if ((length(j1)>0)|(length(j2)>0)){
      if (input$op_cdns==" "){
        showNotification("Please select operator.")
      }else{
        itemLS[itemSel]<<-input$op_cdns
        
        lsNew<-NULL
        for (i in 1:length(itemLS)){
          lsNew<-paste0(lsNew," ",itemLS[i])
        }
        lsNew<-trimws(lsNew)
        allLS[[lsSelNum]]<<-lsNew
        
        lsEditItem$val<-paste0("Logical String to Edit: \n", lsSelNum," : ",lsNew, "\n","Selected Item: \n", itemLS[itemSel])
        updateSelectInput(session,"op_cdns",selected="No Selection")
        updateSelectInput(session,"LS",choices=allLS,selected=lsNew)
        showLs$val<-NULL
        lsEdit<<-lsNew
        
      }
    }
  })
  
  }
  
  #Preview EBCs and LS
  {
    # Output variable to show number of records returned by applying selected EBC on the dataset
    output$cdn_test<-renderText({
      
      if (input$preview_cdns==0)return()
      if (length(cdnAllV)<=1){
        showNotification("Conditions not defined.",duration=2)
        return()
      }
      if (input$load_data==1){
        showNotification("Project Data not loaded",duration=2)
        return()
      }
      EBCs<-c()
      if (isolate(input$cdns)!="No Selection"){
        for (i in 2:length(cdnAllV)){
          EBCs<-c(EBCs,paste0(names(cdnAllV)[i]," = (",cdnAllV[[i]],")"))
          if (isolate(input$cdns)==cdnAllVis[[i]]){
            LogicalString<-paste0("( ",names(cdnAllV)[i]," )")
          }
        }  
      }
      
      # if (isolate(input$LS)!="No Selection"){
      #   # for (i in 1:length(allLS)){
      #   #   if (names(allLS)[i]==isolate(input$LS)){
      #   #     LogicalString<<-allLS[[i]]
      #   #   }
      #   # }
      #   LogicalString<<-paste0("( ",isolate(input$LS)," )")
      # }
      
      dataFilter<-EquationBasedDataFilter(EBCs,LogicalString,PROJECT_DATA_VXX)
      paste0("Number of Records in Filtered Data with condition ", LogicalString," : ",length(which(dataFilter)))
    })
    
    # Output variable to show number of records returned by applying selected Logical String on the dataset
    output$LS_test<-renderText({
      
      if (input$preview_LS==0)return()
      selected_LS<-isolate(input$LS)
      if (selected_LS==" ")return()
      if (length(cdnAllV)<=1){
        showNotification("Conditions not defined.",duration=2)
        return()
      }
      if (isolate(input$load_data)==1){
        showNotification("Project Data not loaded",duration=2)
        return()
      }
      EBCs<-c()
      # if (isolate(input$cdns)!="No Selection"){
      for (i in 2:length(cdnAllV)){
        EBCs<-c(EBCs,paste0(names(cdnAllV)[i]," = (",cdnAllV[[i]],")"))
        if (isolate(input$cdns)==cdnAllVis[[i]]){
          LogicalString<-paste0("( ",names(cdnAllV)[i]," )")
        }
      }  
      # }
      
      
      # if (isolate(input$LS)!="No Selection"){
      if (selected_LS!="No Selection"){
        # for (i in 1:length(allLS)){
        #   if (names(allLS)[i]==isolate(input$LS)){
        #     LogicalString<<-allLS[[i]]
        #   }
        # }
        # LogicalString<-paste0("( ",isolate(input$LS)," )")
        LogicalString<-paste0("( ",selected_LS," )")
      }
      
      dataFilter<-EquationBasedDataFilter(EBCs,LogicalString,PROJECT_DATA_VXX)
      paste0("Records in Filtered Data with Logical String ", LogicalString," : ",length(which(dataFilter)))  
    })
  }
  
  #apply LS and EBCs to data set for creating nD data filter.
  {
  EBCs<-NULL                                                # list to store defined EBCs and sent as argument to EquationBasedDataFilter function
  LogicalString<-NULL                                       # string to store selected LS and sent as argument to EquationBasedDataFilter function

  #Function calls EquationBasedDataFilter to apply selected Logical String. The resulting data filter is saved as nD filter in dataFilternD.
  observeEvent(input$app_LS,{
    if (input$load_data==1){
      showNotification("Project Data not loaded",duration=2)
      return()
    }
    if (input$LS == "No Selection"){
      showNotification("Please select logical string",duration = 2)
      return()
    }
    if (length(cdnAllV)<=1){
      showNotification("Conditions not defined.",duration=2)
      return()
    }
    EBCs<<-NULL
    for (i in 2:length(cdnAllV)){
      EBCs<<-c(EBCs,paste0(names(cdnAllV)[i]," = (",cdnAllV[[i]],")"))
    }
    
    if (isolate(input$LS)!="No Selection"){
      # for (i in 1:length(allLS)){
      #   if (names(allLS)[i]==isolate(input$LS)){
      #     LogicalString<<-allLS[[i]]
      #   }
      # }
      LogicalString<<-paste0("( ",isolate(input$LS)," )")
    }
    
    if ((!is.null(EBCs))&(!is.null(LogicalString))){
      dataFilternD<<-EquationBasedDataFilter(EBCs,LogicalString,PROJECT_DATA_VXX) # returns nD filter
      if (length(which(dataFilternD))>0){
        updateExtentStatus(3)           # updates selection of data filter  option to 'nD Filter' on main GUI  
      }else{
        showNotification('Data Filter resulted no records.',duration=2)
        return()
      }
      
    }
    
  })
  
  #Function calls EquationBasedDataFilter to apply selected EBC. The resulting data filter is saved as nD filter in dataFilternD.
  observeEvent(input$app_cdns,{
    if (input$load_data==1){
      showNotification("Project Data not loaded",duration=2)
      return()
    }
    if (input$cdns == "No Selection"){
      showNotification("Please select condition.",duration = 2)
      return()
    }
   
    if (length(cdnAllV)<=1){
      showNotification("Conditions not defined.",duration=2)
      return()
    }
    EBCs<<-NULL
    LogicalString<<-NULL
    if (isolate(input$cdns)!="No Selection"){
      for (i in 2:length(cdnAllV)){
        EBCs<<-c(EBCs,paste0(names(cdnAllV)[i]," = (",cdnAllV[[i]],")"))
        if (isolate(input$cdns)==cdnAllVis[[i]]){
          LogicalString<<-paste0("( ",names(cdnAllV)[i]," )")
        }
      }
    }
    
    if ((!is.null(EBCs))&(!is.null(LogicalString))){
      dataFilternD<<-EquationBasedDataFilter(EBCs,LogicalString,PROJECT_DATA_VXX)
      if (length(which(dataFilternD))>0){
        updateExtentStatus(3) #updates Data filter option on main GUI to 'nD Filter'  
      }else{
        showNotification('Data Filter resulted no records.',duration=2)
        return()
      }
      
    }
    
  })
  }
  
  
  #Save and Load conditions (EBCs) and Logical String (LS) to RCode File
  {
  #lines<-reactiveValues(val=NULL)
  
  # Saves EBCs and Logical strings to Rcode file
  observeEvent(input$save_LS,{
    # if ((length(cdnAllVis)==1)&(!cdnFound)){
    #   showNotification("Conditions not defined.",duration=2)
    #   return()
    # }else 
    #   
    if (length(cdnAllVis)==1){
      cdn_vector<-c()
    }else if(length(cdnAllVis)>1){
      cdn_vector<-c("# Shiny Data Filtering Conditions Start","# Visualization")
      for (i in 2:length(cdnAllVis)){
        if (cdnDes[[i]]!="R-file")
          cdn_vector<-c(cdn_vector,paste0("# ",names(cdnAllVis)[i]," = ",cdnAllVis[[i]]," ",cdnDes[[i]]))
      }
      cdn_vector<-c(cdn_vector,"# Vector form")
      for (i in 2:length(cdnAllV)){
        if (cdnDes[[i]]!="R-file")
          cdn_vector<-c(cdn_vector,paste0("# ",names(cdnAllV)[i]," = ",cdnAllV[[i]]," ",cdnDes[[i]]))
      }
      # cdn_vector<-c(cdn_vector,"# Code")
      # for (i in 3:length(cdnAll)){
      #   cdn_vector<-c(cdn_vector,paste0("# ",cdnAll[[i]]))
      # }
      
      cdn_vector<-c(cdn_vector,"# Logical Strings")
      for (i in 2:length(allLS)){
        cdn_vector<-c(cdn_vector,paste0("# ",names(allLS)[i]," = ",allLS[[i]]))
      }
      
      
      cdn_vector<-c(cdn_vector,"# Shiny Data Filtering Conditions End")
      # lines$val<-cdn_vector  
    }
    
    filename<-paste0(in_path,"/",input$sel1,"/",input$sel2,"/",input$sel2,".R")
    rl<-readLines(con=filename)
    i<-which(rl=="# Shiny Data Filtering Conditions Start")
    j<-which(rl=="# Shiny Data Filtering Conditions End")
    if ((length(i)!=0)&(length(j)!=0)){
      if (j==length(rl)){
        new_rl<-c(rl[1:(i-1)],cdn_vector)
      }else if(j<length(rl)){
        new_rl<-c(rl[1:(i-1)],cdn_vector,rl[(j+1):length(rl)])  
      }
      
    }else{
      new_rl<-c(rl,cdn_vector)
    }
    # cat("#testing \n #testing again",file=filename,append=TRUE,sep="\n")
    
    writeLines(new_rl,con=filename)
    showNotification("Conditions Saved.",duration=2)
  })
  
  
  cdnFound<-FALSE                  #boolean variable to indicate if any stored EBCs and LS found in Rcode file while loading
  sigFound<-FALSE                  #boolean variable to indicate if any stored derived signals found in Rcode file while loading
  
  ###############################
  # Function loadCdnRfile checks for EBCs, LS and derived signal definitions in RCode file and loads them into shiny if found.
  
  # Arguments: NONE. Uses variables defined in parent's scope
  
  # Return: NONE. Updates variables defined in parent's scope using '<<-' operator.
  ###############################
  
  loadCdnRfile<-function(){
    filename<-paste0(in_path,"/",input$sel1,"/",input$sel2,"/",input$sel2,".R")
    # rl<-readLines(con=filename)
    # i<-which(grepl("INPUT_NUMBER_OF_EBCs",rl,fixed=TRUE))[1]
    rl<-readLines(con=filename)
    cdn_i<-which(rl=="# Shiny Data Filtering Conditions Start")
    cdn_j<-which(rl=="# Shiny Data Filtering Conditions End")
    cdnFound<<-FALSE
    if (INPUT_NUMBER_OF_EBCs>0){
      for (i in 1:INPUT_NUMBER_OF_EBCs){
        cdnAllV[[paste0("EBC",i)]]<<-eval(parse(text=paste0("EBC",i))[[1]])
        cdnAllVis[[paste0("EBC",i)]]<<-eval(parse(text=paste0("EBC",i))[[1]])
        cdnDes[[paste0("EBC",i)]]<<-"#R-file"
        cdnNum<<-length(cdnAllVis)
      }
      cdnFound<<-TRUE
    }
    if ((length(cdn_i)!=0)&(length(cdn_j)!=0)){
      i1<-which(rl=="# Visualization")
      i2<-which(rl=="# Vector form")
      
      for (ii in 1:(i2-i1-1)){
        # cdnNum<<-cdnNum+1
        st<-regexpr("=",rl[i1+ii],fixed=TRUE)[1]+1
        stall<-gregexpr("#",rl[i1+ii],fixed=TRUE)[[1]]
        en<-stall[length(stall)]-1
        cdnvis<-substr(rl[i1+ii],st,en)
        en<-regexpr("=",rl[i1+ii],fixed=TRUE)[1]-1
        cdnname<-trimws(substr(rl[i1+ii],2,en))
        cdnAllVis[[cdnname]]<<-trimws(cdnvis)
        cdnNum<<-length(cdnAllVis)
        
        st<-regexpr("=",rl[i2+ii],fixed=TRUE)[1]+1
        stall<-gregexpr("#",rl[i2+ii],fixed=TRUE)[[1]]
        en<-stall[length(stall)]-1
        cdnv<-substr(rl[i2+ii],st,en)
        cdnDes[[cdnname]]<<-substr(rl[i2+ii],en,nchar(rl[i2+ii]))
        cdnAllV[[cdnname]]<<-trimws(cdnv)
      }
      cdnFound<<-TRUE
    }
    if (cdnFound){
      if (input$der_sig=="No"){
        cdnCnt$val <- cdnNum
        showCdn$val<-NULL
      }
      updateSelectInput(session,"cdns",choices=cdnAllVis,selected="No Selection")
      updateSelectInput(session,"evt_cdn",choices=cdnAllVis,selected="No Selection")
    }
    
    lsFound<-FALSE
    if (INPUT_LOGICAL_EBC_STRING!=""){
      allLS[[paste0("LS",length(allLS))]]<<-trimws(INPUT_LOGICAL_EBC_STRING)
      lsFound<-TRUE
    }
    if ((length(cdn_i)!=0)&(length(cdn_j)!=0)){
      i3<-which(rl=="# Logical Strings")
      lsFound<-TRUE
      for (ii in (i3+1):(cdn_j-1)){
        st<-regexpr("=",rl[ii],fixed=TRUE)[1]+1
        ls<-substr(rl[ii],st,nchar(rl[ii]))
        en<-regexpr("=",rl[ii],fixed=TRUE)[1]-1
        lsname<-trimws(substr(rl[ii],2,en))
        allLS[[lsname]]<<-trimws(ls)
        if (grepl("LS1(Default)",lsname,fixed=TRUE)){
          def_cdn_string_vis$val<-trimws(ls)  
        }
        
      }
    }
    if (lsFound){
      lsCnt$val <- length(allLS)
      updateSelectInput(session,"LS",choices=allLS,selected="No Selection")
    }
    if ((cdnFound)&(lsFound)){
      updateSelectInput(session,"evt_cdn",choices=c(cdnAllVis,allLS),selected="No Selection") 
    }else if(cdnFound){
      updateSelectInput(session,"evt_cdn",choices=cdnAllVis,selected="No Selection") 
    }
    
    sig_i<-which(rl=="# Shiny Derived Signals Start")
    sig_j<-which(rl=="# Shiny Derived Signals End")
    sigFound<<-FALSE
    
    if ((length(sig_i)!=0)&(length(sig_j)!=0)){
      i1<-which(rl=="# DS Visualization")
      i2<-which(rl=="# DS Vector form")
      i3<-which(rl=="# DS RCode form")
      
      for (ii in 1:(i2-i1-1)){
        sigNum<<-sigNum+1
        st<-regexpr("=",rl[i1+ii],fixed=TRUE)[1]+1
        stall<-gregexpr("#",rl[i1+ii],fixed=TRUE)[[1]]
        en<-stall[length(stall)]-1
        sigvis<-substr(rl[i1+ii],st,en)
        en<-regexpr("=",rl[i1+ii],fixed=TRUE)[1]-1
        signame<-trimws(substr(rl[i1+ii],2,en))
        sigAllVis[[signame]]<<-trimws(sigvis)
        
        st<-regexpr("=",rl[i2+ii],fixed=TRUE)[1]+1
        stall<-gregexpr("#",rl[i2+ii],fixed=TRUE)[[1]]
        en<-stall[length(stall)]-1
        sigv<-substr(rl[i2+ii],st,en)
        sigDes[[signame]]<<-substr(rl[i2+ii],en+1,nchar(rl[i2+ii]))
        sigAllV[[signame]]<<-trimws(sigv)
        
        st<-regexpr("=",rl[i3+ii],fixed=TRUE)[1]+1
        stall<-gregexpr("#",rl[i3+ii],fixed=TRUE)[[1]]
        en<-stall[length(stall)]-1
        sig<-substr(rl[i3+ii],st,en)
        # sigDes[[signame]]<<-substr(rl[i2+ii],en+1,nchar(rl[i2+ii]))
        sigAll[[signame]]<<-trimws(sig)
      }
      sigFound<<-TRUE
    }
    if (sigFound){
      updateSelectInput(session,"sigs",choices=c("No Selection"="No Selection",sigAllVis),selected="No Selection")
      if (input$der_sig=="Yes"){
        sigCnt$val <- sigNum
        showSig$val<-NULL
      }
      
    }
  
  }
  
  # output$test_string<-renderPrint({
  #   req(lines$val)
  #   print(lines$val)
  # })
  }
  
  # Output variable to display signal information, time steps information on screen when a plant is selected
  output$Plant<-renderText({
    req(input$sel1)
    req(input$sel2)
    txt<-NULL
    if (input$sel2!="No Selection"){
      project<-input$sel1
      plant<-input$sel2
      fl<-list.files(path=paste0(in_path,"/",project,"/",plant),pattern="*.txt", recursive=FALSE)
      input_txt<-paste0(plant,".txt")
      if (length(fl[grepl(input_txt,fl,fixed=TRUE)])==0){
        # showNotification("Project data file not present",duration=2)
        return()
      }else{
        INPUT_PROJECT_INPUT_TXT_FILE<-paste0(in_path,"/",project,"/",plant,"/",plant,".txt")
      }
      
      interface_txt<-paste0("R_CODE_INTERFACE_FILE_",plant,".txt")
      interface_file_found<-FALSE
      if (length(fl[grepl(interface_txt,fl,fixed=TRUE)])>0){
        interface_file_found<-TRUE
        interfacefile<-paste0(in_path,"/",project,"/",plant,"/R_CODE_INTERFACE_FILE_",plant,".txt")
        signal_info <- fread(interfacefile,skip = 0,na.strings=c("NULL", "ERROR", "-1.#INF", "1.#INF", "NA", "-1.#INF0", "1.#INF0"),blank.lines.skip=TRUE)
      }
      
      
      header_info <- fread(INPUT_PROJECT_INPUT_TXT_FILE,
                           header=TRUE,
                           nrows=5,
                           skip = 0,
                           dec=".",
                           na.strings=c("NULL", "ERROR", "-1.#INF", "1.#INF", "NA", "-1.#INF0", "1.#INF0"),blank.lines.skip=TRUE)  
      
      data_start_row<-2
      if (length(which(header_info[[1]]!=""))>0){
        data_start_row<-which(header_info[[1]]!="")[1]  
      }
      
      
      txt_file_source<-NULL
      time_check_tde<-length(which(colnames(header_info)=="Time:"))#checks for time column coming in TDE txt file
      date_check_tde<-length(which(colnames(header_info)=="Date:"))#checks for Date column coming in TDE txt file
      time_check_probopdata<-length(which(colnames(header_info)=="Time"))#checks for time column coming in Probopdata txt file
      
      # mydate_col<-length(which(colnames(header_info)=="MYDATE"))#checks for presence of MYDATE column (only for temporary use)
      
      if ((time_check_tde>0)&(date_check_tde>0)){
        txt_file_source<-"TDE"
        # INPUT_NUMBER_OF_LINES_SKIPPED<-2
        INPUT_NUMBER_OF_LINES_SKIPPED<-data_start_row
        columnstoSkip<-2 # Date and Time column
      }else if(time_check_probopdata>0){
        txt_file_source<-"ProbOpData"
        INPUT_NUMBER_OF_LINES_SKIPPED<-1
        columnstoSkip<-1 # Time column
      }else{
        showNotification("Unrecognized txt file.",duration=2)
        return()
      }
      
      txt<-paste0("<br> <b>PROJECT : </b>",input$sel1, "<br>", 
                  "<b>PLANT : </b>",input$sel2, "<br>","<br>",
                  "<b>NUMBER OF SIGNALS : </b>",INPUT_MY_NUMBER_OF_INPUTS, "<br>") 
      
      
      for (i in 1:INPUT_MY_NUMBER_OF_INPUTS){
        ii<-NULL
        
        if (interface_file_found){
          if (txt_file_source=="TDE"){
            ii<-which(signal_info$SignalName==colnames(header_info)[i+columnstoSkip])
          }else if(txt_file_source=="ProbOpData"){
            ii<-which(signal_info$Info==colnames(header_info)[i+columnstoSkip])  
          }
          # ii<-which(signal_info$Info==colnames(header_info)[i])
          # ii<-which(signal_info$SignalName==colnames(header_info)[i+2]) #skipping first two columns of date & time
        }
        if (length(ii)>0){
          unt<-signal_info$SignalUnit[ii]
        }else{
          if (!is.na(header_info[[1]][1])){
            if (header_info[[1]][1]=="Date:"){
              unt<-header_info[[i+columnstoSkip]][2]  
            }else{
              unt<-header_info[[i+columnstoSkip]][1]
            }
          }
          else{
            unt<-header_info[[i+columnstoSkip]][1]
          }
          
        }
        unt<-gsub(" ","_",unt,fixed=TRUE)
        txt<-paste0(txt,"<b> SIGNAL - ",i, " : </b>",INPUT_VECTOR_VXX_Title_Type_Text[i],", ",
                    "<b> LOWER BOUND : </b>",INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE[i],", ",
                    "<b> UPPER BOUND : </b>",INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE[i],", ",
                    "<b> UNIT : </b>",unt, "<br>")
      }
      
      txt<-paste0(txt, "<br>", "<br>")
      
      
      if(!is.null(data_load$dl)){
        txt<-paste0(txt,"<b>PLANT DATA SUMMARY : </b>","<br>")  
        for (i in 1:INPUT_MY_NUMBER_OF_INPUTS){
          if (length(which(!is.na(PROJECT_DATA_VXX[[i]])))>0){
            minvalue<-round(min(PROJECT_DATA_VXX[[i]],na.rm = TRUE),5)
            maxvalue<-round(max(PROJECT_DATA_VXX[[i]],na.rm = TRUE),5)
            meanvalue<-round(mean(PROJECT_DATA_VXX[[i]],na.rm = TRUE),5)
            txt<-paste0(txt,"<b> SIGNAL - ",i, " : </b>"," <b>MIN : </b>",minvalue," <b>MAX : </b>",maxvalue," <b>MEAN : </b>",meanvalue,  "<br>")  
          }else{
            minvalue <- NA
            maxvalue <- NA
            meanvalue <- NA
            txt<-paste0(txt,"<b> SIGNAL - ",i, " : </b>"," <b>MIN : </b>",minvalue," <b>MAX : </b>",maxvalue," <b>MEAN : </b>",meanvalue,  "<br>")  
          }
          
        }
        txt<-paste0(txt, "<br>")
        td<-difftime(PROJECT_DATA_VXX$MY_DATE[2],PROJECT_DATA_VXX$MY_DATE[1],units = "secs")
        txt<-paste0(txt,"<b>TIME RESOLUTION : </b>",td, " seconds", "<br>")
        txt<-paste0(txt,"<b>START TIMESTAMP : </b>",format(PROJECT_DATA_VXX$MY_DATE[1],format="%Y-%m-%d %H:%M:%S"), "<br>",
                    "<b>END TIMESTAMP : </b>",format(PROJECT_DATA_VXX$MY_DATE[dim(PROJECT_DATA_VXX)[1]],format="%Y-%m-%d %H:%M:%S"), "<br>",
                    "<b>NUMBER OF ROWS : </b>",dim(PROJECT_DATA_VXX)[1], "<br>")
        
      }
      
    }
    
    txt
    
    # input$sel1
  })
  
	}
	
	#Event Detection
	{
	  # Updates event detection drop down menu with EBCs defined in boundary conditions tab
	observeEvent(input$datapro_tabs,{
	  # cdns<-list()
	  # if (length(cdnAllVis)>1){
	  #   updateSelectInput(session,"evt_cdn",choices=cdnAllVis,selected="No Selection")
	  # }
	  
	  cdns<-allLS
	  cdns[["No Selection"]]<-NULL
	  if (length(cdnAllVis)>1){
	    updateSelectInput(session,"evt_cdn",choices=c(cdnAllVis,cdns),selected="No Selection")
	  }
	})
	
	  # Output variable to display EBCs on event detection tab
	output$evt_cdn_sel<-renderText({
	  if (is.null(showCdn$val)){
	    cdnDisp()
	  }
    # showCdn$val
	  a<-paste0("Logical Strings:-")
	  if (length(allLS)>1){
	    for (i in 2:length(allLS)){
	      a1<-paste0(names(allLS)[i]," : (",allLS[[i]],")")
	      a<-paste0(a," \n",a1)
	    }
	  }
	  paste0(showCdn$val," \n", a)
	})
	
	allEvts<-list("No Selection"="No Selection")                    # list to store all event definitions
	evtStrend<-FALSE
	evtStringpart<-c()                                              # character vector to store parts of event definition when being created by user
	showEvt<-reactiveValues(val=NULL)                               # reactive value to trigger display of defined events on screen
	evtCount<-reactiveValues(val=NULL)                              # reactive value to keep track of event counter
	evtNum<-0                                                       # number of events defined
	evtDefinition<-reactiveValues(val=NULL)                         # reactive value to display event definition on screen when being created by user
	# Adds selected EBC, operator and event duration to event definition
	observeEvent(input$evt_add,{
	
	  if ((input$evt_cdn=="No Selection")){
	    showNotification("Please select Condition for Event Definition.",duration=2)
	    return()
	  }
	  if ((input$evt_op!=" ")|(!is.na(input$evt_dur))|(input$evt_dur_unit!=" ")){
	    if (input$evt_op==" "){
	      showNotification("Please select operator.",duration=2)
	      return()
	    }
	    if (is.na(input$evt_dur)){
	      showNotification("Please input duration.",duration=2)
	      return()
	    }
	    if (input$evt_dur_unit==" "){
	      showNotification("Please select units.",duration=2)
	      return()
	    }
	  }
	  # if ((input$evt_cdn=="No Selection")&(input$evt_op==" ")&(is.na(input$evt_dur))&(input$evt_dur_unit==" ")
	  #     &(evtStrend)&(input$evt_log_op!=" ")){
	  #   evtDefinition$val<-paste0(evtDefinition$val," ",input$evt_log_op)
	  #   evtStrend<<-FALSE
	  #   return()
	  # }
	  evtSubstr<-NULL
	  if (input$evt_cdn!="No Selection"){
	    # i<-which(unlist(cdnAllVis)==input$evt_cdn)
	    # evtSubstr<-paste0("( ",names(cdnAllVis)[i])  
	    
	    i<-which(unlist(cdnAllVis)==input$evt_cdn)
	    j<-which(unlist(allLS)==input$evt_cdn)
	    if (length(i)>0){
	      evtSubstr<-paste0("( ",names(cdnAllVis)[i])    
	    }else{
	      if (names(allLS)[j]=="LS1(Default)"){
	        evtSubstr<-paste0("( LS1")
	      }else{
	        evtSubstr<-paste0("( ",names(allLS)[j])  
	      }
	      
	    }
	  }
	  
	  if (!is.na(input$evt_dur)){
	    evtSubstr<-paste0(evtSubstr," ", input$evt_op, " ", input$evt_dur, " ", input$evt_dur_unit)
	  }
	  updateSelectInput(session,"evt_cdn",selected="No Selection")
	  updateNumericInput(session,"evt_dur",value=NA)
	  updateSelectInput(session,"evt_op",selected=" ")
	  updateSelectInput(session,"evt_dur_unit",selected=" ")
	  evtSubstr<-paste0(evtSubstr," )")
	  # if (input$evt_log_op!=" "){
	  #   evtSubstr<-substr(evtSubstr,1,(nchar(evtSubstr)-2))
	  #   evtSubstr<-paste0(evtSubstr," ", input$evt_log_op)
	  #   updateSelectInput(session,"evt_log_op",selected=" ")
	  # }
	  
	  
	  # if (is.null(evtString)){
	  #   evtString<<-paste0("( ",names(cdnAllVis)[i])
	  # }else{
	  #   evtString<<-substr(evtString,1,(nchar(evtString)-2))
	  #   evtString<<-paste0(evtString," AND ",names(cdnAllVis)[i])
	  # }
	  # if (!is.na(input$evt_dur)){
	  #   evtString<<-paste0(evtString," ",input$evt_op," ",input$evt_dur," ",input$evt_dur_unit)  
	  # }
	  # evtString<-paste0(evtString," )")
	  if (is.null(evtDefinition$val)){
	    evtDefinition$val<-trimws(evtSubstr)
	  }else{
	    evtSubstr<-paste0(" AND ", evtSubstr)
	    evtDefinition$val<-paste0(evtDefinition$val," ",trimws(evtSubstr))
	  }
	  evtStringpart<<-c(evtStringpart,evtSubstr)
	})
	
	# removes last add EBC, operator or event duration from event definition
	observeEvent(input$evt_rem,{
	  evtStringpart<<-evtStringpart[-length(evtStringpart)]
	  evtDefinition$val<-NULL
	  if (length(evtStringpart)>0){
	    for (i in 1:length(evtStringpart))evtDefinition$val<-paste0(evtDefinition$val," ",evtStringpart[i])
	  }
	})
	
	# Completes event definition and adds to event list
	observeEvent(input$evt_plus,{
	  if (is.null(evtDefinition$val)){
	    showNotification("Event not defined.",duration=2)
	    return()
	  }
	  allEvts[[paste0("EVT",(length(allEvts)))]]<<-evtDefinition$val
	  evtNum<<-evtNum+1
	  evtCount$val<-evtNum
	  showEvt$val<-NULL
	  updateSelectInput(session,"evts",choices=allEvts,selected="No Selection")
	  tsevt<-"No Selection"
	  if (input$ts_event!="No Selection"){
	    tsevt<-allEvts[[which(allEvts==input$ts_event)]]
	  }
	  updateSelectInput(session,"ts_event",choices=allEvts,selected=tsevt)
	  showNotification("Event added to list.",duration=2)
	  evtDefinition$val<-NULL
	  evtStringpart<<-c()
	  
	})
	
	# Removes selected event from event list
	observeEvent(input$evt_minus,{
	  if (input$evts=="No Selection"){
	    showNotification("Event not selected.",duration=2)
	    return()
	  }
	  i<-which(unlist(allEvts)==input$evts)
	  allEvts[[i]]<<-NULL
	  evtNum<<-evtNum-1
	  evtCount$val<-evtNum
	  updateSelectInput(session,"evts",choices=allEvts,selected="No Selection")
	  updateSelectInput(session,"ts_event",choices=allEvts,selected="No Selection")
	  showNotification("Event removed from list.",duration=2)
	})
	
	#Output variable to display on screen all defined events in event list 
	output$all_events<-renderText({
	  if (is.null(showEvt$val)){
	    events<-"Defined Events:-"
	    
	    if (!is.null(evtCount$val)){
	      if (length(allEvts)>1){
	        for (i in 2:length(allEvts)){
	          events<-paste0(events," \n ",names(allEvts)[i]," : (",allEvts[[i]],")")
	        }
	        showEvt$val<- events
	      }
	    }
	  }
	  showEvt$val
	})
	
	# Output variable to display on screen event being defined
	output$evt_def<-renderText({
	  req(evtDefinition$val)
	  evtDefinition$val
	})
	
	#########################################################
	# Function deconstructEvt breaks the selected event definition string into parts (EBC, operator, duration) for applying to dataset
	
	# Argument: selected event definition as string
	
	# Return value: list with separate EBCs, operators and duration
	###########################################################
	
	deconstructEvt<-function(st){
	  #finds location of brackets in event definition
	  b1<-gregexpr('(',st,fixed=TRUE)[[1]]
	  b2<-gregexpr(")",st,fixed=TRUE)[[1]]
	  
	  # extracts substrings between bracket locations found (could be EBC, operator or duration)
	  cdns<-mapply(function(x,y){
	    return(trimws(substr(st,x+1,y-1)))
	  },b1,b2)
	  
	  # searches for operators and units of event duration in the substrings found above
	  op<-c(" < "," <= "," > "," >= "," == ")
	  unts<-c("secs","mins","hours")
	  l1<-lapply(cdns, function(x){
	    return(sapply(op,function(y){
	      return(gregexpr(y,x,fixed=TRUE)[[1]])
	    }))
	  })
	  
	  l2<-lapply(cdns, function(x){
	    return(sapply(unts,function(y){
	      return(gregexpr(y,x,fixed=TRUE)[[1]])
	    }))
	  })
	  
	  # checks for EBCs with earch operator and stores found EBC in list LS
	  LS<-mapply(function(cdn,x){
	    return(ifelse(as.logical(sum(which(x>0))),paste0(" ",trimws(substr(cdn,1,x[which(x>0)]))," "),paste0(" ",trimws(cdn)," ")))
	    
	  }, cdns,l1,SIMPLIFY = TRUE,USE.NAMES = FALSE)
	  
	  # returns a list with all durations corresponding to EBCs found and store in LS above
	  dur<-as.numeric(mapply(function(cdn,x,y){
	    return(ifelse(as.logical(sum(which(x>0))),trimws(substr(cdn,(x[which(x>0)]+nchar(names(x)[which(x>0)])),y[which(y>0)]-1)),0))
	    
	  }, cdns,l1,l2,SIMPLIFY = TRUE,USE.NAMES = FALSE))
	  
	  # populates a list with units of durations found above. If duration value not specified for any EBC, default unit of mins is used.
	  dur_unit<-sapply(l2,function(x)return(ifelse(as.logical(sum(which(x>0))),names(x)[which(x>0)],"mins")),simplify=TRUE)
	  
	  # populates a list with operators corresponding to durations specified for EBCs. Sets "NA" if no duration is specified.
	  dur_op<-sapply(l1,function(x)return(ifelse(as.logical(sum(which(x>0))),names(x)[which(x>0)],"NA")))
	  
	  return(list("LS"=LS,"dur"=dur,"dur_unit"=dur_unit,"dur_op"=dur_op))
	}
	
	#########################################################
	# Function evtDetection applies event definition to dataset using output of deconstructEvt function
	
	# Arguments: 
	# 1. cdn_bool - list of boolean vectors representing result of applying EBC on dataset which form part of event definition
	# 2. dur - list of duration of EBCs from event definition
	# 3. dur_unit - list of unit of durations of EBCs from event definition
	# 4. dur_op - list of operator specified for EBCs in event definition
	
	# Return value: list with events found
	###########################################################
	
	evtDetection<-function(cdns_bool,dur,dur_unit,dur_op){
	  # removes NAs if present
	  for (i in 1:length(cdns_bool)){
	    cdns_bool[[i]][is.na(cdns_bool[[i]])]<-FALSE
	  }
	  
	  #Collects all intervals where each EBC is true. Stores start and end index of each such interval for all EBCs
	  
	  # creates list storing index from dataset for start location of EBC being true
	  cdn_st<-lapply(cdns_bool,function(x)return(which(diff(as.numeric(x))==1)))
	  
	  # creates list storing index from dataset for end location of EBC being true
	  cdn_end<-lapply(cdns_bool,function(x)return(which(diff(as.numeric(x))==-1)))
	  
	  # checks whether EBCs returned some records or not
	  cdn_check<-sapply(cdn_st,function(x)return(length(x)))
	  
	  evt<-data.frame()
	  evtn<-data.frame()
	  evt_dur<-NULL
	  naEvt<-NULL
	  evtBound<-NULL
	  
	  if (length(which(cdn_check==0))>0){
	    txt<-paste0(which(cdn_check==0)) #EBC not returning any record
	    showNotification(paste0("Event condition ",txt, " returned no result. Please check"),duration=2)
	    # evt<-NULL
	    # evtn<-NULL
	  }else{
	    
	    if (input$time_seq==FALSE){ # common time step option on GUI unchecked
	      for (i in 1:length(cdn_st[[1]])){ # iterates through all start indices found for first EBC in event definition
	        st<-cdn_st[[1]][i]  # ith start index of first EBC being true
	        # en<-cdn_end[[1]][i]
	        en<-cdn_end[[1]][min(which(cdn_end[[1]]>st))]-1 #
	        
	        #stores duration for the interval for first EBC in event definition
	        subDur<-difftime(PROJECT_DATA_VXX$MY_DATE[en],PROJECT_DATA_VXX$MY_DATE[st],units=dur_unit[1])
	        
	        if (subDur>0){
	          subEvt<-st
	        }else{
	          subEvt<-0
	        }
	        
	        
	        if ((length(cdns_bool)>1)){ #more than one EBC in event definition
	          
	          for (j in 2:length(cdns_bool)){
	            stj<-which((cdn_st[[j]]>=st)&(cdn_st[[j]]<=en)) #finds start index of EBC within interval of first EBC
	            enj<-which((cdn_end[[j]]>=st)&(cdn_end[[j]]<=en)) #finds end index of EBC within interval of first EBC
	            if (length(stj)>0){
	              enDate<-cdn_end[[j]][enj]
	              stDate<-cdn_st[[j]][stj]
	              if (length(stj)-length(enj)==1){ 
	                enDate<-c(enDate,en)
	              }else if (length(stj)-length(enj)==-1){
	                stDate<-c(st,stDate)
	              }
	              # if (length(stDate)==length(enDate)){
	              t<-mapply(difftime,PROJECT_DATA_VXX$MY_DATE[enDate],PROJECT_DATA_VXX$MY_DATE[stDate], MoreArgs = list(units=dur_unit[j]),SIMPLIFY = TRUE)  
	              
	              subDur<-c(subDur,max(t))
	              # subEvt<-c(subEvt,cdn_st[[j]][stj[which(t==max(t))]])
	              subEvt<-c(subEvt,cdn_st[[j]][stj[1]])
	            }else{
	              t=0
	              subDur<-c(subDur,0)
	              subEvt<-c(subEvt,0)
	            }
	          }  
	        }
	        
	        # nacheck<-vapply(cdnV,function(x){
	        #   return(length(which(is.na(PROJECT_DATA_VXX[[x]][st:en]))))
	        # },FUN.VALUE = c("NA Entries"=0))
	        
	        #checks if any NAs are still present in the interval. 
	        nacheck<-sapply(cdns_bool,function(x){
	          return(length(which(is.na(x[st:en]))))
	        })
	        naEvt<-rbind(naEvt,nacheck)
	        
	        
	        if (length(which(subEvt!=0))==length(cdns_bool)){ #checks if all EBCs in the event definition are satisfied
	          checkDur<-mapply(function(x,y,z){ # checks if interval durations for all EBCs satisfy event definition durations
	            result<-TRUE
	            if(y!="NA"){
	              expr<-parse(text=paste0(x,y,z))[[1]]
	              result<-eval(expr)
	            }
	            return(result)},subDur,dur_op,dur)
	          
	          if (length(which(checkDur))==length(cdns_bool)){
	            evt<-rbind(evt,c(1,st,(en),subEvt)) #saves interval as detected event
	          } else{
	            evt<-rbind(evt,c(0,st,(en),subEvt)) #saves interval as failed event
	          } 
	          
	        }else{
	          evt<-rbind(evt,c(0,st,(en),subEvt))
	          # evtn<-rbind(evtn,subEvt)
	        }
	        evt_dur<-rbind(evt_dur,subDur)
	      }  
	    }else if (input$time_seq==TRUE){
	      
	      for (i in 1:length(cdn_st[[1]])){
	        st<-cdn_st[[1]][i]
	        en<-cdn_end[[1]][min(which(cdn_end[[1]]>st))]-1
	        
	        bool_vector<-cdns_bool[[1]][(st+1):(en+1)]
	        
	        if ((length(cdns_bool)>1)){
	          
	          for (j in 2:length(cdns_bool)){
	            bool_vector_j<-cdns_bool[[j]][(st+1):(en+1)]
	            bool_vector<-bool_vector & bool_vector_j
	          }
	        }
	        na_length<-length(which(is.na(bool_vector)))
	        bool_vector[is.na(bool_vector)]<-FALSE
	        if (length(which(bool_vector))>0){
	          transition<-diff(c(0,as.numeric(bool_vector),0))
	          st_transition<-st+which(transition==1)-1
	          en_transition<-st-1+which(transition==-1)-1
	          for (k in 1:length(st_transition)){
	            
	            l<-lapply(seq(1:length(cdns_bool)),function(x,dur_unit,en_transition,st_transition,k){
	              td<-difftime(PROJECT_DATA_VXX$MY_DATE[en_transition[k]],PROJECT_DATA_VXX$MY_DATE[st_transition[k]], units=dur_unit[x])
	              return(td)
	            },dur_unit,en_transition,st_transition,k)
	            
	            t<-unlist(l)
	            # td<-difftime(PROJECT_DATA_VXX$MY_DATE[en_transition[k]],PROJECT_DATA_VXX$MY_DATE[st_transition[k]], units=dur_unit[x])  
	            checkDur<-mapply(function(x,y,z){
	              result<-TRUE
	              if(y!="NA"){
	                expr<-parse(text=paste0(x,y,z))[[1]]
	                result<-eval(expr)
	              }
	              return(result)},t,dur_op,dur)
	            if(length(which(checkDur))==length(cdns_bool)){
	              if (st_transition[k]<en_transition[k]){
	                evt<-rbind(evt,c(1,st_transition[k],en_transition[k],rep(st_transition[k],length(cdns_bool))))
	              }
	              
	            }else{
	              evt<-rbind(evt,c(0,st_transition[k],en_transition[k],rep(st_transition[k],length(cdns_bool))))
	              
	            }
	            evt_dur<-rbind(evt_dur,t)  
	            
	            nacheck<-sapply(cdns_bool,function(x){
	              return(length(which(is.na(x[st_transition[k]:en_transition[k]]))))
	            })
	            naEvt<-rbind(naEvt,nacheck)
	          }  
	        }
	      }  
	    }
	    
	  }
	  
	  
	  return(list("evt"=evt,"evtn"=evtn,"evt_dur"=evt_dur,"naEvt"=naEvt))
	}
	
	appEvt<-NULL                                           # variable to store applied event definition
	appEvtName<-NULL                                       # variable to store name of applied event eg. EVT1
	evtOutput<-NULL                                        # list to store output of applied event
	PROJECT_DATA_VXX_EVENTS<-NULL
	dataFilterEvents<-NULL                                 # vector of booleans (TRUE/FALSE) representing result of event detection
	
	evt_status<-reactiveValues(val=NULL)
	output$event_outcome<-renderText({
	  evt_status$val
	})
	# applies selected event definition to dataset
  observeEvent(input$evt_app,{
    if (input$load_data!=2){
      showNotification("Incorrect source selection. All Data option required.",duration=2)
      return()
    }
    if (input$evts=="No Selection"){
      showNotification("No Event selected.",duration=2)
      return()
    }
    id<<-showNotification("Applying Event Detection..",duration=NULL)
    evtInput<-deconstructEvt(input$evts)
    LS<-evtInput[["LS"]]
    dur<-evtInput[["dur"]]
    dur_unit<-evtInput[["dur_unit"]]
    dur_op<-evtInput[["dur_op"]]
    
    # v<-mapply(function(x)return(substr(gregexpr("V",x,fixed=TRUE))),LS,c("<","<=",">",">=","==")
    
    LS<-lapply(LS,function(x,allLS){
      if(grepl("LS",x,fixed=TRUE)){
        if (trimws(x)=="LS1") x=paste0(trimws(x),"(Default)")
        i<-which(names(allLS)==trimws(x))
        return(paste0(" ",allLS[[i]]," "))
      }else{
        return(x)
      }
    },allLS)
              
    cdns<-cdnAllV
    cdns[["No Selection"]]<-NULL
    cdns[["SelectAll"]]<-NULL
    cdnVector<-mapply(function(x,y)return(paste0(y," = (",x,")")),cdns,names(cdns),SIMPLIFY = TRUE,USE.NAMES = FALSE)
    # cdns_bool<-lapply(LS,function(x)return(EquationBasedDataFilter(EBCs=cdnVector,x,PROJECT_DATA=PROJECT_DATA_VXX)))
    cdns_bool<-lapply(LS,function(x)return(c(FALSE,EquationBasedDataFilter(EBCs=cdnVector,x,PROJECT_DATA=PROJECT_DATA_VXX),FALSE)))
    evtCalc<-evtDetection(cdns_bool,dur,dur_unit,dur_op)
    appEvt<<-input$evts
    appEvtName<<-names(allEvts)[which(unlist(allEvts)==input$evts)]
    evtOutput<<-evtCalc[["evt"]]
    
    
    # PROJECT_DATA_VXX_EVENTS<<-PROJECT_DATA_VXX[data_filterEvents,]
    if (dim(evtOutput)[1]>0){
      
      evt<-which(evtOutput[,1]==1)
      success_evt_count<-length(evt)
      failed_evt_count<-length(which(evtOutput[,1]==0))
      all_evt_count<-dim(evtOutput)[1]
      
      evt_status$val<-paste0('Applied Event = ',appEvtName, "\n",
                             'All Events Count = ',all_evt_count, "\n", 
                             'Successful Events Count = ', success_evt_count, "\n",
                             'Failed Events Count = ', failed_evt_count)
      if (success_evt_count>0){
        dataFilterEvents<<-rep("FALSE",dim(PROJECT_DATA_VXX)[1])
        index_val<-unlist(lapply(evt,function(x)return(seq(evtOutput[x,2],(evtOutput[x,3]),1))))
        dataFilterEvents[index_val]<<-"TRUE"
        dataFilterEvents<<-as.logical(dataFilterEvents)
        if (!is.null(id)){
          removeNotification(id)
        }
        showNotification("Events found. Plots can be viewed in Time Series.",duration=2)
        val<-input$load_data
        l<-list()
        l[["Last Analysed Output"]]<-1
        l[["New Analysis-All Data"]]<-2
        l[[paste0("Event Data - ",appEvtName)]]<-3
        updateRadioButtons(session,"load_data",choices=l,selected=val)  
      }else if((failed_evt_count>0)&(failed_evt_count==all_evt_count)){
        if (!is.null(id)){
          removeNotification(id)
        }
        showNotification('Only failed events found.',duration=2)
        return()
      }
      
    }else{
      if (!is.null(id)){
        removeNotification(id)
      }
      showNotification("No Events found. Check event definition.",duration=2)
    }
    
  })
  
  #Export Event Data to TXT
  observeEvent(input$evt_list,ignoreInit = TRUE,{
    if (is.null(evtOutput)){
      showNotification('Event not applied.',duration=2)
      return()
    }
    if (input$evt_txt_name==""){
      showNotification('Event Name empty',duration =2)
      return()
    }
    evt_txt<-NULL
    evt_data_txt<-NULL
    evt_index<-NULL
    found_evt<-NULL
    evtdata_type<-NULL
    if(dim(evtOutput)[1]>0){
      if (input$list_evt_sel=="Successful Events"){
        evt_index<-which(evtOutput[,1]==1)
        if (length(evt_index)>0){
          found_evt<-evtOutput[evt_index,]  
        }else{
          showNotification('No successful events found',duration=2)
          return()
        }
        evtdata_type<-"Successful_Evts"
      }else if(input$list_evt_sel=="Failed Events"){
        evt_index<-which(evtOutput[,1]==0)
        if (length(evt_index)>0){
          found_evt<-evtOutput[evt_index,]  
        }else{
          showNotification('No failed events found',duration=2)
          return()
        }
        evtdata_type<-"Failed_Evts"
      }else if(input$list_evt_sel=="All Events"){
        evt_index<-seq(1,dim(evtOutput)[1],1)
        found_evt<-evtOutput
        evtdata_type<-"All_Evts"
      }
      id<-NULL
      id<-showNotification('Creating Event Files..',duration=NULL)
      
      evt_seq<-paste0("Event_",seq(1,length(evt_index),1))
      evt_type_col<-rep('Successful',dim(evtOutput)[1])
      evt_st<-as.character(format(PROJECT_DATA_VXX$MY_DATE[found_evt[,2]],tz="UTC",format= "%Y-%m-%d %H:%M:%S"))
      evt_en<-as.character(format(PROJECT_DATA_VXX$MY_DATE[found_evt[,3]],tz="UTC",format= "%Y-%m-%d %H:%M:%S"))
      
      if (evtdata_type=='All_Evts'){
        s_evts<-which(evtOutput[,1]==1)
        f_evts<-which(evtOutput[,1]==0)
        evt_type_col[f_evts]<-'Failed'
        evt_txt<-data.frame(Event_Number = evt_seq,Event_Start = evt_st,Event_End = evt_en, Event_Type=evt_type_col,
                            stringsAsFactors = FALSE)
      }else{
        evt_txt<-data.frame(Event_Number = evt_seq,Event_Start = evt_st,Event_End = evt_en, stringsAsFactors = FALSE)  
      }
      
      if (input$inc_sig_data_evt){
        for (i in 1:length(evt_index)){
          # print(i)
          evt_data_subset<-PROJECT_DATA_VXX[found_evt[i,2]:found_evt[i,3],]
          evt_data_subset$MY_DATE<-as.character(format(evt_data_subset$MY_DATE,tz="UTC",format= "%Y-%m-%d %H:%M:%S"))
          # evt_data_subset<-cbind(evt_data_subset$MY_DATE,evt_data_subset)
          evt_seq<-rep(paste0("Event_",i),times=dim(evt_data_subset)[1])
          evt_data_subset<-cbind(evt_seq,evt_data_subset)
          evt_data_txt<-rbind(evt_data_txt,evt_data_subset)
        }
      }
      
      username<-Sys.getenv('username')
      evt_dir_path<-paste0(out_path,username,"/",input$sel1,"/",input$sel2,"/Event_Detection")
      if (!dir.exists(evt_dir_path)){
        dir.create(evt_dir_path)
      }
      evt_name<-trimws(input$evt_txt_name)
      evt_name<-gsub(" ","_",evt_name,fixed=TRUE)
      evt_data_file<-paste0(evt_dir_path,"/",evt_name,"_",evtdata_type,"_data.txt")
      evt_txt_file<-paste0(evt_dir_path,"/",evt_name,"_",evtdata_type,".txt")
      if (!is.null(evt_txt)){
        write.table(evt_txt,evt_txt_file,row.names = FALSE)
      }
      if (!is.null(evt_data_txt)){
        evt_data_txt<-data.table(evt_data_txt)
        colnames(evt_data_txt)<-c("Event No",INPUT_VECTOR_VXX_Title_Type_Text,"Timestamp")
        fwrite(evt_data_txt,evt_data_file,sep = ",",quote = FALSE)
      }
      if(!is.null(id)){
        removeNotification(id)
      }
      showNotification('Event files created.',duration=2)
    }
  })
  
	}
	
	# Time Series
	{
	  #updates event data on time series GUI
	observeEvent(input$ts_data,{
	  if (input$ts_data==3){ #3 is for Event data
	    updateSelectInput(session,"ts_event",choices = allEvts,selected="No Selection")
	  }
	})
	
	evtCnt<-NULL
	
  pltEvt<-NULL
	observeEvent(input$ts_event,{
	  req(input$ts_event)
	  if (input$ts_event!="No Selection"){
	    if (is.null(appEvt)){
	      showNotification("Event not applied.",duration=2)
	      return()
	    }
	    # if (!is.null(pltEvt)){
	    #   evtname<-names(which(allEvts==input$ts_event))
	    #   if (pltEvt==evtname)
	    #     return()
	    # }
	    # if (input$ts_event!=appEvt){
	    #   showNotification("Event data different from selected event.",duration=2)
	    # }else 
	    if (appEvt==input$ts_event){
	      evt<-evtOutput
	      evtCheck<-colnames(evt)[1]
	      evtSt<-colnames(evt)[2]
	      evtEnd<-colnames(evt)[3]
	      evtCnt<<-length(which(evt[,evtCheck]==1))
	      if (evtCnt>0){
	        # updateCheckboxGroupInput(session,"ts_signal",choiceNames = lapply(INPUT_VECTOR_VXX,function(x)return(x)),
	        #                          choiceValues = lapply(INPUT_VECTOR_VXX,function(x)return(x)),selected=INPUT_VECTOR_VXX)
	        updateSliderInput(session,"ts_event_step",min=0,max=1,value=0,step=1)
	        updateSliderInput(session,"ts_event_step",min=1,max=evtCnt,value=1,step=1)
	        
	      }
	    }  
	  }else if (input$ts_event=="No Selection"){
	    updateSliderInput(session,"ts_event_step",min=0,max=1,value=0,step=1)
	  }
	})
	
	output$ts_event_datetime<-renderText({
	  req(input$ts_event_step)
	  req(input$ts_event)
	  if (input$ts_event=="No Selection")return()
	  if (input$ts_event!=appEvt){
	    showNotification("Event data different from selected event.",duration=2)
	    return()
	  }
	  if ((input$ts_event_step==0)|(input$ts_event=="No Selection"))return()
	  if (evtCnt>0){
	    evt<-evtOutput[which(evtOutput[,1]==1),]
	    txt<-paste0("Start: ",PROJECT_DATA_VXX$MY_DATE[evt[input$ts_event_step,2]],"\n",
	                "End: ",PROJECT_DATA_VXX$MY_DATE[evt[input$ts_event_step,3]], "\n",
	                "Duration: ", difftime(PROJECT_DATA_VXX$MY_DATE[evt[input$ts_event_step,3]],
	                                      PROJECT_DATA_VXX$MY_DATE[evt[input$ts_event_step,2]],
	                                      units="mins")," mins")
	  }
	  
	})
	
	output$cdn_evt_disp<-renderText({
	  txt<-NULL
	  if (input$show_evt_cdn){
	    if (input$ts_event!="No Selection"){
	      if (is.null(showCdn$val)){
	        cdnDisp()
	      }
	      txt<-paste0(showCdn$val,"\n","Selected Event:\n",input$ts_event)  
	    }else{
	      showNotification("Event not selected.",duration=2)
	    }
	  }
	  txt
	})
	
	dataTimeSeries<-NULL
	meanDataFull<-NULL
	
	# dataTS<-reactive({
	dataTS<-function(signalTS){
	  
	  
	  if (is.null(dataTimeSeries)){
	    PROJECT_DATA_VXX_COPY<-copy(PROJECT_DATA_VXX)
	    
	    dataTimeSeries<<-PROJECT_DATA_VXX_COPY %>% mutate(y = year(PROJECT_DATA_VXX_COPY$MY_DATE),m=month(PROJECT_DATA_VXX_COPY$MY_DATE),
	                                                     d=day(PROJECT_DATA_VXX_COPY$MY_DATE),h=hour(PROJECT_DATA_VXX_COPY$MY_DATE),
	                                                     # mi=minute(PROJECT_DATA_VXX_COPY$MY_DATE),s=second(PROJECT_DATA_VXX_COPY$MY_DATE))
	                                                     mi=minute(PROJECT_DATA_VXX_COPY$MY_DATE))
	  } 
    
	  i<-which(INPUT_VECTOR_VXX_Title_Type_Text==signalTS)
	  v<-INPUT_VECTOR_VXX[i]
	  lvl<-NULL
	  if ((is.null(rangexts$x))&(is.null(rangexts$y))){
	    if (is.null(meanDataFull)){
	      meanDataFull<<-dataTimeSeries %>% dplyr::group_by(y,m,d)%>%dplyr::summarize(vmean=mean(!!sym(v),na.rm=TRUE)) %>% 
	        dplyr::mutate(dd=parse_date_time(paste(as.character(y),as.character(m),as.character(d),sep="-"),"ymd"))  
	    }
	    meanData<-meanDataFull
	    lvl<-"Day"
	  }else{
	    startDate<-as.POSIXct(rangexts$x[1],origin = "1970-01-01 00:00.00", tz = "UTC")
	    endDate<-as.POSIXct(rangexts$x[2],origin = "1970-01-01 00:00.00", tz = "UTC")
	    td<-difftime(endDate,startDate,units = "hours")
	    if (td>8760){ #day level
	      meanData<-dataTimeSeries %>% dplyr::filter(MY_DATE>=startDate,MY_DATE<=endDate) %>%
	        dplyr::group_by(y,m,d)%>%dplyr::summarize(vmean=mean(!!sym(v),na.rm=TRUE)) %>%
	        dplyr::mutate(dd=parse_date_time(paste(as.character(y),as.character(m),as.character(d),
	                                               sep="-"),"ymd"))
	      # meanData<-dataTimeSeries %>% dplyr::group_by(y,m,d)%>%dplyr::summarize(vmean=mean(!!sym(v),na.rm=TRUE)) %>% 
	      #   dplyr::mutate(dd=parse_date_time(paste(as.character(y),as.character(m),as.character(d),
	      #                                          sep="-"),"ymd"))
	      lvl<-"Day"
	    }
	    else if ((td>3000)&(td<=8760)){#hour level
	      meanData<-dataTimeSeries %>% dplyr::filter(MY_DATE>=startDate,MY_DATE<=endDate) %>%
	        dplyr::group_by(y,m,d,h)%>%dplyr::summarize(vmean=mean(!!sym(v),na.rm=TRUE)) %>%
	        dplyr::mutate(dd=parse_date_time(paste(as.character(y),as.character(m),as.character(d),as.character(h),
	                                               sep="-"),"ymdH"))
	      # meanData<-dataTimeSeries %>% dplyr::group_by(y,m,d,h)%>%dplyr::summarize(vmean=mean(!!sym(v),na.rm=TRUE)) %>% 
	      #   dplyr::mutate(dd=parse_date_time(paste(as.character(y),as.character(m),as.character(d),as.character(h),
	      #                                          sep="-"),"ymdH"))
	      lvl<-"Hour"
	    }else if((td>500)&(td<=3000)){#minute level
	      # meanData<-dataTimeSeries %>% dplyr::group_by(y,m,d,h,mi)%>%dplyr::summarize(vmean=mean(!!sym(v),na.rm=TRUE)) %>% 
	      #   dplyr::mutate(dd=parse_date_time(paste(as.character(y),as.character(m),as.character(d),as.character(h),
	      #                                          as.character(mi),sep="-"),"ymdHM"))
	      meanData<-dataTimeSeries %>% dplyr::filter(MY_DATE>=startDate,MY_DATE<=endDate) %>%
	        dplyr::group_by(y,m,d,h,mi)%>%dplyr::summarize(vmean=mean(!!sym(v),na.rm=TRUE)) %>%
	        dplyr::mutate(dd=parse_date_time(paste(as.character(y),as.character(m),as.character(d),as.character(h),
	                                               as.character(mi),sep="-"),"ymdHM"))
	      lvl<-"Min"
	    }else if(td<=500){#second level
	      meanData<-dataTimeSeries %>% dplyr::filter(MY_DATE>=startDate,MY_DATE<=endDate) %>%
	                dplyr::select(vmean=!!sym(v),dd=MY_DATE)
	        # dplyr::group_by(y,m,d,h,mi,s)%>%dplyr::summarize(vmean=mean(!!sym(v),na.rm=TRUE)) %>%
	        # dplyr::mutate(dd=parse_date_time(paste(as.character(y),as.character(m),as.character(d),as.character(h),
	        #                                        as.character(mi),as.character(s),sep="-"),"ymdHMS"))
	      # meanData<-dataTimeSeries %>% dplyr::group_by(y,m,d,h,mi,s)%>%dplyr::summarize(vmean=mean(!!sym(v),na.rm=TRUE)) %>% 
	      #   dplyr::mutate(dd=parse_date_time(paste(as.character(y),as.character(m),as.character(d),as.character(h),
	      #                                          as.character(mi),as.character(s),sep="-"),"ymdHMS"))
	      lvl<-"Sec"
	    }
	  }
	  list("data"=meanData,"level"=lvl)
	}
	
	observeEvent(input$plot_ts_alldata_dblclick, {
	  brush <- input$plot_ts_alldata_brush
	  if (!is.null(brush)) {
	    rangexts$x <- c(brush$xmin, brush$xmax)
	    rangexts$y <- c(brush$ymin, brush$ymax)
	  }else {
	    rangexts$x <- NULL
	    rangexts$y <- NULL
	  }
	  
	})
	
	observeEvent(input$dateRange,{
	  if (length(inserted_TSall)>0){
	    for (i in 1:length(inserted_TSall)){
	      ranges1d_time_seriesx[[inserted_TSall[i]]]<-NULL
	      ranges1d_time_seriesy[[inserted_TSall[i]]]<-NULL
	    } 
	  }
	})
	
	inserted_TSall<-c()
	inserted_TSevt<-c()
	ranges1d_time_seriesx<-reactiveValues()
	ranges1d_time_seriesy<-reactiveValues()
	time_series_plts<-reactiveValues()
	PROJECT_DATA_VXX_COPY<-NULL
	startDate<-NULL
	endDate<-NULL
	observeEvent(input$time_series_add, {
	  if (input$ts_data ==1){#no selection
	    showNotification("Please select data source for time series.",duration=2)
	    return()
	  }
	  if (input$load_data==1){
	    showNotification("Plant data not loaded.",duration=2)
	    return()
	  }
	  req(input$time_series_sig)
	  plts<-0
	  if (input$time_series_sig=="No Selection"){
	    showNotification("Please select signal for plot.",duration=2)
	    return()
	  }
	  if (input$time_series_sig!="No Selection"){
	    found<-FALSE
	    if (input$ts_data==2){#all data
	      if (length(inserted_TSall[grepl(input$time_series_sig,inserted_TSall, fixed=TRUE)])==1){
	        found<-TRUE
	      }
	    }else if (input$ts_data==3){#event data
	      if (length(inserted_TSevt[grepl(input$time_series_sig,inserted_TSevt, fixed=TRUE)])==1){
	        found<-TRUE
	      }
	    }
	    if (found){
	      showNotification("Signal plot already added",type="message",duration=3)
	    }else{
	      
	      idn<-paste0(input$time_series_sig,"_",input$ts_data)
	      sig<-input$time_series_sig
	      if (input$ts_data==2){
	        selector='#placeholder1_TS_alldata'  
	      }else if (input$ts_data==3){
	        selector='#placeholder1_TS_evtdata'
	      }else if (input$ts_data=="No Selection"){
	        showNotification("Time series data source not selected.",duration=2)
	        return()
	      }
	      
	      insertUI(
	        selector = selector, where="afterEnd",
	        ui = plotOutput(paste0("plot1d_time_series_",idn),click=paste0("plot1d_time_series_click_",idn),
	                        dblclick = paste0("plot1d_time_series_dblclick_",idn),
	                        brush = brushOpts(id = paste0("plot1d_time_series_brush_",idn),resetOnNew = TRUE),height="200px")
	      )
	      ranges1d_time_seriesx[[idn]] <- NULL
	      ranges1d_time_seriesy[[idn]] <- NULL
	      
	      if (is.null(PROJECT_DATA_VXX_COPY)){
	        PROJECT_DATA_VXX_COPY<-copy(PROJECT_DATA_VXX)  
	        id<<-showNotification("Preparing Data for Time Series...",duration=NULL)
	      }
	      
	      
	      if (is.null(dataTimeSeries)){
	        
	        dataTimeSeries<<-PROJECT_DATA_VXX_COPY %>% mutate(y = year(PROJECT_DATA_VXX_COPY$MY_DATE),m=month(PROJECT_DATA_VXX_COPY$MY_DATE),
	                                                          d=day(PROJECT_DATA_VXX_COPY$MY_DATE),h=hour(PROJECT_DATA_VXX_COPY$MY_DATE),
	                                                          # mi=minute(PROJECT_DATA_VXX_COPY$MY_DATE),s=second(PROJECT_DATA_VXX_COPY$MY_DATE))
	                                                          mi=minute(PROJECT_DATA_VXX_COPY$MY_DATE))
	      }
	      if (!is.null(id)){
	        removeNotification(id)
	        id<<-NULL
	      }
	      
	      output[[paste0("plot1d_time_series_",idn)]]<-renderPlot({
	        dataSourceType<-isolate(input$ts_data)
	        time_series_plts[[idn]]<-ggplot()
	        
	        # PROJECT_DATA_VXX_COPY<-copy(PROJECT_DATA_VXX)
	        # 
	        # if (is.null(dataTimeSeries)){
	        #   
	        #   dataTimeSeries<<-PROJECT_DATA_VXX_COPY %>% mutate(y = year(PROJECT_DATA_VXX_COPY$MY_DATE),m=month(PROJECT_DATA_VXX_COPY$MY_DATE),
	        #                                                     d=day(PROJECT_DATA_VXX_COPY$MY_DATE),h=hour(PROJECT_DATA_VXX_COPY$MY_DATE),
	        #                                                     # mi=minute(PROJECT_DATA_VXX_COPY$MY_DATE),s=second(PROJECT_DATA_VXX_COPY$MY_DATE))
	        #                                                     mi=minute(PROJECT_DATA_VXX_COPY$MY_DATE))
	        # }
	        
	        i<-which(INPUT_VECTOR_VXX_Title_Type_Text==sig)
	        v<-INPUT_VECTOR_VXX[i]
	        lvl<-NULL
	        ang<-0
	        xlab<-"Time"
	        td<-NULL
	        if ((is.null(ranges1d_time_seriesx[[idn]]))&(is.null(ranges1d_time_seriesy[[idn]]))){
	          if (dataSourceType==2){#All Data
	            startDate<-as.POSIXct(input$dateRange[1],origin = "1970-01-01 00:00.00", tz = "UTC")
	            endDate<-as.POSIXct(input$dateRange[2],origin = "1970-01-01 00:00.00", tz = "UTC")
	            td=difftime(endDate,startDate,units="hours")  
	          } else if (dataSourceType==3){#Event Data
	            req(input$ts_event_step)
	            if(is.null(evtOutput)){
	              showNotification("No events detected. Check event definition.",duration=2)
	              return()
	            }
	            if (input$ts_event_step==0)return()
	            if ((input$ts_event!=appEvt)|(input$ts_event=="No Selection")){
	              # showNotification("Selected event different from Event Data.",duration=2)
	              return()
	            }
	            pltEvt<<-appEvtName
	            evt<-evtOutput[which(evtOutput[,1]==1),]
	            stEvt<-evt[input$ts_event_step,2]
	            endEvt<-evt[input$ts_event_step,3]
	            # data<-PROJECT_DATA_VXX_COPY[stEvt:endEvt,.(MY_DATE,!!sym(v))]
	            startDate<<-PROJECT_DATA_VXX_COPY$MY_DATE[stEvt]
	            endDate<<-PROJECT_DATA_VXX_COPY$MY_DATE[endEvt]
	            
	            td<-difftime(endDate,startDate,units = "hours")
	            # meanData<-PROJECT_DATA_VXX_COPY %>% dplyr::filter(MY_DATE>=startDate,MY_DATE<=endDate) %>%
	            #   dplyr::select(vmean=!!sym(v),dd=MY_DATE)
	            
	          }
	          
	          # if (is.null(meanDataFull)){
	          #   meanDataFull<<-dataTimeSeries %>% dplyr::group_by(y,m,d)%>%dplyr::summarize(vmean=mean(!!sym(v),na.rm=TRUE)) %>% 
	          #     dplyr::mutate(dd=parse_date_time(paste(as.character(y),as.character(m),as.character(d),sep="-"),"ymd"))  
	          # # }
	          # meanData<-meanDataFull
	          # lvl<-"Day"
	        }else{
	          startDate<-as.POSIXct(ranges1d_time_seriesx[[idn]][1],origin = "1970-01-01 00:00.00", tz = "UTC")
	          endDate<-as.POSIXct(ranges1d_time_seriesx[[idn]][2],origin = "1970-01-01 00:00.00", tz = "UTC")
	          td<-difftime(endDate,startDate,units = "hours")
	        }
          
	        if (is.null(td))return()
	        if (is.null(dataTimeSeries))return()
	        
	        if (td>8760){ #day level
            meanData<-dataTimeSeries %>% dplyr::filter(MY_DATE>=startDate,MY_DATE<=endDate) %>%
              dplyr::group_by(y,m,d)%>%dplyr::summarize(vmean=mean(!!sym(v),na.rm=TRUE)) %>%
              dplyr::mutate(dd=parse_date_time(paste(as.character(y),as.character(m),as.character(d),
                                                     sep="-"),"ymd"))
            # meanData<-dataTimeSeries %>% dplyr::group_by(y,m,d)%>%dplyr::summarize(vmean=mean(!!sym(v),na.rm=TRUE)) %>% 
            #   dplyr::mutate(dd=parse_date_time(paste(as.character(y),as.character(m),as.character(d),
            #                                          sep="-"),"ymd"))
            lvl<-"Day"
            # ft<-"%Y-%m-%d"
            ft<-"%Y" 
            br<-"1 year"
            xlab <- paste0(xlab," Resolution (Days)")
            # xlab <- paste0(xlab," Resolution (Minutes) Scale = ", td, " hours" )
          }
          else if ((td>1500)&(td<=8760)){#hour level
            meanData<-dataTimeSeries %>% dplyr::filter(MY_DATE>=startDate,MY_DATE<=endDate) %>%
              dplyr::group_by(y,m,d,h)%>%dplyr::summarize(vmean=mean(!!sym(v),na.rm=TRUE)) %>%
              dplyr::mutate(dd=parse_date_time(paste(as.character(y),as.character(m),as.character(d),as.character(h),
                                                     sep="-"),"ymdH"))
            # meanData<-dataTimeSeries %>% dplyr::group_by(y,m,d,h)%>%dplyr::summarize(vmean=mean(!!sym(v),na.rm=TRUE)) %>% 
            #   dplyr::mutate(dd=parse_date_time(paste(as.character(y),as.character(m),as.character(d),as.character(h),
            #                                          sep="-"),"ymdH"))
            lvl<-"Hour"
            # ft<-"%Y-%m-%d-%H"
            ft<-"%b-%Y"
            br<-"1 month"
            xlab <- paste0(xlab," Resolution (Hours)")
            # xlab <- paste0(xlab," Resolution (Hours) Scale = ", td, " hours" )
          }else if((td>500)&(td<=1500)){#minute level
            # meanData<-dataTimeSeries %>% dplyr::group_by(y,m,d,h,mi)%>%dplyr::summarize(vmean=mean(!!sym(v),na.rm=TRUE)) %>% 
            #   dplyr::mutate(dd=parse_date_time(paste(as.character(y),as.character(m),as.character(d),as.character(h),
            #                                          as.character(mi),sep="-"),"ymdHM"))
            meanData<-dataTimeSeries %>% dplyr::filter(MY_DATE>=startDate,MY_DATE<=endDate) %>%
              dplyr::group_by(y,m,d,h,mi)%>%dplyr::summarize(vmean=mean(!!sym(v),na.rm=TRUE)) %>%
              dplyr::mutate(dd=parse_date_time(paste(as.character(y),as.character(m),as.character(d),as.character(h),
                                                     as.character(mi),sep="-"),"ymdHM"))
            lvl<-"Min"
            # ft<-"%Y-%m-%d-%H-%M"
            ft<-"%d-%b %n %Y"
            br<-"5 days"
            xlab <- paste0(xlab," Resolution (Minutes)")
            # xlab <- paste0(xlab," Resolution (Minutes) Scale = ", td, " hours" )
          }else if(td<=500){#second level
            meanData<-dataTimeSeries %>% dplyr::filter(MY_DATE>=startDate,MY_DATE<=endDate) %>%
              dplyr::select(vmean=!!sym(v),dd=MY_DATE)
            # dplyr::group_by(y,m,d,h,mi,s)%>%dplyr::summarize(vmean=mean(!!sym(v),na.rm=TRUE)) %>%
            # dplyr::mutate(dd=parse_date_time(paste(as.character(y),as.character(m),as.character(d),as.character(h),
            #                                        as.character(mi),as.character(s),sep="-"),"ymdHMS"))
            # meanData<-dataTimeSeries %>% dplyr::group_by(y,m,d,h,mi,s)%>%dplyr::summarize(vmean=mean(!!sym(v),na.rm=TRUE)) %>% 
            #   dplyr::mutate(dd=parse_date_time(paste(as.character(y),as.character(m),as.character(d),as.character(h),
            #                                          as.character(mi),as.character(s),sep="-"),"ymdHMS"))
            lvl<-"Sec"
            # ft<-"%Y-%m-%d-%H-%M-%S"
            # if (td>24){
            #   # ft<-"%Y-%b-%d"
            #   ft<-"%Y %n %d-%b"
            #   br<-"1 days"  
            # }else 
            ft<-"%d-%b-%Y %n %H:%M:%S"
            br<-"1 min"
            if ((td/4)>=1){
              br<-paste0(ceiling(td/4)," hours")
              # xlab <- paste0(xlab," Resolution (Seconds) Scale = ", ceiling(td/4), " hours" )
            }else if ((td/4)<1){
              # ft<-"%d-%b-%Y %n %H:%M"
              if (60*td/4>=1){
                br<-paste0(ceiling(60*td/4)," mins")
                # xlab <- paste0(xlab," Resolution (Seconds) Scale = ", ceiling(60*td/4), " mins" )  
              }else if(60*td/4<1){
                br<-paste0(ceiling(60*60*td/4)," secs")
                # xlab <- paste0(xlab," Resolution (Seconds) Scale = ", ceiling(60*60*td/4), " mins" ) 
              }
              
            }
            
            # ang<-45
            xlab <- paste0(xlab," Resolution (Seconds)")
            
          }
	        # }
	        
	        req(meanData)
	        
	        timeseriesplt<-ggplot(data=meanData,aes(x=dd,y=vmean))+geom_path(na.rm = TRUE) +
	          #   # geom_point(col="indianred") + 
	          scale_x_datetime(date_labels = ft,date_breaks = br)+
	          xlab(xlab)+ylab(sig)+ggtitle(paste0(INPUT_Analysis_Title,": ",appEvtName),subtitle = paste0("Creation Date: ",Sys.Date()))+
	          theme(plot.title=element_text(size=15),axis.title.x = element_text(size=12),axis.title.y = element_text(size=12),
	                axis.text.x = element_text(size=10,angle=ang),axis.text.y = element_text(size=10))
	          
	        
	        # annotate("text",x=my_mean_minus_my_sigma,y=sigma_my_height, colour="red", label=paste("-",  round(my_sigma_of_PROJECT_DATA_VXX___FILTER_RESULT_V_CI,2),sep=""),size=4)+
	          
	        # +	    coord_cartesian(xlim=c(startDate,endDate))
	        
	        # rm(meanData)
	        # gc()
	        
	        time_series_plts[[idn]]<-timeseriesplt
	        
	        timeseriesplt
	        
	        
	      })
	      
	      insertUI(
	        selector = selector, where="afterEnd",
	        ## wrap element in a div with id for ease of removal
	        ui = tags$div(
	          tags$p(input$time_series_sig), 
	          id = idn
	        )
	      )
	      
	      if (input$ts_data==2){
	        inserted_TSall <<- c(inserted_TSall,idn)
	      }else if (input$ts_data==3){
	        inserted_TSevt <<- c(inserted_TSevt,idn)  
	      }
	      
	      
	      observeEvent(input[[paste0("plot1d_time_series_dblclick_",idn)]],{
	        brush <- input[[paste0("plot1d_time_series_brush_",idn)]]
	        if (input$ts_data==2){
	          plotsList<-inserted_TSall 
	        }else if (input$ts_data==3){
	          plotsList<-inserted_TSevt
	        }
	        if (!is.null(brush)) {
	          if (input$ts_data==3){
	            if (startDate>as.POSIXct(brush$xmin,origin = "1970-01-01 00:00.00", tz = "UTC"))brush$xmin<-as.numeric(startDate)
	            if(endDate<as.POSIXct(brush$xmax,origin = "1970-01-01 00:00.00", tz = "UTC"))brush$xmax<-as.numeric(endDate)
	          }
	          if (input$tszoom){
	            for (i in 1:length(plotsList)){
	              ranges1d_time_seriesx[[plotsList[i]]] <- c(brush$xmin, brush$xmax)
	              ranges1d_time_seriesy[[plotsList[i]]] <- c(brush$ymin, brush$ymax)  
	            }
	          }else{
	            ranges1d_time_seriesx[[idn]] <- c(brush$xmin, brush$xmax)
	            ranges1d_time_seriesy[[idn]] <- c(brush$ymin, brush$ymax)  
	          }
	          
	        }else {
	          if (input$tszoom){
	            for (i in 1:length(plotsList)){
	              ranges1d_time_seriesx[[plotsList[i]]] <- NULL
	              ranges1d_time_seriesy[[plotsList[i]]] <- NULL  
	            }
	          }else{
	            ranges1d_time_seriesx[[idn]] <- NULL
	            ranges1d_time_seriesy[[idn]] <- NULL  
	          }
	          
	        }
	      })
	      
	    }
	  }
	})
	
	observeEvent(input$time_series_rem,ignoreInit = TRUE,{
	  if (input$time_series_sig=="No Selection"){
	    showNotification("Time series plot not selected.",duration=2)
	    return()
	  }else{
	    idn<-paste0(input$time_series_sig,"_",input$ts_data)
	    ranges1d_time_seriesx[[idn]] <- NULL
	    ranges1d_time_seriesy[[idn]] <- NULL 
	  }
	  
	  if (input$ts_data==2){
	    if (length(inserted_TSall)>0){
	      
	      i<-which(inserted_TSall==idn)
	      selector = paste0('#', inserted_TSall[i])
	      removeUI(selector=selector)  
	      selector = paste0('#plot1d_time_series_', inserted_TSall[i])
	      removeUI(selector=selector)
	      # inserted_TSall <<- inserted_TSall[-length(inserted_TSall)]
	      inserted_TSall<<-inserted_TSall[!inserted_TSall==idn]
	      time_series_plts[[idn]]<-NULL
	      
	    }else{
	      showNotification("Time series plot not added.",duration=2)
	      return()
	    }
	  }else if(input$ts_data==3){
	    if (length(inserted_TSevt)>0){
	      i<-which(inserted_TSevt==idn)
	      selector = paste0('#', inserted_TSevt[i])
	      removeUI(selector=selector)  
	      selector = paste0('#plot1d_time_series_', inserted_TSevt[i])
	      removeUI(selector=selector)
	      # inserted_TSevt <<- inserted_TSevt[-length(inserted_TSevt)]
	      inserted_TSevt<<-inserted_TSevt[!inserted_TSevt==idn]
	      time_series_plts[[idn]]<-NULL
	    }else{
	      showNotification("Time series plot not added.",duration=2)
	      return()
	    }
	  }
	  
	})
	
	output$plot_ts_alldata<-renderPlot({
	  dataVal<-dataTS()
	  data<-dataVal[[1]]
	  lvl<-dataVal[[2]]
	  req(data)
	  if (lvl=="Day")ft<-"%Y-%m-%d"
	  if (lvl=="Hour")ft<-"%Y-%m-%d-%H"
	  if (lvl=="Min")ft<-"%Y-%m-%d-%H-%M"
	  if (lvl=="Sec")ft<-"%Y-%m-%d-%H-%M-%S"
	  # if (!is.null(rangexts$x)){
	  #   startDate<-as.POSIXct(rangexts$x[1],origin = "1970-01-01 00:00.00", tz = "UTC")
	  #   endDate<-as.POSIXct(rangexts$x[2],origin = "1970-01-01 00:00.00", tz = "UTC")  
	  # }else{
	  #   startDate<-data$dd[1]
	  #   endDate<-data$dd[dim(data)[1]]
	  # }
	  
	  
	  ggplot(data=data,aes(x=dd,y=vmean))+geom_path(na.rm = TRUE) +
	  #   # geom_point(col="indianred") + 
	    scale_x_datetime(date_labels = ft)
	  # +	    coord_cartesian(xlim=c(startDate,endDate))
	})
	
	
	output$plot1d_time_series_d<-downloadHandler(
	  filename = function(){
  	  if (input$ts_data==3){
  	    paste0(input$sel1,"_",input$sel2,"_TIME_SERIES_PLOTS_EVENT_DATA.pdf")
  	  }else if(input$ts_data==2){
  	    paste0(input$sel1,"_",input$sel2,"_TIME_SERIES_PLOTS_ALL_DATA.pdf")
  	  }
	    
	  },
	  content=function(file){
	    
	    pdf(file=file,width=15, height=7)
	    if (input$ts_data==3){
	      for (i in 1:length(inserted_TSevt)){
	        # time_series_plts[[inserted_TSevt[i]]]<-time_series_plts[[inserted_TSevt[i]]]
	        plt<-time_series_plts[[inserted_TSevt[i]]]
	        # plt<-plt+annotate("text", -Inf, Inf, label = paste0("Creation Date: ",Sys.Date()), hjust = 0, vjust = 1)
	        plot(plt)
	      }  
	    }else if (input$ts_data==2){
	      for (i in 1:length(inserted_TSall)){
	        # time_series_plts[[inserted_TSall[i]]]<-time_series_plts[[inserted_TSall[i]]]
	        plt<-time_series_plts[[inserted_TSall[i]]]
	        # plt<-plt+annotate("text", -Inf, Inf, label = paste0("Creation Date: ",Sys.Date()), hjust = 0, vjust = 1)
	        # d<-paste0("Creation Date: ",as.POSIXct(Sys.Date(), format = "%Y-%m-%d"))
	        # plt<-plt + annotate("text", -Inf, Inf, label = d, hjust = 0, vjust = 1)
	        plot(plt)
	        
	      }  
	    }
	    dev.off()
	  }
	)
	}
	
	##########################################
	#1D Frequency Sensitivity
	##########################################
	{
	  inserted <- c()
	  freq_sens_plts<-reactiveValues()
	  freq_steps<-1
	  freq_sens_upd<-"Yes"
	  freq_sens_dat<-list()
	  freq_sens_left_x<-list()
	  freq_sens_right_x<-list()
	  freq_sens_x_unit<-list()
	  freq_sens_title<-list()
	  # freq_sens_xmin<-reactiveValues()
	  # freq_sens_xmax<-reactiveValues()
	  freq_sens_brks03<-list()
	  ranges1d_freq_sensx<-reactiveValues()
	  ranges1d_freq_sensy<-reactiveValues()
	  freq_high_val<-NULL
	  freq_low_val<-NULL
	  PROJECT_DATA_VXX___FILTER_RESULT_RECOMPUTED<-NULL
	  PROJECT_DATA_VXX_COPY_FOR_DENSITY_ESTIMATE<-NULL
	  freq_sens_brks<-list()
	  freq_sens_xmin<-list()
	  freq_sens_xmax<-list()
	  freq_sens_ylim<-list()
	  
	  freqStepval<-NULL
	  boundMin<-NULL
	  boundMax<-NULL
	  freqSteps<-NULL
	  dataManualFreqsens<-NULL
	  datasplitFreqsens<-NULL
	  freqsensStepRange<-list()
	  
	  freqsensdataFilter<-reactiveValues(val=paste0("Data Source : FULL \n","Filtering Source : NONE"))
	  
	  sensStep<-function(){
	    i<-which(INPUT_VECTOR_VXX_Title_Type_Text==isolate(input$sig1))
	    # absMin = min(abs(diff(PROJECT_DATA_VXX[[i]])),na.rm=TRUE)
	    # absMax = max(abs(diff(PROJECT_DATA_VXX[[i]])),na.rm=TRUE)
	    boundMin <<- INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE[i]
	    boundMax <<- INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE[i]
	    
	    freqStepval <<- ((boundMax - boundMin)/input$freq_interval)
	    freqSteps<<-input$freq_interval
	    
	    updateSliderInput(session,"freq_sens_step",value=1,min=1,max=freqSteps,step=1)
	    # updateNumericInput(session,"freq_interval",value=INPUT_NUMBER_OF_INTERVALLS_FOR_1D_SENSITIVITY_ANALYSIS)
	   
	    # low_val<-boundMin
	    # high_val<-low_val+freqStepval
	    # updateNumericInput(session,"freq_sens_low", value=low_val)
	    # updateNumericInput(session,"freq_sens_high", value=high_val)
	    
	  }
	  
	  prepFreqsensdata<-reactive({
	    
	    # filterSelection<-ifelse(!is.null(isolate(input$extent)),isolate(input$extent),0)
	    filterSelection<-ifelse(!is.null((input$extent)),(input$extent),0)
	    signal1<-isolate(input$sig1)
	    sourceSelection<-isolate(input$load_data)
	    freqInt<-isolate(input$freq_interval)
	    # manual<-isolate(input$freqsens_manual)
	    
	    low_val<-isolate(input$freq_sens_low)
	    high_val<-isolate(input$freq_sens_high)
	    
	    if (plt_freq_sens$y=="No")return()
	    
	    if (!isolate(checkExtent())){
	      freqsensdataFilter$val<-NULL
	      # showNotification("Filter not defined.",duration=2)
	      return()  
	    }
	    
	    data<-NULL
	    
	    if (freqInt<=0){
	      showNotification("Incorrect number of intervals.",duration=2)
	    }else if (length(filterSelection)>1){
	      showNotification("Incorrect Data Filter Selection.",duration=2)
	    }else if (signal1=="No Selection"){
	      showNotification("Please select Signal 1",duration=2)
	    }else if (((!is.na(low_val))&(is.na(high_val)))|((is.na(low_val))&(!is.na(high_val)))){
	      showNotification("Both Low/High values required for manual range setting.",duration=2)
	    }else if ((sourceSelection=="2")&&(signal1!="No Selection")){
	      freqsensdataFilter$val<-NULL
	      id<<-showNotification("Preparing Data for Plots.....",duration=NULL)
	      
	      i<-which(INPUT_VECTOR_VXX_Title_Type_Text==signal1)
	      boundMin <<- INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE[i]
	      boundMax <<- INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE[i]
	      
	      # if (freqsensUpdsrc$val!="Manual"){
	      if ((is.na(low_val))&(is.na(high_val))){
	        freqStepval <<- ((boundMax - boundMin)/freqInt)
	        freqSteps<<-freqInt
	        datasplitFreqsens<<-NULL
	        if (is.null(datasplitFreqsens)){
	          updateSliderInput(session,"freq_sens_step",value=1,min=1,max=freqSteps,step=1)
	          freq_low_val<<-boundMin
	          freq_high_val<<-freq_low_val+freqStepval
	          # freqIntrange$low<-freq_low_val
	          # freqIntrange$high<-freq_high_val
	        }else{
	          freq_low_val<<-boundMin+(isolate(input$freq_sens_step)-1)*freqStepval
	          freq_high_val<<-freq_low_val+freqStepval
	        }
	        # 
	        
	        # updateNumericInput(session,"freq_sens_low", value=low_val)
	        # updateNumericInput(session,"freq_sens_high", value=high_val)  
	      }else if ((!is.na(low_val))&(!is.na(high_val))){
	        
	        if (high_val<=low_val){
	          showNotification("Signal High should be greater than Signal Low.",duration=2,type="warning")
	          return()
	        }else if ((low_val<boundMin)|(high_val>boundMax)){
	          showNotification("Signal High/Low not in range.",duration=2,type="warning")
	          return()
	        }else{
	          dataManualFreqsens<<-NULL
	          freq_high_val<<-high_val
	          freq_low_val<<-low_val
	        }
	      }
	      
  	    i<-which(INPUT_VECTOR_VXX_Title_Type_Text==signal1)
  	    filterEventResult<-dataFilterEventApply(filterSelection,dataFilter1D,dataFilter2D,
  	                                            dataFilternD,i,0,PROJECT_DATA_VXX,
  	                                            dataFilterEvents=NULL,appEvtName=NULL)#callerfilter arg =default
  	    
  	    PROJECT_DATA_VXX_COPY<-filterEventResult[[1]]
  	    val<-filterEventResult[[2]]
  	    freqsensdataFilter$val<-val
  	    
  	    PROJECT_DATA_VXX_COPY_FOR_DENSITY_ESTIMATE<<-PROJECT_DATA_VXX_COPY
  	    
  	    for (j in 1:length(INPUT_VECTOR_VXX_Title_Type_Text)){
  	      idn<-INPUT_VECTOR_VXX_Title_Type_Text[j]
  	      freq_sens_left_x[[idn]]<<-INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE[j]
  	      freq_sens_right_x[[idn]]<<-INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE[j]
  	      freq_sens_title[[idn]]<<-INPUT_VECTOR_VXX_Title_Type_Underscored_TEXT[j]
  	      freq_sens_x_unit[[idn]]<<-INPUT_VECTOR_UNIT[j]
  	      freq_sens_brks03[[idn]]<<-INPUT_VECTOR_breaks_03[j]
  	    
    	    PROJECT_DATA_VXX_TEMP<-copy(PROJECT_DATA_VXX_COPY[[j]])
    	    PROJECT_DATA_VXX_TEMP<-PROJECT_DATA_VXX_TEMP[!is.na(PROJECT_DATA_VXX_TEMP)]
    	    ii<-(PROJECT_DATA_VXX_TEMP>=INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE[j])&(PROJECT_DATA_VXX_TEMP<=INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE[j])
    	    PROJECT_DATA_VXX_TEMP<-PROJECT_DATA_VXX_TEMP[ii]
    	    hist_br<-hist(PROJECT_DATA_VXX_TEMP,breaks=freq_sens_brks03[[idn]],plot=FALSE)
    	    
    	    freq_sens_brks[[idn]]<<-hist_br$breaks
    	    freq_sens_ylim[[idn]]<<-c(0,max(hist_br$counts)*1.01)
  	    }
  	    
  	    if ((is.na(low_val))&(is.na(high_val))){ #manual high low values not specified
  	      if (is.null(datasplitFreqsens)){ #data for sensitivity plot not present
  	        freqsensStepRange<-lapply(seq_len(freqInt),function(x,freqStepval,boundMin){
  	          return(c(boundMin+(x-1)*freqStepval,boundMin+x*freqStepval))
  	        },freqStepval,boundMin)
  	        
  	        data<-lapply(freqsensStepRange,function(x,i){
  	          return(PROJECT_DATA_VXX_COPY[(PROJECT_DATA_VXX_COPY[[i]]>=x[1])&(PROJECT_DATA_VXX_COPY[[i]]<x[2]),])
  	        },i)
  	        
  	      }else{
  	        data<-datasplitFreqsens
  	      }
  	    }else if ((!is.na(low_val))&(!is.na(high_val))){ #manual high low values specified
  	      
  	      data<-PROJECT_DATA_VXX_COPY[(PROJECT_DATA_VXX_COPY[[i]]>=freq_low_val)&(PROJECT_DATA_VXX_COPY[[i]]<freq_high_val),]
  	    }
	    }
	    
	    return(data)
	    
	  })
	  
	  output$freqsenslow_txt<-renderText({
	    # if (isolate(input$freq_sens_step)==0)return()
	    if (input$freq_sens_step==0)return()
	    # req(freqIntrange$low)
	    # freqIntrange$low
	    # req(freqsensdataFilter$val)
	    req(freq_low_val)
	    round(freq_low_val,2)
	    
	  })
	  
	  output$freqsenshigh_txt<-renderText({
      # if (isolate(input$freq_sens_step)==0)return()
	    if (input$freq_sens_step==0)return()
	    # req(freqIntrange$high)
	    # freqIntrange$high
	    # req(freqsensdataFilter$val)
	    req(freq_high_val)
	    round(freq_high_val,2)
	    
	  })
	  
	  output$freqsensDatasource<-renderText({
	    
	    low_val<-isolate(input$freq_sens_low)
	    high_val<-isolate(input$freq_sens_high)
	    
      if ((is.na(low_val))&(is.na(high_val))){
        datasplitFreqsens<<-prepFreqsensdata()
      }else if ((!is.na(low_val))&(!is.na(high_val))){
        dataManualFreqsens<<-prepFreqsensdata()
      }
	    
	    if((!is.null(id))&(!is.null(datasplitFreqsens))){
	      removeNotification(id)
	      id<<-NULL
	      showNotification("Data ready for plot.",duration=2)
	    }
	    isolate(freqsensdataFilter$val)
	  })
	  
	  calculate_prob_1d_density<-function(brush,density1D){
	    
	    denCurveX<-density1D$x[(density1D$x>=brush$xmin)&(density1D$x<=brush$xmax)]
	    denCurveY<-density1D$y[(density1D$x>=brush$xmin)&(density1D$x<=brush$xmax)]
	    # 
	    # if (max(denCurveY)>brush$ymax){
	    #   showNotification("Selected region does not enclose density curve. Please check.",duration=2)
	    #   return()
	    # }else 
	      
	     if(brush$ymin>0){
	      showNotification("Selected region does not enclose X axis. Please check.",duration=2)
	      return()
	    }
	    
	    if (length(denCurveX)>0){
	      denCurveX<-c(denCurveX[1],denCurveX,denCurveX[length(denCurveX)])
	      denCurveY<-c(0,denCurveY,0)
	      
	      xl1<-denCurveX
	      yl1<-denCurveY
	      sml1<-0
	      for (j in 1:(length(xl1)-1)){
	        al<-0.5*((yl1[j+1]-yl1[j])*xl1[j]-(xl1[j+1]-xl1[j])*yl1[j])
	        sml1<-sml1+ al
	      }
	      
	      sml1<-abs(sml1)
	      # sml1<-round(abs(sml1),5)
	    }else{
	      sml1<-0
	    }
	   return(sml1)
	  }
	  
	  
	  #################
	  # observeEvent(input$freq_add) - function adds plots for sensitivity analysis.
	  #################
	  observeEvent(input$freq_add, {
	    req(input$freq_sens)
	    plts<-0
	    if (input$freq_sens=="No Selection"){
	      showNotification("Please select signal for plot.",duration=2)
	      return()
	    }
	    if (input$freq_sens!="No Selection"){
	      if (length(inserted[grepl(input$freq_sens,inserted, fixed=TRUE)])==1){
	        showNotification("Signal plot already added",type="message",duration=3)
	      }else{
	        idn<-(input$freq_sens)
	        
	        cn<-which(INPUT_VECTOR_VXX_Title_Type_Text==idn)
	        if ((length(inserted)%%2)==0){
	          selector='#placeholder1'
	        }else{
	          selector='#placeholder2'
	        }
	        
	        insertUI(
	          selector = selector, where="afterEnd",
	          ui = div(style = "position:relative",id = paste0("plot_",idn),
	          # ui = tags$div(
	                   plotOutput(paste0("plot1d_freq_sens_",idn),click=paste0("plot1d_freq_sens_click_",idn),
	                          dblclick = paste0("plot1d_freq_sens_dblclick_",idn),
	                          brush = brushOpts(id = paste0("plot1d_freq_sens_brush_",idn),resetOnNew = TRUE),
	                          height="400px"),
	                   uiOutput(paste0("prob_",idn)))
	        )
	        ranges1d_freq_sensx[[idn]] <- NULL
	        ranges1d_freq_sensy[[idn]] <- NULL
	        
	        freq_sens_dat[[idn]]<-NULL
	        
	        output[[paste0("plot1d_freq_sens_",idn)]]<-renderPlot({
	          
	          low_val<-isolate(input$freq_sens_low)
	          high_val<-isolate(input$freq_sens_high)
	          
	          # high_val<-freqIntrange$high
	          freqstep<-(input$freq_sens_step)
	          if (freqstep==0)return()
	          freq_sens_plts[[idn]]<-ggplot()
	          # req(freqIntrange$low)
	          req(freqsensdataFilter$val)
	          i<-which(INPUT_VECTOR_VXX_Title_Type_Text==idn)
	          if ((is.na(low_val))&(is.na(high_val))){ #manual high/low setting not present
	            req(datasplitFreqsens)
	            if (freqstep!=0) {
	              freq_sens_dat[[idn]]<<-datasplitFreqsens[[freqstep]][[i]]  
	            }
	          }else if ((!is.na(low_val))&(!is.na(high_val))){#manual high/low setting present
	            req(dataManualFreqsens)
	            freq_sens_dat[[idn]]<<-dataManualFreqsens[[i]]
	          }
	          
	          if (!is.null(freq_sens_dat[[idn]])){
	            
	            j<-which(INPUT_VECTOR_VXX_Title_Type_Text==idn)
	            
	            PROJECT_DATA_VXX___FILTER_RESULT_V_CI <- freq_sens_dat[[idn]]
	         
	            PROJECT_DATA_VXX___FILTER_RESULT_V_CI<-PROJECT_DATA_VXX___FILTER_RESULT_V_CI[!is.na(PROJECT_DATA_VXX___FILTER_RESULT_V_CI)]
	            PROJECT_DATA_VXX___FILTER_RESULT_V_CI<-PROJECT_DATA_VXX___FILTER_RESULT_V_CI[PROJECT_DATA_VXX___FILTER_RESULT_V_CI>=freq_sens_left_x[[idn]]]
	            PROJECT_DATA_VXX___FILTER_RESULT_V_CI<-PROJECT_DATA_VXX___FILTER_RESULT_V_CI[PROJECT_DATA_VXX___FILTER_RESULT_V_CI<=freq_sens_right_x[[idn]]]
	            
	            if (length(PROJECT_DATA_VXX___FILTER_RESULT_V_CI)>0){
	              my_xlab_03 = paste(round(freq_sens_left_x[[idn]],2), " <= " , freq_sens_title[[idn]], " <= ",
	                                 round(freq_sens_right_x[[idn]],2),"\n breaks = ", freq_sens_brks03[[isolate(input$sig1)]],  sep="")
	               
	              my_xlab = my_xlab_03
	              # my_main = paste(freq_sens_title[[idn]] , "\n" ,
	              #                 freq_sens_x_unit[[idn]] ,
	              #                 sep="")
	              
	              if (!is.null(freq_high_val)){
	                my_main<-paste0(INPUT_Analysis_Title, "\n",round(freq_low_val,2)," <= ",isolate(input$sig1)," < ",round(freq_high_val,2))
	              }
	              # else{
	              #   my_main = paste(freq_sens_title[[idn]] , "\n" ,freq_sens_x_unit[[idn]] ,sep="")
	              # }
	              
	              
	              # min_PROJECT_DATA_VXX_l <- freq_sens_xmin[[idn]]
	              # max_PROJECT_DATA_VXX_l <- freq_sens_xmax[[idn]]
	              # my_xlim = c(min_PROJECT_DATA_VXX_l,max_PROJECT_DATA_VXX_l)
	              
	              my_freq<-TRUE
	              
	              brks<-freq_sens_brks[[idn]]
	              
	              my_ylim<-freq_sens_ylim[[idn]]
	              
	              hist1 <- hist(PROJECT_DATA_VXX___FILTER_RESULT_V_CI,
	                            # freq=my_freq,
	                            breaks = brks,
	                            # breaks = freq_sens_brks03[[isolate(input$sig1)]],
	                            plot=FALSE)
	              
	              my_mean_of_PROJECT_DATA_VXX___FILTER_RESULT_V_CI =  mean(PROJECT_DATA_VXX___FILTER_RESULT_V_CI)
	              
	              my_Hist1_T_F <- hist1$breaks >= my_mean_of_PROJECT_DATA_VXX___FILTER_RESULT_V_CI
	              my_min_index = min(which(my_Hist1_T_F %in% TRUE))
	              
	              # my_min_index = min(which((hist1$mids >= my_mean_of_PROJECT_DATA_VXX___FILTER_RESULT_V_CI)))
	              # my_min_index_br = min(which((hist1$breaks >= my_mean_of_PROJECT_DATA_VXX___FILTER_RESULT_V_CI)))
	              # my_min_index<-max(which(hist1$mids<hist1$breaks[my_min_index_br]))
	              
	              my_height = hist1$counts[my_min_index]
	              my_height_max = max(hist1$counts)
	              my_text = paste( "(",round(my_mean_of_PROJECT_DATA_VXX___FILTER_RESULT_V_CI, 2)," / " ,round(my_height,2),
	                               ")" ,
	                               sep="")
	              
	              my_var_of_PROJECT_DATA_VXX___FILTER_RESULT_V_CI =  var(PROJECT_DATA_VXX___FILTER_RESULT_V_CI)
	              my_sigma_of_PROJECT_DATA_VXX___FILTER_RESULT_V_CI = sqrt(my_var_of_PROJECT_DATA_VXX___FILTER_RESULT_V_CI)
	              my_mean_plus_my_sigma = (my_mean_of_PROJECT_DATA_VXX___FILTER_RESULT_V_CI + my_sigma_of_PROJECT_DATA_VXX___FILTER_RESULT_V_CI)
	              
	              
	              if(my_height == 0 | is.na(my_height) == TRUE){
	                sigma_my_height = my_height_max* 0.1
	              }else{
	                sigma_my_height = my_height * 0.25
	              }
	              
	              my_mean_minus_my_sigma =  (my_mean_of_PROJECT_DATA_VXX___FILTER_RESULT_V_CI - my_sigma_of_PROJECT_DATA_VXX___FILTER_RESULT_V_CI)
	              
	              # my_xlim = c(min(min_PROJECT_DATA_VXX_l,my_mean_minus_my_sigma),max(max_PROJECT_DATA_VXX_l,my_mean_plus_my_sigma))
	              # my_xlim = c(min(min_PROJECT_DATA_VXX_l,my_mean_minus_my_sigma),max(max_PROJECT_DATA_VXX_l,my_mean_plus_my_sigma))
	              data1dDen<-data.frame(x=hist1$mids,y=hist1$density,z=rep("Density",length(hist1$density)))
	              data1dFreq<-data.frame(x=hist1$mids,y=hist1$count,z=rep("Frequency",length(hist1$count)))
	              data1d<-rbind(data1dFreq,data1dDen)
	              
	              avg<-my_mean_of_PROJECT_DATA_VXX___FILTER_RESULT_V_CI
	              # data<-data.frame(x=hist1$mids,y=hist1$count)
	              pt<-data.frame(x1=my_mean_of_PROJECT_DATA_VXX___FILTER_RESULT_V_CI,y1=0,x2=my_mean_of_PROJECT_DATA_VXX___FILTER_RESULT_V_CI,y2=my_height,z="Frequency")
	              if (!is.na(my_mean_plus_my_sigma)){
	                pt1<-data.frame(x1=my_mean_of_PROJECT_DATA_VXX___FILTER_RESULT_V_CI,y1=0,x2=my_mean_plus_my_sigma,y2=0,z="Frequency")  
	              }
	              
	              if (!is.na(my_mean_minus_my_sigma)){
	                pt2<-data.frame(x1=my_mean_of_PROJECT_DATA_VXX___FILTER_RESULT_V_CI,y1=0,x2=my_mean_minus_my_sigma,y2=0,z="Frequency")
	              }
	              
	              if ((!is.na(my_mean_plus_my_sigma))&(!is.na(my_mean_minus_my_sigma))){
	                xt<-rbind(my_mean_plus_my_sigma,my_mean_minus_my_sigma)
	                yt<-rbind(0,0)
	                l<-rbind(paste("+", round(my_sigma_of_PROJECT_DATA_VXX___FILTER_RESULT_V_CI,2), sep=""),
	                         paste("-",  round(my_sigma_of_PROJECT_DATA_VXX___FILTER_RESULT_V_CI,2),sep=""))
	                txt<-data.frame(x=xt,y=yt,z=rep("Frequency",length(xt)),label=l)
	              }
	              
	              
	              gridsize<-500
	              dataDenEst<-PROJECT_DATA_VXX___FILTER_RESULT_V_CI
	              den_estimate<-NULL
	              if (length(unique(dataDenEst))>1){
	                den_estimate<-bkde(x=dataDenEst,gridsize=gridSize)  
	              }
	              
	              
	              
	              # denPlt<<- ggplot()+geom_col(data=data1dDen,aes(x=x, y=y),alpha=0.6)+
	              #   xlab(my_xlab1)+
	              #   ylab("DENSITY")+
	              #   ggtitle(my_main)+
	              #   # annotate("text",x=avg,y=my_height, colour="blue", label=my_text,size=5)+
	              #   theme(plot.title = element_text(size=20),axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
	              #         axis.text.x = element_text(size=12),axis.text.y = element_text(size=12),
	              #         strip.text.x = element_text(size=12, face="bold"))
	              
	              # freqPlt<<- ggplot()+geom_col(data=data1dFreq,aes(x=x, y=y),alpha=0.6)+
	              #   geom_segment(data=pt1,aes(x=x1,y=y1,xend=x2,yend=y2),colour="red", size=1)+
	              #   geom_text(data=data.frame(x=my_mean_plus_my_sigma,y=0,label=paste("+", round(stdev,2), sep="")),
	              #             aes(x=x,y=y,label=label),colour="red",size=3,vjust="right",hjust=0)+
	              #   geom_segment(data=pt2,aes(x=x1,y=y1,xend=x2,yend=y2),colour="red", size=1)+
	              #   geom_text(data=data.frame(x=my_mean_minus_my_sigma,y=0,label=paste("-",  round(stdev,2),sep="")),
	              #             aes(x=x,y=y,label=label),colour="red",size=3,vjust="right",hjust=1)+
	              #   geom_segment(data=pt,aes(x=x1,y=y1,xend=x2,yend=y2),colour="blue", size=1)+
	              #   
	              #   geom_text(data=data.frame(x=avg,y=my_height,label=my_text,z="Frequency"), aes(x=x,y=y,label=label),colour="blue",size=3,vjust="left")+
	              #   xlab(my_xlab1)+
	              #   ylab("FREQUENCY")+
	              #   ggtitle(my_main)+
	              #   # annotate("text",x=avg,y=my_height, colour="blue", label=my_text,size=5)+
	              #   theme(plot.title = element_text(size=20),axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
	              #         axis.text.x = element_text(size=12),axis.text.y = element_text(size=12),
	              #         strip.text.x = element_text(size=12, face="bold"))
	              
	              
	              freqdenPlt<- ggplot()+geom_col(data=data1d,aes(x=x, y=y),alpha=0.6)+
	                
	                geom_segment(data=pt,aes(x=x1,y=y1,xend=x2,yend=y2),colour="blue", size=1)+
	                geom_text(data=data.frame(x=avg,y=my_height,label=my_text,z="Frequency"), aes(x=x,y=y,label=label),colour="blue",size=3,vjust="left")+
	                xlab(my_xlab)+
	                ylab("DENSITY / FREQUENCY")+
	                ggtitle(my_main)+
	                # annotate("text",x=avg,y=my_height, colour="blue", label=my_text,size=5)+
	                theme(plot.title = element_text(size=15),axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
	                      axis.text.x = element_text(size=12),axis.text.y = element_text(size=12),
	                      strip.text.x = element_text(size=12, face="bold"))
	              # strip.text.y = element_text(size=12, face="bold"))
	              # facet_grid(z~.,scales="free_y")+
	              if ((!is.na(my_mean_plus_my_sigma))&(!is.na(my_mean_minus_my_sigma))){
	                freqdenPlt<-freqdenPlt+geom_segment(data=pt1,aes(x=x1,y=y1,xend=x2,yend=y2),colour="red", size=1)+
	                  geom_text(data=txt, aes(x=x,y=y,label=label),colour="red",size=3,vjust="right")+
	                  geom_segment(data=pt2,aes(x=x1,y=y1,xend=x2,yend=y2),colour="red", size=1)
	              }
	              
	              if (!is.null(den_estimate)){
	                # density1D<<-den_estimate  
	                freqdenPlt<-freqdenPlt+
	                  geom_line(data=data.frame(x=den_estimate$x,y=den_estimate$y,z=rep("Density",length(den_estimate$x))),
	                            aes(x=x,y=y),size=0.5)
	                # denPlt<<-denPlt+
	                #   geom_line(data=data.frame(x=den_estimate$x,y=den_estimate$y,z=rep("Density",length(den_estimate$x))),
	                #             aes(x=x,y=y),size=0.5)
	              }
	              
	              freqdenPlt<-freqdenPlt+facet_wrap(~z,ncol=1,scales="free_y")+
	              # freqdenPlt<-freqdenPlt+facet_wrap(~z,ncol=1)+ylim(my_ylim)+
	                coord_cartesian(xlim=ranges1d_freq_sensx[[idn]], ylim=ranges1d_freq_sensy[[idn]])
	              
	              # denPlt<<-denPlt+coord_cartesian(xlim=ranges1d_freq$x, ylim=ranges1d_freq$y)
	              # freqPlt<<-freqPlt+coord_cartesian(xlim=ranges1d_freq$x, ylim=ranges1d_freq$y)
	              
	              
	              # freq_sens_plts[[idn]]<-ggplot()+geom_col(data=data,aes(x=x, y=y))+
	              #   # ggplot()+geom_col(data=data,aes(x=x, y=y),width=0.9)+
	              #   geom_segment(data=pt,aes(x=x1,y=y1,xend=x2,yend=y2),colour="blue", size=0.5)+
	              #   annotate("text",x=my_mean_of_PROJECT_DATA_VXX___FILTER_RESULT_V_CI,y=my_height *1.01, colour="blue", label=my_text,size=4)+
	              #   # xlim(my_xlim)+
	              #   ylim(my_ylim)+
	              #   xlab(my_xlab)+ylab("FREQUENCY")+ggtitle(my_main)+
	              #   geom_segment(data=pt1,aes(x=x1,y=y1,xend=x2,yend=y2),colour="red", size=0.5)+
	              #   annotate("text",x=my_mean_plus_my_sigma,y=sigma_my_height, colour="red", label=paste("+", round(my_sigma_of_PROJECT_DATA_VXX___FILTER_RESULT_V_CI,2), sep=""),size=4)+
	              #   geom_segment(data=pt2,aes(x=x1,y=y1,xend=x2,yend=y2),colour="red", size=0.5)+
	              #   annotate("text",x=my_mean_minus_my_sigma,y=sigma_my_height, colour="red", label=paste("-",  round(my_sigma_of_PROJECT_DATA_VXX___FILTER_RESULT_V_CI,2),sep=""),size=4)+
	              #   
	              #   theme(plot.title=element_text(size=15),axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
	              #         axis.text.x = element_text(size=12),axis.text.y = element_text(size=12))+
	              #   coord_cartesian(xlim=ranges1d_freq_sensx[[idn]], ylim=ranges1d_freq_sensy[[idn]])
	              
	              freq_sens_plts[[idn]]<-freqdenPlt
	            }
	            
	            
	          }
	          # 	        else{
	          #              # ggplot()
	          #             freq_sens_plts[[idn]]<-ggplot()
	          #           }
	          
	          freq_sens_plts[[idn]]
	          # plts1d$freq_sens
	          
	        })
	        
	        output[[paste0("prob_",idn)]]<-renderUI({
	          
	          # low_val<-isolate(input$freq_sens_low)
	          # high_val<-isolate(input$freq_sens_high)
	          
	          freqstep<-isolate(input$freq_sens_step)
	          
	          brush <- input[[paste0("plot1d_freq_sens_brush_",idn)]]
	          if ((freqstep==0)|(is.null(freq_sens_dat[[idn]])))return()
	          if (is.null(brush)) return()
	          if (is.null(brush$panelvar1))return()
	          if (brush$panelvar1!="Density")return()
	          
	          
	          # req(freqsensdataFilter$val)
	          # i<-which(INPUT_VECTOR_VXX_Title_Type_Text==idn)
	          # if ((is.na(low_val))&(is.na(high_val))){ #manual high/low setting not present
	          #   req(datasplitFreqsens)
	          #   if (freqstep!=0) {
	          #     freq_sens_dat[[idn]]<-datasplitFreqsens[[freqstep]][[i]]  
	          #   }
	          # }else if ((!is.na(low_val))&(!is.na(high_val))){#manual high/low setting present
	          #   req(dataManualFreqsens)
	          #   freq_sens_dat[[idn]]<-dataManualFreqsens[[i]]
	          # }
	          
	          if (!is.null(freq_sens_dat[[idn]])){
	            
	            j<-which(INPUT_VECTOR_VXX_Title_Type_Text==idn)
	            
	            PROJECT_DATA_VXX___FILTER_RESULT_V_CI <- freq_sens_dat[[idn]]
	            PROJECT_DATA_VXX___FILTER_RESULT_V_CI<-PROJECT_DATA_VXX___FILTER_RESULT_V_CI[!is.na(PROJECT_DATA_VXX___FILTER_RESULT_V_CI)]
	            PROJECT_DATA_VXX___FILTER_RESULT_V_CI<-PROJECT_DATA_VXX___FILTER_RESULT_V_CI[!is.na(PROJECT_DATA_VXX___FILTER_RESULT_V_CI)]
	            PROJECT_DATA_VXX___FILTER_RESULT_V_CI<-PROJECT_DATA_VXX___FILTER_RESULT_V_CI[PROJECT_DATA_VXX___FILTER_RESULT_V_CI>=freq_sens_left_x[[idn]]]
	            PROJECT_DATA_VXX___FILTER_RESULT_V_CI<-PROJECT_DATA_VXX___FILTER_RESULT_V_CI[PROJECT_DATA_VXX___FILTER_RESULT_V_CI<=freq_sens_right_x[[idn]]]
	            
	            if (length(PROJECT_DATA_VXX___FILTER_RESULT_V_CI)>0){
	              gridsize<-500
	              dataDenEst<-PROJECT_DATA_VXX___FILTER_RESULT_V_CI
	              den_estimate<-NULL
	              if (length(unique(dataDenEst))>1){
	                den_estimate<-bkde(x=dataDenEst,gridsize=gridSize)  
	                prob_interval<-calculate_prob_1d_density(brush,den_estimate)
	              }
	              complete_data<-PROJECT_DATA_VXX_COPY_FOR_DENSITY_ESTIMATE[[j]]
	              complete_data<-complete_data[!is.na(complete_data)]
	              den_estimate_complete_data<-bkde(x=complete_data,gridsize=gridsize)
	              prob_complete_data<-calculate_prob_1d_density(brush,den_estimate_complete_data)
	              
	              if ((!is.null(prob_interval))&(!is.null(prob_complete_data))){
	                # calculate point position INSIDE the image as percent of total dimensions
	                # from left (horizontal) and from top (vertical)
	                
	                left_pct <- (brush$xmax - brush$domain$left) / (brush$domain$right - brush$domain$left)
	                top_pct <- (brush$domain$top - brush$ymin) / (brush$domain$top - brush$domain$bottom)
	                # 
	                # # calculate distance from left and bottom side of the picture in pixels
	                # 
	                left_px <- brush$range$left + left_pct * (brush$range$right - brush$range$left)
	                top_px <- brush$range$top + top_pct * (brush$range$bottom - brush$range$top)
	                
	                # left_px<-hover$xmax
	                # top_px<-hover$ymin
	                # create style property fot tooltip
	                # background color is set so tooltip is a bit transparent
	                # z-index is set so we are sure are tooltip will be on top
	                style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
	                                "left:", left_px + 2, "px; top:", top_px + 2, "px;")
	                
	                # actual tooltip created as wellPanel
	                wellPanel(
	                  style = style,
	                  p(HTML(paste0("<b> Probability with Interval Data: </b>",prob_interval , "<br/>",
	                                "<b> Probability with Total Data: </b>",prob_complete_data)))
	                )  
	              }
	            }
	          }
	        })
	              
	        
	        insertUI(
	          selector = selector, where="afterEnd",
	          ## wrap element in a div with id for ease of removal
	          ui = tags$div(
	            tags$p(input$freq_sens), 
	            id = idn
	          )
	        )
	        inserted <<- c(inserted,idn)  
	        
	        observeEvent(input[[paste0("plot1d_freq_sens_dblclick_",idn)]],{
	          brush <- input[[paste0("plot1d_freq_sens_brush_",idn)]]
	          if (!is.null(brush)) {
	            ranges1d_freq_sensx[[idn]] <- c(brush$xmin, brush$xmax)
	            ranges1d_freq_sensy[[idn]] <- c(brush$ymin, brush$ymax)
	          }else {
	            ranges1d_freq_sensx[[idn]] <- NULL
	            ranges1d_freq_sensy[[idn]] <- NULL
	          }
	        })
	        
	      }
	    }
	  })
	  
	  #################
	  # observeEvent(input$freq_rem) - function removes plots for sensitivity analysis.
	  #################
	  observeEvent(input$freq_rem,ignoreInit = TRUE,{
	    idn<-(input$freq_sens)
	    if (length(which(inserted==idn))>0){
	    # if (length(inserted)>0){
	      # selector = paste0('#', inserted[length(inserted)])
	      # removeUI(selector=selector)
	      selector = paste0('#', idn)
	      removeUI(selector=selector)
	      selector = paste0('#', paste0("plot_",idn))
	      removeUI(selector=selector)
	      inserted <<- inserted[!(inserted==idn)]
	      # selector = paste0('#plot1d_freq_sens_', inserted[length(inserted)])
	      # removeUI(selector=selector)
	      # selector = paste0('#prob_', inserted[length(inserted)])
	      # removeUI(selector=selector)
	      # inserted <<- inserted[-length(inserted)]
	    }else{
	      showNotification("Plot not found",duration=2)
	    }
	    
	  })
	  
	 
	  
	  #################
	  # observeEvent(input$freq_sens_step) - function updates signal interval value with slider
	  #################
	  freqIntrange<-reactiveValues(low=NULL,high=NULL)
	  observeEvent(input$freq_sens_step,{
	    
      if((!is.na(input$freq_sens_low))&(!is.na(input$freq_sens_high))){
        showNotification("Clear Low (Manual) & High (Manual) and click GENERATE PLOT before animation.",duration = 2)
      }
      if (input$freq_sens_step!=0){
        low_val<-boundMin+(input$freq_sens_step-1)*freqStepval
        high_val<-low_val+freqStepval
        freqIntrange$low<-low_val
        freqIntrange$high<-high_val
        # low_val<-freqsensStepRange[[input$freq_sens_step]][1]
        # high_val<-freqsensStepRange[[input$freq_sens_step]][2]
        freq_high_val<<-high_val
        freq_low_val<<-low_val
        
        
        # updateNumericInput(session,"freq_sens_low", value=low_val)
        # updateNumericInput(session,"freq_sens_high", value=high_val)
        
      }
      
	  })
  
	  
	  observeEvent(input$freq_interval,ignoreInit = TRUE,{
	    if (input$stat=="FREQUENCY_SENSITIVITY"){
	      if ((!is.na(input$freq_sens_low))|(!is.na(input$freq_sens_high))){
	        showNotification("Clear Low (Manual) & High (Manual) and click GENERATE PLOT",duration=2)
	        # return()
	      }else{
	        showNotification("Please click GENERATE PLOT update data",duration=2)   
	      }
	    }
	    updateSliderInput(session,"freq_sens_step",value=0,min=0,max=1,step=1)
	    datasplitFreqsens<<-NULL
	    dataManualFreqsens<<-NULL
	    freqsensdataFilter$val<-NULL
	    # freqIntrange$low<-NULL
	    # freqIntrange$high<-NULL
	    freq_high_val<<-NULL
	    freq_low_val<<-NULL
	    
	    
	    
	  })
	  
	  #################
	  # data1d_freq_sens() - function loads data for creating sensitivity plots in the backgroun using R-kernel
	  ###NOT USED ANYMORE##
	  #################
	  data1d_freq_sens<-reactive({
	    create_plot<-0
	    data<-NULL
	    ext_data<-data.frame("sig1"=c(input$sig1_low,input$sig1_high),"sig2"=c(input$sig2_low,input$sig2_high))
	    range_apply<-0
	    range_data<-NULL
	    
	    if (input$extent==TRUE){
	      range_apply<-1
	      range_data<-ext_data
	    }
	    if ((input$load_data=="1")&&(input$extent==TRUE)){
	      INPUT_keep_last_loaded_data_WITHOUT_DOMAIN_BASED_and_EVENT_BASED_conditions_or_reload_complete_data<-"reload_complete_data"
	      source(paste0(scripts_path,"/Load_Data.R")) 
	    }
	    if ((input$load_data == "2")||(input$extent==TRUE)){
	      if (input$sig1!="No Selection"){
	        #if (input$sig1!=input$sig2){
	        i<-which(INPUT_VECTOR_VXX_Title_Type_Text==input$sig1)
	        j<-which(INPUT_1D_STATISTICS_TYPE_VECTOR=="FREQUENCY_SENSITIVITY_ANALYSIS")
	        if (length(j)==0){
	          INPUT_1D_STATISTICS_TYPE_VECTOR=c(INPUT_1D_STATISTICS_TYPE_VECTOR,"FREQUENCY_SENSITIVITY_ANALYSIS")
	          j<-which(INPUT_1D_STATISTICS_TYPE_VECTOR=="FREQUENCY_SENSITIVITY_ANALYSIS")
	        }
	        
	        INPUT_VECTOR_GT_VALUE<-INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE 
	        INPUT_VECTOR_LT_VALUE<-INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE
	        INPUT_OVERWRITE_or_SKIP_if_file_exists<-"OVERWRITE"
	        
	        DensityFreqProbabSENSITIVITYUniversalFunction(j,i,
	                                                      INPUT_GENERATE_HISTOGRAMS_FOR_CONDITIONS_TOO_TRUE_OR_FALSE,
	                                                      INPUT_VECTOR_1D_STATISTICS_path,
	                                                      INPUT_1D_STATISTICS_TYPE_VECTOR,
	                                                      INPUT_VECTOR_histogram_jpeg_file_name_overall,
	                                                      INPUT_VECTOR_histogram_txt_file_name_overall,
	                                                      INPUT_Analysis_Title,
	                                                      INPUT_OVERWRITE_or_SKIP_if_file_exists,
	                                                      PROJECT_DATA_VXX,
	                                                      INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE,
	                                                      INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE,
	                                                      INPUT_VECTOR_VXX_Title_Type_Text,
	                                                      INPUT_VECTOR_1D_STATISTICS_TABLES_path
	        )
	        
	        # if(length(INPUT_VECTOR_VXX_Title_Type_Text[i]) == 1)
	        # {
	        #   my_xlab = paste(round(INPUT_VECTOR_GT_VALUE[i],2), " <= " , INPUT_VECTOR_VXX_Title_Type_Text[i], " <= ", round(INPUT_VECTOR_LT_VALUE[i],2), sep="")
	        # }
	        # else
	        # {
	        #   my_xlab = paste(round(INPUT_VECTOR_GT_VALUE[i],2), " <= " , 
	        #                   INPUT_VECTOR_VXX_Title_Type_Text[i], " <=", 
	        #                   round(INPUT_VECTOR_LT_VALUE[i],2), " ( X:=", INPUT_VECTOR_VXX_Title_Type_Text[i], " )" , sep="")
	        # }
	        # 
	        # cn<-colnames(XYARR)
	        # mylabECDF = paste("ECDF(", INPUT_VECTOR_VXX_Title_Type_Text[i], ") in %", sep="")
	        # mylabECDFmin = paste("1 - ECDF(", INPUT_VECTOR_VXX_Title_Type_Text[i], ") in %", sep="")
	        # my_ylab = ifelse(cn[2]=="ECDF",mylabECDF,mylabECDFmin)
	        # data<-list(XYARR,labs=c(my_xlab,my_ylab))
	        # 
	        create_plot<-1
	        #}
	      }
	    }
	    else if ((input$load_data == "1")&&(input$extent==FALSE)){
	      if (input$sig1!="No Selection"){
	        # XYARR<-data1d_ecdf_old()
	        # i<-which(INPUT_VECTOR_VXX_Title_Type_Text==input$sig1)
	        # j<-which(INPUT_1D_STATISTICS_TYPE_VECTOR=="ECDF")
	        # 
	        # INPUT_VECTOR_GT_VALUE<-INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE 
	        # INPUT_VECTOR_LT_VALUE<-INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE
	        # if(length(INPUT_VECTOR_VXX_Title_Type_Text[i]) == 1)
	        # {
	        #   my_xlab = paste(round(INPUT_VECTOR_GT_VALUE[i],2), " <= " , INPUT_VECTOR_VXX_Title_Type_Text[i], " <= ", round(INPUT_VECTOR_LT_VALUE[i],2), sep="")
	        # }
	        # else
	        # {
	        #   my_xlab = paste(round(INPUT_VECTOR_GT_VALUE[i],2), " <= " , 
	        #                   INPUT_VECTOR_VXX_Title_Type_Text[i], " <=", 
	        #                   round(INPUT_VECTOR_LT_VALUE[i],2), " ( X:=", INPUT_VECTOR_VXX_Title_Type_Text[i], " )" , sep="")
	        # }
	        # 
	        # cn<-colnames(XYARR)
	        # mylabECDF = paste("ECDF(", INPUT_VECTOR_VXX_Title_Type_Text[i], ") in %", sep="")
	        # mylabECDFmin = paste("1 - ECDF(", INPUT_VECTOR_VXX_Title_Type_Text[i], ") in %", sep="")
	        # my_ylab = ifelse(cn[2]=="ECDF",mylabECDF,mylabECDFmin)
	        # data<-list(XYARR,labs=c(my_xlab,my_ylab))
	        
	      }
	    }
	    data
	  })
	  
	  ################
	  # Function for downloading sensitivity plots.
	  ################
	  output$plot1d_freq_sens_d<-downloadHandler(
	    filename = function(){
	      paste0(input$sel1,"_",input$sel2,"_Frequency_Sensitivity_Low_",input$freq_sens_low,
	             "_High_",input$freq_sens_high,"_",input$sig1,".pdf")
	    },
	    content=function(file){
	      
	      pdf(file=file,width=15, height=7)
	      for (i in 1:length(inserted)){
	        plt<-freq_sens_plts[[inserted[i]]]
	        plt<-plt+annotate("text", Inf, Inf, label = freqsensdataFilter$val, hjust = 1, vjust = 1)
	        plt<-plt+annotate("text", -Inf, Inf, label = paste0("Creation Date: ",Sys.Date()), hjust = 0, vjust = 1)
	        plot(plt)
	      }
	      dev.off()
	    }
	  )
	}
	
	##########################################
	#1d ecdf sensitivity
	##########################################
	{
	  ecdf_inserted <- c()
	  ecdf_plts<-"Yes"
	  ranges1d_ecdf_sensx<-reactiveValues()
	  ranges1d_ecdf_sensy<-reactiveValues()
	  ecdf_sens_plts<-reactiveValues()
	  ecdf_steps<-1
	  ecdf_sens_upd<-"Yes"
	  ecdf_sens_dat<-list()
	  ecdf_sens_left_x<-list()
	  ecdf_sens_right_x<-list()
	  ecdf_sens_x_unit<-list()
	  ecdf_sens_title<-list()
	  ecdf_sens_brks03<-list()
	  
	  ecdf_sens_ylim<-list()
	  ecdf_sens_xlim<-list()
	  ecdf_sens_brks<-list()
	  ecdf_sens_xmin<-list()
	  ecdf_sens_xmax<-list()
	  # ecdf_sens_brks03<-reactiveValues()
	  ecdf_high_val<-NULL
	  ecdf_low_val<-NULL

	  ecdfStepval<-NULL
	  ecdfboundMin<-NULL
	  ecdfboundMax<-NULL
	  ecdfSteps<-NULL
	  dataManualecdfsens<-NULL
	  datasplitecdfsens<-NULL
	  ecdfsensStepRange<-list()
	  
	  ecdfsensdataFilter<-reactiveValues(val=paste0("Data Source : FULL \n","Filtering Source : NONE"))
	  
	  sensStep<-function(){
	    i<-which(INPUT_VECTOR_VXX_Title_Type_Text==isolate(input$sig1))
	    # absMin = min(abs(diff(PROJECT_DATA_VXX[[i]])),na.rm=TRUE)
	    # absMax = max(abs(diff(PROJECT_DATA_VXX[[i]])),na.rm=TRUE)
	    boundMin <<- INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE[i]
	    boundMax <<- INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE[i]
	    
	    freqStepval <<- ((boundMax - boundMin)/input$freq_interval)
	    freqSteps<<-input$freq_interval
	    
	    updateSliderInput(session,"freq_sens_step",value=1,min=1,max=freqSteps,step=1)
	    # updateNumericInput(session,"freq_interval",value=INPUT_NUMBER_OF_INTERVALLS_FOR_1D_SENSITIVITY_ANALYSIS)
	    
	    # low_val<-boundMin
	    # high_val<-low_val+freqStepval
	    # updateNumericInput(session,"freq_sens_low", value=low_val)
	    # updateNumericInput(session,"freq_sens_high", value=high_val)
	    
	  }
	  
	  prepecdfsensdata<-reactive({
	    if (plt_ecdf_sens$y=="No")return()
	    # filterSelection<-ifelse(!is.null(isolate(input$extent)),isolate(input$extent),0)
	    filterSelection<-ifelse(!is.null((input$extent)),(input$extent),0)
	    signal1<-isolate(input$sig1)
	    sourceSelection<-isolate(input$load_data)
	    ecdfInt<-isolate(input$ecdf_interval)
	    
	    low_val<-isolate(input$ecdf_sens_low)
	    high_val<-isolate(input$ecdf_sens_high)
	    
	    if (sourceSelection==1){
	      showNotification("'New Analysis - All Data' Option Required", duration = 2)
	      return()
	    }
	    
	    if (!isolate(checkExtent())){
	      ecdfsensdataFilter$val<-NULL
	      # showNotification("Filter not defined.",duration=2)
	      return()  
	    }
	    
	    data<-NULL
	    
	    if (ecdfInt<=0){
	      showNotification("Incorrect number of intervals.",duration=2)
	    }else if (length(filterSelection)>1){
	      showNotification("Incorrect Data Filter Selection.",duration=2)
	    }else if (signal1=="No Selection"){
	      showNotification("Please select Signal 1",duration=2)
	    }else if (((!is.na(low_val))&(is.na(high_val)))|((is.na(low_val))&(!is.na(high_val)))){
	      showNotification("Both Low/High values required for manual range setting.",duration=2)
	    }else if ((sourceSelection=="2")&&(signal1!="No Selection")){
	      ecdfsensdataFilter$val<-NULL
	      id<<-showNotification("Preparing Data for Plots.....",duration=NULL)
	      
	      i<-which(INPUT_VECTOR_VXX_Title_Type_Text==signal1)
	      ecdfboundMin <<- INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE[i]
	      ecdfboundMax <<- INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE[i]
	      
	      # if (freqsensUpdsrc$val!="Manual"){
	      if ((is.na(low_val))&(is.na(high_val))){
	        ecdfStepval <<- ((ecdfboundMax - ecdfboundMin)/ecdfInt)
	        ecdfSteps<<-ecdfInt
	        datasplitecdfsens<<-NULL
	        if (is.null(datasplitecdfsens)){
	          updateSliderInput(session,"ecdf_sens_step",value=1,min=1,max=ecdfSteps,step=1)
	          ecdf_low_val<<-ecdfboundMin
	          ecdf_high_val<<-ecdf_low_val+ecdfStepval
	          # freqIntrange$low<-freq_low_val
	          # freqIntrange$high<-freq_high_val
	        }else{
	          ecdf_low_val<<-ecdfboundMin+(isolate(input$ecdf_sens_step)-1)*ecdfStepval
	          ecdf_high_val<<-ecdf_low_val+ecdfStepval
	        }
	      }else if ((!is.na(low_val))&(!is.na(high_val))){
	        
	        if (high_val<=low_val){
	          showNotification("Signal High should be greater than Signal Low.",duration=2,type="warning")
	          return()
	        }else if ((low_val<ecdfboundMin)|(high_val>ecdfboundMax)){
	          showNotification("Signal High/Low not in range.",duration=2,type="warning")
	          return()
	        }else{
	          dataManualecdfsens<<-NULL
	          ecdf_high_val<<-high_val
	          ecdf_low_val<<-low_val
	        }
	      }
	      
	      i<-which(INPUT_VECTOR_VXX_Title_Type_Text==signal1)
	      filterEventResult<-dataFilterEventApply(filterSelection,dataFilter1D,dataFilter2D,
	                                              dataFilternD,i,0,PROJECT_DATA_VXX,
	                                              dataFilterEvents=NULL,appEvtName=NULL)#callerfilter arg =default
	      
	      PROJECT_DATA_VXX_COPY<-filterEventResult[[1]]
	      val<-filterEventResult[[2]]
	      ecdfsensdataFilter$val<-val
	      
	      for (j in 1:length(INPUT_VECTOR_VXX_Title_Type_Text)){
	        idn<-INPUT_VECTOR_VXX_Title_Type_Text[j]
	        ecdf_sens_left_x[[idn]]<<-INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE[j]
	        ecdf_sens_right_x[[idn]]<<-INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE[j]
	        ecdf_sens_title[[idn]]<<-INPUT_VECTOR_VXX_Title_Type_Underscored_TEXT[j]
	        ecdf_sens_x_unit[[idn]]<<-INPUT_VECTOR_UNIT[j]
	        ecdf_sens_brks03[[idn]]<<-INPUT_VECTOR_breaks_03[j]
	        
	        PROJECT_DATA_VXX_TEMP<-copy(PROJECT_DATA_VXX_COPY[[j]])
	        PROJECT_DATA_VXX_TEMP<-PROJECT_DATA_VXX_TEMP[!is.na(PROJECT_DATA_VXX_TEMP)]
	        ii<-(PROJECT_DATA_VXX_TEMP>=INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE[j])&(PROJECT_DATA_VXX_TEMP<=INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE[j])
	        PROJECT_DATA_VXX_TEMP<-PROJECT_DATA_VXX_TEMP[ii]
	        hist_br<-hist(PROJECT_DATA_VXX_TEMP,breaks=ecdf_sens_brks03[[idn]],plot=FALSE)
	        
	        ecdf_sens_brks[[idn]]<<-hist_br$breaks
	        ecdf_sens_ylim[[idn]]<<-c(0,max(hist_br$counts)*1.01)
	        ecdf_sens_xlim[[idn]]<<-c(min(PROJECT_DATA_VXX_TEMP),max(PROJECT_DATA_VXX_TEMP))
	      }
	      
	      if ((is.na(low_val))&(is.na(high_val))){ #manual high low values not specified
	        if (is.null(datasplitecdfsens)){ #data for sensitivity plot not present
	          ecdfsensStepRange<-lapply(seq_len(ecdfInt),function(x,ecdfStepval,ecdfboundMin){
	            return(c(ecdfboundMin+(x-1)*ecdfStepval,ecdfboundMin+x*ecdfStepval))
	          },ecdfStepval,ecdfboundMin)
	          
	          data<-lapply(ecdfsensStepRange,function(x,i){
	            return(PROJECT_DATA_VXX_COPY[(PROJECT_DATA_VXX_COPY[[i]]>=x[1])&(PROJECT_DATA_VXX_COPY[[i]]<x[2]),])
	          },i)
	          
	        }else{
	          data<-datasplitecdfsens
	        }
	      }else if ((!is.na(low_val))&(!is.na(high_val))){ #manual high low values specified
	        
	        data<-PROJECT_DATA_VXX_COPY[(PROJECT_DATA_VXX_COPY[[i]]>=ecdf_low_val)&(PROJECT_DATA_VXX_COPY[[i]]<ecdf_high_val),]
	      }
	    }
	    
	    return(data)
	    
	  })
	  
	  output$ecdfsenslow_txt<-renderText({
	    # if (isolate(input$freq_sens_step)==0)return()
	    if (input$ecdf_sens_step==0)return()
	    # req(freqIntrange$low)
	    # freqIntrange$low
	    # req(freqsensdataFilter$val)
	    req(ecdf_low_val)
	    round(ecdf_low_val,2)
	    
	  })
	  
	  output$ecdfsenshigh_txt<-renderText({
	    # if (isolate(input$freq_sens_step)==0)return()
	    if (input$ecdf_sens_step==0)return()
	    # req(freqIntrange$high)
	    # freqIntrange$high
	    # req(freqsensdataFilter$val)
	    req(ecdf_high_val)
	    round(ecdf_high_val,2)
	    
	  })
	  
	  output$ecdfsensDatasource<-renderText({
	    
	    low_val<-isolate(input$ecdf_sens_low)
	    high_val<-isolate(input$ecdf_sens_high)
	    
	    if ((is.na(low_val))&(is.na(high_val))){
	      datasplitecdfsens<<-prepecdfsensdata()
	    }else if ((!is.na(low_val))&(!is.na(high_val))){
	      dataManualecdfsens<<-prepecdfsensdata()
	    }
	    
	    if((!is.null(id))&(!is.null(datasplitecdfsens))){
	      removeNotification(id)
	      id<<-NULL
	      showNotification("Data ready for plot.",duration=2)
	    }
	    isolate(ecdfsensdataFilter$val)
	  })

	  
	  #################
	  # observeEvent(input$ecdf_add) - function adds plot for sensitivity analysis.
	  #################
	  observeEvent(input$ecdf_add, {
	    req(input$ecdf_sens)
	    if (input$ecdf_sens=="No Selection"){
	      showNotification("Please select signal for plot.",duration=2)
	      return()
	    }
	    if (input$ecdf_sens!="No Selection"){
	      if (length(ecdf_inserted[grepl(input$ecdf_sens,ecdf_inserted, fixed=TRUE)])==1){
	        showNotification("Signal plot already added",type="message",duration=3)
	      }else{
	        idn<-input$ecdf_sens

	        cn<-which(INPUT_VECTOR_VXX_Title_Type_Text==idn)
	        if ((length(ecdf_inserted)%%2)==0){
	          selector='#ecdf_placeholder1'
	        }else{
	          selector='#ecdf_placeholder2'
	        }

	        insertUI(
	          selector = selector, where="afterEnd",
	          ui = plotOutput(paste0("plot1d_ecdf_sens_",idn),click=paste0("plot1d_ecdf_sens_click_",idn),
	                          dblclick = paste0("plot1d_ecdf_sens_dblclick_",idn),
	                          brush = brushOpts(id = paste0("plot1d_ecdf_sens_brush_",idn),resetOnNew = TRUE),height="400px")
	        )

	        output[[paste0("plot1d_ecdf_sens_",idn)]]<-renderPlot({

	          low_val<-isolate(input$ecdf_sens_low)
	          high_val<-isolate(input$ecdf_sens_high)
	          
	          ecdfstep<-(input$ecdf_sens_step)
	          if (ecdfstep==0)return()
	          ecdf_sens_plts[[idn]]<-ggplot()
	          # req(freqIntrange$low)
	          req(ecdfsensdataFilter$val)
	          i<-which(INPUT_VECTOR_VXX_Title_Type_Text==idn)
	          if ((is.na(low_val))&(is.na(high_val))){ #manual high/low setting not present
	            req(datasplitecdfsens)
	            if (ecdfstep!=0) {
	              ecdf_sens_dat[[idn]]<-datasplitecdfsens[[ecdfstep]][[i]]  
	            }
	          }else if ((!is.na(low_val))&(!is.na(high_val))){#manual high/low setting present
	            req(dataManualecdfsens)
	            ecdf_sens_dat[[idn]]<-dataManualecdfsens[[i]]
	          }
	          
	          if (length(ecdf_sens_dat[[idn]])>0){
	            PROJECT_DATA_VXX___FILTER_RESULT_V_CI <- ecdf_sens_dat[[idn]]
	            PROJECT_DATA_VXX___FILTER_RESULT_V_CI<-PROJECT_DATA_VXX___FILTER_RESULT_V_CI[!is.na(PROJECT_DATA_VXX___FILTER_RESULT_V_CI)]
	            PROJECT_DATA_VXX___FILTER_RESULT_V_CI<-PROJECT_DATA_VXX___FILTER_RESULT_V_CI[PROJECT_DATA_VXX___FILTER_RESULT_V_CI>=ecdf_sens_left_x[[idn]]]
	            PROJECT_DATA_VXX___FILTER_RESULT_V_CI<-PROJECT_DATA_VXX___FILTER_RESULT_V_CI[PROJECT_DATA_VXX___FILTER_RESULT_V_CI<=ecdf_sens_right_x[[idn]]]

	            min_PROJECT_DATA_VXX_l <- min(PROJECT_DATA_VXX___FILTER_RESULT_V_CI)
	            max_PROJECT_DATA_VXX_l <- max(PROJECT_DATA_VXX___FILTER_RESULT_V_CI)

	            my_values <- PROJECT_DATA_VXX___FILTER_RESULT_V_CI
	            my_cdf = ecdf(my_values)
	            r <- range(my_values)

	            mylabECDF = paste("ECDF(", idn, ")", sep="")
	            mylabECDFmin = paste("1 - ECDF(", idn, ")", sep="")
	            my_ylab = ifelse(isolate(input$ecdf_sens_check)=="ECDF",mylabECDF,mylabECDFmin)

	            # my_main = INPUT_Analysis_Title

	            if (!is.null(ecdf_high_val)){
	              my_main<-paste0(INPUT_Analysis_Title, "\n",isolate(ecdf_low_val)," <= ",isolate(input$sig1)," < ",isolate(ecdf_high_val))
	            }else{
	              my_main = paste(ecdf_sens_title[[idn]], "\n" ,ecdf_sens_x_unit[[idn]],sep="")
	            }

	            ylim<-c(0,1)
	            my_xlim<-ecdf_sens_xlim[[idn]]

	            if(r[1]-r[2]!=0){
	              if (isolate(input$ecdf_sens_check)=="ECDF"){
	                curve1 <- curve(my_cdf(x), from=r[1], to=r[2])
	                # curve1 <- curve(my_cdf(x), from=r[1], to=r[2],plot=FALSE)
	                data<-data.frame(x=curve1$x,y=curve1$y)
	              }else if (isolate(input$ecdf_sens_check)=="1-ECDF"){
	                curve1 <- curve(1-my_cdf(x), from=r[1], to=r[2])
	                # curve1 <- curve(1-my_cdf(x), from=r[1], to=r[2],plot=FALSE)
	                data<-data.frame(x=curve1$x,y=curve1$y)
	              }
	            }else{
	              data<-data.frame(x=r[1],y=1)
	              }

	            #----------------------------------------------------------------
	            # plot mean
	            #----------------------------------------------------------------
	            my_mean_of_PROJECT_DATA_VXX___FILTER_RESULT_V_CI =  mean(PROJECT_DATA_VXX___FILTER_RESULT_V_CI)
              avg<-my_mean_of_PROJECT_DATA_VXX___FILTER_RESULT_V_CI
	            if(r[1]-r[2]!=0){
	              my_min_index_br = min(which((curve1$x >= my_mean_of_PROJECT_DATA_VXX___FILTER_RESULT_V_CI)))
	              my_min_index<-min(my_min_index_br)

	              my_height = curve1$y[my_min_index]
	              my_height_max = max(curve1$y)
	            }else{
	              my_height_max = 1
	              my_height<-my_height_max
	            }

	            my_text = paste( "(", round(my_mean_of_PROJECT_DATA_VXX___FILTER_RESULT_V_CI, 2),
	                             " / " , round(my_height * 100,2),
	                             ")" , sep="")

	            my_xlab_03 = paste(round(ecdf_sens_left_x[[idn]],2), " <= ", ecdf_sens_title[[idn]], " <= ",
	                               round(ecdf_sens_right_x[[idn]],2),  sep="")

	            # if(length(isolate(ecdf_sens_title[[isolate(input$sig1)]])) == 1)
	            # {
	            # }
	            # else
	            # {
	            #   my_xlab_03 = paste(my_xlab_03 , " ( X:=", ecdf_sens_title[[idn]], " )" , sep="")
	            # }

	            my_xlab = my_xlab_03
	            # my_main = paste(ecdf_sens_title[[idn]] , "\n" ,
	            #                 ecdf_sens_x_unit[[idn]] ,
	            #                 sep="")

	            min_PROJECT_DATA_VXX_l <- ecdf_sens_xmin[[idn]]
	            max_PROJECT_DATA_VXX_l <- ecdf_sens_xmax[[idn]]

	            my_var_of_PROJECT_DATA_VXX___FILTER_RESULT_V_CI =  var(PROJECT_DATA_VXX___FILTER_RESULT_V_CI)
	            my_sigma_of_PROJECT_DATA_VXX___FILTER_RESULT_V_CI = sqrt(my_var_of_PROJECT_DATA_VXX___FILTER_RESULT_V_CI)
	            my_mean_plus_my_sigma = (my_mean_of_PROJECT_DATA_VXX___FILTER_RESULT_V_CI + my_sigma_of_PROJECT_DATA_VXX___FILTER_RESULT_V_CI)


	            if(my_height == 0 | is.na(my_height) == TRUE){
	              sigma_my_height = my_height_max* 0.1
	            }else{
	              sigma_my_height = my_height * 0.25
	            }

	            my_mean_minus_my_sigma =  (my_mean_of_PROJECT_DATA_VXX___FILTER_RESULT_V_CI - my_sigma_of_PROJECT_DATA_VXX___FILTER_RESULT_V_CI)
	            
	            if ((!is.na(my_mean_plus_my_sigma))&(!is.na(my_mean_minus_my_sigma))){
	              xt<-rbind(my_mean_plus_my_sigma,my_mean_minus_my_sigma)
	              yt<-rbind(0,0)
	              l<-rbind(paste("+", round(my_sigma_of_PROJECT_DATA_VXX___FILTER_RESULT_V_CI,2), sep=""),
	                       paste("-",  round(my_sigma_of_PROJECT_DATA_VXX___FILTER_RESULT_V_CI,2),sep=""))
	              txt<-data.frame(x=xt,y=yt,label=l)
	            }

	            pt<-data.frame(x1=my_mean_of_PROJECT_DATA_VXX___FILTER_RESULT_V_CI,y1=0,x2=my_mean_of_PROJECT_DATA_VXX___FILTER_RESULT_V_CI,y2=my_height)
	            pt1<-data.frame(x1=my_mean_of_PROJECT_DATA_VXX___FILTER_RESULT_V_CI,y1=0,x2=my_mean_plus_my_sigma,y2=0)
	            pt2<-data.frame(x1=my_mean_of_PROJECT_DATA_VXX___FILTER_RESULT_V_CI,y1=0,x2=my_mean_minus_my_sigma,y2=0)
	            ecdfsensplts<-ggplot()
	            if (length(unique(data$x))>1){
	              ecdfsensplts<-ecdfsensplts+geom_line(data=data,aes(x=x, y=y))
	            }
	            
	            ecdfsensplts<-ecdfsensplts+geom_segment(data=pt,aes(x=x1,y=y1,xend=x2,yend=y2),colour="blue", size=0.5)+
	              
	              ylim(ylim)+xlab(my_xlab)+ylab(my_ylab)+ggtitle(my_main)+
	              geom_segment(data=pt,aes(x=x1,y=y1,xend=x2,yend=y2),colour="blue", size=1)+
	              geom_text(data=data.frame(x=avg,y=my_height,label=my_text), aes(x=x,y=y,label=label),colour="blue",size=3,vjust="left")+
	              # geom_segment(data=pt1,aes(x=x1,y=y1,xend=x2,yend=y2),colour="red", size=0.5)+
	              # annotate("text",x=my_mean_plus_my_sigma,y=sigma_my_height, colour="red", label=paste("+", round(my_sigma_of_PROJECT_DATA_VXX___FILTER_RESULT_V_CI,2), sep=""),size=4)+
	              # geom_segment(data=pt2,aes(x=x1,y=y1,xend=x2,yend=y2),colour="red", size=0.5)+
	              # annotate("text",x=my_mean_minus_my_sigma,y=sigma_my_height, colour="red", label=paste("-",  round(my_sigma_of_PROJECT_DATA_VXX___FILTER_RESULT_V_CI,2),sep=""),size=4)+
	              # annotate("text",x=my_mean_of_PROJECT_DATA_VXX___FILTER_RESULT_V_CI,y=my_height *1.05, colour="blue", label=my_text,size=4)+
	              theme(plot.title=element_text(size=15),axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
	                    axis.text.x = element_text(size=12),axis.text.y = element_text(size=12))+
	              coord_cartesian(xlim=ranges1d_ecdf_sensx[[idn]], ylim=ranges1d_ecdf_sensy[[idn]])
	            
	            
	            if ((!is.na(my_mean_plus_my_sigma))&(!is.na(my_mean_minus_my_sigma))){
	              if (my_mean_plus_my_sigma>my_xlim[2]){
	                my_xlim[2]<-my_mean_plus_my_sigma
	              }
	              if (my_mean_minus_my_sigma<my_xlim[1]){
	                my_xlim[1]<-my_mean_minus_my_sigma
	              }
	              ecdfsensplts<-ecdfsensplts+geom_segment(data=pt1,aes(x=x1,y=y1,xend=x2,yend=y2),colour="red", size=1)+
	                geom_text(data=txt, aes(x=x,y=y,label=label),colour="red",size=3,vjust="right")+
	                geom_segment(data=pt2,aes(x=x1,y=y1,xend=x2,yend=y2),colour="red", size=1)+xlim(my_xlim)
	            }
	          }else{
	            ecdfsensplts<-ggplot()
	          }

	          ecdf_sens_plts[[idn]]<-ecdfsensplts
	          ecdfsensplts

	        })

	        insertUI(
	          selector = selector, where="afterEnd",
	          ## wrap element in a div with id for ease of removal
	          ui = tags$div(
	            tags$p(input$ecdf_sens),
	            id = idn
	          )
	        )
	        ecdf_inserted <<- c(ecdf_inserted,idn)
	        observeEvent(input[[paste0("plot1d_ecdf_sens_dblclick_",idn)]],{
	          brush <- input[[paste0("plot1d_ecdf_sens_brush_",idn)]]
	          if (!is.null(brush)) {
	            ranges1d_ecdf_sensx[[idn]] <- c(brush$xmin, brush$xmax)
	            ranges1d_ecdf_sensy[[idn]] <- c(brush$ymin, brush$ymax)
	          }else {
	            ranges1d_ecdf_sensx[[idn]] <- NULL
	            ranges1d_ecdf_sensy[[idn]] <- NULL
	          }
	        })
	      }
	    }
	  })

	  #################
	  # observeEvent(input$ecdf_rem) - function removes plot
	  #################
	  observeEvent(input$ecdf_rem,{
	    if (length(ecdf_inserted)>0){
	      selector = paste0('#', ecdf_inserted[length(ecdf_inserted)])
	      removeUI(selector=selector)
	      selector = paste0('#plot1d_ecdf_sens_', ecdf_inserted[length(ecdf_inserted)])
	      removeUI(selector=selector)
	      ecdf_inserted <<- ecdf_inserted[-length(ecdf_inserted)]
	    }

	  })

	  #################
	  # observeEvent(input$ecdf_sens_step) - function updates signal interval values based on slider selection.
	  #################
	  observeEvent(input$ecdf_sens_step,{
	    if((!is.na(input$ecdf_sens_low))&(!is.na(input$ecdf_sens_high))){
	      showNotification("Clear Low (Manual) & High (Manual) and click GENERATE PLOT before animation.",duration = 2)
	    }
	    if (input$ecdf_sens_step!=0){
	      low_val<-ecdfboundMin+(input$ecdf_sens_step-1)*ecdfStepval
	      high_val<-low_val+ecdfStepval
	      
	      # low_val<-freqsensStepRange[[input$freq_sens_step]][1]
	      # high_val<-freqsensStepRange[[input$freq_sens_step]][2]
	      ecdf_high_val<<-high_val
	      ecdf_low_val<<-low_val
	      
	      
	      # updateNumericInput(session,"freq_sens_low", value=low_val)
	      # updateNumericInput(session,"freq_sens_high", value=high_val)
	      
	    }

	    
	  })
	  
	  observeEvent(input$ecdf_interval,ignoreInit = TRUE,{
	    if (input$stat=="ECDF_SENSITIVITY"){
	      if ((!is.na(input$ecdf_sens_low))|(!is.na(input$ecdf_sens_high))){
	        showNotification("Clear Low (Manual) & High (Manual) and click GENERATE PLOT",duration=2)
	        # return()
	      }else{
	        showNotification("Please click GENERATE PLOT update data",duration=2)   
	      }
	    }
	    updateSliderInput(session,"ecdf_sens_step",value=0,min=0,max=1,step=1)
	    datasplitecdfsens<<-NULL
	    dataManualecdfsens<<-NULL
	    ecdfsensdataFilter$val<-NULL
	    # freqIntrange$low<-NULL
	    # freqIntrange$high<-NULL
	    ecdf_high_val<<-NULL
	    ecdf_low_val<<-NULL
	    
	  })

	  #################
	  # data1d_ecdf_sens - function loads data for creating plots in the background using R- kernel.
	  #################
	  data1d_ecdf_sens<-reactive({
	    create_plot<-0
	    data<-NULL
	    ext_data<-data.frame("sig1"=c(input$sig1_low,input$sig1_high),"sig2"=c(input$sig2_low,input$sig2_high))
	    range_apply<-0
	    range_data<-NULL

	    if (input$extent==TRUE){
	      range_apply<-1
	      range_data<-ext_data
	    }
	    if ((input$load_data=="1")&&(input$extent==TRUE)){
	      INPUT_keep_last_loaded_data_WITHOUT_DOMAIN_BASED_and_EVENT_BASED_conditions_or_reload_complete_data<-"reload_complete_data"
	      source(paste0(scripts_path,"/Load_Data.R"))
	    }
	    if ((input$load_data == "2")||(input$extent==TRUE)){
	      if (input$sig1!="No Selection"){
	        if (input$ecdf_sens_check=="No Selection"){
	          showNotification("Please select plot type.",duration=2)
	          return()
	        }
	        if (input$ecdf_sens_check=="ECDF"){
	          plt_typ<-"ECDF_SENSITIVITY_ANALYSIS"
	        }else if (input$ecdf_sens_check=="1-ECDF"){
	          plt_typ<-"1-ECDF_SENSITIVITY_ANALYSIS"
	        }
	        i<-which(INPUT_VECTOR_VXX_Title_Type_Text==input$sig1)
	        j<-which(INPUT_1D_STATISTICS_TYPE_VECTOR==plt_typ)
	        if (length(j)==0){
	          INPUT_1D_STATISTICS_TYPE_VECTOR=c(INPUT_1D_STATISTICS_TYPE_VECTOR,plt_typ)
	          j<-which(INPUT_1D_STATISTICS_TYPE_VECTOR==plt_typ)
	        }
	        INPUT_VECTOR_GT_VALUE<-INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE
	        INPUT_VECTOR_LT_VALUE<-INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE
	        INPUT_OVERWRITE_or_SKIP_if_file_exists<-"OVERWRITE"
	        XYARR<-ECDFsENSITIVITYUniversalFunction(j,i,
	                                                INPUT_GENERATE_HISTOGRAMS_FOR_CONDITIONS_TOO_TRUE_OR_FALSE,
	                                                INPUT_VECTOR_1D_STATISTICS_path,
	                                                INPUT_1D_STATISTICS_TYPE_VECTOR,
	                                                INPUT_VECTOR_histogram_jpeg_file_name_overall,
	                                                INPUT_VECTOR_histogram_txt_file_name_overall,
	                                                INPUT_Analysis_Title,
	                                                INPUT_OVERWRITE_or_SKIP_if_file_exists,
	                                                PROJECT_DATA_VXX,
	                                                INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE,
	                                                INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE,
	                                                INPUT_VECTOR_VXX_Title_Type_Text,
	                                                INPUT_VECTOR_1D_STATISTICS_TABLES_path)

	        if(length(INPUT_VECTOR_VXX_Title_Type_Text[i]) == 1)
	        {
	          my_xlab = paste(round(INPUT_VECTOR_GT_VALUE[i],2), " <= " , INPUT_VECTOR_VXX_Title_Type_Text[i], " <= ", round(INPUT_VECTOR_LT_VALUE[i],2), sep="")
	        }
	        else
	        {
	          my_xlab = paste(round(INPUT_VECTOR_GT_VALUE[i],2), " <= " ,
	                          INPUT_VECTOR_VXX_Title_Type_Text[i], " <=",
	                          round(INPUT_VECTOR_LT_VALUE[i],2), " ( X:=", INPUT_VECTOR_VXX_Title_Type_Text[i], " )" , sep="")
	        }

	        cn<-colnames(XYARR)
	        mylabECDF = paste("ECDF(", INPUT_VECTOR_VXX_Title_Type_Text[i], ") in %", sep="")
	        mylabECDFmin = paste("1 - ECDF(", INPUT_VECTOR_VXX_Title_Type_Text[i], ") in %", sep="")
	        my_ylab = ifelse(cn[2]=="ECDF",mylabECDF,mylabECDFmin)
	        data<-list(XYARR,labs=c(my_xlab,my_ylab))

	        create_plot<-1
	        #}
	      }
	    }
	    else if ((input$load_data == "1")&&(input$extent==FALSE)){
	      if (input$sig1!="No Selection"){
	        XYARR<-data1d_ecdf_old()
	        i<-which(INPUT_VECTOR_VXX_Title_Type_Text==input$sig1)
	        j<-which(INPUT_1D_STATISTICS_TYPE_VECTOR=="ECDF")

	        INPUT_VECTOR_GT_VALUE<-INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE
	        INPUT_VECTOR_LT_VALUE<-INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE
	        if(length(INPUT_VECTOR_VXX_Title_Type_Text[i]) == 1)
	        {
	          my_xlab = paste(round(INPUT_VECTOR_GT_VALUE[i],2), " <= " , INPUT_VECTOR_VXX_Title_Type_Text[i], " <= ", round(INPUT_VECTOR_LT_VALUE[i],2), sep="")
	        }
	        else
	        {
	          my_xlab = paste(round(INPUT_VECTOR_GT_VALUE[i],2), " <= " ,
	                          INPUT_VECTOR_VXX_Title_Type_Text[i], " <=",
	                          round(INPUT_VECTOR_LT_VALUE[i],2), " ( X:=", INPUT_VECTOR_VXX_Title_Type_Text[i], " )" , sep="")
	        }

	        cn<-colnames(XYARR)
	        mylabECDF = paste("ECDF(", INPUT_VECTOR_VXX_Title_Type_Text[i], ") in %", sep="")
	        mylabECDFmin = paste("1 - ECDF(", INPUT_VECTOR_VXX_Title_Type_Text[i], ") in %", sep="")
	        my_ylab = ifelse(cn[2]=="ECDF",mylabECDF,mylabECDFmin)
	        data<-list(XYARR,labs=c(my_xlab,my_ylab))

	      }
	    }
	    # data
	  })

	 

	  #################
	  # Function for downloading sensitivity plots
	  #################
	  output$plot1d_ecdf_sens_d<-downloadHandler(
	    filename = function(){
	      paste0("ECDF_Sensitivity_",input$sig1,".pdf")
	    },
	    content=function(file){

	      pdf(file=file,width=15, height=7)
	      for (i in 1:length(ecdf_inserted)){
	        plt<-ecdf_sens_plts[[ecdf_inserted[i]]]
	        plt<-plt+annotate("text", Inf, Inf, label = ecdfsensdataFilter$val, hjust = 1, vjust = 1)
	        plt<-plt+annotate("text", -Inf, Inf, label = paste0("Creation Date: ",Sys.Date()), hjust = 0, vjust = 1)
	        plot(plt)
	      }
	      dev.off()
	    }
	  )
	}
	
	# 1D HOURS DISTRIBUTION
	{
	data1d_hours_dist_old<-function(){
	  
	  pa<-isolate(path_to_tables())
	  INPUT_TXT_FILE<-paste0(pa,isolate(input$last_ext))
	  PROJECT_DATA_VXX_LAST_1 <- fread(INPUT_TXT_FILE, 
	                                   skip = 0, 
	                                   dec=".", 
	                                   na.strings=c("NULL", "ERROR", "-1.#INF", "1.#INF", "NA"),sep=";" )
	  return(PROJECT_DATA_VXX_LAST_1)
	}
	
	conditionscheckhoursdist<-function(){
	  ok<-TRUE
	  if (input$stat=="HOURS_DISTRIBUTION"){
	    if ((input$load_data==1)&&(input$last_ext=="No Selection")){
	      showNotification("Please select last analysis output",duration=2)
	      ok<-FALSE
	    } else if (input$sig1=="No Selection"){
	      showNotification("Please select Signal 1 for Analysis",duration=2)
	      ok<-FALSE
	    } else if ((input$load_data!=1)&(input$hours_dist_brk=="No Selection")){
	      showNotification("Please select breaks for Analysis",duration=2)
	      ok<-FALSE
	    } else {
	      # id<<-NULL
	      # id<<-showNotification("Generating Hours Distribution Plot.",duration=0)
	      # plt_hours_dist$y<-"No"  
	      # plt_hours_dist$y<-"Yes"  
	    }
	  }
	  return(ok)
	}
	
	hoursdistdataFilter<-reactiveValues(val=NULL)
	
	data1d_hours_dist<-reactive({
	  
	  data<-NULL
	  if (plt_hours_dist$y=="No")return()
	  if (!isolate(conditionscheckhoursdist()))return()
	  
	  # filterSelection<-ifelse(!is.null(isolate(input$extent)),isolate(input$extent),0)
	  filterSelection<-ifelse(!is.null((input$extent)),(input$extent),0)
	  signal1<-isolate(input$sig1)
	  
	  sourceSelection<-isolate(input$load_data)
	  # sourceSelection<-(input$load_data)
	  brk<-isolate(input$hours_dist_brk)
	  if (brk!="No Selection"){
	    brk<-as.numeric(brk)
	  }
	  
	  i<-which(INPUT_VECTOR_VXX_Title_Type_Text==signal1)
	  j<-which(INPUT_1D_STATISTICS_TYPE_VECTOR=="OP_HOURS_DISTRIBUTION")
	  if (length(j)==0){
	    INPUT_1D_STATISTICS_TYPE_VECTOR=c(INPUT_1D_STATISTICS_TYPE_VECTOR,"OP_HOURS_DISTRIBUTION")
	    j<-which(INPUT_1D_STATISTICS_TYPE_VECTOR=="OP_HOURS_DISTRIBUTION")
	  }
	  
	  
	  # if ((sourceSelection==3)&(is.null(appEvt))&(is.null(dataFilterEvents)))return()
	  
	  if (!isolate(checkExtent())){
	    hoursdistdataFilter$val<-NULL
	    return()
	  }else{
	    id<<-NULL
	    id<<-showNotification("Generating Hours Distribution Plot.",duration=NULL)
	  }
	  
	  if (sourceSelection == 2){#all data
	    if (signal1!="No Selection"){
	      
	      
	      
	      filterEventResult<-dataFilterEventApply(filterSelection,dataFilter1D,dataFilter2D,
	                                              dataFilternD,i,j=0,PROJECT_DATA_VXX,
	                                              dataFilterEvents=NULL,appEvtName=NULL)
	      
	      PROJECT_DATA_VXX_COPY<-filterEventResult[[1]]
	      val<-filterEventResult[[2]]
	      hoursdistdataFilter$val<-val
	      
	      data_dim<-dim(PROJECT_DATA_VXX_COPY)
	      if (data_dim[1]>0){
	        delta_time<-difftime(PROJECT_DATA_VXX_COPY$MY_DATE[2:data_dim[1]],PROJECT_DATA_VXX_COPY$MY_DATE[1:(data_dim[1]-1)],units = 'secs')
	        time_jump_index<-(delta_time>INPUT_MY_FREQUENCY)
	        if(length(which(time_jump_index))>0){
	          delta_time<-delta_time[!time_jump_index]
	        }
	        # TOTAL_OPERATION_HOURS_OF_THIS_STEP <<- (dim(PROJECT_DATA_VXX_COPY)[1]/3600) * INPUT_MY_FREQUENCY
	        TOTAL_OPERATION_HOURS_OF_THIS_STEP <<- (as.numeric(sum(delta_time))/3600)
	      }
	      
	      INPUT_OVERWRITE_or_SKIP_if_file_exists<-"OVERWRITE"
	      XYARR<-HOURS_DISTRIBUTIONuniversalFunction(j,i,
	                                                 INPUT_GENERATE_HISTOGRAMS_FOR_CONDITIONS_TOO_TRUE_OR_FALSE,
	                                                 INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS,
	                                                 INPUT_VECTOR_1D_STATISTICS_path,
	                                                 INPUT_1D_STATISTICS_TYPE_VECTOR,
	                                                 INPUT_VECTOR_histogram_jpeg_file_name_overall,
	                                                 INPUT_VECTOR_histogram_txt_file_name_overall,
	                                                 INPUT_Analysis_Title,
	                                                 INPUT_OVERWRITE_or_SKIP_if_file_exists,
	                                                 PROJECT_DATA_VXX_COPY,
	                                                 INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE,
	                                                 INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE,
	                                                 INPUT_VECTOR_VXX_Title_Type_Text,
	                                                 INPUT_VECTOR_1D_STATISTICS_TABLES_path,
	                                                 brk
	      )
	      # my_xlab1<-INPUT_VECTOR_xlab_01[i]
	      cn<-colnames(XYARR)
	      t1<-regexpr("TITLE-",cn[3])
	      t3<-regexpr("TOTAL-HOURS=",cn[3])
	      t2<-regexpr("BREAKS=",cn[3])
	      my_xlab1<-substr(cn[3],(t1[1]+6),(t2[1]-1))
	      brks<-substr(cn[3],(t2[1]+7),(t3[1]-1))
	      total_hours<-as.numeric(as.character(substr(cn[3],(t3[1]+12),nchar(cn[3]))))
	      data<-list(XYARR,lab=my_xlab1,th=total_hours,br=brks)
	      create_plot<-1
	    }
	  }else if (sourceSelection==3){#events
	    if (signal1!="No Selection"){
	      filterEventResult<-dataFilterEventApply(filterSelection,dataFilter1D,dataFilter2D,
	                                              dataFilternD,i,j=0,PROJECT_DATA_VXX,
	                                              dataFilterEvents,appEvtName)
	      
	      PROJECT_DATA_VXX_COPY<-filterEventResult[[1]]
	      val<-filterEventResult[[2]]
	      hoursdistdataFilter$val<-val
	      
	      data_dim<-dim(PROJECT_DATA_VXX_COPY)
	      if (data_dim[1]>0){
	        delta_time<-difftime(PROJECT_DATA_VXX_COPY$MY_DATE[2:data_dim[1]],PROJECT_DATA_VXX_COPY$MY_DATE[1:(data_dim[1]-1)],units = 'secs')
	        time_jump_index<-(delta_time>INPUT_MY_FREQUENCY)
	        if(length(which(time_jump_index))>0){
	          delta_time<-delta_time[!time_jump_index]
	        }
	        # TOTAL_OPERATION_HOURS_OF_THIS_STEP <<- (dim(PROJECT_DATA_VXX_COPY)[1]/3600) * INPUT_MY_FREQUENCY
	        TOTAL_OPERATION_HOURS_OF_THIS_STEP <<- (as.numeric(sum(delta_time))/3600)
	      }
	      INPUT_OVERWRITE_or_SKIP_if_file_exists<-"OVERWRITE"
	      XYARR<-HOURS_DISTRIBUTIONuniversalFunction(j,i,
	                                                 INPUT_GENERATE_HISTOGRAMS_FOR_CONDITIONS_TOO_TRUE_OR_FALSE,
	                                                 INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS,
	                                                 INPUT_VECTOR_1D_STATISTICS_path,
	                                                 INPUT_1D_STATISTICS_TYPE_VECTOR,
	                                                 INPUT_VECTOR_histogram_jpeg_file_name_overall,
	                                                 INPUT_VECTOR_histogram_txt_file_name_overall,
	                                                 INPUT_Analysis_Title,
	                                                 INPUT_OVERWRITE_or_SKIP_if_file_exists,
	                                                 PROJECT_DATA_VXX_COPY,
	                                                 INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE,
	                                                 INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE,
	                                                 INPUT_VECTOR_VXX_Title_Type_Text,
	                                                 INPUT_VECTOR_1D_STATISTICS_TABLES_path,
	                                                 brk
	      )
	      # my_xlab1<-INPUT_VECTOR_xlab_01[i]
	      cn<-colnames(XYARR)
	      t1<-regexpr("TITLE-",cn[3])
	      t3<-regexpr("TOTAL-HOURS=",cn[3])
	      t2<-regexpr("BREAKS=",cn[3])
	      my_xlab1<-substr(cn[3],(t1[1]+6),(t2[1]-1))
	      brks<-substr(cn[3],(t2[1]+7),(t3[1]-1))
	      total_hours<-as.numeric(as.character(substr(cn[3],(t3[1]+12),nchar(cn[3]))))
	      data<-list(XYARR,lab=my_xlab1,th=total_hours,br=brks)
	      create_plot<-1
	    }
	  }
	  else if (sourceSelection == 1){#last analysed output
	    if (signal1!="No Selection"){
	      XYARR<-data1d_hours_dist_old()
	      hoursdistdataFilter$val<-" "
	      cn<-colnames(XYARR)
	      t1<-regexpr("TITLE-",cn[3])
	      t3<-regexpr("TOTAL-HOURS=",cn[3])
	      t2<-regexpr("BREAKS=",cn[3])
	      my_xlab1<-substr(cn[3],(t1[1]+6),(t2[1]-1))
	      brks<-substr(cn[3],(t2[1]+7),(t3[1]-1))
	      total_hours<-as.numeric(as.character(substr(cn[3],(t3[1]+12),nchar(cn[3]))))
	      data<-list(XYARR,lab=my_xlab1,th=total_hours,br=brks)
	      create_plot<-1
	    }
	  }
	  data
	})
	
	output$hoursdistDatasource<-renderText({
	  req(hoursdistdataFilter$val)
	  hoursdistdataFilter$val
	})
	
	output$plot1d_hours_dist<-renderPlot({

	  data<-NULL
	  data_all<-(data1d_hours_dist())
	  data<-data_all[[1]]
	  hours_dist<-NULL
	  if(!is.null(data)){
	    
	    colnames(data)<-c("Lx","Rx","Dist")
	    my_xlab1<-data_all[[2]][1]
	    total_hours<-data_all[[3]][1]
	    brks<-data.frame(br=data_all[[4]][1])
	    # if (isolate(input$load_data)=="1"){
	    #   updateSelectInput(session,"hours_dist_brk",choices=brks$br,selected=brks$br)
	    # }
	    
	    data1d<-data.frame(x=(data$Lx+(data$Rx-data$Lx)/2),ct=data$Dist)
	    
	    # INPUT_Analysis_Title_FOR_OH =
	    my_main<-paste(INPUT_Analysis_Title,
	            "\n",
	            "TOTAL OPERATION-TIME = ",
	            # round(TOTAL_OPERATION_HOURS_OF_THIS_STEP,1) , " h",
	            round(total_hours,1), " h", 
	            sep="")
	    
	    hours_dist<-ggplot()+geom_col(data=data1d,aes(x=x, y=ct),alpha=0.6)+
	      annotate("text",x=data1d$x,y=data1d$ct, colour="red", label=data1d$ct,size=4,angle=90)+
	      xlab(my_xlab1)+ylab("Operation time in h")+ ggtitle(my_main)+
	      theme(plot.title = element_text(size=20),axis.title.x = element_text(size=18),axis.title.y = element_text(size=18),
	            axis.text.x = element_text(size=12),axis.text.y = element_text(size=12))+
	      coord_cartesian(xlim=ranges1d_hours_dist$x, ylim=ranges1d_hours_dist$y)
	    
	  }
	  plts1d$hours_dist<-hours_dist
	  isolate(plts1d$hours_dist)
	})
	
	observeEvent(input$plot1d_hours_dist_dblclick, {
	  brush <- input$plot1d_hours_dist_brush
	  if (!is.null(brush)) {
	    ranges1d_hours_dist$x <- c(brush$xmin, brush$xmax)
	    ranges1d_hours_dist$y <- c(brush$ymin, brush$ymax)
	  }else {
	    ranges1d_hours_dist$x <- NULL
	    ranges1d_hours_dist$y <- NULL
	  }
	  
	})
	
	output$plot1d_hours_dist_d<-downloadHandler(
	  filename = function(){
	    paste0("HOURS DISTRIBUTION_",input$sig1,"_",input$hours_dist_brk,"_breaks.pdf")
	  },
	  content=function(file){
	    pdf(file=file,width=15, height=7)
	    plt<-plts1d$hours_dist
	    # if (grepl("FILTERED",hoursdistdataFilter$val,fixed=TRUE)){
	      plt<-plt+annotate("text", Inf, Inf, label = hoursdistdataFilter$val, hjust = 1, vjust = 1)
	      plt<-plt+annotate("text", -Inf, Inf, label = paste0("Creation Date: ",Sys.Date()), hjust = 0, vjust = 1)
	    # }
	    plot(plt)
	    # plot(plts1d$hours_dist)
	    dev.off()
	  }
	)
	}
	
	#1D Violin/Box Plots
	{
	data1d_vio_box_old<-function(){
	  pa<-isolate(path_to_tables())
	  INPUT_TXT_FILE<-paste0(pa,isolate(input$last_ext))
	  PROJECT_DATA_VXX_LAST_1 <- fread(INPUT_TXT_FILE, 
	                                   skip = 0, 
	                                   dec=".", 
	                                   na.strings=c("NULL", "ERROR", "-1.#INF", "1.#INF", "NA"),sep=";" )
	  return(PROJECT_DATA_VXX_LAST_1)
	}
	  
	checkconditionsviobox<-function(){
	  ok<-TRUE
	  if (input$stat=="VIOLIN / BOX PLOTS"){
	    if (((input$load_data==2)|(input$load_data==3)) &&(input$sig1=="No Selection")){
	      showNotification("Please select Signal 1.",duration=2)
	      ok<-FALSE
	    }else if((input$load_data==1) &&(input$last_ext=="No Selection")){
	      showNotification("Please select last analysis.",duration=2)
	      ok<-FALSE
	    }else{
	      # id<<-NULL
	      # id<<-showNotification("Generating VIOLIN / BOX Plot.",duration=0)
	      # plt_vio_box$y<-"No"
	      # plt_vio_box$y<-"Yes"
	    }
	  }
	  return(ok)
	}
	  
	vioboxdataFilter<-reactiveValues(val=NULL)
	
	output$vioboxDatasource<-renderText({
	  req(vioboxdataFilter$val)
	  vioboxdataFilter$val
	})
	  
	data1d_vio_box<-reactive({
	 
	  data<-NULL
	  if (plt_vio_box$y=="No")return()
	  if(!isolate(checkconditionsviobox()))return()
	  # filterSelection<-ifelse(!is.null(isolate(input$extent)),isolate(input$extent),0)
	  filterSelection<-ifelse(!is.null((input$extent)),(input$extent),0)
	  visual1dFilter<-isolate(input$filter_1d)
	  signal1<-isolate(input$sig1)
	  sourceSelection<-isolate(input$load_data)
	  # sourceSelection<-(input$load_data)
	  vioboxPlot<-isolate(input$vio_box)
	  
	  i<-which(INPUT_VECTOR_VXX_Title_Type_Text==signal1)
	  # j=which(as.numeric(regexpr('BoxPlot',INPUT_1D_STATISTICS_TYPE_VECTOR))>0)
	  j=which(INPUT_1D_STATISTICS_TYPE_VECTOR==vioboxPlot)
	  if (length(j)==0){
	    INPUT_1D_STATISTICS_TYPE_VECTOR<-c(INPUT_1D_STATISTICS_TYPE_VECTOR,vioboxPlot)
	    j=which(INPUT_1D_STATISTICS_TYPE_VECTOR==vioboxPlot)
	  }
	  
	  
	  
	  # if ((sourceSelection==3)&(is.null(appEvt))&(is.null(dataFilterEvents)))return()
	  
	  if (!isolate(checkExtent())){
	    vioboxdataFilter$val<-NULL
	    # showNotification("Filter not defined.",duration=2)
	    return()  
	  }else{
	    id<<-NULL
	    id<<-showNotification("Generating Violin/Box Plot.",duration=NULL)
	  }
	  
	  
	  if (sourceSelection == 2){#New Analysis - All Data
	    if (signal1!="No Selection"){
	  
	      filterEventResult<-dataFilterEventApply(filterSelection,dataFilter1D,dataFilter2D,
	                                              dataFilternD,i,j=0,PROJECT_DATA_VXX,
	                                              dataFilterEvents=NULL,appEvtName=NULL)
	                                              
	      
	      PROJECT_DATA_VXX_COPY<-filterEventResult[[1]]
	      val<-filterEventResult[[2]]
	      
	      vioboxdataFilter$val<-val
	      
	      INPUT_OVERWRITE_or_SKIP_if_file_exists<-"OVERWRITE"
	      XYARR<-BoxPlotsUniversalFunction(j,i,
	                                       INPUT_VECTOR_1D_STATISTICS_path,
	                                       INPUT_1D_STATISTICS_TYPE_VECTOR,
	                                       INPUT_VECTOR_histogram_jpeg_file_name_overall,
	                                       INPUT_VECTOR_histogram_txt_file_name_overall,
	                                       INPUT_Analysis_Title,
	                                       INPUT_OVERWRITE_or_SKIP_if_file_exists,
	                                       PROJECT_DATA_VXX_COPY,
	                                       INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE,
	                                       INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE,
	                                       INPUT_VECTOR_VXX_Title_Type_Text,
	                                       INPUT_VECTOR_1D_STATISTICS_TABLES_path
	      )
	      my_xlab = paste(round(min(XYARR[,1]),2), " <= " , colnames(XYARR)[1], " <= ", round(max(XYARR[,1]),2), sep="")
	      data<-list(XYARR,lab=my_xlab)
	    }
	  }else if (sourceSelection == 3){# Event Data
	    if (signal1!="No Selection"){
	      #if (input$sig1!=input$sig2){
	      
	      filterEventResult<-dataFilterEventApply(filterSelection,dataFilter1D,dataFilter2D,
	                                              dataFilternD,i,j=0,PROJECT_DATA_VXX,
	                                              dataFilterEvents,appEvtName)
	                                              
	      
	      PROJECT_DATA_VXX_COPY<-filterEventResult[[1]]
	      val<-filterEventResult[[2]]
	      
	      vioboxdataFilter$val<-val
	      
	      
	      INPUT_OVERWRITE_or_SKIP_if_file_exists<-"OVERWRITE"
	      XYARR<-BoxPlotsUniversalFunction(j,i,
	                                       INPUT_VECTOR_1D_STATISTICS_path,
	                                       INPUT_1D_STATISTICS_TYPE_VECTOR,
	                                       INPUT_VECTOR_histogram_jpeg_file_name_overall,
	                                       INPUT_VECTOR_histogram_txt_file_name_overall,
	                                       INPUT_Analysis_Title,
	                                       INPUT_OVERWRITE_or_SKIP_if_file_exists,
	                                       PROJECT_DATA_VXX_COPY,
	                                       INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE,
	                                       INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE,
	                                       INPUT_VECTOR_VXX_Title_Type_Text,
	                                       INPUT_VECTOR_1D_STATISTICS_TABLES_path
	      )
	      my_xlab = paste(round(min(XYARR[,1]),2), " <= " , colnames(XYARR)[1], " <= ", round(max(XYARR[,1]),2), sep="")
	      data<-list(XYARR,lab=my_xlab)
	    }
	  }
	  else if (sourceSelection == 1){#Last Analysis
	    if (signal1!="No Selection"){
	       XYARR<-data1d_vio_box_old()
	       vioboxdataFilter$val<-" "
	       my_xlab = paste(round(min(XYARR[,1]),2), " <= " , colnames(XYARR)[1], " <= ", round(max(XYARR[,1]),2), sep="")
	       data<-list(XYARR,lab=my_xlab)
	      # create_plot<-1
	    }
	  }
	  data
	})
	
	output$plot1d_vio_box<-renderPlot({
  	
	  data<-NULL
	  data_all<-(data1d_vio_box())

	  data<-data_all[[1]]
	  
	  if(!is.null(data)){
	    
	    my_ylab<-data_all[[2]][1]
	    title=INPUT_Analysis_Title
	    if (isolate(input$vio_box)=="BoxPlot24hours"){
	      colnames(data)<-c("x","time")
	      dataSmall = data[(data$x!=0),]
	      TimePosition = length(dataSmall)
	      dataSmall$time=as.POSIXct(dataSmall$time)
	      dataSmall$time=as.factor(hour(as.ITime(dataSmall$time)))#to create per hours groups
	      
	      my_xlab = "time, hours"
	      
	      colnames(dataSmall)<-c("x","time")
	      
	      plts1d$vio_box<-ggplot(data=dataSmall,aes(time,x))+geom_boxplot(outlier.colour="chocolate3", outlier.shape=8, outlier.size=0.5)+
	        labs(x = my_xlab,y = my_ylab,title=title)+
	        theme(axis.text=element_text(size=15),
	              axis.title=element_text(size=15,face="bold"),plot.title=element_text(size=20))+
	        stat_summary(data=dataSmall,aes(time,x),fun.y=mean, colour="red", geom="point", shape=18, size=5,show.legend = FALSE) 
	      
	    } else if(isolate(input$vio_box)=="BoxPlotWeekDays"){
	      colnames(data)<-c("x","time")
	      dataSmall = data[(data$x!=0),]
	      TimePosition = length(dataSmall)
	      dataSmall$time=as.POSIXct(dataSmall$time)
	      dataSmall$time=wday(dataSmall$time,label=TRUE)#to create per hours groups#to create per hours groups
	      #dataSmall[[TimePosition]]=wday(dataSmall[[TimePosition]],label=TRUE)#to create per hours groups#to create per hours groups
	      
	      my_xlab = "Week days"
	      
	      plts1d$vio_box<-ggplot(data=dataSmall,aes(time,x))+geom_boxplot(outlier.colour="chocolate3", outlier.shape=8, outlier.size=0.5)+
	        labs(x = my_xlab,y = my_ylab,title=title)+
	        theme(axis.text=element_text(size=15),
	              axis.title=element_text(size=15,face="bold"),plot.title=element_text(size=20))+
	        stat_summary(data=dataSmall,aes(time,x),fun.y=mean, colour="red", geom="point", shape=18, size=5,show.legend = FALSE) 
	      
	    }else if(isolate(input$vio_box)=="BoxPlotMonths"){
	      colnames(data)<-c("x","time")
	      dataSmall = data[(data$x!=0),]
	      TimePosition = length(dataSmall)
	      dataSmall$time=as.POSIXct(dataSmall$time)
	      dataSmall$time=month(dataSmall$time,label=TRUE)#to create per hours groups#to create per hours groups
	      #dataSmall[[TimePosition]]=month(dataSmall[[TimePosition]],label=TRUE)#to create per hours groups#to create per hours groups
	      
	      my_xlab = "Month"
	      
	      plts1d$vio_box<-ggplot(data=dataSmall,aes(time,x))+geom_boxplot(outlier.colour="chocolate3", outlier.shape=8, outlier.size=0.5)+
	        labs(x = my_xlab,y = my_ylab,title=title)+
	        theme(axis.text=element_text(size=15),
	              axis.title=element_text(size=15,face="bold"),plot.title=element_text(size=20))+
	        stat_summary(data=dataSmall,aes(time,x),fun.y=mean, colour="red", geom="point", shape=18, size=5,show.legend = FALSE) 
	      
	    }else if(isolate(input$vio_box)=="BoxPlotCalenderWeeks"){
	      colnames(data)<-c("x","time")
	      dataSmall = data[(data$x!=0),]
	      TimePosition = length(dataSmall)
	      dataSmall$time=as.POSIXct(dataSmall$time)
	      dataSmall$time=factor(week(dataSmall$time))
	      # dataSmall$time=factor(week(dataSmall$time,label=TRUE))#to create per hours groups#to create per hours groups
	      #dataSmall[[TimePosition]]=factor(week(dataSmall[[TimePosition]]))#to create per hours groups#to create per hours groups
	      my_xlab = "Weeks"
	      
	      plts1d$vio_box<-ggplot(data=dataSmall,aes(time,x))+geom_boxplot(outlier.colour="chocolate3", outlier.shape=8, outlier.size=0.5)+
	        labs(x = my_xlab,y = my_ylab,title=title)+
	        theme(axis.text=element_text(size=15),
	              axis.title=element_text(size=15,face="bold"),plot.title=element_text(size=20))+
	        stat_summary(data=dataSmall,aes(time,x),fun.y=mean, colour="red", geom="point", shape=18, size=5,show.legend = FALSE) 
	      
	    }else if(isolate(input$vio_box)=="BoxPlotBusinessDaysWeekends"){
	      colnames(data)<-c("x","time")
	      dataSmall = data[(data$x!=0),]
	      TimePosition = length(dataSmall)
	      dataSmall$time=as.POSIXct(dataSmall$time)
	      
	      #dataSmall$time=factor(week(dataSmall$time,label=TRUE))#to create per hours groups#to create per hours groups
	      dataSmall[[TimePosition]]=wday(dataSmall[[TimePosition]])#to create per hours groups#to create per hours groups
	      fBusWeE=function(x){ifelse(x==1 | x==6,"Weekends","Business days")}
	      dataSmall[[TimePosition]] = factor(sapply(dataSmall[[TimePosition]],fBusWeE))
	      my_xlab = "Days"
	      
	      plts1d$vio_box<-ggplot(data=dataSmall,aes(time,x))+geom_boxplot(outlier.colour="chocolate3", outlier.shape=8, outlier.size=0.5)+
	        labs(x = my_xlab,y = my_ylab,title=title)+
	        theme(axis.text=element_text(size=15),
	              axis.title=element_text(size=15,face="bold"),plot.title=element_text(size=20))+
	        stat_summary(data=dataSmall,aes(time,x),fun.y=mean, colour="red", geom="point", shape=18, size=5,show.legend = FALSE) 
	      
	    }else if(isolate(input$vio_box)=="BoxPlotYears"){
	      colnames(data)<-c("x","time")
	      dataSmall = data[(data$x!=0),]
	      TimePosition = length(dataSmall)
	      dataSmall$time=as.POSIXct(dataSmall$time)
	      
	      dataSmall[[TimePosition]]=factor(year(dataSmall[[TimePosition]]))#to create per hours groups#to create per hours groups
	      my_xlab = "Years"
	      
	      plts1d$vio_box<-ggplot(data=dataSmall,aes(time,x))+geom_boxplot(outlier.colour="chocolate3", outlier.shape=8, outlier.size=0.5)+
	        labs(x = my_xlab,y = my_ylab,title=title)+
	        theme(axis.text=element_text(size=15),
	              axis.title=element_text(size=15,face="bold"),plot.title=element_text(size=20))+
	        stat_summary(data=dataSmall,aes(time,x),fun.y=mean, colour="red", geom="point", shape=18, size=5,show.legend = FALSE) 
	      
	    }else if(isolate(input$vio_box)=="BoxPlotSeasons"){
	      colnames(data)<-c("x","time")
	      dataSmall = data[(data$x!=0),]
	      TimePosition = length(dataSmall)
	      dataSmall$time=as.POSIXct(dataSmall$time)
	      
	      options(warn=-1)
	      seasons0=rep(c("Winter","Spring","Summer","Autumn"),each=3)
	      seasons=shift(seasons0,type="lead",fill=seasons0[1])
	      monate=month(dataSmall[[TimePosition]])
	      dataSmall[[TimePosition]]=factor(seasons[monate],levels=unique(seasons0))#to create per hours groups#to create per hours groups
	      my_xlab = "Days"
	      
	      plts1d$vio_box<-ggplot(data=dataSmall,aes(time,x))+geom_boxplot(outlier.colour="chocolate3", outlier.shape=8, outlier.size=0.5)+
	        labs(x = my_xlab,y = my_ylab,title=title)+
	        theme(axis.text=element_text(size=15),
	              axis.title=element_text(size=15,face="bold"),plot.title=element_text(size=20))+
	        stat_summary(data=dataSmall,aes(time,x),fun.y=mean, colour="red", geom="point", shape=18, size=5,show.legend = FALSE) 
	      
	    }else if(isolate(input$vio_box)=="violinBoxPlot24hours"){
	      colnames(data)<-c("x","time")
	      dataSmall = data[(data$x!=0),]
	      TimePosition = length(dataSmall)
	      dataSmall$time=as.POSIXct(dataSmall$time)
	      
	      dataSmall[[TimePosition]]=as.factor(hour(as.ITime(dataSmall[[TimePosition]]) ))#to create per hours groups
	      my_xlab = "time, hours"
	      varNum<-1
	      timeVarName=names(dataSmall)[TimePosition]
	      varName=names(dataSmall)[varNum]
	      
	 
	      #creates faceGrid structure and adds the number of data for each colomn
	      statHours=dlply(dataSmall,timeVarName) #combine by hours#special function for facet_grid labels
	      numberOfDataPerH = sapply(1:length(statHours),function(i){nrow(statHours[[i]])})
	      numberOfDataPerH = numberOfDataPerH*100/sum(numberOfDataPerH)
	      yMedian=sapply(seq_along(statHours),FUN=function(i){(median(statHours[[i]][[varNum]])+unique(quantile(statHours[[i]][[varNum]],0.65)))/2})
	      hIQR=sapply(seq_along(statHours),FUN=function(i){IQR(statHours[[i]][[varNum]],na.rm=TRUE)})
	      yMedian = yMedian + hIQR/10
	      hours=sapply(seq_along(statHours),FUN=function(i){statHours[[i]][[TimePosition]][1]})
	      
	      med<-median(dataSmall$x)+unique(quantile(dataSmall$x,0.65))/2+IQR(dataSmall$x,na.rm=TRUE)/10
	      iq<-IQR(dataSmall$x,na.rm=TRUE)
	      
	      df<-data.frame(x=dataSmall$x,y=med,iqr=iq)
	      
	      LabeFun=function(numb)#transforms number into exponential format
	      { numb=round(numb,digits=ifelse(numb<1,2,1))
	      str=paste0(c(format(numb,scientific=FALSE),"%"),collapse = "")
	      }
	      hoursStr = as.character(hours)
	      numberOfDataPerHstr = sapply(numberOfDataPerH,FUN=LabeFun)
	      dummyList = list()
	      labeList=mapply(function(name,value){dummyList[[name]]=value},hoursStr,numberOfDataPerHstr)#creates named list
	      
	      #creates faceGrid structure and adds the number of data for each colomn,as_laberller for user defined facet labels
	      
	      txt<-data.frame(x=hours, y=yMedian, l=as.character(round(hIQR,digits=1)))
	      dataIQR=data.table(x=yMedian,time=as.numeric(hours),IQR=as.character(round(hIQR,digits=1)) )
	      
	      plts1d$vio_box<-ggplot()+geom_violin(data=dataSmall,aes(time,x),colour="white",fill="navajowhite3")+
	        geom_boxplot(data=dataSmall,aes(time,x),outlier.colour="chocolate3", outlier.shape=NA, outlier.size=1,
	                         notch=FALSE, width=0.5, alpha=0.3,coef=0,lwd=1,fatten = 2)+
	        stat_summary(data=dataSmall,aes(time,x),fun.y=mean, colour="red", geom="point", shape=18, size=2,show.legend = FALSE)+
	        theme(axis.text=element_text(size=15),
	              axis.title=element_text(size=15,face="bold"),plot.title=element_text(size=15),
	              strip.text.x = element_text(size = 10,face="bold"))+
	    
	        geom_text(data=data.frame(time=hours,x=yMedian),aes(x=time,y=x,label=as.character(round(hIQR,digits=1)),angle=90,size=7,fontface="bold",color="firebrick"),show.legend = FALSE)+
	        labs(x = my_xlab,y = my_ylab,title=title)+
	        facet_grid(.~time, scales="free",labeller=as_labeller(labeList))
	      
	    }else if(isolate(input$vio_box)=="violinBoxPlotMonths"){
	      colnames(data)<-c("x","time")
	      dataSmall = data[(data$x!=0),]
	      TimePosition = length(dataSmall)
	      dataSmall$time=as.POSIXct(dataSmall$time)
	      
	      dataSmall[[TimePosition]]=month(dataSmall[[TimePosition]],label=TRUE)#to create per hours groups#to create per hours groups
	      my_xlab = "Month"
	      varNum<-1
	      timeVarName=names(dataSmall)[TimePosition]
	      varName=names(dataSmall)[varNum]
	      
	      
	      #creates faceGrid structure and adds the number of data for each colomn
	      statHours=dlply(dataSmall,timeVarName) #combine by hours#special function for facet_grid labels
	      numberOfDataPerH = sapply(1:length(statHours),function(i){nrow(statHours[[i]])})
	      numberOfDataPerH = numberOfDataPerH*100/sum(numberOfDataPerH)
	      yMedian=sapply(seq_along(statHours),FUN=function(i){(median(statHours[[i]][[varNum]])+unique(quantile(statHours[[i]][[varNum]],0.65)))/2})
	      hIQR=sapply(seq_along(statHours),FUN=function(i){IQR(statHours[[i]][[varNum]],na.rm=TRUE)})
	      yMedian = yMedian + hIQR/10
	      hours=sapply(seq_along(statHours),FUN=function(i){statHours[[i]][[TimePosition]][1]})
	      
	      # med<-median(dataSmall$x)+unique(quantile(dataSmall$x,0.65))/2+IQR(dataSmall$x,na.rm=TRUE)/10
	      # iq<-IQR(dataSmall$x,na.rm=TRUE)
	      # 
	      # df<-data.frame(x=dataSmall$x,y=med,iqr=iq)
	      # 
	      LabeFun=function(numb)#transforms number into exponential format
	      { numb=round(numb,digits=ifelse(numb<1,2,1))
	      str=paste0(c(format(numb,scientific=FALSE),"%"),collapse = "")
	      }
	      hoursStr = as.character(hours)
	      numberOfDataPerHstr = sapply(numberOfDataPerH,FUN=LabeFun)
	      dummyList = list()
	      labeList=mapply(function(name,value){dummyList[[name]]=value},hoursStr,numberOfDataPerHstr)#creates named list
	      
	      #creates faceGrid structure and adds the number of data for each colomn,as_laberller for user defined facet labels
	      
	      # txt<-data.frame(x=hours, y=yMedian, l=as.character(round(hIQR,digits=1)))
	      dataIQR=data.table(x=yMedian,time=as.numeric(hours),IQR=as.character(round(hIQR,digits=1)) )
	      
	      plts1d$vio_box<-ggplot()+geom_violin(data=dataSmall,aes(time,x),colour="white",fill="navajowhite3")+
	        geom_boxplot(data=dataSmall,aes(time,x),outlier.colour="chocolate3", outlier.shape=NA, outlier.size=1,
	                     notch=FALSE, width=0.5, alpha=0.3,coef=0,lwd=1,fatten = 2)+
	        stat_summary(data=dataSmall,aes(time,x),fun.y=mean, colour="red", geom="point", shape=18, size=2,show.legend = FALSE)+
	        theme(axis.text=element_text(size=15),
	              axis.title=element_text(size=15,face="bold"),plot.title=element_text(size=15),
	              strip.text.x = element_text(size = 10,face="bold"))+
	        
	        geom_text(data=data.frame(time=hours,x=yMedian),aes(x=time,y=x,label=as.character(round(hIQR,digits=1)),angle=90,size=7,fontface="bold",color="firebrick"),show.legend = FALSE)+
	        labs(x = my_xlab,y = my_ylab,title=title)+
	        facet_grid(.~time, scales="free",labeller=as_labeller(labeList))
	      
	    }else if(isolate(input$vio_box)=="violinBoxPlotWeekDays"){
	      colnames(data)<-c("x","time")
	      dataSmall = data[(data$x!=0),]
	      TimePosition = length(dataSmall)
	      dataSmall$time=as.POSIXct(dataSmall$time)
	      
	      newOrder = c("Mon","Tues","Wed","Thurs","Fri","Sat","Sun")
	      wd=wday(dataSmall[[TimePosition]],label=TRUE)#to create per week days groups
	      wd2=factor(wd,levels=newOrder)#makes Monday the first day of the week
	      dataSmall[[TimePosition]]=wd2#to create per week days groups
	      my_xlab = "Week days"
	      varNum<-1
	      timeVarName=names(dataSmall)[TimePosition]
	      varName=names(dataSmall)[varNum]
	      
	      
	      #creates faceGrid structure and adds the number of data for each colomn
	      statHours=dlply(dataSmall,timeVarName) #combine by hours#special function for facet_grid labels
	      numberOfDataPerH = sapply(1:length(statHours),function(i){nrow(statHours[[i]])})
	      numberOfDataPerH = numberOfDataPerH*100/sum(numberOfDataPerH)
	      yMedian=sapply(seq_along(statHours),FUN=function(i){(median(statHours[[i]][[varNum]])+unique(quantile(statHours[[i]][[varNum]],0.65)))/2})
	      hIQR=sapply(seq_along(statHours),FUN=function(i){IQR(statHours[[i]][[varNum]],na.rm=TRUE)})
	      yMedian = yMedian + hIQR/10
	      hours=sapply(seq_along(statHours),FUN=function(i){statHours[[i]][[TimePosition]][1]})
	      
	      # med<-median(dataSmall$x)+unique(quantile(dataSmall$x,0.65))/2+IQR(dataSmall$x,na.rm=TRUE)/10
	      # iq<-IQR(dataSmall$x,na.rm=TRUE)
	      # 
	      # df<-data.frame(x=dataSmall$x,y=med,iqr=iq)
	      # 
	      LabeFun=function(numb)#transforms number into exponential format
	      { numb=round(numb,digits=ifelse(numb<1,2,1))
	      str=paste0(c(format(numb,scientific=FALSE),"%"),collapse = "")
	      }
	      hoursStr = as.character(hours)
	      numberOfDataPerHstr = sapply(numberOfDataPerH,FUN=LabeFun)
	      dummyList = list()
	      labeList=mapply(function(name,value){dummyList[[name]]=value},hoursStr,numberOfDataPerHstr)#creates named list
	      
	      #creates faceGrid structure and adds the number of data for each colomn,as_laberller for user defined facet labels
	      
	      # txt<-data.frame(x=hours, y=yMedian, l=as.character(round(hIQR,digits=1)))
	      dataIQR=data.table(x=yMedian,time=as.numeric(hours),IQR=as.character(round(hIQR,digits=1)) )
	      
	      plts1d$vio_box<-ggplot()+geom_violin(data=dataSmall,aes(time,x),colour="white",fill="navajowhite3")+
	        geom_boxplot(data=dataSmall,aes(time,x),outlier.colour="chocolate3", outlier.shape=NA, outlier.size=1,
	                     notch=FALSE, width=0.5, alpha=0.3,coef=0,lwd=1,fatten = 2)+
	        stat_summary(data=dataSmall,aes(time,x),fun.y=mean, colour="red", geom="point", shape=18, size=2,show.legend = FALSE)+
	        theme(axis.text=element_text(size=15),
	              axis.title=element_text(size=15,face="bold"),plot.title=element_text(size=15),
	              strip.text.x = element_text(size = 10,face="bold"))+
	        
	        geom_text(data=data.frame(time=hours,x=yMedian),aes(x=time,y=x,label=as.character(round(hIQR,digits=1)),angle=90,size=7,fontface="bold",color="firebrick"),show.legend = FALSE)+
	        labs(x = my_xlab,y = my_ylab,title=title)+
	        facet_grid(.~time, scales="free",labeller=as_labeller(labeList))+
	        coord_cartesian(xlim=ranges1d_vio_box$x, ylim=ranges1d_vio_box$y)
	      
	    }else if(isolate(input$vio_box)=="violinBoxPlotCalendarWeeks"){
	      colnames(data)<-c("x","time")
	      dataSmall = data[(data$x!=0),]
	      TimePosition = length(dataSmall)
	      dataSmall$time=as.POSIXct(dataSmall$time)
	      
	      dataSmall[[TimePosition]]=factor(week(dataSmall[[TimePosition]]))#translate datatime format into weeks
	      my_xlab = "Weeks"
	      varNum<-1
	      timeVarName=names(dataSmall)[TimePosition]
	      varName=names(dataSmall)[varNum]
	      
	      
	      #creates faceGrid structure and adds the number of data for each colomn
	      statHours=dlply(dataSmall,timeVarName) #combine by hours#special function for facet_grid labels
	      numberOfDataPerH = sapply(1:length(statHours),function(i){nrow(statHours[[i]])})
	      numberOfDataPerH = numberOfDataPerH*100/sum(numberOfDataPerH)
	      yMedian=sapply(seq_along(statHours),FUN=function(i){(median(statHours[[i]][[varNum]])+unique(quantile(statHours[[i]][[varNum]],0.65)))/2})
	      hIQR=sapply(seq_along(statHours),FUN=function(i){IQR(statHours[[i]][[varNum]],na.rm=TRUE)})
	      yMedian = yMedian + hIQR/10
	      hours=sapply(seq_along(statHours),FUN=function(i){statHours[[i]][[TimePosition]][1]})
	      
	      # med<-median(dataSmall$x)+unique(quantile(dataSmall$x,0.65))/2+IQR(dataSmall$x,na.rm=TRUE)/10
	      # iq<-IQR(dataSmall$x,na.rm=TRUE)
	      # 
	      # df<-data.frame(x=dataSmall$x,y=med,iqr=iq)
	      # 
	      LabeFun=function(numb)#transforms number into exponential format
	      { numb=round(numb,digits=ifelse(numb<1,2,1))
	      str=paste0(c(format(numb,scientific=FALSE),"%"),collapse = "")
	      }
	      hoursStr = as.character(hours)
	      numberOfDataPerHstr = sapply(numberOfDataPerH,FUN=LabeFun)
	      dummyList = list()
	      labeList=mapply(function(name,value){dummyList[[name]]=value},hoursStr,numberOfDataPerHstr)#creates named list
	      
	      #creates faceGrid structure and adds the number of data for each colomn,as_laberller for user defined facet labels
	      
	      # txt<-data.frame(x=hours, y=yMedian, l=as.character(round(hIQR,digits=1)))
	      dataIQR=data.table(x=yMedian,time=as.numeric(hours),IQR=as.character(round(hIQR,digits=1)) )
	      
	      plts1d$vio_box<-ggplot()+geom_violin(data=dataSmall,aes(time,x),colour="white",fill="navajowhite3")+
	        geom_boxplot(data=dataSmall,aes(time,x),outlier.colour="chocolate3", outlier.shape=NA, outlier.size=1,
	                     notch=FALSE, width=0.5, alpha=0.3,coef=0,lwd=0.5,fatten = 3)+
	        stat_summary(data=dataSmall,aes(time,x),fun.y=mean, colour="red", geom="point", shape=18, size=2,show.legend = FALSE)+
	        theme(axis.text=element_text(size=10),
	              axis.title=element_text(size=15,face="bold"),plot.title=element_text(size=15),
	              strip.text.x = element_text(angle=90,size = 10,face="bold"))+
	        
	        geom_text(data=data.frame(time=hours,x=yMedian),aes(x=time,y=x,label=as.character(round(hIQR,digits=1)),angle=90,size=4,color="firebrick"),show.legend = FALSE)+
	        labs(x = my_xlab,y = my_ylab,title=title)+
	        facet_grid(.~time, scales="free",labeller=as_labeller(labeList))+
	        coord_cartesian(xlim=ranges1d_vio_box$x, ylim=ranges1d_vio_box$y)
	      
	    }else if(isolate(input$vio_box)=="violinBoxPlotBusinessDaysWeekends"){
	      colnames(data)<-c("x","time")
	      dataSmall = data[(data$x!=0),]
	      TimePosition = length(dataSmall)
	      dataSmall$time=as.POSIXct(dataSmall$time)
	      
	      dataSmall[[TimePosition]]=wday(dataSmall[[TimePosition]])#to create per hours groups#to create per hours groups
	      fBusWeE=function(x){ifelse(x==1 | x==6,"Weekends","Business days")}
	      dataSmall[[TimePosition]] = factor(sapply(dataSmall[[TimePosition]],fBusWeE))
	      my_xlab = "Days"
	      
	      varNum<-1
	      timeVarName=names(dataSmall)[TimePosition]
	      varName=names(dataSmall)[varNum]
	      
	      
	      #creates faceGrid structure and adds the number of data for each colomn
	      statHours=dlply(dataSmall,timeVarName) #combine by hours#special function for facet_grid labels
	      numberOfDataPerH = sapply(1:length(statHours),function(i){nrow(statHours[[i]])})
	      numberOfDataPerH = numberOfDataPerH*100/sum(numberOfDataPerH)
	      yMedian=sapply(seq_along(statHours),FUN=function(i){(median(statHours[[i]][[varNum]])+unique(quantile(statHours[[i]][[varNum]],0.65)))/2})
	      hIQR=sapply(seq_along(statHours),FUN=function(i){IQR(statHours[[i]][[varNum]],na.rm=TRUE)})
	      yMedian = yMedian + hIQR/10
	      hours=sapply(seq_along(statHours),FUN=function(i){statHours[[i]][[TimePosition]][1]})
	      
	      # med<-median(dataSmall$x)+unique(quantile(dataSmall$x,0.65))/2+IQR(dataSmall$x,na.rm=TRUE)/10
	      # iq<-IQR(dataSmall$x,na.rm=TRUE)
	      # 
	      # df<-data.frame(x=dataSmall$x,y=med,iqr=iq)
	      # 
	      LabeFun=function(numb)#transforms number into exponential format
	      { numb=round(numb,digits=ifelse(numb<1,2,1))
	      str=paste0(c(format(numb,scientific=FALSE),"%"),collapse = "")
	      }
	      hoursStr = as.character(hours)
	      numberOfDataPerHstr = sapply(numberOfDataPerH,FUN=LabeFun)
	      dummyList = list()
	      labeList=mapply(function(name,value){dummyList[[name]]=value},hoursStr,numberOfDataPerHstr)#creates named list
	      
	      #creates faceGrid structure and adds the number of data for each colomn,as_laberller for user defined facet labels
	      
	      # txt<-data.frame(x=hours, y=yMedian, l=as.character(round(hIQR,digits=1)))
	      dataIQR=data.table(x=yMedian,time=as.numeric(hours),IQR=as.character(round(hIQR,digits=1)) )
	      
	      plts1d$vio_box<-ggplot()+geom_violin(data=dataSmall,aes(time,x),colour="white",fill="navajowhite3")+
	        geom_boxplot(data=dataSmall,aes(time,x),outlier.colour="chocolate3", outlier.shape=NA, outlier.size=1,
	                     notch=FALSE, width=0.5, alpha=0.3,coef=0,lwd=0.5,fatten = 3)+
	        stat_summary(data=dataSmall,aes(time,x),fun.y=mean, colour="red", geom="point", shape=18, size=2,show.legend = FALSE)+
	        theme(axis.text=element_text(size=10),
	              axis.title=element_text(size=15,face="bold"),plot.title=element_text(size=15),
	              strip.text.x = element_text(size = 10,face="bold"))+
	        
	        geom_text(data=data.frame(time=hours,x=yMedian),aes(x=time,y=x,label=as.character(round(hIQR,digits=1)),angle=90,size=4,color="firebrick"),show.legend = FALSE)+
	        labs(x = my_xlab,y = my_ylab,title=title)+
	        facet_grid(.~time, scales="free",labeller=as_labeller(labeList))+
	        coord_cartesian(xlim=ranges1d_vio_box$x, ylim=ranges1d_vio_box$y)
	      
	    }else if(isolate(input$vio_box)=="violinBoxPlotYears"){
	      colnames(data)<-c("x","time")
	      dataSmall = data[(data$x!=0),]
	      TimePosition = length(dataSmall)
	      dataSmall$time=as.POSIXct(dataSmall$time)
	      
	      options(warn=-1)
	      dataSmall[[TimePosition]]=factor(year(dataSmall[[TimePosition]]))#to create per hours groups#to create per hours groups
	      my_xlab = "Years"
	      
	      varNum<-1
	      timeVarName=names(dataSmall)[TimePosition]
	      varName=names(dataSmall)[varNum]
	      
	      
	      #creates faceGrid structure and adds the number of data for each colomn
	      statHours=dlply(dataSmall,timeVarName) #combine by hours#special function for facet_grid labels
	      numberOfDataPerH = sapply(1:length(statHours),function(i){nrow(statHours[[i]])})
	      numberOfDataPerH = numberOfDataPerH*100/sum(numberOfDataPerH)
	      yMedian=sapply(seq_along(statHours),FUN=function(i){(median(statHours[[i]][[varNum]])+unique(quantile(statHours[[i]][[varNum]],0.65)))/2})
	      hIQR=sapply(seq_along(statHours),FUN=function(i){IQR(statHours[[i]][[varNum]],na.rm=TRUE)})
	      yMedian = yMedian + hIQR/10
	      hours=sapply(seq_along(statHours),FUN=function(i){statHours[[i]][[TimePosition]][1]})
	      
	      # med<-median(dataSmall$x)+unique(quantile(dataSmall$x,0.65))/2+IQR(dataSmall$x,na.rm=TRUE)/10
	      # iq<-IQR(dataSmall$x,na.rm=TRUE)
	      # 
	      # df<-data.frame(x=dataSmall$x,y=med,iqr=iq)
	      # 
	      LabeFun=function(numb)#transforms number into exponential format
	      { numb=round(numb,digits=ifelse(numb<1,2,1))
	      str=paste0(c(format(numb,scientific=FALSE),"%"),collapse = "")
	      }
	      hoursStr = as.character(hours)
	      numberOfDataPerHstr = sapply(numberOfDataPerH,FUN=LabeFun)
	      dummyList = list()
	      labeList=mapply(function(name,value){dummyList[[name]]=value},hoursStr,numberOfDataPerHstr)#creates named list
	      
	      #creates faceGrid structure and adds the number of data for each colomn,as_laberller for user defined facet labels
	      
	      # txt<-data.frame(x=hours, y=yMedian, l=as.character(round(hIQR,digits=1)))
	      dataIQR=data.table(x=yMedian,time=as.numeric(hours),IQR=as.character(round(hIQR,digits=1)) )
	      
	      plts1d$vio_box<-ggplot()+geom_violin(data=dataSmall,aes(time,x),colour="white",fill="navajowhite3")+
	        geom_boxplot(data=dataSmall,aes(time,x),outlier.colour="chocolate3", outlier.shape=NA, outlier.size=1,
	                     notch=FALSE, width=0.5, alpha=0.3,coef=0,lwd=0.5,fatten = 3)+
	        stat_summary(data=dataSmall,aes(time,x),fun.y=mean, colour="red", geom="point", shape=18, size=2,show.legend = FALSE)+
	        theme(axis.text=element_text(size=10),
	              axis.title=element_text(size=15,face="bold"),plot.title=element_text(size=15),
	              strip.text.x = element_text(size = 10,face="bold"))+
	        
	        geom_text(data=data.frame(time=hours,x=yMedian),aes(x=time,y=x,label=as.character(round(hIQR,digits=1)),angle=90,size=4,color="firebrick"),show.legend = FALSE)+
	        labs(x = my_xlab,y = my_ylab,title=title)+
	        facet_grid(.~time, scales="free",labeller=as_labeller(labeList))+
	        coord_cartesian(xlim=ranges1d_vio_box$x, ylim=ranges1d_vio_box$y)
	      
	    }else if(isolate(input$vio_box)=="violinBoxPlotSeasons"){
	      colnames(data)<-c("x","time")
	      dataSmall = data[(data$x!=0),]
	      TimePosition = length(dataSmall)
	      dataSmall$time=as.POSIXct(dataSmall$time)
	      
	      options(warn=-1)
	      seasons0=rep(c("Winter","Spring","Summer","Autumn"),each=3)
	      seasons=shift(seasons0,type="lead",fill=seasons0[1])
	      monate=month(dataSmall[[TimePosition]])
	      dataSmall[[TimePosition]]=factor(seasons[monate],levels=unique(seasons0))#to create per hours groups#to create per hours groups
	      my_xlab = "Seasons"
	      
	      varNum<-1
	      timeVarName=names(dataSmall)[TimePosition]
	      varName=names(dataSmall)[varNum]
	      
	      
	      #creates faceGrid structure and adds the number of data for each colomn
	      statHours=dlply(dataSmall,timeVarName) #combine by hours#special function for facet_grid labels
	      numberOfDataPerH = sapply(1:length(statHours),function(i){nrow(statHours[[i]])})
	      numberOfDataPerH = numberOfDataPerH*100/sum(numberOfDataPerH)
	      yMedian=sapply(seq_along(statHours),FUN=function(i){(median(statHours[[i]][[varNum]])+unique(quantile(statHours[[i]][[varNum]],0.65)))/2})
	      hIQR=sapply(seq_along(statHours),FUN=function(i){IQR(statHours[[i]][[varNum]],na.rm=TRUE)})
	      yMedian = yMedian + hIQR/10
	      hours=sapply(seq_along(statHours),FUN=function(i){statHours[[i]][[TimePosition]][1]})
	      
	      # med<-median(dataSmall$x)+unique(quantile(dataSmall$x,0.65))/2+IQR(dataSmall$x,na.rm=TRUE)/10
	      # iq<-IQR(dataSmall$x,na.rm=TRUE)
	      # 
	      # df<-data.frame(x=dataSmall$x,y=med,iqr=iq)
	      # 
	      LabeFun=function(numb)#transforms number into exponential format
	      { numb=round(numb,digits=ifelse(numb<1,2,1))
	      str=paste0(c(format(numb,scientific=FALSE),"%"),collapse = "")
	      }
	      hoursStr = as.character(hours)
	      numberOfDataPerHstr = sapply(numberOfDataPerH,FUN=LabeFun)
	      dummyList = list()
	      labeList=mapply(function(name,value){dummyList[[name]]=value},hoursStr,numberOfDataPerHstr)#creates named list
	      
	      #creates faceGrid structure and adds the number of data for each colomn,as_laberller for user defined facet labels
	      
	      # txt<-data.frame(x=hours, y=yMedian, l=as.character(round(hIQR,digits=1)))
	      dataIQR=data.table(x=yMedian,time=as.numeric(hours),IQR=as.character(round(hIQR,digits=1)) )
	      
	      plts1d$vio_box<-ggplot()+geom_violin(data=dataSmall,aes(time,x),colour="white",fill="navajowhite3")+
	        geom_boxplot(data=dataSmall,aes(time,x),outlier.colour="chocolate3", outlier.shape=NA, outlier.size=1,
	                     notch=FALSE, width=0.5, alpha=0.3,coef=0,lwd=0.5,fatten = 3)+
	        stat_summary(data=dataSmall,aes(time,x),fun.y=mean, colour="red", geom="point", shape=18, size=2,show.legend = FALSE)+
	        theme(axis.text=element_text(size=10),
	              axis.title=element_text(size=15,face="bold"),plot.title=element_text(size=15),
	              strip.text.x = element_text(size = 10,face="bold"))+
	        
	        geom_text(data=data.frame(time=hours,x=yMedian),aes(x=time,y=x,label=as.character(round(hIQR,digits=1)),angle=90,size=4,color="firebrick"),show.legend = FALSE)+
	        labs(x = my_xlab,y = my_ylab,title=title)+
	        facet_grid(.~time, scales="free",labeller=as_labeller(labeList))+
	        coord_cartesian(xlim=ranges1d_vio_box$x, ylim=ranges1d_vio_box$y)
	      # options(warn=0) 
	    }
	    plts1d$vio_box
	  }
	  else{
	    plts1d$vio_box<-NULL
	    isolate(plts1d$vio_box)
	  }
	  
	})
	
	observeEvent(input$plot1d_vio_box_dblclick, {
	  brush <- input$plot1d_vio_box_brush
	  if (!is.null(brush)) {
	    ranges1d_vio_box$x <- c(brush$xmin, brush$xmax)
	    ranges1d_vio_box$y <- c(brush$ymin, brush$ymax)
	  }else {
	    ranges1d_vio_box$x <- NULL
	    ranges1d_vio_box$y <- NULL
	  }
	  
	})
	
	output$plot1d_vio_box_d<-downloadHandler(
	  filename = function(){
	    paste0(input$vio_box,"_",input$sig1,".pdf")
	  },
	  content=function(file){
	    pdf(file=file,width=15, height=7)
	    plt<-plts1d$vio_box
	    # if (grepl("FILTERED",vioboxdataFilter$val,fixed=TRUE)){
	      plt<-plt+annotate("text", Inf, Inf, label = vioboxdataFilter$val, hjust = 1, vjust = 1)
	      plt<-plt+annotate("text", -Inf, Inf, label = paste0("Creation Date: ",Sys.Date()), hjust = 0, vjust = 1)
	    # }
	    plot(plt)
	    # plot(plts1d$vio_box)
	    dev.off()
	  }
	)
	}
	
	# 1D Frequency
	{
	dataFilter1D<-NULL
	
	PROJECT_DATA_VXX_FILTERED_1D<-NULL
	
	data1d_freq_old<-function(){
	  pa<-isolate(path_to_tables())
	  INPUT_TXT_FILE<-paste0(pa,isolate(input$last_ext))
	  PROJECT_DATA_VXX_LAST_1 <- fread(INPUT_TXT_FILE, 
	                                   skip = 0, 
	                                   dec=".", 
	                                   na.strings=c("NULL", "ERROR", "-1.#INF", "1.#INF", "NA"),sep=";" )
	 return(PROJECT_DATA_VXX_LAST_1)
	}
	
	density1D<-NULL
	gridSize<-500
	freqdataFilter<-reactiveValues(val=paste0("Data Source : COMPLETE \n","Filtering Source : NONE"))
	
	conditionscheckfreq<-function(){
	  ok<-TRUE
	  if (input$stat=="FREQUENCY"){
	    if (((input$load_data==2)|(input$load_data==3)) &&(input$sig1=="No Selection")){
	      showNotification("Please select Signal 1.",duration=2)
	      ok<-FALSE
	    }else if (((input$load_data==2)|(input$load_data==3)) &&(input$freq_brk=="No Selection")){
	      showNotification("Please select breaks.",duration=2)
	      ok<-FALSE
	    }
	    else if((input$load_data==1) &&(input$last_ext=="No Selection")){
	      showNotification("Please select last analysis.",duration=2)
	      ok<-FALSE
	    }else{
	      # id<<-NULL
	      # id<<-showNotification("Generating Frequency Plots.",duration=0)
	      
	      # plt_freq$y<-"No"
	      # plt_freq$y<-"Yes"
	      
	    }
	  }
	  return(ok)
	}
	
	data1d_freq<-reactive({
	  data<-NULL
	  if ((plt_freq$y=="No"))return()
	  if(!isolate(conditionscheckfreq()))return()
	  # filterSelection<-ifelse(!is.null(isolate(input$extent)),isolate(input$extent),0)
	  filterSelection<-ifelse(!is.null((input$extent)),(input$extent),0)
	  visual1dFilter<-isolate(input$filter_1d)
	  signal1<-isolate(input$sig1)
	  sourceSelection<-isolate(input$load_data)
	  # sourceSelection<-(input$load_data)
	  freqBreaks<-isolate(input$freq_brk)
	  
	  i<-which(INPUT_VECTOR_VXX_Title_Type_Text==signal1)
	  j<-which(INPUT_1D_STATISTICS_TYPE_VECTOR=="FREQUENCY")
	  if (length(j)==0){
	    INPUT_1D_STATISTICS_TYPE_VECTOR=c(INPUT_1D_STATISTICS_TYPE_VECTOR,"FREQUENCY")
	    j<-which(INPUT_1D_STATISTICS_TYPE_VECTOR=="FREQUENCY")
	  }
	  if (freqBreaks!="No Selection"){
	    brk<-as.numeric(freqBreaks)
	  }
	  
	  
	  # if ((plt_freq$y=="No")&(updatePlotCaller!="EXTENT"))return()
	  
	  
	  # if ((sourceSelection==3)&(is.null(appEvt))&(is.null(dataFilterEvents)))return()
	  
	  if (!isolate(checkExtent())){
	    freqdataFilter$val<-NULL
	    # showNotification("Filter not defined.",duration=2)
	    return()  
	  }else{
	    id<<-NULL
	    id<<-showNotification("Generating Frequency Plots.",duration=NULL)
	  }
	  

	  if (sourceSelection == 2){ #All Data
	    if (signal1!="No Selection"){
	        
	        
	      filterEventResult<-dataFilterEventApply(filterSelection,dataFilter1D,dataFilter2D,
	                                              dataFilternD,i,j=0,PROJECT_DATA_VXX,
	                                              dataFilterEvents=NULL,appEvtName=NULL,callerFilter)
	      
	      PROJECT_DATA_VXX_COPY<-filterEventResult[[1]]
	      val<-filterEventResult[[2]]
        
        freqdataFilter$val<-val
        
        
        ii<-(PROJECT_DATA_VXX_COPY[[i]]>=INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE[i])&(PROJECT_DATA_VXX_COPY[[i]]<=INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE[i])
        dataDenEst<-PROJECT_DATA_VXX_COPY[[i]][ii]
        den_estimate<-bkde(x=dataDenEst,gridsize=gridSize)
        
        INPUT_OVERWRITE_or_SKIP_if_file_exists<-"OVERWRITE"
        XYARR<-DensityFreqProbabUniversalFunction(j,i,
                                                  INPUT_GENERATE_HISTOGRAMS_FOR_CONDITIONS_TOO_TRUE_OR_FALSE,
                                                  INPUT_VECTOR_1D_STATISTICS_path,
                                                  INPUT_1D_STATISTICS_TYPE_VECTOR,
                                                  INPUT_VECTOR_histogram_jpeg_file_name_overall,
                                                  INPUT_VECTOR_histogram_txt_file_name_overall,
                                                  INPUT_Analysis_Title,
                                                  INPUT_OVERWRITE_or_SKIP_if_file_exists,
                                                  PROJECT_DATA_VXX_COPY,
                                                  INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE,
                                                  INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE,
                                                  INPUT_VECTOR_VXX_Title_Type_Text,
                                                  INPUT_VECTOR_1D_STATISTICS_TABLES_path,
                                                  brk
        )
        
        rm(PROJECT_DATA_VXX_COPY)
        # my_xlab1<-INPUT_VECTOR_xlab_01[i]
        my_xlab1<-substr(colnames(XYARR)[4],7,nchar(colnames(XYARR)[4]))
        cn<-colnames(XYARR)
        t1<-regexpr("MEAN",cn[3])
        t2<-regexpr("STDEV",cn[3])
        t3<-regexpr("SIGNAL",cn[3])
        # t4<-regexpr("MAX",cn[3])
        # t5<-regexpr("MIN",cn[3])
        avg<-as.numeric(substr(cn[3],(t1[1]+4),(t2[1]-1)))
        stdev<-as.numeric(substr(cn[3],(t2[1]+5),(t3[1]-1)))
        # max_val<-as.numeric(substr(cn[3],(t4[1]+3),(t5[1]-1)))
        # min_val<-as.numeric(substr(cn[3],(t5[1]+3),nchar(cn[3])))
        # data<-list(XYARR,lab=my_xlab1,avg=avg,stdev=stdev,max_val=max_val,min_val=min_val)
        # data<-list(XYARR,lab=my_xlab1,avg=avg,stdev=stdev)
        data<-list(XYARR,lab=my_xlab1,avg=avg,stdev=stdev,den_estimate=den_estimate)
        
	    }
	  }else if (sourceSelection == 3){#events
	    if (signal1!="No Selection"){
	      
	      filterEventResult<-dataFilterEventApply(filterSelection,dataFilter1D,dataFilter2D,
	                                              dataFilternD,i,j=0,PROJECT_DATA_VXX,
	                                              dataFilterEvents,appEvtName,callerFilter)
	      
	      PROJECT_DATA_VXX_COPY<-filterEventResult[[1]]
	      val<-filterEventResult[[2]]
	      freqdataFilter$val<-val
	      
	      ii<-(PROJECT_DATA_VXX_COPY[[i]]>=INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE[i])&(PROJECT_DATA_VXX_COPY[[i]]<=INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE[i])
	      dataDenEst<-PROJECT_DATA_VXX_COPY[[i]][ii]
	      den_estimate<-bkde(x=dataDenEst,gridsize=gridSize)
	      
	      INPUT_OVERWRITE_or_SKIP_if_file_exists<-"OVERWRITE"
	      XYARR<-DensityFreqProbabUniversalFunction(j,i,
	                                                INPUT_GENERATE_HISTOGRAMS_FOR_CONDITIONS_TOO_TRUE_OR_FALSE,
	                                                INPUT_VECTOR_1D_STATISTICS_path,
	                                                INPUT_1D_STATISTICS_TYPE_VECTOR,
	                                                INPUT_VECTOR_histogram_jpeg_file_name_overall,
	                                                INPUT_VECTOR_histogram_txt_file_name_overall,
	                                                INPUT_Analysis_Title,
	                                                INPUT_OVERWRITE_or_SKIP_if_file_exists,
	                                                PROJECT_DATA_VXX_COPY,
	                                                INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE,
	                                                INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE,
	                                                INPUT_VECTOR_VXX_Title_Type_Text,
	                                                INPUT_VECTOR_1D_STATISTICS_TABLES_path,
	                                                brk
	      )
	      
	      rm(PROJECT_DATA_VXX_COPY)
	      # my_xlab1<-INPUT_VECTOR_xlab_01[i]
	      my_xlab1<-substr(colnames(XYARR)[4],7,nchar(colnames(XYARR)[4]))
	      cn<-colnames(XYARR)
	      t1<-regexpr("MEAN",cn[3])
	      t2<-regexpr("STDEV",cn[3])
	      t3<-regexpr("SIGNAL",cn[3])
	      # t4<-regexpr("MAX",cn[3])
	      # t5<-regexpr("MIN",cn[3])
	      avg<-as.numeric(substr(cn[3],(t1[1]+4),(t2[1]-1)))
	      stdev<-as.numeric(substr(cn[3],(t2[1]+5),(t3[1]-1)))
	      # max_val<-as.numeric(substr(cn[3],(t4[1]+3),(t5[1]-1)))
	      # min_val<-as.numeric(substr(cn[3],(t5[1]+3),nchar(cn[3])))
	      # data<-list(XYARR,lab=my_xlab1,avg=avg,stdev=stdev,max_val=max_val,min_val=min_val)
	      # data<-list(XYARR,lab=my_xlab1,avg=avg,stdev=stdev)
	      data<-list(XYARR,lab=my_xlab1,avg=avg,stdev=stdev,den_estimate=den_estimate)
	      
	    }
	  }
	  else if (sourceSelection == 1){#last analysed output
	    if (signal1!="No Selection"){
	        XYARR<-data1d_freq_old()
	        freqdataFilter$val<-" "
	        my_xlab1<-substr(colnames(XYARR)[4],7,nchar(colnames(XYARR)[4]))
	        cn<-colnames(XYARR)
	        t1<-regexpr("MEAN",cn[3])
	        t2<-regexpr("STDEV",cn[3])
	        t3<-regexpr("SIGNAL",cn[3])
	        # t4<-regexpr("MAX",cn[3])
	        # t5<-regexpr("MIN",cn[3])
	        avg<-as.numeric(substr(cn[3],(t1[1]+4),(t2[1]-1)))
	        stdev<-as.numeric(substr(cn[3],(t2[1]+5),(t3[1]-1)))
	        # max_val<-as.numeric(substr(cn[3],(t4[1]+3),(t5[1]-1)))
	        # min_val<-as.numeric(substr(cn[3],(t5[1]+3),nchar(cn[3])))
	        # data<-list(XYARR,lab=my_xlab1,avg=avg,stdev=stdev,max_val=max_val,min_val=min_val)
	        data<-list(XYARR,lab=my_xlab1,avg=avg,stdev=stdev,den_estimate=NULL)
	    }
	  }
	  data
	})
	
	freqData<-NULL
	denPlt<-NULL
	freqPlt<-NULL
	output$plot1d_freq<-renderPlot({
	  
	  data<-NULL
	  # data_all<-(data1d_den())
	  data_all<-(data1d_freq())
	  
	  data<-data_all[[1]]
	  
	  if(!is.null(data)){
	    colnames(data)<-c("Lx","Rx","Density","Count")
	    freqData<<-data
	    my_xlab1<-data_all[[2]][1]
	    # my_xlab1<-INPUT_VECTOR_xlab_03[which(INPUT_VECTOR_VXX_Title_Type_Text==isolate(input$sig1))]
	    if (isolate(input$freq_brk)=="20"){
	      my_xlab1<-INPUT_VECTOR_xlab_01[which(INPUT_VECTOR_VXX_Title_Type_Text==isolate(input$sig1))]
	    }else if (isolate(input$freq_brk)=="40"){
	      my_xlab1<-INPUT_VECTOR_xlab_02[which(INPUT_VECTOR_VXX_Title_Type_Text==isolate(input$sig1))]
	    }else if (isolate(input$freq_brk)=="200"){
	      my_xlab1<-INPUT_VECTOR_xlab_03[which(INPUT_VECTOR_VXX_Title_Type_Text==isolate(input$sig1))]
	    }
	    avg<-data_all[[3]][1]
	    stdev<-data_all[[4]][1]
	    # max_val<-data_all[[5]][1]
	    # min_val<-data_all[[6]][1]
	    # data_all_den<-(data1d_den())
	    # dataden<-data_all_den[[1]]
	    data1dDen<-data.frame(x=data$Lx+(data$Rx-data$Lx)/2,y=data$Density,z=rep("Density",length(data$Density)))
	    data1dFreq<-data.frame(x=data$Lx+(data$Rx-data$Lx)/2,y=data$Count,z=rep("Frequency",length(data$Count)))
	    data1d<-rbind(data1dFreq,data1dDen)
	    # colnames(data1d)<-c("x","y","z")
	    my_main<-INPUT_Analysis_Title
	    
	    brks<-c(data$Lx,data$Rx[length(data$Rx)])
	    
	    my_Hist1_T_F <- brks >= avg
	    my_min_index = min(which(my_Hist1_T_F %in% TRUE))
	    
	    my_height<-data1dFreq$y[my_min_index]
	    
	    my_mean_plus_my_sigma = (avg + stdev)
	    my_mean_minus_my_sigma = (avg - stdev)
	    my_text = paste( "(",round(avg, 2)," / " ,round(my_height,2), ")", sep="")
	    pt<-data.frame(x1=avg,y1=0,x2=avg,y2=my_height,z="Frequency")
	    pt1<-data.frame(x1=avg,y1=0,x2=my_mean_plus_my_sigma,y2=0,z="Frequency")
	    pt2<-data.frame(x1=avg,y1=0,x2=my_mean_minus_my_sigma,y2=0,z="Frequency")
	    xt<-rbind(my_mean_plus_my_sigma,my_mean_minus_my_sigma)
	    yt<-rbind(0,0)
	    l<-rbind(paste("+", round(stdev,2), sep=""),paste("-",  round(stdev,2),sep=""))
	    txt<-data.frame(x=xt,y=yt,z=rep("Frequency",length(xt)),label=l)
	    den_estimate<-data_all[[5]]
	    # den_estimate<-cbind(den_estimate,rep("Density",length(den_estimate$x)))
	    # colnames(den_estimate)<-c("x","y","z")
	    
	    denPlt<<- ggplot()+geom_col(data=data1dDen,aes(x=x, y=y),alpha=0.6)+
	      xlab(my_xlab1)+
	      ylab("DENSITY")+
	      ggtitle(my_main)+
	      # annotate("text",x=avg,y=my_height, colour="blue", label=my_text,size=5)+
	      theme(plot.title = element_text(size=20),axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
	            axis.text.x = element_text(size=12),axis.text.y = element_text(size=12),
	            strip.text.x = element_text(size=12, face="bold"))
	    
	    freqPlt<<- ggplot()+geom_col(data=data1dFreq,aes(x=x, y=y),alpha=0.6)+
	      geom_segment(data=pt1,aes(x=x1,y=y1,xend=x2,yend=y2),colour="red", size=1)+
	      geom_text(data=data.frame(x=my_mean_plus_my_sigma,y=0,label=paste("+", round(stdev,2), sep="")),
	                aes(x=x,y=y,label=label),colour="red",size=3,vjust="right",hjust=0)+
	      geom_segment(data=pt2,aes(x=x1,y=y1,xend=x2,yend=y2),colour="red", size=1)+
	      geom_text(data=data.frame(x=my_mean_minus_my_sigma,y=0,label=paste("-",  round(stdev,2),sep="")),
	                aes(x=x,y=y,label=label),colour="red",size=3,vjust="right",hjust=1)+
	      geom_segment(data=pt,aes(x=x1,y=y1,xend=x2,yend=y2),colour="blue", size=1)+
	      
	      geom_text(data=data.frame(x=avg,y=my_height,label=my_text,z="Frequency"), aes(x=x,y=y,label=label),colour="blue",size=3,vjust="left")+
	      xlab(my_xlab1)+
	      ylab("FREQUENCY")+
	      ggtitle(my_main)+
	      # annotate("text",x=avg,y=my_height, colour="blue", label=my_text,size=5)+
	      theme(plot.title = element_text(size=20),axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
	            axis.text.x = element_text(size=12),axis.text.y = element_text(size=12),
	            strip.text.x = element_text(size=12, face="bold"))
	      
	    
	    freqdenPlt<- ggplot()+geom_col(data=data1d,aes(x=x, y=y),alpha=0.6)+
	      geom_segment(data=pt1,aes(x=x1,y=y1,xend=x2,yend=y2),colour="red", size=1)+
	      geom_text(data=txt, aes(x=x,y=y,label=label),colour="red",size=3,vjust="right")+
	      geom_segment(data=pt2,aes(x=x1,y=y1,xend=x2,yend=y2),colour="red", size=1)+
	      geom_segment(data=pt,aes(x=x1,y=y1,xend=x2,yend=y2),colour="blue", size=1)+
	      geom_text(data=data.frame(x=avg,y=my_height,label=my_text,z="Frequency"), aes(x=x,y=y,label=label),colour="blue",size=3,vjust="left")+
	      xlab(my_xlab1)+
	      ylab("DENSITY / FREQUENCY")+
	      ggtitle(my_main)+
	      # annotate("text",x=avg,y=my_height, colour="blue", label=my_text,size=5)+
	      theme(plot.title = element_text(size=20),axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
	            axis.text.x = element_text(size=12),axis.text.y = element_text(size=12),
	            strip.text.x = element_text(size=12, face="bold"))
	            # strip.text.y = element_text(size=12, face="bold"))
	    # facet_grid(z~.,scales="free_y")+
	    
	    
	    if (!is.null(den_estimate)){
	      density1D<<-den_estimate  
	      freqdenPlt<-freqdenPlt+
	        geom_line(data=data.frame(x=den_estimate$x,y=den_estimate$y,z=rep("Density",length(den_estimate$x))),
	                  aes(x=x,y=y),size=0.5)
	      denPlt<<-denPlt+
	        geom_line(data=data.frame(x=den_estimate$x,y=den_estimate$y,z=rep("Density",length(den_estimate$x))),
	                  aes(x=x,y=y),size=0.5)
	    }
	    
	    freqdenPlt<-freqdenPlt+facet_wrap(~z,ncol=1,scales="free_y")+
	      coord_cartesian(xlim=ranges1d_freq$x, ylim=ranges1d_freq$y)
	    
	    denPlt<<-denPlt+coord_cartesian(xlim=ranges1d_freq$x, ylim=ranges1d_freq$y)
	    freqPlt<<-freqPlt+coord_cartesian(xlim=ranges1d_freq$x, ylim=ranges1d_freq$y)
	    
	  }else{
	    freqdenPlt<-NULL
	  }
	  plts1d$freq<-freqdenPlt
	  isolate(plts1d$freq)
	})
	
	output$Val_1D_freq <- renderUI({
	  # req(freqData)
	  hover <- input$plot1d_freq_hover
	  if (isolate(input$load_data)==1)return()
	    
	  if ((is.null(hover))|(is.null(freqData))|(is.null(hover$panelvar1))) return()
	  if (hover$panelvar1=="Density") return()
	  if ((hover$x>max(freqData$Rx))|(hover$x<min(freqData$Lx))) return()
	  
	  loc<-which(freqData$Lx<=hover$x)
	  if (length(loc)>0){
	    i<-max(loc)  
	  }else{
	    return()
	  }
	  
	  
	  if (((hover$y-freqData$Count[i])>10)|(hover$y<0)) return()
	  
	  
	  
	  # calculate point position INSIDE the image as percent of total dimensions
	  # from left (horizontal) and from top (vertical)
	  
	  left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
	  top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
	  # 
	  # # calculate distance from left and bottom side of the picture in pixels
	  # 
	  left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
	  top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
	  
	  # left_px<-hover$xmax
	  # top_px<-hover$ymin
	  # create style property fot tooltip
	  # background color is set so tooltip is a bit transparent
	  # z-index is set so we are sure are tooltip will be on top
	  style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
	                  "left:", left_px + 2, "px; top:", top_px + 2, "px;")
	  
	  # txt<-paste0("Frequency = ",freqData$ct)
	  # actual tooltip created as wellPanel
	  wellPanel(
	    style = style,
	    p(HTML(paste0("<b> Frequency = </b>",freqData$Count[i] , "<br/>"
	    )))
	  )
	})
	
	output$Prob_1D_Density <- renderUI({
	  
	  # brush <- input$plot1d_den_brush
	  brush <- input$plot1d_freq_brush
	  if (isolate(input$load_data)==1)return()
	  if (is.null(brush)) return()
	  if (is.null(brush$panelvar1))return()
	  if (brush$panelvar1!="Density")return()
	  
	  denCurveX<-density1D$x[(density1D$x>=brush$xmin)&(density1D$x<=brush$xmax)]
	  denCurveY<-density1D$y[(density1D$x>=brush$xmin)&(density1D$x<=brush$xmax)]
	  
	  if (max(denCurveY)>brush$ymax){
	    showNotification("Selected region does not enclose density curve. Please check.",duration=2)
	    return()
	  }else if(brush$ymin>0){
	    showNotification("Selected region does not enclose X axis. Please check.",duration=2)
	    return()
	  }
	  
	  denCurveX<-c(denCurveX[1],denCurveX,denCurveX[length(denCurveX)])
	  denCurveY<-c(0,denCurveY,0)
	  
	  xl1<-denCurveX
	  yl1<-denCurveY
	  sml1<-0
	  for (j in 1:(length(xl1)-1)){
	    al<-0.5*((yl1[j+1]-yl1[j])*xl1[j]-(xl1[j+1]-xl1[j])*yl1[j])
	    sml1<-sml1+ al
	  }
	  sml1<-abs(sml1)
	  
	  # calculate point position INSIDE the image as percent of total dimensions
	  # from left (horizontal) and from top (vertical)
	  
	  left_pct <- (brush$xmax - brush$domain$left) / (brush$domain$right - brush$domain$left)
	  top_pct <- (brush$domain$top - brush$ymin) / (brush$domain$top - brush$domain$bottom)
	  # 
	  # # calculate distance from left and bottom side of the picture in pixels
	  # 
	  left_px <- brush$range$left + left_pct * (brush$range$right - brush$range$left)
	  top_px <- brush$range$top + top_pct * (brush$range$bottom - brush$range$top)
	  
	  # left_px<-hover$xmax
	  # top_px<-hover$ymin
	  # create style property fot tooltip
	  # background color is set so tooltip is a bit transparent
	  # z-index is set so we are sure are tooltip will be on top
	  style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
	                  "left:", left_px + 2, "px; top:", top_px + 2, "px;")
	  
	  # getVolume(ker_data)
	  # sp<-Polygon(coords=data.frame(x=denCurveX,y=denCurveY))
	  # sps<-Polygons(list(sp),ID="C1")
	  # spgeom<-SpatialPolygons(list(sps), proj4string=CRS(as.character(NA)))
	  # prob<-gArea(spgeom)
	  
	  # actual tooltip created as wellPanel
	  wellPanel(
	    style = style,
	    p(HTML(paste0("<b> Probability in region: </b>",sml1 , "<br/>"
	    )))
	  )
	})
	
	output$freqDatasource<-renderText({
	  if (plt_freq$y=="No") return()
	  freqdataFilter$val
	})
	
	output$filter_1d_range<-renderText({
	  brush <- input$plot1d_freq_brush
	  dRange<-NULL
	  if (!is.null(brush)) {
	    dRange<-paste0("Xmin = ",brush$xmin, " , Xmax = ", brush$xmax)
	    # dRange<-paste0(dRange,"Ymin = ",brush$ymin, " , Ymax = ", brush$ymax)
	  }
	  dRange
	})
	
	cdnReset1d<-reactiveValues(val="NA") #triggers update text on 1D filter
	updPlot1d<-reactiveValues(val="NA")  #triggers frequency plot update
	
	observeEvent(input$filter_1d,{
	  req(input$filter_1d)
	  if (input$filter_1d=="1"){
	    if (input$load_data=="1"){
	      showNotification("Plant data not loaded.",duration=2)
	      updateSelectInput(session,"filter_id",selected = 2)
	      return()
	    }
	  }
	})
	
	observeEvent(input$filter_1d_clear,{
	  cdnReset1d$val<-"Yes"
	  if (!is.null(appEBCs1d)){
	    EBCs1d<<-appEBCs1d
	    cdnCounter1d<<-length(EBCs1d)  
	    LogicalString1d<<-appLS1d
	  }else{
	    EBCs1d<<-NULL
	    LogicalString1d<<-NULL  
	    cdnCounter1d<<-0
	    cdnCompleteString1d<<-NULL
	    # cdnAllList1d<<-c()
	  }
	  
	  cdnList1d<<-NULL  
	  
	})
	
	observeEvent(input$filter_1d_sel,{
	  cdnReset1d$val<-NULL #change triggers reactivity
	  cdnReset1d$val<-"No"
	  
	})
	
	appEBCs1d<-NULL
	appLS1d<-NULL
	
	observeEvent(input$filter_1d_app,{
	  if (input$load_data==1){
	    showNotification("Project Data not loaded",duration=2)
	    return()
	  }
	  filterSelection<-input$extent
	  if ((length(filterSelection)>1)){
	    showNotification("Incorrect filter selection.",duration=2)
	    return()
	  }
	  #cdnAllList1d<<-c(cdnAllList1d,cdnList1d)
	  cdnList1d<<-NULL
	  LogicalString1d<<-lsTxt1d
	  # cdnCompleteString1d<<-cdnString1d
	  # cdnCounter1d<<-cdnPartCounter1d
	  # 
	  if ((!is.null(EBCs1d))&(!is.null(LogicalString1d))){
	    dataFilter1D<<-EquationBasedDataFilter(EBCs1d,LogicalString1d,PROJECT_DATA_VXX)
	    cdnCompleteString1d<<-cdnString1d
	    cdnCounter1d<<-length(EBCs1d)
	    # cdnCounter1d<<-cdnPartCounter1d
	    appEBCs1d<<-EBCs1d
	    appLS1d<<-LogicalString1d
	  }else{
	    showNotification("Conditions not defined.",duration=2)
	    return()
	  }
	  
	  # visualfilter1D2Dupdate()
	  # updatePlots()
	  updateExtentStatus(4)
	  updateExtentStatus(1)
	  callerFilter<<-"1D_Apply"
	  
	  # plt_freq$y<-"No"
	  # plt_freq$y<-"Yes"
	  
	  
	})
	
	observeEvent(input$filter_1d_reset,{
	  
	  # updPlot1d$val<-"NA"
	  dataFilter1D<<-NULL
	  callerFilter<<-"Reset"
	  # plt_freq$y<-"No"
	  # plt_freq$y<-"Yes"
	  
	  cdnReset1d$val<-"Yes"
	  updateExtentStatus(0)
	  EBCs1d<<-c()
	  
	  cdnCounter1d<<-0
	  cdnCompleteString1d<<-NULL
	  # cdnAllList1d<<-c()
	  cdnList1d<<-NULL
	  
	  LogicalString1d<<-NULL
	  appEBCs1d<<-NULL
	  appLS1d<<-NULL
	})
	
	EBCs1d<-c()               #all 1d conditions applied
	LogicalString1d<-NULL     #logical string
	
	lsTxt1d<-NULL             #temporary logical string
	
	cdnCounter1d<-0           #Applied condition counter
	cdnPartCounter1d<-0       #temporary counter used for selecting conditions graphically
	cdnString1d<-NULL         #condition string to display before each apply action
	cdnCompleteString1d<-NULL #overall condition string applied 
	cdnList1d<-c()            #vector for conditions generated through graphical selection
	
	
	observeEvent(input$filter_1d_transfer,{
	  if (length(EBCs1d)==0){
	    showNotification("Conditions not defined.",duration=2)
	    return()
	  }else{
	    txt<-LogicalString1d
	    cdnNum<<-extractcdnNum(cdnAllVis)
	    for (i in 1:length(EBCs1d)){
	      cdnNum<<-cdnNum+1
	      
	      cdn<-substr(EBCs1d[i],(regexpr("=",EBCs1d[i],fixed=TRUE)[1]+1),nchar(EBCs1d[i]))
	      ebc<-substr(EBCs1d[i],1,(regexpr("=",EBCs1d[i],fixed=TRUE)[1]-1))
	      cdnAllV[[paste0("EBC",cdnNum)]]<<-trimws(cdn)
	      cdnAllVis[[paste0("EBC",cdnNum)]]<<-trimws(cdn)
	      cdnDes[[paste0("EBC",cdnNum)]]<<-"# 1D Filter"
	      txt<-gsub(paste0(" ",trimws(ebc)," "),paste0(" EBC",cdnNum," "),txt,fixed=TRUE)
	    }
	    updateSelectInput(session,"cdns",choices=cdnAllVis,selected="No Selection")
	    showCdn$val<-NULL
	    cdnCnt$val<-cdnNum
	    allLS[[paste0("LS",length(allLS))]]<<-txt
	    updateSelectInput(session,"LS",choices=allLS,selected="No Selection")
	    lsCnt$val<-length(allLS)
	    showNotification("1D filter transferred to Boundary condtions.",duration=2)
	  }
	})

	output$filter_1d_cdns<-renderText({
	  req(cdnReset1d$val)
	  xmin<-NA
	  xmax<-NA
	  xmin<-isolate(input$filter_1d_xmin)
	  xmax<-isolate(input$filter_1d_xmax)
	  
	  cdnTxt1d<-NULL
	  dispcdnTxt1d<-NULL
	  if (cdnReset1d$val=="No"){
	    # if (!is.null(appEBCs1d)){
	    #   cdnTxt1d<-c(EBCs1d," \n")
	    # }
	    i<-which(INPUT_VECTOR_VXX_Title_Type_Text==isolate(input$sig1))
	    brush <- isolate(input$plot1d_freq_brush)
	    if (is.null(brush)){
	      showNotification("Select rectangle on plot.",duration=2)
	      return()
	    }
      xminSel<-ifelse(!is.na(xmin),xmin,round(brush$xmin,2))
      xmaxSel<-ifelse(!is.na(xmax),xmax,round(brush$xmax,2))
	      
	    if (input$filter_1d==1){
	      if (brush$ymin>0){
	        showNotification("X axis not selected in rectangle",duration=2)
	        return()
	      }
	      if (isolate(input$filter_1d_option)==1){
	        relType=1
	        cdn<-genCondition(i,xminSel,xmaxSel,relType)
	        cdnList1d<<-c(cdnList1d,"NEW_INC",cdn)
	      }else{
	        relType=0
	        cdn<-genCondition(i,xminSel,xmaxSel,relType)
	        cdnList1d<<-c(cdnList1d,"NEW_EXC",cdn)
	      }
	    }
	    
	    lsTxt1d<<-"( "
	    
	    if (length(cdnList1d)>1){
	      prevCdn1d<-cdnList1d[1]
	      nxtCdn1d<-NULL
	      if ((!is.null(cdnCompleteString1d))|(!is.null(appEBCs1d))){
	        cdnPartCounter1d<<-cdnCounter1d
	      }
	      else{
	        cdnPartCounter1d<<-0
	      }
	      
	      for (i in 2:length(cdnList1d)){
	        if ((cdnList1d[i]!="NEW_INC")&(cdnList1d[i]!="NEW_EXC")){
	          cdnPartCounter1d<<-cdnPartCounter1d+1
	          cdnTxt1d<-paste0(cdnTxt1d,"1D-EBC", cdnPartCounter1d, "= " ,cdnList1d[i],"\n")
	          EBCs1d<<-c(EBCs1d,paste0("1D-EBC", cdnPartCounter1d, "= " ,cdnList1d[i]))
	          lsTxt1d<<-paste0(lsTxt1d,"1D-EBC",cdnPartCounter1d)
	          
	          if ((i<length(cdnList1d))&(prevCdn1d=="NEW_INC")){
	            lsTxt1d<<-paste0(lsTxt1d," & ")
	          }
	          else if ((i<length(cdnList1d))&(prevCdn1d=="NEW_EXC")){
	            lsTxt1d<<-paste0(lsTxt1d," | ")
	          }
	        }else if((cdnList1d[i]=="NEW_INC")|(cdnList1d[i]=="NEW_EXC")){
	          nxtCdn1d<-cdnList1d[i]
	          lsTxt1d<<-substr(lsTxt1d,1,(nchar(lsTxt1d)-3))
	        }
	        if ((!is.null(nxtCdn1d))&(i<length(cdnList1d))){
	          if ((prevCdn1d=="NEW_INC")&(nxtCdn1d=="NEW_INC")){
	            lsTxt1d<<-paste0(lsTxt1d," ) | ( ") 
	          }else if ((prevCdn1d=="NEW_INC")&(nxtCdn1d=="NEW_EXC")){
	            lsTxt1d<<-paste0(lsTxt1d," ) & ( ") 
	          }else if ((prevCdn1d=="NEW_EXC")&(nxtCdn1d=="NEW_EXC")){
	            lsTxt1d<<-paste0(lsTxt1d," ) & ( ") 
	          }else if ((prevCdn1d=="NEW_EXC")&(nxtCdn1d=="NEW_INC")){
	            lsTxt1d<<-paste0(lsTxt1d," ) & ( ") 
	          }
	          prevCdn1d<-nxtCdn1d
	          nxtCdn1d<-NULL
	        }else if (i==length(cdnList1d)){
	          lsTxt1d<<-paste0(lsTxt1d," )")
	        }  
	      }
	      if (!is.null(cdnCompleteString1d)){
	        cdnTxt1d<-paste0(cdnCompleteString1d," \n",cdnTxt1d)
	      }else{
	        cdnTxt1d<-paste0("Conditions - 1D Filter: \n"  ,cdnTxt1d)
	      }
	      
	      # 
	      # if (!is.null(LogicalString1d)){
	      #   lsTxt1d<<-paste0("(",LogicalString1d," & ",lsTxt1d,")")
	      # }
	      
	      filterSelection<-ifelse(!is.null(isolate(input$extent)),isolate(input$extent),0)
	      
        ebccheckOutput<-checkEBCs(caller=1,filterSelection=filterSelection,
                                  EBCs1d=EBCs1d,LogicalString1d=LogicalString1d,
                                  EBCs2d=EBCs2d,LogicalString2d=LogicalString2d,
                                  EBCs=EBCs,LogicalString=LogicalString,
                                  cdnPartCounter=cdnPartCounter1d,cdnTxt=cdnTxt1d,lsTxt=lsTxt1d)

        EBCs1d<<-ebccheckOutput[[1]]
        LogicalString1d<<-ebccheckOutput[[2]]
        cdnTxt1d<-ebccheckOutput[[3]]
        dispcdnTxt1d<-ebccheckOutput[[4]]
        lsTxt1d<<-ebccheckOutput[[5]]
	        
        cdnString1d<<-cdnTxt1d 
	    }
	    # eval(parse(text=EquationBasedConditions(EBCs,LogicalString)))
	    # cdnListShow$val<-cdnTxt
	    
	  }
	  # cdnListShow1d$val<-cdnTxt1d
	  dispcdnTxt1d
	})
	
	observeEvent(input$plot1d_freq_dblclick, {
	  brush <- input$plot1d_freq_brush
	  if (!is.null(brush)) {
	    ranges1d_freq$x <- c(brush$xmin, brush$xmax)
	    ranges1d_freq$y <- c(brush$ymin, brush$ymax)
	  }else {
	    ranges1d_freq$x <- NULL
	    ranges1d_freq$y <- NULL
	  }
	  
	})
	
	output$plot1d_freq_d<-downloadHandler(
	  filename = function(){
	    if ((is.null(ranges1d_freq$x))&(is.null(ranges1d_freq$y))){
	      paste0(input$sel1,"_",input$sel2,"_FREQUENCY_",input$sig1,"_",input$freq_brk,"_breaks.pdf")
	    }else{
	      paste0(input$sel1,"_",input$sel2,"_FREQUENCY_",input$sig1,"_Xlim_",round(ranges1d_freq$x[1]),"_",
	             round(ranges1d_freq$x[2]),"_Ylim_",round(ranges1d_freq$y[1]),"_",round(ranges1d_freq$y[2]), "_" ,input$freq_brk,"_breaks.pdf")
	    }
	    
	  },
	  content=function(file){
	    plt<-NULL
	    if (input$download_freq==1){
	      plt<-freqPlt
	    }else if(input$download_freq==2){
	      plt<-denPlt
	    }else if (input$download_freq==3){
	      plt<-plts1d$freq  
	    }
	    if (!is.null(plt)){
	      pdf(file=file,width=15, height=7)
	      plt<-plt+annotate("text", Inf, Inf, label = freqdataFilter$val, hjust = 1, vjust = 1)
	      plt<-plt+annotate("text", -Inf, Inf, label = paste0("Creation Date: ",Sys.Date()), hjust = 0, vjust = 1)
	      plot(plt)
	      dev.off()  
	    }
	  }
	)

	}
	
	#1D ECDF
	{
	data1d_ecdf_old<-function(){
	  pa<-isolate(path_to_tables())
	  INPUT_TXT_FILE<-paste0(pa,isolate(input$last_ext))
	  PROJECT_DATA_VXX_LAST_1 <- fread(INPUT_TXT_FILE, 
	                                   skip = 0, 
	                                   dec=".", 
	                                   na.strings=c("NULL", "ERROR", "-1.#INF", "1.#INF", "NA"),sep=";" )
	  return(PROJECT_DATA_VXX_LAST_1)
	}
	
	conditionscheckECDF<-function(){
	  ok<-TRUE
	  if (input$stat=="ECDF"){
	    if (((input$load_data==2)|(input$load_data==3))&&(input$sig1=="No Selection")){
	      showNotification("Please select Signal 1.",duration=2)
	      ok<-FALSE
	    }else if((input$load_data==1) &&(input$last_ext=="No Selection")){
	      showNotification("Please select last analysis.",duration=2)
	      ok<-FALSE
	    }else if (input$ecdf_check=="No Selection"){
	      showNotification("Please select plot type.",duration=2)
	      ok<-FALSE
	    }else{
	      # id<<-NULL
	      # id<<-showNotification("Generating ECDF Plot.",duration=0)
	      # plt_ecdf$y<-"No"
	      # plt_ecdf$y<-"Yes"
	    }
	  }
	  return(ok)
	}
	ecdfdataFilter<-reactiveValues(val=NULL)
	
	data1d_ecdf<-reactive({
	 
	  data<-NULL
	  if (plt_ecdf$y=="No")return()
	  if (!isolate(conditionscheckECDF()))return()
	  
	  # filterSelection<-ifelse(!is.null(isolate(input$extent)),isolate(input$extent),0)
	  filterSelection<-ifelse(!is.null((input$extent)),(input$extent),0)
	  signal1<-isolate(input$sig1)
	  sourceSelection<-isolate(input$load_data)
	  # sourceSelection<-(input$load_data)
	  ecdfCheck<-isolate(input$ecdf_check)
	  
	  
	  # if ((sourceSelection==3)&(is.null(appEvt))&(is.null(dataFilterEvents)))return
	  
	  if (!isolate(checkExtent())){
	    ecdfdataFilter$val<-NULL
	    # showNotification("Filter not defined.",duration=2)
	    return()  
	  }else{
	    id<<-NULL
	    id<<-showNotification("Generating ECDF Plot.",duration=NULL)
	  }
	  
	  i<-which(INPUT_VECTOR_VXX_Title_Type_Text==signal1)
	  
	  if (ecdfCheck=="1-ECDF"){
	    j<-which(INPUT_1D_STATISTICS_TYPE_VECTOR=="1-ECDF")  
	    if (length(j)==0){
	      INPUT_1D_STATISTICS_TYPE_VECTOR=c(INPUT_1D_STATISTICS_TYPE_VECTOR,"1-ECDF")
	      j<-which(INPUT_1D_STATISTICS_TYPE_VECTOR=="1-ECDF")
	    }
	  }else if(ecdfCheck=="ECDF"){
	    j<-which(INPUT_1D_STATISTICS_TYPE_VECTOR=="ECDF")  
	    if (length(j)==0){
	      INPUT_1D_STATISTICS_TYPE_VECTOR=c(INPUT_1D_STATISTICS_TYPE_VECTOR,"ECDF")
	      j<-which(INPUT_1D_STATISTICS_TYPE_VECTOR=="ECDF")
	    }
	  }
	  
	  
	  if(length(INPUT_VECTOR_VXX_Title_Type_Text[i]) == 1)
	  {
	    my_xlab = paste(round(INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE[i],2), " <= " , INPUT_VECTOR_VXX_Title_Type_Text[i], 
	                    " <= ", round(INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE[i],2), sep="")
	  }
	  else
	  {
	    my_xlab = paste(round(INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE[i],2), " <= " , 
	                    INPUT_VECTOR_VXX_Title_Type_Text[i], " <=", 
	                    round(INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE[i],2), " ( X:=", INPUT_VECTOR_VXX_Title_Type_Text[i], " )" , sep="")
	  }
	  
	  if (sourceSelection == 2){#All Data
	    if (signal1!="No Selection"){
	      
	      filterEventResult<-dataFilterEventApply(filterSelection,dataFilter1D,dataFilter2D,
	                                              dataFilternD,i,j=0,PROJECT_DATA_VXX,
	                                              dataFilterEvents=NULL,appEvtName=NULL)
	      
	      PROJECT_DATA_VXX_COPY<-filterEventResult[[1]]
	      val<-filterEventResult[[2]]
	      
	      
	      
	      ecdfdataFilter$val<-val
	      
	      
	      INPUT_OVERWRITE_or_SKIP_if_file_exists<-"OVERWRITE"
	      XYARR<-ECDFuniversalFunction(j,i,
	                                   INPUT_GENERATE_HISTOGRAMS_FOR_CONDITIONS_TOO_TRUE_OR_FALSE,
	                                   INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS,
	                                   INPUT_VECTOR_1D_STATISTICS_path,
	                                   INPUT_1D_STATISTICS_TYPE_VECTOR,
	                                   INPUT_VECTOR_histogram_jpeg_file_name_overall,
	                                   INPUT_VECTOR_histogram_txt_file_name_overall,
	                                   INPUT_Analysis_Title,
	                                   INPUT_OVERWRITE_or_SKIP_if_file_exists,
	                                   PROJECT_DATA_VXX_COPY,
	                                   INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE,
	                                   INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE,
	                                   INPUT_VECTOR_VXX_Title_Type_Text,
	                                   INPUT_VECTOR_1D_STATISTICS_TABLES_path
	      )
	      rm(PROJECT_DATA_VXX_COPY)
	      
	      cn<-colnames(XYARR)
	      mylabECDF = paste("ECDF(", INPUT_VECTOR_VXX_Title_Type_Text[i], ") in %", sep="")
	      mylabECDFmin = paste("1 - ECDF(", INPUT_VECTOR_VXX_Title_Type_Text[i], ") in %", sep="")
	      my_ylab = ifelse(cn[2]=="ECDF",mylabECDF,mylabECDFmin)
	      # data<-list(XYARR,labs=c(my_xlab,my_ylab))
	      
	      # cn<-colnames(XYARR)
	      t1<-regexpr("MEAN",cn[1])
	      t2<-regexpr("STDEV",cn[1])
	      t3<-regexpr("SIGNAL",cn[1])
	      avg<-as.numeric(substr(cn[1],(t1[1]+4),(t2[1]-1)))
	      stdev<-as.numeric(substr(cn[1],(t2[1]+5),(t3[1]-1)))
	      data<-list(XYARR,lab=c(my_xlab, my_ylab),avg=avg,stdev=stdev)
	      
	    }
	  }
	  else if (sourceSelection == 3){#Events
	    if (signal1!="No Selection"){
	      
	      filterEventResult<-dataFilterEventApply(filterSelection,dataFilter1D,dataFilter2D,
	                                              dataFilternD,i,j=0,PROJECT_DATA_VXX,
	                                              dataFilterEvents,appEvtName)
	                                              
	      
	      PROJECT_DATA_VXX_COPY<-filterEventResult[[1]]
	      val<-filterEventResult[[2]]
	      
	      ecdfdataFilter$val<-val
	      
	      
	      INPUT_OVERWRITE_or_SKIP_if_file_exists<-"OVERWRITE"
	      XYARR<-ECDFuniversalFunction(j,i,
	                                   INPUT_GENERATE_HISTOGRAMS_FOR_CONDITIONS_TOO_TRUE_OR_FALSE,
	                                   INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS,
	                                   INPUT_VECTOR_1D_STATISTICS_path,
	                                   INPUT_1D_STATISTICS_TYPE_VECTOR,
	                                   INPUT_VECTOR_histogram_jpeg_file_name_overall,
	                                   INPUT_VECTOR_histogram_txt_file_name_overall,
	                                   INPUT_Analysis_Title,
	                                   INPUT_OVERWRITE_or_SKIP_if_file_exists,
	                                   PROJECT_DATA_VXX_COPY,
	                                   INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE,
	                                   INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE,
	                                   INPUT_VECTOR_VXX_Title_Type_Text,
	                                   INPUT_VECTOR_1D_STATISTICS_TABLES_path
	      )
	      rm(PROJECT_DATA_VXX_COPY)
	      
	      cn<-colnames(XYARR)
	      mylabECDF = paste("ECDF(", INPUT_VECTOR_VXX_Title_Type_Text[i], ") in %", sep="")
	      mylabECDFmin = paste("1 - ECDF(", INPUT_VECTOR_VXX_Title_Type_Text[i], ") in %", sep="")
	      my_ylab = ifelse(cn[2]=="ECDF",mylabECDF,mylabECDFmin)
	      # data<-list(XYARR,labs=c(my_xlab,my_ylab))
	      
	      # cn<-colnames(XYARR)
	      t1<-regexpr("MEAN",cn[1])
	      t2<-regexpr("STDEV",cn[1])
	      t3<-regexpr("SIGNAL",cn[1])
	      avg<-as.numeric(substr(cn[1],(t1[1]+4),(t2[1]-1)))
	      stdev<-as.numeric(substr(cn[1],(t2[1]+5),(t3[1]-1)))
	      data<-list(XYARR,lab=c(my_xlab, my_ylab),avg=avg,stdev=stdev)
	      
	    }
	  }
	  else if (sourceSelection == 1){#Last analysed output
	    if (signal1!="No Selection"){
	       XYARR<-data1d_ecdf_old()
	       ecdfdataFilter$val<-" "
	       
	       cn<-colnames(XYARR)
	       mylabECDF = paste("ECDF(", INPUT_VECTOR_VXX_Title_Type_Text[i], ") in %", sep="")
	       mylabECDFmin = paste("1 - ECDF(", INPUT_VECTOR_VXX_Title_Type_Text[i], ") in %", sep="")
	       my_ylab = ifelse(cn[2]=="ECDF",mylabECDF,mylabECDFmin)
	       
	       t1<-regexpr("MEAN",cn[1])
	       t2<-regexpr("STDEV",cn[1])
	       t3<-regexpr("SIGNAL",cn[1])
	       avg<-as.numeric(substr(cn[1],(t1[1]+4),(t2[1]-1)))
	       stdev<-as.numeric(substr(cn[1],(t2[1]+5),(t3[1]-1)))
	       data<-list(XYARR,lab=c(my_xlab, my_ylab),avg=avg,stdev=stdev)
	       
	    }
	  }
	  data
	})
	
	output$plot1d_ecdf<-renderPlot({
	  data<-(data1d_ecdf())
	  ecdfplot<-NULL
	  if (!is.null(data)){
	    ecdf_data<-data[[1]]
	    colnames(ecdf_data)<-c("x","value")
	    data_plot<-data.frame(x=ecdf_data$x,y=ecdf_data$value*100)
	    my_xlab<-data[[2]][1]
	    my_ylab<-data[[2]][2]
	    
	    avg<-data[[3]][1]
	    stdev<-data[[4]][1]
	    
	    my_Hist1_T_F <- data_plot$x >= avg
	    my_min_index = min(which(my_Hist1_T_F %in% TRUE))
	    my_height = data_plot$y[my_min_index]
	    
	    # my_min_index = min(which(data_plot$x >=avg))
	    # my_height<-data_plot$y[my_min_index]
	    
	    my_mean_plus_my_sigma = (avg + stdev)
	    my_mean_minus_my_sigma = (avg - stdev)
	    my_text = paste( "(",round(avg, 2),
	                     " / " ,round(my_height,2), ")", sep="")
	    pt<-data.frame(x1=avg,y1=0,x2=avg,y2=my_height)
	    pt1<-data.frame(x1=avg,y1=0,x2=my_mean_plus_my_sigma,y2=0)
	    pt2<-data.frame(x1=avg,y1=0,x2=my_mean_minus_my_sigma,y2=0)
	    
	    xt<-rbind(my_mean_plus_my_sigma,my_mean_minus_my_sigma)
	    yt<-rbind(0,0)
	    l<-rbind(paste("+", round(stdev,2), sep=""),paste("-",  round(stdev,2),sep=""))
	    txt<-data.frame(x=xt,y=yt,label=l)
	    
	    my_main = INPUT_Analysis_Title
	    ecdfplot<-ggplot(data_plot,aes(x=x,y=y))+geom_line()+labs(x=my_xlab,y=my_ylab, title=my_main) +
	      geom_segment(data=pt1,aes(x=x1,y=y1,xend=x2,yend=y2),colour="red", size=1)+
	      geom_text(data=txt, aes(x=x,y=y,label=label),colour="red",size=3,vjust="right")+
	      geom_segment(data=pt2,aes(x=x1,y=y1,xend=x2,yend=y2),colour="red", size=1)+
	      geom_segment(data=pt,aes(x=x1,y=y1,xend=x2,yend=y2),colour="blue", size=1)+
	      geom_text(data=data.frame(x=avg,y=my_height,label=my_text), aes(x=x,y=y,label=label),colour="blue",size=3,vjust="left")+
	      # theme(plot.title = element_text(size=20),axis.title.x = element_text(size=15),axis.title.y = element_text(size=15))+
	      theme(plot.title = element_text(size=20),axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
	            axis.text.x = element_text(size=12),axis.text.y = element_text(size=12))+
	      coord_cartesian(xlim=ranges1d_ecdf$x, ylim=ranges1d_ecdf$y)
	  }
	  plts1d$ecdf<-ecdfplot
	  isolate(plts1d$ecdf)
	})
	
	output$ecdfDatasource<-renderText({
	  req(ecdfdataFilter$val)
	  ecdfdataFilter$val
	})
	
	observeEvent(input$plot1d_ecdf_dblclick, {
	  brush <- input$plot1d_ecdf_brush
	  if (!is.null(brush)) {
	    ranges1d_ecdf$x <- c(brush$xmin, brush$xmax)
	    ranges1d_ecdf$y <- c(brush$ymin, brush$ymax)
	  }else {
	    ranges1d_ecdf$x <- NULL
	    ranges1d_ecdf$y <- NULL
	  }
	  
	})
	
	output$plot1d_ecdf_d<-downloadHandler(
	  filename = function(){
	    paste0("PLOT_",input$ecdf_check,"_",input$sig1,".pdf")
	  },
	  content=function(file){
	    pdf(file=file,width=15, height=7)
	    
	    plt<-plts1d$ecdf
	    # if (grepl("FILTERED",ecdfdataFilter$val,fixed=TRUE)){
	      plt<-plt+annotate("text", Inf, Inf, label = ecdfdataFilter$val, hjust = 1, vjust = 1)
	      plt<-plt+annotate("text", -Inf, Inf, label = paste0("Creation Date: ",Sys.Date()), hjust = 0, vjust = 1)
	    # }
	    plot(plt)
	    # plot(plts1d$ecdf)
	    dev.off()
	  }
	)
	}
	
	#1D OPERATING HOURS
	{
	data1d_op_hours_old<-function(){
	  pa<-isolate(path_to_tables())
	  INPUT_TXT_FILE<-paste0(pa,isolate(input$last_ext))
	  PROJECT_DATA_VXX_LAST_1 <- fread(INPUT_TXT_FILE, 
	                                   skip = 0, 
	                                   dec=".", 
	                                   na.strings=c("NULL", "ERROR", "-1.#INF", "1.#INF", "NA"),sep=";" )
	  return(PROJECT_DATA_VXX_LAST_1)
	}
	  
	conditionscheckophours<-function(){
	  ok<-TRUE
	  if (input$stat=="OPERATING_HOURS"){
	    if ((input$load_data==1)&&(input$last_ext=="No Selection")){
	      showNotification("Please select last analysis output",duration=2)
	      ok<-FALSE
	    } else if ((input$load_data==1)&&(input$op_hours_plot=="No Selection")){
	      showNotification("Please select plot type",duration=2)
	      ok<-FALSE
	    } else if (((input$load_data==2)|(input$load_data==3))&&(input$sig1=="No Selection")){
	      showNotification("Please select Signal 1 for Analysis",duration=2)
	      ok<-FALSE
	    } else if (((input$load_data==2)|(input$load_data==3))&&(input$op_hours_plot=="No Selection")){
	      showNotification("Please select plot type",duration=2)
	      ok<-FALSE
	    } else {
	      # id<<-NULL
	      # id<<-showNotification("Generating Operating Hours Plot.",duration=0)
	      # plt_op_hours$y<-"No"
	      # plt_op_hours$y<-"Yes"
	    }
	  }
	  return(ok)
	}
	
	ophoursdataFilter<-reactiveValues(val=NULL)
	
	data1d_op_hours<-reactive({
	  data<-NULL
	  if (plt_op_hours$y=="No")return()
	  
	  if (!isolate(conditionscheckophours()))return()
	  
	  signal1<-isolate(input$sig1)
	  sourceSelection<-isolate(input$load_data)
	  # sourceSelection<-(input$load_data)
	  op_hours_plot<-isolate(input$op_hours_plot)
	  # filterSelection<-ifelse(!is.null(isolate(input$extent)),isolate(input$extent),0)
	  filterSelection<-ifelse(!is.null((input$extent)),(input$extent),0)
	  
	  
	  # if ((sourceSelection==3)&(is.null(appEvt))&(is.null(dataFilterEvents)))return()
	  
	  if (!isolate(checkExtent())){
	    ophoursdataFilter$val<-NULL
	    # showNotification("Filter not defined.",duration=2)
	    return()  
	  }else{
	    id<<-NULL
	    id<<-showNotification("Generating Operating Hours Distribution Plot.",duration=NULL)
	  }
	  
	  i<-which(INPUT_VECTOR_VXX_Title_Type_Text==signal1)
	  
	  if (op_hours_plot=="Operating Hours Accumulate"){
	    j<-which(INPUT_1D_STATISTICS_TYPE_VECTOR=="OP_HOURS_ACCUM")  
	    if (length(j)==0){
	      INPUT_1D_STATISTICS_TYPE_VECTOR<-c(INPUT_1D_STATISTICS_TYPE_VECTOR,"OP_HOURS_ACCUM")
	      j<-which(INPUT_1D_STATISTICS_TYPE_VECTOR=="OP_HOURS_ACCUM")
	    }
	  }else if(op_hours_plot=="Operating Hours Remaining"){
	    j<-which(INPUT_1D_STATISTICS_TYPE_VECTOR=="OP_HOURS_REMAINING")  
	    if (length(j)==0){
	      INPUT_1D_STATISTICS_TYPE_VECTOR<-c(INPUT_1D_STATISTICS_TYPE_VECTOR,"OP_HOURS_REMAINING")
	      j<-which(INPUT_1D_STATISTICS_TYPE_VECTOR=="OP_HOURS_REMAINING")
	    }  
	  }
	  
	  if(length(INPUT_VECTOR_VXX_Title_Type_Text[i]) == 1)
	  {
	    my_xlab = paste(round(INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE[i],2), " <= " , INPUT_VECTOR_VXX_Title_Type_Text[i], 
	                    " <= ", round(INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE[i],2), sep="")
	  }
	  else
	  {
	    my_xlab = paste(round(INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE[i],2), " <= " , 
	                    INPUT_VECTOR_VXX_Title_Type_Text[i], " <=", 
	                    round(INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE[i],2), " ( X:=", INPUT_VECTOR_VXX_Title_Type_Text[i], " )" , sep="")
	  }
	  
	  if (sourceSelection == 2){#All Data
	    if (signal1!="No Selection"){
	      
	      filterEventResult<-dataFilterEventApply(filterSelection,dataFilter1D,dataFilter2D,
	                                              dataFilternD,i,j=0,PROJECT_DATA_VXX,
	                                              dataFilterEvents=NULL,appEvtName=NULL)
	                                              
	      
	      PROJECT_DATA_VXX_COPY<-filterEventResult[[1]]
	      val<-filterEventResult[[2]]
	      
	      ophoursdataFilter$val<-val
	      
	      data_dim<-dim(PROJECT_DATA_VXX_COPY)
	      if (data_dim[1]>0){
	        delta_time<-difftime(PROJECT_DATA_VXX_COPY$MY_DATE[2:data_dim[1]],PROJECT_DATA_VXX_COPY$MY_DATE[1:(data_dim[1]-1)],units = 'secs')
	        time_jump_index<-(delta_time>INPUT_MY_FREQUENCY)
	        if(length(which(time_jump_index))>0){
	          delta_time<-delta_time[!time_jump_index]
	        }
	        # TOTAL_OPERATION_HOURS_OF_THIS_STEP <<- (dim(PROJECT_DATA_VXX_COPY)[1]/3600) * INPUT_MY_FREQUENCY
	        TOTAL_OPERATION_HOURS_OF_THIS_STEP <<- (as.numeric(sum(delta_time))/3600)
	      }
	      INPUT_OVERWRITE_or_SKIP_if_file_exists<-"OVERWRITE"
	      XYARR<-ACCUM_OP_HOURS_REMAININGuniversalFunction(j,i,
	                                                       TOTAL_OPERATION_HOURS_OF_THIS_STEP,
	                                                       INPUT_GENERATE_HISTOGRAMS_FOR_CONDITIONS_TOO_TRUE_OR_FALSE,
	                                                       INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS,
	                                                       INPUT_VECTOR_1D_STATISTICS_path,
	                                                       INPUT_1D_STATISTICS_TYPE_VECTOR,
	                                                       INPUT_VECTOR_histogram_jpeg_file_name_overall,
	                                                       INPUT_VECTOR_histogram_txt_file_name_overall,
	                                                       INPUT_Analysis_Title,
	                                                       INPUT_OVERWRITE_or_SKIP_if_file_exists,
	                                                       PROJECT_DATA_VXX_COPY,
	                                                       INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE,
	                                                       INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE,
	                                                       INPUT_VECTOR_VXX_Title_Type_Text,
	                                                       INPUT_VECTOR_1D_STATISTICS_TABLES_path,range_apply,range_data
	      )
	      rm(PROJECT_DATA_VXX_COPY)
	
	      cn<-colnames(XYARR)
	      
	      mylabECDF = paste("OP_HOURS_ACCUM", " in h", sep="")
	      mylabECDFmin = paste("OP_HOURS_REMAINING", " in h", sep="")
	      my_ylab = ifelse(cn[2]=="OP_HOURS_ACCUM",mylabECDF,mylabECDFmin)
	      data<-list(XYARR,labs=c(my_xlab,my_ylab))
	    }
	  }
	  else if (sourceSelection == 3){#Events
	    if (signal1!="No Selection"){
	      
	      filterEventResult<-dataFilterEventApply(filterSelection,dataFilter1D,dataFilter2D,
	                                              dataFilternD,i,j=0,PROJECT_DATA_VXX,
	                                              dataFilterEvents,appEvtName)
	                                              
	      
	      PROJECT_DATA_VXX_COPY<-filterEventResult[[1]]
	      val<-filterEventResult[[2]]
	      
	      ophoursdataFilter$val<-val
	      data_dim<-dim(PROJECT_DATA_VXX_COPY)
	      if (data_dim[1]>0){
	        delta_time<-difftime(PROJECT_DATA_VXX_COPY$MY_DATE[2:data_dim[1]],PROJECT_DATA_VXX_COPY$MY_DATE[1:(data_dim[1]-1)],units = 'secs')
	        time_jump_index<-(delta_time>INPUT_MY_FREQUENCY)
	        if(length(which(time_jump_index))>0){
	          delta_time<-delta_time[!time_jump_index]
	        }
	        # TOTAL_OPERATION_HOURS_OF_THIS_STEP <<- (dim(PROJECT_DATA_VXX_COPY)[1]/3600) * INPUT_MY_FREQUENCY
	        TOTAL_OPERATION_HOURS_OF_THIS_STEP <<- (as.numeric(sum(delta_time))/3600)
	      }
	      INPUT_OVERWRITE_or_SKIP_if_file_exists<-"OVERWRITE"
	      XYARR<-ACCUM_OP_HOURS_REMAININGuniversalFunction(j,i,
	                                                       TOTAL_OPERATION_HOURS_OF_THIS_STEP,
	                                                       INPUT_GENERATE_HISTOGRAMS_FOR_CONDITIONS_TOO_TRUE_OR_FALSE,
	                                                       INPUT_VECTOR_OF_EVENT_BASED_BOUNDARY_CONDITIONS,
	                                                       INPUT_VECTOR_1D_STATISTICS_path,
	                                                       INPUT_1D_STATISTICS_TYPE_VECTOR,
	                                                       INPUT_VECTOR_histogram_jpeg_file_name_overall,
	                                                       INPUT_VECTOR_histogram_txt_file_name_overall,
	                                                       INPUT_Analysis_Title,
	                                                       INPUT_OVERWRITE_or_SKIP_if_file_exists,
	                                                       PROJECT_DATA_VXX_COPY,
	                                                       INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE,
	                                                       INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE,
	                                                       INPUT_VECTOR_VXX_Title_Type_Text,
	                                                       INPUT_VECTOR_1D_STATISTICS_TABLES_path,range_apply,range_data
	      )
	      rm(PROJECT_DATA_VXX_COPY)
	      
	      cn<-colnames(XYARR)
	      
	      mylabECDF = paste("OP_HOURS_ACCUM", " in h", sep="")
	      mylabECDFmin = paste("OP_HOURS_REMAINING", " in h", sep="")
	      my_ylab = ifelse(cn[2]=="OP_HOURS_ACCUM",mylabECDF,mylabECDFmin)
	      data<-list(XYARR,labs=c(my_xlab,my_ylab))
	    }
	  }
	  else if (sourceSelection == 1){#last analysed output
	    if (input$sig1!="No Selection"){
	      XYARR<-data1d_op_hours_old()
	      ophoursdataFilter$val<-" "
	      cn<-colnames(XYARR)
	      mylabECDF = paste("OP_HOURS_ACCUM", " in h", sep="")
	      mylabECDFmin = paste("OP_HOURS_REMAINING", " in h", sep="")
	      my_ylab = ifelse(cn[2]=="OP_HOURS_ACCUM",mylabECDF,mylabECDFmin)
	      data<-list(XYARR,labs=c(my_xlab,my_ylab))
	      
	    }
	  }
	  data
	})
	
	output$plot1d_op_hours<-renderPlot({
	  
	  data_op_hours<-data1d_op_hours()
	  op_hours<-NULL
	  if (!is.null(data_op_hours)){
	    data<-data.frame(x=data_op_hours[[1]][,1],y=data_op_hours[[1]][,2])
	    my_xlab<-data_op_hours[[2]][1]
	    my_ylab<-data_op_hours[[2]][2]
	    my_main = INPUT_Analysis_Title
	    op_hours<-ggplot(data,aes(x=data[,1],y=data[,2]))+geom_line()+labs(x=my_xlab,y=my_ylab, title=my_main) +
	      theme(plot.title = element_text(size=20),axis.title.x = element_text(size=18),axis.title.y = element_text(size=18),
	            axis.text.x = element_text(size=12),axis.text.y = element_text(size=12))+
	      coord_cartesian(xlim=ranges1d_op_hours$x, ylim=ranges1d_op_hours$y)
	    
	    
	  }
	  plts1d$op_hours<-op_hours
	  isolate(plts1d$op_hours)
	})
	
	output$ophoursDatasource<-renderText({
	  req(ophoursdataFilter$val)
	  ophoursdataFilter$val
	})
	
	output$plot1d_op_hours_d<-downloadHandler(
	  filename = function(){
	    paste0("OPERATING_HOURS_",input$sig1,".pdf")
	  },
	  content=function(file){
	    pdf(file=file,width=15, height=7)
	    plt<-plts1d$op_hours
	    # if (grepl("FILTERED",ophoursdataFilter$val,fixed=TRUE)){
	      plt<-plt+annotate("text", Inf, Inf, label = ophoursdataFilter$val, hjust = 1, vjust = 1)
	      plt<-plt+annotate("text", -Inf, Inf, label = paste0("Creation Date: ",Sys.Date()), hjust = 0, vjust = 1)
	    # }
	    plot(plt)
	    # plot(plts1d$op_hours)
	    dev.off()
	  }
	)
	}
	
	#1D Density - not used - merged with frequency plot
	{
	  ##############
	  # Function to load tables from last analysis
	  ##############
	  data1d_den_old<-reactive({
	    pa<-path_to_tables()
	    
	    INPUT_TXT_FILE<-paste0(pa,input$last_ext)
	    PROJECT_DATA_VXX_LAST_1 <- fread(INPUT_TXT_FILE, 
	                                     skip = 0, 
	                                     dec=".", 
	                                     na.strings=c("NULL", "ERROR", "-1.#INF", "1.#INF", "NA"),sep=";" )
	    PROJECT_DATA_VXX_LAST_1
	  })
	  
	  ##############
	  # Function to load data for new evaluation
	  ##############
	  dendataFilter<-reactiveValues(val=paste0("Data Source : COMPLETE \n","Filtering Source : NONE"))
	  data1d_den<-reactive({
	    
	    data<-NULL
	    den_estimate<-NULL
	    filterSelection<-ifelse(!is.null(isolate(input$extent)),isolate(input$extent),0)
	    signal1<-isolate(input$sig1)
	    sourceSelection<-isolate(input$load_data)
	    denBreaks<-isolate(input$den_brk)
	    freqBreaks<-isolate(input$freq_brk)
	    
	    i<-which(INPUT_VECTOR_VXX_Title_Type_Text==signal1)
	    j<-which(INPUT_1D_STATISTICS_TYPE_VECTOR=="DENSITY")
	    if (length(j)==0){
	      INPUT_1D_STATISTICS_TYPE_VECTOR=c(INPUT_1D_STATISTICS_TYPE_VECTOR,"DENSITY")
	      j<-which(INPUT_1D_STATISTICS_TYPE_VECTOR=="DENSITY")
	    }
	    # if (denBreaks!="No selection"){
	    #   # brk<-as.numeric(denBreaks)
	    #   brk<-as.numeric(freqBreaks)
	    # }
	    if (freqBreaks!="No selection"){
	      # brk<-as.numeric(denBreaks)
	      brk<-as.numeric(freqBreaks)
	    }
	    
	    
	    # if ((length(filterSelection)>1)|(plt_den$y=="No"))return()
	    if ((length(filterSelection)>1)|(plt_freq$y=="No"))return()
	    if ((sourceSelection==3)&(is.null(appEvt))&(is.null(dataFilterEvents)))return()
	    
	    
	    if (sourceSelection == 2){#All data
	      if (signal1!="No Selection"){
	        
	        filterEventResult<-dataFilterEventApply(filterSelection,dataFilter1D,dataFilter2D,
	                                                dataFilternD,i,j=0,PROJECT_DATA_VXX,
	                                                dataFilterEvents=NULL,appEvtName=NULL,
	                                                visual1dFilter=2,updPlot1d="No",
	                                                visual2dFilter=2,updPlot2d="No")
	        
	        PROJECT_DATA_VXX_COPY<-filterEventResult[[1]]
	        val<-filterEventResult[[2]]
	        
	        dendataFilter$val<-val
	        
	        
	        ii<-(PROJECT_DATA_VXX_COPY[[i]]>=INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE[i])&(PROJECT_DATA_VXX_COPY[[i]]<=INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE[i])
	        dataDenEst<-PROJECT_DATA_VXX_COPY[[i]][ii]
	        den_estimate<-bkde(x=dataDenEst,gridsize=gridSize)
	        # den_estimate<-density(x=PROJECT_DATA_VXX_COPY[[i]],kernel="gaussian",n=gridSize)
	        
	        INPUT_OVERWRITE_or_SKIP_if_file_exists<-"OVERWRITE"
	        XYARR<-DensityFreqProbabUniversalFunction(j,i,
	                                                  INPUT_GENERATE_HISTOGRAMS_FOR_CONDITIONS_TOO_TRUE_OR_FALSE,
	                                                  INPUT_VECTOR_1D_STATISTICS_path,
	                                                  INPUT_1D_STATISTICS_TYPE_VECTOR,
	                                                  INPUT_VECTOR_histogram_jpeg_file_name_overall,
	                                                  INPUT_VECTOR_histogram_txt_file_name_overall,
	                                                  INPUT_Analysis_Title,
	                                                  INPUT_OVERWRITE_or_SKIP_if_file_exists,
	                                                  PROJECT_DATA_VXX_COPY,
	                                                  INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE,
	                                                  INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE,
	                                                  INPUT_VECTOR_VXX_Title_Type_Text,
	                                                  INPUT_VECTOR_1D_STATISTICS_TABLES_path,
	                                                  brk
	        )
	        
	        
	        
	        rm(PROJECT_DATA_VXX_COPY)
	        # my_xlab1<-INPUT_VECTOR_xlab_01[i]
	        my_xlab1<-substr(colnames(XYARR)[4],6,nchar(colnames(XYARR)[4]))
	        cn<-colnames(XYARR)
	        t1<-regexpr("MEAN",cn[3])
	        t2<-regexpr("STDEV",cn[3])
	        t3<-regexpr("SIGNAL",cn[3])
	        t4<-regexpr("MAX",cn[3])
	        t5<-regexpr("MIN",cn[3])
	        avg<-as.numeric(substr(cn[3],(t1[1]+4),(t2[1]-1)))
	        stdev<-as.numeric(substr(cn[3],(t2[1]+5),(t3[1]-1)))
	        # max_val<-as.numeric(substr(cn[3],(t4[1]+3),(t5[1]-1)))
	        # min_val<-as.numeric(substr(cn[3],(t5[1]+3),nchar(cn[3])))
	        # t6<-regexpr("XLABEL",cn[4])
	        # xlab<-substr(cn[4],(t6[1]+6),nchar(cn[4]))
	        data<-list(XYARR,lab=my_xlab1,avg=avg,stdev=stdev,den_estimate=den_estimate)
	        # data<-list(XYARR,lab=xlab,avg=avg,stdev=stdev,max_val=max_val,min_val=min_val)
	        
	      }
	    }
	    else if (sourceSelection == 3){#Events
	      if (signal1!="No Selection"){
	        
	        filterEventResult<-dataFilterEventApply(filterSelection,dataFilter1D,dataFilter2D,
	                                                dataFilternD,i,j=0,PROJECT_DATA_VXX,
	                                                dataFilterEvents,appEvtName,
	                                                visual1dFilter=2,updPlot1d="No",
	                                                visual2dFilter=2,updPlot2d="No")
	        
	        PROJECT_DATA_VXX_COPY<-filterEventResult[[1]]
	        val<-filterEventResult[[2]]
	        
	        dendataFilter$val<-val
	        
	        
	        ii<-(PROJECT_DATA_VXX_COPY[[i]]>=INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE[i])&(PROJECT_DATA_VXX_COPY[[i]]<=INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE[i])
	        dataDenEst<-PROJECT_DATA_VXX_COPY[[i]][ii]
	        den_estimate<-bkde(x=dataDenEst,gridsize=gridSize)
	        # den_estimate<-density(x=PROJECT_DATA_VXX_COPY[[i]],kernel="gaussian",n=gridSize)
	        
	        INPUT_OVERWRITE_or_SKIP_if_file_exists<-"OVERWRITE"
	        XYARR<-DensityFreqProbabUniversalFunction(j,i,
	                                                  INPUT_GENERATE_HISTOGRAMS_FOR_CONDITIONS_TOO_TRUE_OR_FALSE,
	                                                  INPUT_VECTOR_1D_STATISTICS_path,
	                                                  INPUT_1D_STATISTICS_TYPE_VECTOR,
	                                                  INPUT_VECTOR_histogram_jpeg_file_name_overall,
	                                                  INPUT_VECTOR_histogram_txt_file_name_overall,
	                                                  INPUT_Analysis_Title,
	                                                  INPUT_OVERWRITE_or_SKIP_if_file_exists,
	                                                  PROJECT_DATA_VXX_COPY,
	                                                  INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE,
	                                                  INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE,
	                                                  INPUT_VECTOR_VXX_Title_Type_Text,
	                                                  INPUT_VECTOR_1D_STATISTICS_TABLES_path,
	                                                  brk
	        )
	        
	        
	        
	        rm(PROJECT_DATA_VXX_COPY)
	        # my_xlab1<-INPUT_VECTOR_xlab_01[i]
	        my_xlab1<-substr(colnames(XYARR)[4],6,nchar(colnames(XYARR)[4]))
	        cn<-colnames(XYARR)
	        t1<-regexpr("MEAN",cn[3])
	        t2<-regexpr("STDEV",cn[3])
	        t3<-regexpr("SIGNAL",cn[3])
	        t4<-regexpr("MAX",cn[3])
	        t5<-regexpr("MIN",cn[3])
	        avg<-as.numeric(substr(cn[3],(t1[1]+4),(t2[1]-1)))
	        stdev<-as.numeric(substr(cn[3],(t2[1]+5),(t3[1]-1)))
	        # max_val<-as.numeric(substr(cn[3],(t4[1]+3),(t5[1]-1)))
	        # min_val<-as.numeric(substr(cn[3],(t5[1]+3),nchar(cn[3])))
	        # t6<-regexpr("XLABEL",cn[4])
	        # xlab<-substr(cn[4],(t6[1]+6),nchar(cn[4]))
	        data<-list(XYARR,lab=my_xlab1,avg=avg,stdev=stdev,den_estimate=den_estimate)
	        # data<-list(XYARR,lab=xlab,avg=avg,stdev=stdev,max_val=max_val,min_val=min_val)
	        
	      }
	    }
	    else if ((sourceSelection == "1")&&(filterSelection==0)){
	      if (signal1!="No Selection"){
	        XYARR<-data1d_den_old()
	        my_xlab1<-substr(colnames(XYARR)[4],6,nchar(colnames(XYARR)[4]))
	        cn<-colnames(XYARR)
	        dendataFilter$val<-" "
	        t1<-regexpr("MEAN",cn[3])
	        t2<-regexpr("STDEV",cn[3])
	        t3<-regexpr("SIGNAL",cn[3])
	        t4<-regexpr("MAX",cn[3])
	        t5<-regexpr("MIN",cn[3])
	        avg<-as.numeric(substr(cn[3],(t1[1]+4),(t2[1]-1)))
	        stdev<-as.numeric(substr(cn[3],(t2[1]+5),(t3[1]-1)))
	        # max_val<-as.numeric(substr(cn[3],(t4[1]+3),(t5[1]-1)))
	        # min_val<-as.numeric(substr(cn[3],(t5[1]+3),nchar(cn[3])))
	        # t6<-regexpr("XLABEL",cn[4])
	        # xlab<-substr(cn[4],(t6[1]+6),nchar(cn[4]))
	        # data<-list(XYARR,lab=xlab,avg=avg,stdev=stdev,max_val=max_val,min_val=min_val)
	        data<-list(XYARR,lab=my_xlab1,avg=avg,stdev=stdev,den_estimate=den_estimate)
	      }
	    }
	    data
	  })
	  
	  ##############
	  # Function to create density plot
	  ##############
	  
	  density1D<-NULL
	  gridSize<-500
	  
	  output$plot1d_den<-renderPlot({
	    
	    data<-NULL
	    data_all<-(data1d_den())
	    data<-data_all[[1]]
	    
	   
	    if(!is.null(data)){
	      colnames(data)<-c("Lx","Rx","Density","Count")
	      my_xlab1<-data_all[[2]][1]
	      if (isolate(input$den_brk)=="20"){
	        my_xlab1<-INPUT_VECTOR_xlab_01[which(INPUT_VECTOR_VXX_Title_Type_Text==isolate(input$sig1))]
	      }else if (isolate(input$den_brk)=="40"){
	        my_xlab1<-INPUT_VECTOR_xlab_02[which(INPUT_VECTOR_VXX_Title_Type_Text==isolate(input$sig1))]
	      }else if (isolate(input$den_brk)=="200"){
	        my_xlab1<-INPUT_VECTOR_xlab_03[which(INPUT_VECTOR_VXX_Title_Type_Text==isolate(input$sig1))]
	      }
	      avg<-data_all[[3]][1]
	      stdev<-data_all[[4]][1]
	      # max_val<-data_all[[5]][1]
	      # min_val<-data_all[[6]][1]
	      
	      i<-which(INPUT_VECTOR_VXX_Title_Type_Text==isolate(input$sig1))
	      
	      # PROJECT_DATA_VXX_COPY<-PROJECT_DATA_VXX[[i]]
	      # PROJECT_DATA_VXX_COPY<-PROJECT_DATA_VXX_COPY[!is.na(PROJECT_DATA_VXX_COPY)]
	      # ii<-(PROJECT_DATA_VXX_COPY>=INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE[i])&(PROJECT_DATA_VXX_COPY<=INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE[i])
	      # PROJECT_DATA_VXX_COPY<-PROJECT_DATA_VXX_COPY[ii]
	      
	      
	      # data1d<-data.frame(x=data[,1]+(data[,2]-data[,1])/2,den=data[,3])
	      data1d<-data.frame(x=data$Lx+(data$Rx-data$Lx)/2,den=data$Density)
	      my_title<-INPUT_Analysis_Title
	      # my_min_index = which((data1d$x >= avg)&(data1d$den>0))[1]
	      
	      brks<-c(data$Lx,data$Rx[length(data$Rx)])
	      
	      my_Hist1_T_F <- brks >= avg
	      my_min_index = min(which(my_Hist1_T_F %in% TRUE))
	      
	      # my_min_index_br = min(which(data$Rx >= avg))
	      # my_min_index = max(which(data1d$x < data$Rx[my_min_index_br]))
	      
	      my_height<-data1d$den[my_min_index]
	      
	      my_mean_plus_my_sigma = (avg + stdev)
	      my_mean_minus_my_sigma = (avg - stdev)
	      my_text = paste( "(",round(avg, 2),
	                       " / " ,round(my_height,4), ")", sep="")
	      pt<-data.frame(x1=avg,y1=0,x2=avg,y2=my_height)
	      pt1<-data.frame(x1=avg,y1=0,x2=my_mean_plus_my_sigma,y2=0)
	      pt2<-data.frame(x1=avg,y1=0,x2=my_mean_minus_my_sigma,y2=0)
	      
	      den_estimate<-data_all[[5]]  
	      # den_estimate<-bkde(x=PROJECT_DATA_VXX_COPY,gridsize=gridSize)
	      if (!is.null(den_estimate)){
	        density1D<<-den_estimate  
	        denPlt<-
	          ggplot()+
	          geom_line(data=data.frame(x=den_estimate$x,y=den_estimate$y),aes(x=x,y=y),size=0.5)
	        
	      }else{
	        denPlt<-ggplot()
	      }
	      
	      
	      # my_xat <- pretty(c(min_val,max_val),25)
	      
	      plts1d$den<-denPlt+
	        # ggplot(data=data.frame(x=den_estimate$x,y=den_estimate$y),size=2)+
	        # geom_line(aes(x=x,y=y),size=0.5)+
	        geom_col(data=data1d,aes(x=x, y=den),alpha=0.6)+
	        
	        
	        # ggplot(data=data.frame(x=PROJECT_DATA_VXX_COPY),aes(x=x))+
	        # geom_histogram(aes(y=..density..),position="identity",bins=isolate(input$den_brk),alpha=0.6,colour="black")+
	        # geom_density(data=data.frame(x=PROJECT_DATA_VXX_COPY),aes(x=x),colour="red",alpha=0.6)+
	        
	        
	        xlab(my_xlab1)+
	        ylab("DENSITY")+ggtitle(my_title)+
	        # geom_segment(data=pt1,aes(x=x1,y=y1,xend=x2,yend=y2),colour="red", size=2)+
	        # annotate("text",x=my_mean_plus_my_sigma,y=0, colour="blue", label=paste("+", round(stdev,2), sep=""),size=5)+
	        # geom_segment(data=pt2,aes(x=x1,y=y1,xend=x2,yend=y2),colour="red", size=2)+
	        # annotate("text",x=my_mean_minus_my_sigma,y=0, colour="blue", label=paste("-",  round(stdev,2),sep=""),size=5)+
	        # geom_segment(data=pt,aes(x=x1,y=y1,xend=x2,yend=y2),colour="blue", size=2)+
	        # annotate("text",x=avg,y=my_height, colour="blue", label=my_text,size=5)+
	        theme(plot.title = element_text(size=20),axis.title.x = element_text(size=15),axis.title.y = element_text(size=15))+
	        # scale_x_continuous(breaks=my_xat)+
	        coord_cartesian(xlim=ranges1d_den$x, ylim=ranges1d_den$y)
	      
	      isolate(plts1d$den)
	    } 
	  })
	  
	  output$denDatasource<-renderText({
	    if (plt_den$y=="No") return()
	    dendataFilter$val
	  })
	  
	  
	  
	  ##############
	  # Function to set zoom extent for the plot based on mouse double click
	  ##############
	  observeEvent(input$plot1d_den_dblclick, {
	    brush <- input$plot1d_den_brush
	    if (!is.null(brush)) {
	      ranges1d_den$x <- c(brush$xmin, brush$xmax)
	      ranges1d_den$y <- c(brush$ymin, brush$ymax)
	    }else {
	      ranges1d_den$x <- NULL
	      ranges1d_den$y <- NULL
	    }
	    
	  })
	  
	  ##############
	  # Function to download pdf of plot
	  ##############
	  output$plot1d_den_d<-downloadHandler(
	    filename = function(){
	      paste0("DENSITY_",input$sig1,"_",input$den_brk,"_breaks.pdf")
	    },
	    content=function(file){
	      pdf(file=file,width=15, height=7)
	      plt<-plts1d$den
	      if (grepl("FILTERED",dendataFilter$val,fixed=TRUE)){
	        plt<-plt+annotate("text", Inf, Inf, label = "FILTERED DATA SOURCE", hjust = 1, vjust = 1)
	      }
	      # plot(plts1d$den)
	      dev.off()
	    }
	  )
	}
	
	#1D Probability - not used
	{
	data1d_prob_old<-function(){
	  pa<-isolate(path_to_tables())
	  INPUT_TXT_FILE<-paste0(pa,isolate(input$last_ext))
	  PROJECT_DATA_VXX_LAST_1 <- fread(INPUT_TXT_FILE, 
	                                   skip = 0, 
	                                   dec=".", 
	                                   na.strings=c("NULL", "ERROR", "-1.#INF", "1.#INF", "NA"),sep=";" )
	  PROJECT_DATA_VXX_LAST_1
	}
	  
	probdataFilter<-reactiveValues(val=paste0("Data Source : COMPLETE \n","Filtering Source : NONE"))
	data1d_prob<-reactive({
	 
	  data<-NULL
	  filterSelection<-ifelse(!is.null(isolate(input$extent)),isolate(input$extent),0)
	  visual1dFilter<-isolate(input$filter_1d)
	  signal1<-isolate(input$sig1)
	  sourceSelection<-isolate(input$load_data)
	  probBreaks<-isolate(input$prob_brk)
	  i<-which(INPUT_VECTOR_VXX_Title_Type_Text==signal1)
	  j<-which(INPUT_1D_STATISTICS_TYPE_VECTOR=="PROBABILITY")
	  if (length(j)==0){
	    INPUT_1D_STATISTICS_TYPE_VECTOR=c(INPUT_1D_STATISTICS_TYPE_VECTOR,"PROBABILITY")
	    j<-which(INPUT_1D_STATISTICS_TYPE_VECTOR=="PROBABILITY")
	  }
	  if (probBreaks!="No Selection"){
	    brk<-as.numeric(probBreaks)
	  }
	  
	  if ((length(filterSelection)>1)|(plt_prob$y=="No"))return()
	  if ((sourceSelection==3)&(is.null(appEvt))&(is.null(dataFilterEvents)))return()
	  
	  if (sourceSelection == 2){#all data
	    if (signal1!="No Selection"){
	      
	      filterEventResult<-dataFilterEventApply(filterSelection,dataFilter1D,dataFilter2D,
	                                              dataFilternD,i,j=0,PROJECT_DATA_VXX,
	                                              dataFilterEvents=NULL,appEvtName=NULL,
	                                              visual1dFilter=2,updPlot1d="No",
	                                              visual2dFilter=2,updPlot2d="No")
	      
	      PROJECT_DATA_VXX_COPY<-filterEventResult[[1]]
	      val<-filterEventResult[[2]]
	      
	      probdataFilter$val<-val
	      INPUT_OVERWRITE_or_SKIP_if_file_exists<-"OVERWRITE"
	      XYARR<-DensityFreqProbabUniversalFunction(j,i,
	                                                INPUT_GENERATE_HISTOGRAMS_FOR_CONDITIONS_TOO_TRUE_OR_FALSE,
	                                                INPUT_VECTOR_1D_STATISTICS_path,
	                                                INPUT_1D_STATISTICS_TYPE_VECTOR,
	                                                INPUT_VECTOR_histogram_jpeg_file_name_overall,
	                                                INPUT_VECTOR_histogram_txt_file_name_overall,
	                                                INPUT_Analysis_Title,
	                                                INPUT_OVERWRITE_or_SKIP_if_file_exists,
	                                                PROJECT_DATA_VXX_COPY,
	                                                INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE,
	                                                INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE,
	                                                INPUT_VECTOR_VXX_Title_Type_Text,
	                                                INPUT_VECTOR_1D_STATISTICS_TABLES_path,
	                                                brk
	      )
	      my_xlab1<-substr(colnames(XYARR)[4],7,nchar(colnames(XYARR)[4]))
	      cn<-colnames(XYARR)
	      t1<-regexpr("MEAN",cn[3])
	      t2<-regexpr("STDEV",cn[3])
	      t3<-regexpr("SIGNAL",cn[3])
	      avg<-as.numeric(substr(cn[3],(t1[1]+4),(t2[1]-1)))
	      stdev<-as.numeric(substr(cn[3],(t2[1]+5),(t3[1]-1)))
	      data<-list(XYARR,lab=my_xlab1,avg=avg,stdev=stdev)
	    }
	  }else if (sourceSelection == 3){#events
	    if (signal1!="No Selection"){
	      
	      filterEventResult<-dataFilterEventApply(filterSelection,dataFilter1D,dataFilter2D,
	                                              dataFilternD,i,j=0,PROJECT_DATA_VXX,
	                                              dataFilterEvents,appEvtName,
	                                              visual1dFilter=2,updPlot1d="No",
	                                              visual2dFilter=2,updPlot2d="No")
	      
	      PROJECT_DATA_VXX_COPY<-filterEventResult[[1]]
	      val<-filterEventResult[[2]]
	      
	      probdataFilter$val<-val
	      INPUT_OVERWRITE_or_SKIP_if_file_exists<-"OVERWRITE"
	      XYARR<-DensityFreqProbabUniversalFunction(j,i,
	                                                INPUT_GENERATE_HISTOGRAMS_FOR_CONDITIONS_TOO_TRUE_OR_FALSE,
	                                                INPUT_VECTOR_1D_STATISTICS_path,
	                                                INPUT_1D_STATISTICS_TYPE_VECTOR,
	                                                INPUT_VECTOR_histogram_jpeg_file_name_overall,
	                                                INPUT_VECTOR_histogram_txt_file_name_overall,
	                                                INPUT_Analysis_Title,
	                                                INPUT_OVERWRITE_or_SKIP_if_file_exists,
	                                                PROJECT_DATA_VXX_COPY,
	                                                INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE,
	                                                INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE,
	                                                INPUT_VECTOR_VXX_Title_Type_Text,
	                                                INPUT_VECTOR_1D_STATISTICS_TABLES_path,
	                                                brk
	      )
	      my_xlab1<-substr(colnames(XYARR)[4],7,nchar(colnames(XYARR)[4]))
	      cn<-colnames(XYARR)
	      t1<-regexpr("MEAN",cn[3])
	      t2<-regexpr("STDEV",cn[3])
	      t3<-regexpr("SIGNAL",cn[3])
	      avg<-as.numeric(substr(cn[3],(t1[1]+4),(t2[1]-1)))
	      stdev<-as.numeric(substr(cn[3],(t2[1]+5),(t3[1]-1)))
	      data<-list(XYARR,lab=my_xlab1,avg=avg,stdev=stdev)
	    }
	  }
	  else if (sourceSelection == 1){#last analysed output
	    if (signal1!="No Selection"){
	      XYARR<-data1d_prob_old()
	      probdataFilter$val<-" "
	      my_xlab1<-substr(colnames(XYARR)[4],7,nchar(colnames(XYARR)[4]))
	      cn<-colnames(XYARR)
	      t1<-regexpr("MEAN",cn[3])
	      t2<-regexpr("STDEV",cn[3])
	      t3<-regexpr("SIGNAL",cn[3])
	      avg<-as.numeric(substr(cn[3],(t1[1]+4),(t2[1]-1)))
	      stdev<-as.numeric(substr(cn[3],(t2[1]+5),(t3[1]-1)))
	      data<-list(XYARR,lab=my_xlab1,avg=avg,stdev=stdev)
	      
	    }
	  }
	  data
	})
	
	output$plot1d_prob<-renderPlot({
	  
	  data<-NULL
	  data_all<-(data1d_prob())
	  data<-data_all[[1]]
	  
	  if(!is.null(data)){
	    colnames(data)<-c("Lx","Rx","Prob","Count")
	    my_xlab1<-data_all[[2]][1]
	    avg<-data_all[[3]][1]
	    stdev<-data_all[[4]][1]
	    
	    if (isolate(input$prob_brk)=="20"){
	      my_xlab1<-INPUT_VECTOR_xlab_01[which(INPUT_VECTOR_VXX_Title_Type_Text==isolate(input$sig1))]
	    }else if (isolate(input$prob_brk)=="40"){
	      my_xlab1<-INPUT_VECTOR_xlab_02[which(INPUT_VECTOR_VXX_Title_Type_Text==isolate(input$sig1))]
	    }else if (isolate(input$prob_brk)=="200"){
	      my_xlab1<-INPUT_VECTOR_xlab_03[which(INPUT_VECTOR_VXX_Title_Type_Text==isolate(input$sig1))]
	    }
	    
	    ct<-data$Count
	    my_probabilities <- ct / sum(ct) * 100
	    my_title<-INPUT_Analysis_Title
	    my_ylab = "probability (= rel. frequency) in %"
	    brks<-c(data$Lx,data$Rx[length(data$Rx)])
	    data1d<-data.frame(x=data$Lx+(data$Rx-data$Lx)/2,pr=my_probabilities)
	    
	    
	    my_Hist1_T_F <- brks >= avg
	    my_min_index = min(which(my_Hist1_T_F %in% TRUE))
	    
	    # my_min_index_br = min(which(data$Rx >= avg))
	    # my_min_index = max(which(data1d$x < data$Rx[my_min_index_br]))
	    
	    my_height<-data1d[my_min_index,2]
	    my_mean_plus_my_sigma = (avg + stdev)
	    my_mean_minus_my_sigma = (avg - stdev)
	    my_text = paste( "(",round(avg, 2),
	                     " / " ,round(my_height,2), ")", sep="")
	    pt<-data.frame(x1=avg,y1=0,x2=avg,y2=my_height)
	    pt1<-data.frame(x1=avg,y1=0,x2=my_mean_plus_my_sigma,y2=0)
	    pt2<-data.frame(x1=avg,y1=0,x2=my_mean_minus_my_sigma,y2=0)
	    
	    xt<-rbind(my_mean_plus_my_sigma,my_mean_minus_my_sigma)
	    yt<-rbind(0,0)
	    l<-rbind(paste("+", round(stdev,2), sep=""),paste("-",  round(stdev,2),sep=""))
	    txt<-data.frame(x=xt,y=yt,label=l)
	    
	    plts1d$prob<- ggplot()+geom_col(data=data1d,aes(x=x, y=pr),alpha=0.6)+
	      geom_segment(data=pt1,aes(x=x1,y=y1,xend=x2,yend=y2),colour="red", size=1)+
	      geom_text(data=txt, aes(x=x,y=y,label=label),colour="red",size=3,vjust="right")+
	      geom_segment(data=pt2,aes(x=x1,y=y1,xend=x2,yend=y2),colour="red", size=1)+
	      geom_segment(data=pt,aes(x=x1,y=y1,xend=x2,yend=y2),colour="blue", size=1)+
	      xlab(my_xlab1)+ylab(my_ylab)+ggtitle(my_title)+
	      geom_text(data=data.frame(x=avg,y=my_height,label=my_text), aes(x=x,y=y,label=label),colour="blue",size=3,vjust="left")+
	      # theme(plot.title = element_text(size=20),axis.title.x = element_text(size=15),axis.title.y = element_text(size=15))+
	      theme(plot.title = element_text(size=20),axis.title.x = element_text(size=15),axis.title.y = element_text(size=15),
	            axis.text.x = element_text(size=12),axis.text.y = element_text(size=12))+
	      coord_cartesian(xlim=ranges1d_prob$x, ylim=ranges1d_prob$y)
	    
	    isolate(plts1d$prob)
	  } 
	})
	
	output$probDatasource<-renderText({
	  if (plt_prob$y=="No") return()
	  probdataFilter$val
	})
	
	observeEvent(input$plot1d_prob_dblclick, {
	  brush <- input$plot1d_prob_brush
	  if (!is.null(brush)) {
	    ranges1d_prob$x <- c(brush$xmin, brush$xmax)
	    ranges1d_prob$y <- c(brush$ymin, brush$ymax)
	  }else {
	    ranges1d_prob$x <- NULL
	    ranges1d_prob$y <- NULL
	  }
	  
	})
	
	output$plot1d_prob_d<-downloadHandler(
	  filename = function(){
	    paste0("PROBABILITY_",input$sig1,"_",input$prob_brk,"_breaks.pdf")
	  },
	  content=function(file){
	    pdf(file=file,width=15, height=7)
	    plot(plts1d$prob)
	    dev.off()
	  }
	)
	
	}
	
	# 2D Graphs
	{
	data2d_xy_graph_old<-function(){
	  pa<-isolate(path_to_tables())
	  
	  INPUT_TXT_FILE<-paste0(pa,isolate(input$last_ext))
	  PROJECT_DATA_VXX_LAST_1 <- fread(INPUT_TXT_FILE, 
	                                   skip = 0, 
	                                   dec=".", 
	                                   na.strings=c("NULL", "ERROR", "-1.#INF", "1.#INF", "NA"),sep=";" )
	  return(PROJECT_DATA_VXX_LAST_1)
	}
	
	conditionscheckXYgraphs<-function(){
	  ok<-TRUE
	  if (input$stat=="X_Y_GRAPHS"){
	    if (((input$load_data==2)|(input$load_data==3)) &&((input$sig1=="No Selection")||(input$sig2=="No Selection"))){
	      showNotification("Please select Signal 1 and Signal 2.")
	      ok<-FALSE
	    }else if (input$sig1==input$sig2){
	      showNotification("Selected Signal 1 and Signal 2 are same. Please check.")
	      ok<-FALSE
	    }else if((input$load_data==1) &&(input$last_ext=="No Selection")){
	      showNotification("Please select last analysis.")
	      ok<-FALSE
	    }else{
	      # id<<-NULL
	      # id<<-showNotification("Generating XY Graph.",duration=0)
	      # plt_xygraph$y<-"No"
	      # plt_xygraph$y<-"Yes"
	    }
	  }
	  return(ok)
	}
	  
	xygraphDatafilter<-reactiveValues(val=NULL)
	data2d_xy_graph<-reactive({
	  
	  data<-NULL
	  if (plt_xygraph$y=="No")return()
	  if(!isolate(conditionscheckXYgraphs()))return()
	 
	  signal1<-isolate(input$sig1)
	  signal2<-isolate(input$sig2)
	  sourceSelection<-isolate(input$load_data)
	  # sourceSelection<-(input$load_data)
	  # filterSelection<-ifelse(!is.null(isolate(input$extent)),isolate(input$extent),0)
	  filterSelection<-ifelse(!is.null((input$extent)),(input$extent),0)
	  i<-which(INPUT_VECTOR_VXX_Title_Type_Text==signal1)
	  j<-which(INPUT_VECTOR_VXX_Title_Type_Text==signal2)
	  
	  
	  # if ((sourceSelection==3)&(is.null(appEvt))&(is.null(dataFilterEvents)))return()
	  
	  if (!isolate(checkExtent())){
	    xygraphDatafilter$val<-NULL
	    # showNotification("Filter not defined.",duration=2)
	    return()  
	  }else{
	    id<<-NULL
	    id<<-showNotification("Generating XY Graphs Plot.",duration=NULL)
	  }
	  
	  
	  if (sourceSelection == 2){# 2- all data
	    if ((signal1!="No Selection")&&(signal2!="No Selection")){
	      if (signal1!=signal2){
	        
	        filterEventResult<-dataFilterEventApply(filterSelection,dataFilter1D,dataFilter2D,
	                                                          dataFilternD,i,j,PROJECT_DATA_VXX,
	                                                          dataFilterEvents=NULL,appEvtName=NULL)
	          
          PROJECT_DATA_VXX_COPY<-filterEventResult[[1]]
          val<-filterEventResult[[2]]
	        
	        xygraphDatafilter$val<-val
	        
	        INPUT_OVERWRITE_or_SKIP_if_file_exists<-"OVERWRITE"
	        MY_X_Y_COUNT<-XYGraphUniversalFunction01(i,j,
	                                                 Correlations_01_X_Y_GRAPHS_TABLES_path,
	                                                 Correlations_01_X_Y_GRAPHS_path,
	                                                 INPUT_Analysis_Title,
	                                                 INPUT_VECTOR_VXX_Title_Type_Underscored_TEXT,
	                                                 INPUT_OVERWRITE_or_SKIP_if_file_exists,
	                                                 PROJECT_DATA_VXX_COPY,
	                                                 INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE,
	                                                 INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE,
	                                                 INPUT_ROUND_TO_NUMBER)
	        my_xlab<-INPUT_VECTOR_VXX_Title_Type_Underscored_TEXT[i]
	        my_ylab<-INPUT_VECTOR_VXX_Title_Type_Underscored_TEXT[j]
	        colnames(MY_X_Y_COUNT)<-c("x","y","my_count")
	        data<-list(MY_X_Y_COUNT,lab=c(my_xlab,my_ylab))
	        
	      }
	    }
	  }
	  else if (sourceSelection == 3){#3 - events
	    if ((signal1!="No Selection")&&(signal2!="No Selection")){
	      if (signal1!=signal2){
	        
	        filterEventResult<-dataFilterEventApply(filterSelection,dataFilter1D,dataFilter2D,
	                                                dataFilternD,i,j,PROJECT_DATA_VXX,
	                                                dataFilterEvents,appEvtName)
	        
	        PROJECT_DATA_VXX_COPY<-filterEventResult[[1]]
	        val<-filterEventResult[[2]]
	        
	        xygraphDatafilter$val<-val
	        
	        INPUT_OVERWRITE_or_SKIP_if_file_exists<-"OVERWRITE"
	        MY_X_Y_COUNT<-XYGraphUniversalFunction01(i,j,
	                                                 Correlations_01_X_Y_GRAPHS_TABLES_path,
	                                                 Correlations_01_X_Y_GRAPHS_path,
	                                                 INPUT_Analysis_Title,
	                                                 INPUT_VECTOR_VXX_Title_Type_Underscored_TEXT,
	                                                 INPUT_OVERWRITE_or_SKIP_if_file_exists,
	                                                 PROJECT_DATA_VXX_COPY,
	                                                 INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE,
	                                                 INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE,
	                                                 INPUT_ROUND_TO_NUMBER)
	        my_xlab<-INPUT_VECTOR_VXX_Title_Type_Underscored_TEXT[i]
	        my_ylab<-INPUT_VECTOR_VXX_Title_Type_Underscored_TEXT[j]
	        colnames(MY_X_Y_COUNT)<-c("x","y","my_count")
	        data<-list(MY_X_Y_COUNT,lab=c(my_xlab,my_ylab))
	        
	      }
	    }
	  }
	  else if (sourceSelection == 1){#last analysis
	    if ((signal1!="No Selection")&&(signal2!="No Selection")){
	      if (signal1!=signal2){
	        xygraphDatafilter$val<-" "
	        MY_X_Y_COUNT<-data2d_xy_graph_old()
	        my_xlab<-colnames(MY_X_Y_COUNT)[1]
	        my_ylab<-colnames(MY_X_Y_COUNT)[2]
	        colnames(MY_X_Y_COUNT)<-c("x","y","my_count")
	        data<-list(MY_X_Y_COUNT,lab=c(my_xlab,my_ylab))
	      }
	    }
	  }
	  data
	})
	
	output$xygraphDatasource<-renderText({
	  req(xygraphDatafilter$val)
	  xygraphDatafilter$val
	})
	
	output$plot2D_x_y_graph<-renderPlot({
	 
	  data<-data2d_xy_graph()
	  
	  xygraph<-NULL
	  
	  if (!is.null(data)){
	    
	    my_xlab<-data[[2]][1]
	    my_ylab<-data[[2]][2]
	  
	    my_axis_text_size = 20
	    my_axis_text_face = "bold"
	    
	    my_axis_title_size = 20
	    my_axis_title_face = "bold"
	    
	    my_title_size = 20
	    my_title_color = "black"
	    my_title_face = "bold"
	    
	    MY_X_Y_COUNT<-data[[1]]
	    
	    my_xlim = c(min(MY_X_Y_COUNT$x),max(MY_X_Y_COUNT$x))
	    my_ylim = c(min(MY_X_Y_COUNT$y),max(MY_X_Y_COUNT$y))
	    my_low_x = min(MY_X_Y_COUNT$x)
	    my_high_x = max(MY_X_Y_COUNT$x)
	    my_low_y = min(MY_X_Y_COUNT$y)
	    my_high_y = max(MY_X_Y_COUNT$y)
	    
	    my_aggregate_x =  aggregate(MY_X_Y_COUNT$x , by=list(sort(MY_X_Y_COUNT$x)), FUN=mean, na.rm=TRUE)
	    my_aggregate_y = aggregate(MY_X_Y_COUNT$y , by=list(sort(MY_X_Y_COUNT$y)), FUN=mean, na.rm=TRUE)
	    my_main_size = 2
	    my_xy_lab_size = 1.5
	    my_xy_axis_values_size = 2
	    my_font_axis = 2
	    my_font_labels = 2
	    
	    if (dim(my_aggregate_x)[1] > 2 )
	    {
	      if (dim(my_aggregate_y)[1] > 2 )
	      {
	        my_width = 1000
	        my_height = 1000
	        my_size = 1
	        my_pch ="."
	        #my_pch = 20
	        
	        my_legend_key_size = 3
	        my_legend_position = "right"
	        
	      }else
	      {
	        my_width = 1000
	        my_height = 500
	        my_size = 40
	        my_pch ="|"
	        
	        #my_legend_position = c(.5, .5)
	        my_legend_key_size = 2
	        my_legend_position = "right"
	      }
	    }else
	    {
	      if (dim(my_aggregate_y)[1] > 2 )
	      {
	        my_width = 1000
	        my_height = 500
	        my_size = 40
	        my_pch ="_"
	        
	        #my_legend_position = c(.5, .5)
	        my_legend_key_size = 2
	        my_legend_position = "right"
	        
	      }else
	      {
	        
	        my_title_size = 15
	        
	        my_width = 750
	        my_height = 500
	        my_size = 20
	        my_pch = 15
	        
	        my_legend_key_size = 2
	        my_legend_position = "right"
	        
	      }
	    }
	    
	    ti<-INPUT_Analysis_Title
	    i<-0
	    while(ti!=""){
	      if (i==0){
	        my_title<-substr(ti,1,60)
	        ti<-substr(ti,61,nchar(ti))
	      }else{
	        if (nchar(ti)>=60){
	          my_title<-paste0(my_title," - \n -",substr(ti,1,60))
	          ti<-substr(ti,61,nchar(ti))  
	        }else{
	          my_title<-paste0(my_title,"- \n - ",ti)
	          ti<-""
	        }
	      }
	      i<-i+1
	    }
	    
	    xygraph<-ggplot(MY_X_Y_COUNT) +
	      geom_point(aes(x, y),pch=my_pch,  size = my_size) +
	      theme(axis.text=element_text(size=my_axis_text_size, face=my_axis_text_face),
	            axis.title=element_text(size=my_axis_title_size,face=my_axis_title_face))+                      
	      theme(plot.title = element_text(size = my_title_size, colour = my_title_color, face=my_title_face))+
	      scale_x_continuous() +
	      scale_y_continuous() +
	      # scale_x_continuous( breaks=c(min(MY_X_Y_COUNT$x), max(MY_X_Y_COUNT$x)) , limits=c(min(MY_X_Y_COUNT$x), max(MY_X_Y_COUNT$x)) ) +
	      # scale_y_continuous( breaks=c(min(MY_X_Y_COUNT$y), max(MY_X_Y_COUNT$y)) , limits=c(min(MY_X_Y_COUNT$y), max(MY_X_Y_COUNT$y)) ) +
	      # labs( title=INPUT_Analysis_Title,
	      labs( title=my_title,
	            x =my_xlab , y = my_ylab)+ coord_cartesian(xlim=ranges2D_x_y_graph$x, ylim=ranges2D_x_y_graph$y)
	  }
	  plts$graph<-xygraph
	  isolate(plts$graph)
	  
	})
	
	observeEvent(input$plot2D_x_y_graph_dblclick, {
	  brush <- input$plot2D_x_y_graph_brush
	  if (!is.null(brush)) {
	    ranges2D_x_y_graph$x <- c(brush$xmin, brush$xmax)
	    ranges2D_x_y_graph$y <- c(brush$ymin, brush$ymax)
	  }else {
	    ranges2D_x_y_graph$x <- NULL
	    ranges2D_x_y_graph$y <- NULL
	  }
	  
	})
	
	output$plot2D_x_y_graph_d<-downloadHandler(
	  filename = function(){
	    paste0(input$sig1,"__", input$sig2,".pdf")
	  },
	  content=function(file){
	    pdf(file,width=10, height=10)
	    plt<-plts$graph
	    # if (grepl("FILTERED",xygraphDatafilter$val,fixed=TRUE)){
	      plt<-plt+annotate("text", Inf, Inf, label = xygraphDatafilter$val, hjust = 1, vjust = 1)
	      plt<-plt+annotate("text", -Inf, Inf, label = paste0("Creation Date: ",Sys.Date()), hjust = 0, vjust = 1)
	    # }
	    plot(plt)
	    # plot(plts$graph)
	    dev.off()
	  }
	)
	}
	
	# 2D KERNAL DENSITY
	{
	
	dataFilter2D<-NULL
	PROJECT_DATA_VXX_FILTERED_2D<-NULL
	
	data2d_xy_ker_den_old<-function(){

	 pa<-isolate(path_to_tables())
	 INPUT_TXT_FILE<-paste0(pa,isolate(input$last_ext))

	  PROJECT_DATA_VXX_LAST_1 <- fread(INPUT_TXT_FILE,
	                                   skip = 0,
	                                   dec=".",
	                                   na.strings=c("NULL", "ERROR", "-1.#INF", "1.#INF", "NA"),sep=";" )
	  return(PROJECT_DATA_VXX_LAST_1)
	}
	
	conditionscheckXYkerneldensity<-function(){
	  ok<-TRUE
	  if (input$stat=="X_Y_KERNEL DENSITY"){
	    if (((input$load_data==2)|(input$load_data==3)) &((input$sig1=="No Selection")|(input$sig2=="No Selection"))){
	      showNotification("Please select Signal 1 and Signal 2.")
	      ok<-FALSE
	    }else if (input$sig1==input$sig2){
	      showNotification("Selected Signal 1 and Signal 2 are same. Please check.")
	      ok<-FALSE
	    }else if((input$load_data==1) &&(input$last_ext=="No Selection")){
	      showNotification("Please select last analysis.")
	      ok<-FALSE
	    }else{
	      ct1<<-NULL
	      ct_den<<-NULL
	      ct_den_levels<<-NULL
	      min_den<<-NULL
	      max_den<<-NULL
	      den_breaks<-reactiveValues(br=NULL)
	      # id<<-NULL
	      density_data<<-NULL
	      # id<<-showNotification("Generating XY Kernel Density Plots.",duration=0)
	      
	      # plt_xykerden$y<-"No"  
	      # plt_xykerden$y<-"Yes"  
	    }
	  }
	  return(ok)
	}
	
	kerneldendataFilter<-reactiveValues(val=NULL)
	
	data2d_xy_ker_den<-reactive({
	  ker_data<-NULL
	  if ((plt_xykerden$y=="No"))return()
	  if (!isolate(conditionscheckXYkerneldensity()))return()
	  
	  signal1<-isolate(input$sig1)
	  signal2<-isolate(input$sig2)
	  sourceSelection<-isolate(input$load_data)
	  # sourceSelection<-(input$load_data)
	  # filterSelection<-ifelse(!is.null(isolate(input$extent)),isolate(input$extent),0)
	  filterSelection<-ifelse(!is.null(input$extent),input$extent,0)
	  visual2dFilter<-isolate(input$Kdata_filter)
	  
	  i<-which(INPUT_VECTOR_VXX_Title_Type_Text==signal1)
	  j<-which(INPUT_VECTOR_VXX_Title_Type_Text==signal2)
	  
	  # if ((sourceSelection==3)&(is.null(appEvt))&(is.null(dataFilterEvents)))return()
	  
	  if (!isolate(checkExtent())){
	    kerneldendataFilter$val<-NULL
	    # showNotification("Filter not defined.",duration=2)
	    return()  
	  }else{
	    if (is.null(id)){
	      id<<-showNotification("Generating XY Kernel Density Plots.",duration=0)  
	    }
	    
	  }
	  
	   if (sourceSelection == 2){#all data
	    if ((signal1!="No Selection")&&(signal2!="No Selection")){
	       if (signal1!=signal2){
	         
	         filterEventResult<-dataFilterEventApply(filterSelection,dataFilter1D,dataFilter2D,
	                                                 dataFilternD,i,j,PROJECT_DATA_VXX,
	                                                 dataFilterEvents=NULL,appEvtName=NULL,callerFilter)
	         
	         PROJECT_DATA_VXX_COPY<-filterEventResult[[1]]
	         val<-filterEventResult[[2]]
	         xrange<-c(min(PROJECT_DATA_VXX_COPY[[i]]),max(PROJECT_DATA_VXX_COPY[[i]]))
	         yrange<-c(min(PROJECT_DATA_VXX_COPY[[j]]),max(PROJECT_DATA_VXX_COPY[[j]]))
	         
	         kerneldendataFilter$val<-val
	         
           PROJECT_DATA_VXX_COPY<-PROJECT_DATA_VXX_COPY[(!is.na(PROJECT_DATA_VXX_COPY[[i]]))&(!is.na(PROJECT_DATA_VXX_COPY[[j]])),]
           
           INPUT_OVERWRITE_or_SKIP_if_file_exists<-"OVERWRITE"
           ker_data<-XYKernelUniversalFunction02(i,j,
                                                  Correlations_02_X_Y_KERNEL_DENSITY_TABLES_path,
                                                  Correlations_02_X_Y_KERNEL_DENSITY_path,
                                                  INPUT_Analysis_Title,
                                                  INPUT_VECTOR_VXX_Title_Type_Underscored_TEXT,
                                                  INPUT_OVERWRITE_or_SKIP_if_file_exists,
                                                  PROJECT_DATA_VXX_COPY,
                                                  INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE,
                                                  INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE,
                                                  INPUT_ROUND_TO_NUMBER
            )
         
	          
	          my_xlab<-INPUT_VECTOR_VXX_Title_Type_Text[i]
	          my_ylab<-INPUT_VECTOR_VXX_Title_Type_Text[j]
	          ker_data[[3]]<-c(my_xlab,my_ylab)
	          xdata<-PROJECT_DATA_VXX_COPY[[i]]
	          ydata<-PROJECT_DATA_VXX_COPY[[j]]
	          ker_data[[4]]<-xdata
	          ker_data[[5]]<-ydata
	        }
	      }
	   }
	  else if (sourceSelection == 3){#events
	    if ((signal1!="No Selection")&&(signal2!="No Selection")){
	      if (signal1!=signal2){
	        
	        filterEventResult<-dataFilterEventApply(filterSelection,dataFilter1D,dataFilter2D,
	                                                dataFilternD,i,j,PROJECT_DATA_VXX,
	                                                dataFilterEvents,appEvtName,callerFilter)
	        
	        PROJECT_DATA_VXX_COPY<-filterEventResult[[1]]
	        val<-filterEventResult[[2]]
	        
	        xrange<-c(min(PROJECT_DATA_VXX_COPY[[i]]),max(PROJECT_DATA_VXX_COPY[[i]]))
	        yrange<-c(min(PROJECT_DATA_VXX_COPY[[j]]),max(PROJECT_DATA_VXX_COPY[[j]]))
	        kerneldendataFilter$val<-val
	        
	        PROJECT_DATA_VXX_COPY<-PROJECT_DATA_VXX_COPY[(!is.na(PROJECT_DATA_VXX_COPY[[i]]))&(!is.na(PROJECT_DATA_VXX_COPY[[j]])),]
	        INPUT_OVERWRITE_or_SKIP_if_file_exists<-"OVERWRITE"
	        ker_data<-XYKernelUniversalFunction02(i,j,
	                                              Correlations_02_X_Y_KERNEL_DENSITY_TABLES_path,
	                                              Correlations_02_X_Y_KERNEL_DENSITY_path,
	                                              INPUT_Analysis_Title,
	                                              INPUT_VECTOR_VXX_Title_Type_Underscored_TEXT,
	                                              INPUT_OVERWRITE_or_SKIP_if_file_exists,
	                                              PROJECT_DATA_VXX_COPY,
	                                              INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE,
	                                              INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE,
	                                              INPUT_ROUND_TO_NUMBER
	        )
	        
	        
	        my_xlab<-INPUT_VECTOR_VXX_Title_Type_Text[i]
	        my_ylab<-INPUT_VECTOR_VXX_Title_Type_Text[j]
	        ker_data[[3]]<-c(my_xlab,my_ylab)
	        xdata<-PROJECT_DATA_VXX_COPY[[i]]
	        ydata<-PROJECT_DATA_VXX_COPY[[j]]
	        ker_data[[4]]<-xdata
	        ker_data[[5]]<-ydata
	      }
	    }
	  }
	  else if (sourceSelection == 1){
	      if ((signal1!="No Selection")&(signal2!="No Selection")){
	        if (signal1!=signal2){
	          den<-data2d_xy_ker_den_old()
	          MY_X_Y_KERNEL_DENSITY<-data.table(den[den$STATUS==0,1:5])
	          colnames(MY_X_Y_KERNEL_DENSITY)<-c("x","y","z","log10","st")
	          MY_X_Y_COUNT<-data.table(den[den$STATUS==1,1:3])
	          colnames(MY_X_Y_COUNT)<-c("x","y","z")
	          xrange<-c(min(MY_X_Y_COUNT$x),max(MY_X_Y_COUNT$x))
	          yrange<-c(min(MY_X_Y_COUNT$y),max(MY_X_Y_COUNT$y))

	          my_xlab<-colnames(den)[1]
	          my_ylab<-colnames(den)[2]
	          ker_data<-list(MY_X_Y_KERNEL_DENSITY,MY_X_Y_COUNT,lab=c(my_xlab,my_ylab))
	          ker_data[[4]]<-c(xrange,yrange)
	        }
	      }
	    }
	  ker_data
	})
	
	output$Kdata_source<-renderText({
	  if (plt_xykerden$y=="No")return() 
	  kerneldendataFilter$val
	})
	
	observeEvent(input$Kdata_filter,{
	  if (input$Kdata_filter==1){
	    if (input$load_data==1){
	      showNotification("Plant data not loaded.",duration=2)
	      updateSelectInput(session,"Kdata_filter",selected=2)
	      return()
	    }
	  }
	  
	})
	
  den_breaks<-reactiveValues(br=NULL)
 
  typeofSignals<-"Binary"
  
	output$plot2D_x_y_ker_den1<-renderPlot({
	  
	  if (plt_xykerden$y=="No")
	    return()
	  
	  # ker_data<-copy(density_data)
	  kerdenPlt1<-NULL
	  ker_data<-data2d_xy_ker_den()
	  # plotcntDensity<<-pretty(range(ker_data$z), 10)
	  # density_data<<-ker_data
	  my_xlab<-ker_data[[3]][1]
	  my_ylab<-ker_data[[3]][2]
	  if (!is.null(ker_data)){
	    my_legend_title_size = 20
	    my_legend_title_color = "black"
	    my_legend_title_face = "bold"
	    
	    #----- not working ... -----
	    my_legend_title_hjust = 0
	    my_legend_title_vjust = 0
	    #---------------------------
	    
	    my_legend_text_size = 20
	    my_legend_text_color = "black"
	    my_legend_text_angle = 0
	    
	    my_axis_text_size = 20
	    my_axis_text_face = "bold"
	    
	    my_axis_title_size = 20
	    my_axis_title_face = "bold"
	    
	    my_title_size = 20
	    my_title_color = "black"
	    my_title_face = "bold"
	    
	    #MY_X_Y_COUNT<-data.table("x"=XYARR[(XYARR[,4]==1),1],"y"=XYARR[(XYARR[,4]==1),2])
	    MY_X_Y_COUNT<-ker_data[[2]]
	    
	    my_aggregate_x =
	      aggregate(MY_X_Y_COUNT$x , by=list(sort(MY_X_Y_COUNT$x)), FUN=mean, na.rm=TRUE)
	    
	    
	    my_aggregate_y =
	      aggregate(MY_X_Y_COUNT$y , by=list(sort(MY_X_Y_COUNT$y)), FUN=mean, na.rm=TRUE)
	    
	    if (dim(my_aggregate_x)[1] > 2 )
	    {
	      if (dim(my_aggregate_y)[1] > 2 )
	      {
	        typeofSignals<<-"Continuous"
	        my_width = 2500
	        my_height = 1000
	        
	        #--- point size
	        #my_size = 2
	        my_size = 0.01
	        
	        #--- contour size
	        my_contour_thickness = 2
	        
	        #  used
	        # nearly perfect 1
	        # my_pch = 16
	        
	        # most perfect 5
	        my_pch = "."
	        
	        # used
	        my_density_geom = "tile"
	        
	        my_legend_key_size = 3
	        my_legend_position = "right"
	        
	        
	        my_n_x = my_n_x_max
	        my_n_y = my_n_y_max
	        
	        my_hx = bandwidth.nrd(MY_X_Y_COUNT$x) 
	        my_hy = bandwidth.nrd(MY_X_Y_COUNT$y) 
	          
	    #    previous_density_estimation_necessary = TRUE
	        
	      }else
	      {
	        denExtendY<<-1
	        
	        my_width = 1000
	        my_height = 500
	        my_size = 40
	        
	        
	        my_pch ="|"
	        
	        my_legend_key_size = 2
	        my_legend_position = "right"
	        
	        my_n_x = my_n_x_max
	        my_n_y = my_n_y_min
	        
	        my_hx = bandwidth.nrd(MY_X_Y_COUNT$x)
	        my_hy = 2 
	        
	        # previous_density_estimation_necessary = TRUE
	      }
	    }else
	    {
	      if (dim(my_aggregate_y)[1] > 2 )
	      {
	        # denExtendX<<-1
	        my_width = 1000
	        my_height = 500
	        my_size = 40
	        
	        
	        my_pch ="_"
	        
	        my_legend_key_size = 2
	        my_legend_position = "right"
	        
	        my_n_x = my_n_x_min
	        my_n_y = my_n_y_max
	        
	        my_hx = 2 
	        my_hy = bandwidth.nrd(MY_X_Y_COUNT$y) 
	        
	     #   previous_density_estimation_necessary = TRUE
	        
	      }else
	      {
	        # denExtendX<<-1
	        # denExtendY<<-1
	        my_title_size = 15
	        
	        my_width = 750
	        my_height = 500
	        my_size = 20
	        
	        
	        my_pch = 15
	        
	        # not used
	        my_density_geom = "hex"
	        
	        
	        my_legend_key_size = 2
	        my_legend_position = "right"
	        
	        my_n_x = my_n_x_min
	        my_n_y = my_n_y_min
	        
	        my_hx = 2 
	        my_hy = 2
	        
	      }
	    }
	    
	    
	    data<-ker_data[[1]]
	    colnames(data)<-c("x","y","z","LOG10","st")
	    MY_X_Y_KERNEL_DENSITY<-data.frame(x=data$x,y=data$y,z=data$z)
	    
	    
	    # my_log_10_exp = round(abs(log10(max(MY_X_Y_KERNEL_DENSITY$z))),0)
	    # 
	    # MY_X_Y_KERNEL_DENSITY[, my_col_function:= z*10^my_log_10_exp]
	    # 
	    my_legend_title = paste("KERNEL-DENSITY :", "\n", "\n",
	                            # "10 ^ -" , my_log_10_exp, " * ...", "\n", "\n",
	                            sep="")
	    # 
	    # my_log_10_exp_plus_x = my_log_10_exp + INPUT_my_log_10_exp_plus_xValue 
	    # 
	    # my_minimal_to_be_considered_kernel_density= 10^(-(my_log_10_exp_plus_x))
	    # 
	    # MY_X_Y_KERNEL_DENSITY <- 
	    #   MY_X_Y_KERNEL_DENSITY[MY_X_Y_KERNEL_DENSITY$my_col_function >= my_minimal_to_be_considered_kernel_density, ]
	    # 
	    # 
	    # min_of_MY_X_Y_my_col_function = round(min(MY_X_Y_KERNEL_DENSITY$my_col_function),my_log_10_exp) - 10^-my_log_10_exp
	    # max_of_MY_X_Y_my_col_function = round(max(MY_X_Y_KERNEL_DENSITY$my_col_function),my_log_10_exp) + 10^-my_log_10_exp
	    # 
	    # my_legend_breaks =
	    #   round(
	    #     seq(from = min(MY_X_Y_KERNEL_DENSITY$my_col_function) - 10^-my_log_10_exp,
	    #         to = max(MY_X_Y_KERNEL_DENSITY$my_col_function) + 10^-my_log_10_exp,
	    #         length.out = 5) , my_log_10_exp)
	    
	    my_legend_breaks =  seq(from = min(MY_X_Y_KERNEL_DENSITY$z), to = max(MY_X_Y_KERNEL_DENSITY$z),
	            length.out = 5)
	    

	    # 
	    # 
	    my_legend_range=c(min(my_legend_breaks),max(my_legend_breaks))
	    
	    #my_xlab = INPUT_VECTOR_VXX_Title_Type_Underscored_TEXT[j]
	    #my_ylab = INPUT_VECTOR_VXX_Title_Type_Underscored_TEXT[i]
	    
	    my_colors_for_plot = my_colors_for_2D_KERNEL_DENSITY_X_Y
	    
	    if (dim(my_aggregate_x)[1] <= 2)
	    {

	      if (dim(my_aggregate_y)[1] <= 2)
	      {
	        kerdenPlt1<-ggplot(MY_X_Y_KERNEL_DENSITY) +
	          # geom_point(aes(x, y, col =my_col_function), pch=my_pch,  size = my_size) +
	            geom_point(aes(x, y, col =z), pch=my_pch,  size = my_size) +
	          scale_colour_gradientn(my_legend_title, colours = c(my_colors_for_plot), limits = my_legend_range, breaks = my_legend_breaks,  na.value = my_na_color_value) +
	          theme(panel.grid.minor = element_line(colour = "white", linetype = "dotted", size = 1)) +
	          theme(legend.title = element_text(size= my_legend_title_size, colour=my_legend_title_color, hjust = my_legend_title_hjust, vjust = my_legend_title_vjust, face = my_legend_title_face)) +
	          theme(legend.position = my_legend_position ) +
	          theme(legend.background = element_rect(), legend.margin = unit(1, "cm")) +
	          theme(legend.key.size = unit(my_legend_key_size, "cm")) +
	          theme(legend.text = element_text(size = my_legend_text_size, colour = my_legend_text_color, angle = my_legend_text_angle)) +
	          theme(axis.text=element_text(size=my_axis_text_size, face=my_axis_text_face),
	                axis.title=element_text(size=my_axis_title_size,face=my_axis_title_face))+
	          theme(plot.title = element_text(size = my_title_size, colour = my_title_color, face=my_title_face))+
	          scale_x_continuous( breaks=c(min(MY_X_Y_KERNEL_DENSITY$x), max(MY_X_Y_KERNEL_DENSITY$x)) , limits=c(min(MY_X_Y_KERNEL_DENSITY$x), max(MY_X_Y_KERNEL_DENSITY$x)) ) +
	          scale_y_continuous( breaks=c(min(MY_X_Y_KERNEL_DENSITY$y), max(MY_X_Y_KERNEL_DENSITY$y)) , limits=c(min(MY_X_Y_KERNEL_DENSITY$y), max(MY_X_Y_KERNEL_DENSITY$y)) ) +
	          labs( title=INPUT_Analysis_Title,
	                x =my_xlab , y = my_ylab)+ coord_cartesian(xlim=ranges2D_x_y_ker_den$x, ylim=ranges2D_x_y_ker_den$y)
	      }else
	      {
	        kerdenPlt1<-ggplot(MY_X_Y_KERNEL_DENSITY) +
	          # geom_point(aes(x, y, col =my_col_function),pch=my_pch, size = my_size) +
	          geom_point(aes(x, y, col =z),pch=my_pch, size = my_size) +
	          scale_colour_gradientn(my_legend_title, colours = c(my_colors_for_plot), limits = my_legend_range, breaks = my_legend_breaks,  na.value = my_na_color_value) +
	          theme(panel.grid.minor = element_line(colour = "white", linetype = "dotted", size = 1)) +
	          theme(legend.title = element_text(size= my_legend_title_size, colour=my_legend_title_color, hjust = my_legend_title_hjust, vjust = my_legend_title_vjust, face = my_legend_title_face)) +
	          theme(legend.position = my_legend_position ) +
	          theme(legend.background = element_rect(), legend.margin = unit(1, "cm")) +
	          theme(legend.key.size = unit(my_legend_key_size, "cm")) +
	          theme(legend.text = element_text(size = my_legend_text_size, colour = my_legend_text_color, angle = my_legend_text_angle)) +
	          theme(axis.text=element_text(size=my_axis_text_size, face=my_axis_text_face),
	                axis.title=element_text(size=my_axis_title_size,face=my_axis_title_face))+
	          theme(plot.title = element_text(size = my_title_size, colour = my_title_color, face=my_title_face))+
	          scale_x_continuous( breaks=c(min(MY_X_Y_KERNEL_DENSITY$x), max(MY_X_Y_KERNEL_DENSITY$x)) , limits=c(min(MY_X_Y_KERNEL_DENSITY$x), max(MY_X_Y_KERNEL_DENSITY$x)) ) +
	          scale_y_continuous() +
	          labs( title=INPUT_Analysis_Title,
	                x =my_xlab , y = my_ylab)+ coord_cartesian(xlim=ranges2D_x_y_ker_den$x, ylim=ranges2D_x_y_ker_den$y)
	      }

	    }
	    else
	    {
	      if (dim(my_aggregate_y)[1] <= 2)
	      {
	        kerdenPlt1<-ggplot(MY_X_Y_KERNEL_DENSITY) +
	          # geom_point(aes(x, y, col =my_col_function),pch=my_pch, size = my_size) +
	          geom_point(aes(x, y, col = z),pch=my_pch, size = my_size) +
	          scale_colour_gradientn(my_legend_title, colours = c(my_colors_for_plot), limits = my_legend_range, breaks = my_legend_breaks, na.value = my_na_color_value) +
	          theme(panel.grid.minor = element_line(colour = "white", linetype = "dotted", size = 1)) +
	          theme(legend.title = element_text(size= my_legend_title_size, colour=my_legend_title_color, hjust = my_legend_title_hjust, vjust = my_legend_title_vjust, face = my_legend_title_face)) +
	          theme(legend.position = my_legend_position ) +
	          theme(legend.background = element_rect(), legend.margin = unit(1, "cm")) +
	          theme(legend.key.size = unit(my_legend_key_size, "cm")) +
	          theme(legend.text = element_text(size = my_legend_text_size, colour = my_legend_text_color, angle = my_legend_text_angle)) +
	          theme(axis.text=element_text(size=my_axis_text_size, face=my_axis_text_face),
	                axis.title=element_text(size=my_axis_title_size,face=my_axis_title_face))+
	          theme(plot.title = element_text(size = my_title_size, colour = my_title_color, face=my_title_face))+
	          scale_x_continuous() +
	          scale_y_continuous( breaks=c(min(MY_X_Y_KERNEL_DENSITY$y), max(MY_X_Y_KERNEL_DENSITY$y)) , limits=c(min(MY_X_Y_KERNEL_DENSITY$y), max(MY_X_Y_KERNEL_DENSITY$y)) ) +
	          labs( title=INPUT_Analysis_Title,
	                x =my_xlab , y = my_ylab)+ coord_cartesian(xlim=ranges2D_x_y_ker_den$x, ylim=ranges2D_x_y_ker_den$y)
	      }
	      else
	      {

	        #---- preliminary most perfect and USED plot: 5
	        {
	          
	          
	          # dens <- my_density_estimation
	          densdf <- MY_X_Y_KERNEL_DENSITY
	          
	          
	          #---- prepare ratio for quadratic graph ---
	          ratio.display <- 1/1
	          ratio.values <- 
	            (max(MY_X_Y_COUNT$x)-min(MY_X_Y_COUNT$x))/
	            (max(MY_X_Y_COUNT$y)-min(MY_X_Y_COUNT$y))
	          
	          #---- sp0: create contours and labels ---
	          {
	            contour_and_label_plot <- ggplot(densdf, aes(x = x, y = y, z = z)) +
	              xlim(min(MY_X_Y_COUNT$x), max(MY_X_Y_COUNT$x)) +
	              ylim(min(MY_X_Y_COUNT$y), max(MY_X_Y_COUNT$y)) +
	              # xlim(xmin,xmax)+
	              # ylim(ymin,ymax)+
	             
	              # coord_fixed(ratio.values / ratio.display) +
	              # geom_point(aes(colour=z),size=0.1)+
	              # stat_contour(aes(colour = ..level..),size = 1
	              #              # ,breaks=den_breaks$br
	              #              ) +
	              geom_contour(data=densdf,aes(x = x, y = y,z=z,color=..level..),size = 1)+
	              # geom_contour(data=densdf,aes(x = x, y = y,z=z,color=z),size = 1) +
	              scale_colour_gradient(low = "darkblue",
	                                    high = "cyan",
	                                    guide="legend") +
	              # geom_point(data=data.frame(x=cnt$x,y=cnt$y),aes(x,y),colour="red",size=0.5,inherit.aes = FALSE)+
	              
	              theme(axis.text=element_text(size=my_axis_text_size, face=my_axis_text_face),
	                    axis.title=element_text(size=my_axis_title_size,face=my_axis_title_face))+
	              theme(plot.title = element_text(size = my_title_size, colour = my_title_color, face=my_title_face)) +
	              labs( title=INPUT_Analysis_Title,
	                    x =my_xlab , 
	                    y = my_ylab)+ coord_cartesian(xlim=ranges2D_x_y_ker_den$x, ylim=ranges2D_x_y_ker_den$y)
	            
	            
	            
	            # ---dl.combine posiibilities
	            {
	              #--- bad -
	              # dl.combine(top.bumptwice,first.points) 
	              # dl.combine(first.points)
	              #--- mediocre
	              # dl.combine(first.qp )
	              # dl.combine(first.bumpup)
	              #-- good
	              # dl.combine(top.bumptwice)
	              # -- seems to be best
	              # dl.combine(top.bumptwice,last.bumpup)
	              }
	            
	            kerdenPlt1<-direct.label(contour_and_label_plot,
	                                list(dl.combine(top.bumptwice,last.bumpup),
	                                     cex=1,
	                                     fontface="bold"))
	            
	            }
	          
	        }
	        
	      }
	      
	    }

	  }
	  
	  
	  
	  plts$kernel1<-kerdenPlt1
	  isolate(plts$kernel1)
	  
	  # showNotification("Plot Created!!", duration = 2,type="message")
	})
	
	observeEvent(input$plot2D_x_y_ker_den1_dblclick, {
	  brush <- input$plot2D_x_y_ker_den1_brush
	  if (!is.null(brush)) {
	    ranges2D_x_y_ker_den$x <- c(brush$xmin, brush$xmax)
	    ranges2D_x_y_ker_den$y <- c(brush$ymin, brush$ymax)
	  }else {
	    ranges2D_x_y_ker_den$x <- NULL
	    ranges2D_x_y_ker_den$y <- NULL
	  }
	  
	})
	
	output$plot2D_x_y_ker_den1_d<-downloadHandler(
	  filename = function(){
	    paste0("XY_K_Den_Contour",input$sel1,"_",input$sel2,"_",input$sig1,"__", input$sig2,".pdf")
	  },
	  content=function(file){
	    pdf(file,width=12, height=10)
	    plt<-plts$kernel1
	    # if (grepl("FILTERED",kerneldendataFilter$val,fixed=TRUE)){
	    plt<-plt+annotate("text", Inf, Inf, label = kerneldendataFilter$val, hjust = 1, vjust = 1)
	    plt<-plt+annotate("text", -Inf, Inf, label = paste0("Creation Date: ",Sys.Date()), hjust = 0, vjust = 1)
	    # }
	    plot(plt)
	    # plot(plts$kernel2)
	    dev.off()
	  }
	)
	
	
	output$plot2D_x_y_ker_den2<-renderPlot({
	  
	  ker_data<-data2d_xy_ker_den()
	  kerdenPlt2<-NULL
	  
	  my_xlab<-ker_data[[3]][1]
	  my_ylab<-ker_data[[3]][2]
	  if (!is.null(ker_data)){
	    my_legend_title_size = 20
	    my_legend_title_color = "black"
	    my_legend_title_face = "bold"
	    
	    #----- not working ... -----
	    my_legend_title_hjust = 0
	    my_legend_title_vjust = 0
	    #---------------------------
	    
	    my_legend_text_size = 20
	    my_legend_text_color = "black"
	    my_legend_text_angle = 0
	    
	    my_axis_text_size = 20
	    my_axis_text_face = "bold"
	    
	    my_axis_title_size = 20
	    my_axis_title_face = "bold"
	    
	    my_title_size = 20
	    my_title_color = "black"
	    my_title_face = "bold"
	    
	    MY_X_Y_COUNT<-ker_data[[2]]
	    
	    my_aggregate_x =
	      aggregate(MY_X_Y_COUNT$x , by=list(sort(MY_X_Y_COUNT$x)), FUN=mean, na.rm=TRUE)
	    
	    
	    my_aggregate_y =
	      aggregate(MY_X_Y_COUNT$y , by=list(sort(MY_X_Y_COUNT$y)), FUN=mean, na.rm=TRUE)
	    
	    if (dim(my_aggregate_x)[1] > 2 )
	    {
	      if (dim(my_aggregate_y)[1] > 2 )
	      {
	        typeofSignals<<-"Continuous"
	        my_width = 2500
	        my_height = 1000
	        
	        #--- point size
	        #my_size = 2
	        my_size = 0.01
	        
	        #--- contour size
	        my_contour_thickness = 2
	        
	        #  used
	        # nearly perfect 1
	        # my_pch = 16
	        
	        # most perfect 5
	        my_pch = "."
	        
	        # used
	        my_density_geom = "tile"
	        
	        my_legend_key_size = 3
	        my_legend_position = "right"
	        
	        
	        my_n_x = my_n_x_max
	        my_n_y = my_n_y_max
	        
	        my_hx = bandwidth.nrd(MY_X_Y_COUNT$x) 
	        my_hy = bandwidth.nrd(MY_X_Y_COUNT$y) 
	        
	        #    previous_density_estimation_necessary = TRUE
	        
	      }else
	      {
	        my_width = 1000
	        my_height = 500
	        my_size = 40


	        my_pch ="."

	        my_legend_key_size = 2
	        my_legend_position = "right"

	        my_n_x = my_n_x_max
	        my_n_y = my_n_y_min

	        my_hx = bandwidth.nrd(MY_X_Y_COUNT$x)
	        my_hy = 2

	        # previous_density_estimation_necessary = TRUE
	      }
	    }

	    else{
	      if (dim(my_aggregate_y)[1] > 2 )
	      {
	        my_width = 1000
	        my_height = 500
	        my_size = 40


	        my_pch ="."

	        my_legend_key_size = 2
	        my_legend_position = "right"

	        my_n_x = my_n_x_min
	        my_n_y = my_n_y_max

	        my_hx = 2
	        my_hy = bandwidth.nrd(MY_X_Y_COUNT$y)

	        #   previous_density_estimation_necessary = TRUE

	      }else
	      {

	        my_title_size = 15

	        my_width = 750
	        my_height = 500
	        my_size = 20


	        my_pch = "."

	        # not used
	        my_density_geom = "hex"


	        my_legend_key_size = 2
	        my_legend_position = "right"

	        my_n_x = my_n_x_min
	        my_n_y = my_n_y_min

	        my_hx = 2
	        my_hy = 2

	      }
	    }
	    
	    
	    data<-ker_data[[1]]
	    colnames(data)<-c("x","y","z","LOG10","st")
	    MY_X_Y_KERNEL_DENSITY<-data.table(x=data$x,y=data$y,z=data$z)
	    
	    # my_log_10_exp = round(abs(log10(max(MY_X_Y_KERNEL_DENSITY$z))),0)
	    # 
	    # MY_X_Y_KERNEL_DENSITY[, my_col_function:= z*10^my_log_10_exp]
	    
	    my_legend_title = paste("KERNEL-DENSITY :", "\n", "\n", 
	                            # "10 ^ -" , my_log_10_exp, " * ...", "\n", "\n", 
	                            sep="")
	    
	    # my_log_10_exp_plus_x = my_log_10_exp + INPUT_my_log_10_exp_plus_xValue 
	    # 
	    # my_minimal_to_be_considered_kernel_density= 10^(-(my_log_10_exp_plus_x))
	    # 
	    # MY_X_Y_KERNEL_DENSITY <- 
	    #   MY_X_Y_KERNEL_DENSITY[MY_X_Y_KERNEL_DENSITY$my_col_function >= my_minimal_to_be_considered_kernel_density, ]
	    # 
	    # 
	    # min_of_MY_X_Y_my_col_function = round(min(MY_X_Y_KERNEL_DENSITY$my_col_function),my_log_10_exp) - 10^-my_log_10_exp
	    # max_of_MY_X_Y_my_col_function = round(max(MY_X_Y_KERNEL_DENSITY$my_col_function),my_log_10_exp) + 10^-my_log_10_exp
	    
	    # my_legend_breaks =
	    #   round(
	    #     seq(from = min(MY_X_Y_KERNEL_DENSITY$my_col_function) - 10^-my_log_10_exp,
	    #         to = max(MY_X_Y_KERNEL_DENSITY$my_col_function) + 10^-my_log_10_exp,
	    #         length.out = 5) , my_log_10_exp)
	    
	    my_legend_breaks =  seq(from = min(MY_X_Y_KERNEL_DENSITY$z), to = max(MY_X_Y_KERNEL_DENSITY$z),
	                            length.out = 5)
	    
	    my_legend_range=c(min(my_legend_breaks),max(my_legend_breaks))
	    
	    #my_xlab = INPUT_VECTOR_VXX_Title_Type_Underscored_TEXT[j]
	    #my_ylab = INPUT_VECTOR_VXX_Title_Type_Underscored_TEXT[i]
	    
	    my_colors_for_plot = my_colors_for_2D_KERNEL_DENSITY_X_Y
	    
	    if (dim(my_aggregate_x)[1] <= 2)
	    {

	      if (dim(my_aggregate_y)[1] <= 2)
	      {
	        kerdenPlt2<-ggplot(MY_X_Y_COUNT) +
	          #geom_point(aes(x, y, col =my_col_function), pch=my_pch,  size = my_size) +
	          geom_point(aes(x, y), pch=my_pch,  size = my_size) +
	        #   scale_colour_gradientn(my_legend_title, colours = c(my_colors_for_plot), limits = my_legend_range, breaks = my_legend_breaks,  na.value = my_na_color_value) +
	          theme(panel.grid.minor = element_line(colour = "white", linetype = "dotted", size = 1)) +
	        #   theme(legend.title = element_text(size= my_legend_title_size, colour=my_legend_title_color, hjust = my_legend_title_hjust, vjust = my_legend_title_vjust, face = my_legend_title_face)) +
	        #   theme(legend.position = my_legend_position ) +
	        #   theme(legend.background = element_rect(), legend.margin = unit(1, "cm")) +
	        #   theme(legend.key.size = unit(my_legend_key_size, "cm")) +
	        #   theme(legend.text = element_text(size = my_legend_text_size, colour = my_legend_text_color, angle = my_legend_text_angle)) +
	          theme(axis.text=element_text(size=my_axis_text_size, face=my_axis_text_face),
	                axis.title=element_text(size=my_axis_title_size,face=my_axis_title_face))+
	          theme(plot.title = element_text(size = my_title_size, colour = my_title_color, face=my_title_face))+
	          scale_x_continuous( breaks=c(min(MY_X_Y_COUNT$x), max(MY_X_Y_COUNT$x)) , limits=c(min(MY_X_Y_COUNT$x), max(MY_X_Y_COUNT$x)) ) +
	          scale_y_continuous( breaks=c(min(MY_X_Y_COUNT$y), max(MY_X_Y_COUNT$y)) , limits=c(min(MY_X_Y_COUNT$y), max(MY_X_Y_COUNT$y)) ) +
	          labs( title=INPUT_Analysis_Title,
	                x =my_xlab , y = my_ylab)+ coord_cartesian(xlim=ranges2D_x_y_ker_den$x, ylim=ranges2D_x_y_ker_den$y)
	      }else
	      {
	        kerdenPlt2<-ggplot(MY_X_Y_COUNT) +
	          geom_point(aes(x, y, col =my_col_function),pch=my_pch, size = my_size) +
	        #   scale_colour_gradientn(my_legend_title, colours = c(my_colors_for_plot), limits = my_legend_range, breaks = my_legend_breaks,  na.value = my_na_color_value) +
	          theme(panel.grid.minor = element_line(colour = "white", linetype = "dotted", size = 1)) +
	        #   theme(legend.title = element_text(size= my_legend_title_size, colour=my_legend_title_color, hjust = my_legend_title_hjust, vjust = my_legend_title_vjust, face = my_legend_title_face)) +
	        #   theme(legend.position = my_legend_position ) +
	        #   theme(legend.background = element_rect(), legend.margin = unit(1, "cm")) +
	        #   theme(legend.key.size = unit(my_legend_key_size, "cm")) +
	        #   theme(legend.text = element_text(size = my_legend_text_size, colour = my_legend_text_color, angle = my_legend_text_angle)) +
	          theme(axis.text=element_text(size=my_axis_text_size, face=my_axis_text_face),
	                axis.title=element_text(size=my_axis_title_size,face=my_axis_title_face))+
	          theme(plot.title = element_text(size = my_title_size, colour = my_title_color, face=my_title_face))+
	          scale_x_continuous( breaks=c(min(MY_X_Y_COUNT$x), max(MY_X_Y_COUNT$x)) , limits=c(min(MY_X_Y_COUNT$x), max(MY_X_Y_COUNT$x)) ) +
	          scale_y_continuous() +
	          labs( title=INPUT_Analysis_Title,
	                x =my_xlab , y = my_ylab)+ coord_cartesian(xlim=ranges2D_x_y_ker_den$x, ylim=ranges2D_x_y_ker_den$y)
	      }

	    }
	    else
	    {
	      if (dim(my_aggregate_y)[1] <= 2)
	      {
	        kerdenPlt2<-ggplot(MY_X_Y_COUNT) +
	          geom_point(aes(x, y),pch=my_pch, size = my_size) +
	        #   scale_colour_gradientn(my_legend_title, colours = c(my_colors_for_plot), limits = my_legend_range, breaks = my_legend_breaks, na.value = my_na_color_value) +
	          theme(panel.grid.minor = element_line(colour = "white", linetype = "dotted", size = 1)) +
	        #   theme(legend.title = element_text(size= my_legend_title_size, colour=my_legend_title_color, hjust = my_legend_title_hjust, vjust = my_legend_title_vjust, face = my_legend_title_face)) +
	        #   theme(legend.position = my_legend_position ) +
	        #   theme(legend.background = element_rect(), legend.margin = unit(1, "cm")) +
	        #   theme(legend.key.size = unit(my_legend_key_size, "cm")) +
	        #   theme(legend.text = element_text(size = my_legend_text_size, colour = my_legend_text_color, angle = my_legend_text_angle)) +
	          theme(axis.text=element_text(size=my_axis_text_size, face=my_axis_text_face),
	                axis.title=element_text(size=my_axis_title_size,face=my_axis_title_face))+
	          theme(plot.title = element_text(size = my_title_size, colour = my_title_color, face=my_title_face))+
	          scale_x_continuous() +
	          scale_y_continuous( breaks=c(min(MY_X_Y_COUNT$y), max(MY_X_Y_COUNT$y)) , limits=c(min(MY_X_Y_COUNT$y), max(MY_X_Y_COUNT$y)) ) +
	          labs( title=INPUT_Analysis_Title,
	                x =my_xlab , y = my_ylab)+ coord_cartesian(xlim=ranges2D_x_y_ker_den$x, ylim=ranges2D_x_y_ker_den$y)
	      }
	      else
	      {
	        
	        #---- preliminary most perfect and USED plot: 5
	        {
	          # dens <- my_density_estimation
	          densdf <- MY_X_Y_KERNEL_DENSITY
	          
	          # densdf <- data.frame(expand.grid(x = dens$x, y= dens$y),
	          #                      z = as.vector(dens$z))
	          
	          # densdf <- data.frame(expand.grid(x = unique(MY_X_Y_KERNEL_DENSITY$x), y= unique(MY_X_Y_KERNEL_DENSITY$y),
	          #                      z = MY_X_Y_KERNEL_DENSITY$z)
	          # 
	          
	          #---- prepare ratio for quadratic graph ---
	          ratio.display <- 1/1
	          ratio.values <- 
	            (max(MY_X_Y_COUNT$x)-min(MY_X_Y_COUNT$x))/
	            (max(MY_X_Y_COUNT$y)-min(MY_X_Y_COUNT$y))
	          
	          # #---- sp0: create contours and labels ---
	          # {
	          #   contour_and_label_plot <- ggplot(densdf, aes(x = x, y = y, z = z)) +
	          #     xlim(min(MY_X_Y_COUNT$x), max(MY_X_Y_COUNT$x)) +
	          #     ylim(min(MY_X_Y_COUNT$y), max(MY_X_Y_COUNT$y)) +
	          #     coord_fixed(ratio.values / ratio.display) +
	          #     stat_contour(aes(colour = ..level..),
	          #                  size = 1) +
	          #     scale_colour_gradient(low = "darkblue",
	          #                           high = "cyan",
	          #                           guide=FALSE) +
	          #     theme(axis.text=element_text(size=my_axis_text_size, face=my_axis_text_face),
	          #           axis.title=element_text(size=my_axis_title_size,face=my_axis_title_face))+
	          #     theme(plot.title = element_text(size = my_title_size, colour = my_title_color, face=my_title_face)) +
	          #     labs( title=INPUT_Analysis_Title,
	          #           x =my_xlab , 
	          #           y = my_ylab)
	          #   
	          #   # ---dl.combine posiibilities
	          #   {
	          #     #--- bad -
	          #     # dl.combine(top.bumptwice,first.points) 
	          #     # dl.combine(first.points)
	          #     #--- mediocre
	          #     # dl.combine(first.qp )
	          #     # dl.combine(first.bumpup)
	          #     #-- good
	          #     # dl.combine(top.bumptwice)
	          #     # -- seems to be best
	          #     # dl.combine(top.bumptwice,last.bumpup)
	          #     }
	          #   
	          #   direct.label(contour_and_label_plot,
	          #                list(dl.combine(top.bumptwice,last.bumpup),
	          #                     cex=1,
	          #                     fontface="bold"))
	          #   
	          #   }
	          
	          #---- sp1: create kenel dnsity plot with contours ----
	          {
	            kerdenPlt2<-ggplot(MY_X_Y_COUNT, aes(x = x, y = y)) +
	              geom_point(pch=my_pch,
	                         size = my_size) +
	              xlim(min(MY_X_Y_COUNT$x), max(MY_X_Y_COUNT$x)) +
	              ylim(min(MY_X_Y_COUNT$y), max(MY_X_Y_COUNT$y)) +
	              # coord_fixed(ratio.values / ratio.display) +
	              geom_contour(aes(z=z, color=..level..),
	                           data=densdf,
	                           # bins = 100,
	                           size = my_contour_thickness
	              )	+
	              scale_colour_gradient(low = "darkblue",
	                                    high = "cyan",
	                                    guide=FALSE) +
	              geom_contour(aes(z=z),
	                           data=densdf,
	                           color= "white",
	                           # bins = 100,
	                           size= 0.01)  +
	              geom_raster(aes(fill=z,alpha=z),data=densdf,show.legend=FALSE)+
	              # stat_density2d(geom = my_density_geom,
	              #                aes(fill=..density..,alpha=..density..),
	              #                contour = FALSE,
	              #                show.legend=FALSE,
	              #                n = c(my_n_x, my_n_y),
	              #                size = my_size) +
	              scale_fill_gradient(my_legend_title,
	                                  low="darkblue",
	                                  high="cyan",
	                                  na.value=my_na_color_value,guide=FALSE) +
	              theme(panel.grid.minor = element_line(colour = "white", linetype = "dotted", size = 1)) +
	              # theme(legend.title = element_text(size= my_legend_title_size, colour=my_legend_title_color, hjust = my_legend_title_hjust, vjust = my_legend_title_vjust, face = my_legend_title_face)) +
	              # theme(legend.position = my_legend_position ) +
	              # theme(legend.background = element_rect(fill="NA"), legend.margin = unit(1, "cm")) +
	              # theme(legend.key.size = unit(my_legend_key_size, "cm")) +
	              # theme(legend.text = element_text(size = my_legend_text_size, colour = my_legend_text_color, angle = my_legend_text_angle)) +
	              theme(axis.text=element_text(size=my_axis_text_size, face=my_axis_text_face),
	                    axis.title=element_text(size=my_axis_title_size,face=my_axis_title_face))+
	              theme(plot.title = element_text(size = my_title_size, colour = my_title_color, face=my_title_face))+
	              # scale_x_continuous() +
	              # scale_y_continuous() +
	              labs( title=INPUT_Analysis_Title,
	                    x =my_xlab , y = my_ylab)+ coord_cartesian(xlim=ranges2D_x_y_ker_den$x, ylim=ranges2D_x_y_ker_den$y)

	          }

	          #---- combine spo and sp1
	          #sp <- grid.arrange(sp0,sp1,  nrow=1, ncol=2)
	          #grid.arrange(sp0,sp1,  nrow=1, ncol=2)
	          
	        }
	        
	      }
	      
	    }

	  }
	  
	  plts$kernel2<-kerdenPlt2
	  isolate(plts$kernel2)
	})
	
	output$Kdata_range<-renderText({
	  
	  brush <- input$plot2D_x_y_ker_den2_brush
	  dRange<-NULL
	  if (!is.null(brush)) {
	    dRange<-paste0("Xmin = ",brush$xmin, " , Xmax = ", brush$xmax, " \n")
	    dRange<-paste0(dRange,"Ymin = ",brush$ymin, " , Ymax = ", brush$ymax)
	  }
	  dRange
	})
	
	cdnReset<-reactiveValues(val="NA")
	updPlot<-reactiveValues(val="NA")
	
	observeEvent(input$Kdata_range_clear,{
	  cdnReset$val<-"Yes"
	  if (!is.null(appEBCs2d)){
	    EBCs2d<<-appEBCs2d
	    LogicalString2d<<-appLS2d
	    cdnCounter<<-length(EBCs2d)
	  }else{
	    EBCs2d<<-NULL
	    LogicalString2d<<-NULL
	    cdnCompleteString<<-NULL
	    cdnCounter<<-0
	  }
	 
	  cdnList<<-NULL
	  
	  # cdnAllList<<-c()
	})
	
	observeEvent(input$Kdata_range_sel,{
	  cdnReset$val<-NULL
	  # cdnListShow$val<-NULL
	  cdnReset$val<-"No"
	})
	
	observeEvent(input$Kdata_range_app,{
	  if (input$load_data==1){
	    showNotification("Project Data not loaded",duration=2)
	    return()
	  }
	  filterSelection<-input$extent
	  if ((length(filterSelection)>1)){
	    showNotification("Incorrect filter selection.",duration=2)
	    return()
	  }
	  cdnList<<-NULL
	  LogicalString2d<<-lsTxt
	  
	  # cdnCounter<<-cdnPartCounter
	  if ((!is.null(EBCs2d))&(!is.null(LogicalString2d))){
	    dataFilter2D<<-EquationBasedDataFilter(EBCs2d,LogicalString2d,PROJECT_DATA_VXX)
	    appEBCs2d<<-EBCs2d
	    cdnCompleteString<<-cdnString
	    appLS2d<<-LogicalString2d
	    cdnCounter<<-length(EBCs2d)
	  }
	  # updatePlots()
	  # visualfilter1D2Dupdate()
	  updateExtentStatus(4)
	  updateExtentStatus(2)
	  callerFilter<<-"2D_Apply"
	  
	  # plt_xykerden$y<-"No"
	  # plt_xykerden$y<-"Yes"
	 
	})
	
	observeEvent(input$Kdata_range_reset,{
	  # cdnReset$val<-"No"
	  dataFilter2D<<-NULL
	  
	  callerFilter<<-"Reset"
	  # plt_xykerden$y<-"No"  
	  # plt_xykerden$y<-"Yes"  
	  
	  cdnReset$val<-"Yes"
	  updateExtentStatus(0)
	  
	  EBCs2d<<-c()
	  
	  cdnCounter<<-0
	  cdnCompleteString<<-NULL
	  # cdnAllList1d<<-c()
	  cdnList<<-NULL
	  
	  LogicalString2d<<-NULL
	  appEBCs2d<<-NULL
	  appLS2d<<-NULL
	})
	
	
	cdnList<-c()
	cdnListShow<-reactiveValues(val=NULL)
	genCondition<-function(VXX,LowerLimit,UpperLimit,relType){
	  if ((length(VXX)!=length(LowerLimit))|(length(VXX)!=length(UpperLimit))){
	    showNotification("Vector and Limits not of equal length.",duration=2)
	    return()
	  }
	  allCdn<-c()
	  for (i in 1:length(VXX)){
	    if (relType[i]==1){ #include data in extent
	      allCdn[i]<-paste0("(V",VXX[i]," >=",LowerLimit[i],") & (V",VXX[i]," <=",UpperLimit[i],")")
	    }else if(relType[i]==0){ #exclude data in extent
	      allCdn[i]<-paste0("(V",VXX[i]," <=",LowerLimit[i],") | (V",VXX[i]," >=",UpperLimit[i],")")
	    }
	  }

	  return(allCdn)

	}
	
	EBCs2d<-c()                                     #vector of all conditions applied
	LogicalString2d<-NULL                           #Applied logical string
	LSList<-list()                                 
	lsTxt<-NULL                                    
	cdnAllList<-NULL
	cdnCounter<-0
	cdnPartCounter<-0
	cdnString<-NULL
	cdnCompleteString<-NULL
	appEBCs2d<-NULL
	appLS2d<-NULL

	observeEvent(input$transfer_bnd,{
	  if (length(EBCs2d)==0){
	    showNotification("Conditions not defined.",duration=2)
	    return()
	  }else{
	    txt<-LogicalString2d
	    cdnNum<<-extractcdnNum(cdnAllVis)
	    for (i in 1:length(EBCs2d)){
	      cdnNum<<-cdnNum+1
	      cdn<-substr(EBCs2d[i],(regexpr("=",EBCs2d[i],fixed=TRUE)[1]+1),nchar(EBCs2d[i]))
	      ebc<-substr(EBCs2d[i],1,(regexpr("=",EBCs2d[i],fixed=TRUE)[1]-1))
	      cdnAllV[[paste0("EBC",cdnNum)]]<<-trimws(cdn)
	      cdnAllVis[[paste0("EBC",cdnNum)]]<<-trimws(cdn)
	      cdnDes[[paste0("EBC",cdnNum)]]<<-"# 2D Filter"
	      txt<-gsub(paste0(" ",trimws(ebc)," "),paste0(" EBC",cdnNum," "),txt,fixed=TRUE)
	    }
	    updateSelectInput(session,"cdns",choices=cdnAllVis,selected="No Selection")
	    showCdn$val<-NULL
	    cdnCnt$val<-cdnNum
	    allLS[[paste0("LS",length(allLS))]]<<-txt
	    updateSelectInput(session,"LS",choices=allLS,selected="No Selection")
	    lsCnt$val<-length(allLS)
	    showNotification("2D filter transferred to Boundary condtions.",duration=2)
	  }
	})
	
	output$Kdata_cdns<-renderText({
	  req(cdnReset$val)
	  xmin<-isolate(input$Kdata_xmin)
	  xmax<-isolate(input$Kdata_xmax)
	  ymin<-isolate(input$Kdata_ymin)
	  ymax<-isolate(input$Kdata_ymax)
	  
	  cdnTxt<-NULL
	  dispcdnTxt<-NULL
	  if (cdnReset$val=="No"){
	    i<-which(INPUT_VECTOR_VXX_Title_Type_Text==isolate(input$sig1))
	    j<-which(INPUT_VECTOR_VXX_Title_Type_Text==isolate(input$sig2))
	    brush <- isolate(input$plot2D_x_y_ker_den2_brush)
	    if (is.null(brush)){
	      showNotification("Select rectangle on plot.",duration=2)
	      return()
	    }
	    if ((!is.na(xmin))&(!is.na(xmin))&(!is.na(xmin))&(!is.na(xmin))){
	      xminSel<-xmin
	      xmaxSel<-xmax
	      yminSel<-ymin
	      ymaxSel<-ymax
	    }else if(!is.null(brush)){
	      xminSel<-round(brush$xmin,2)
	      xmaxSel<-round(brush$xmax,2)
	      yminSel<-round(brush$ymin,2)
	      ymaxSel<-round(brush$ymax,2)
	    }
	    if (input$Kdata_filter==1){
	      if (isolate(input$Kdata_sel)==1){
	        relType=c(1,1)
	        # cdnList<<-c(cdnList,"NEW_INC",genCondition(c(i,j),c(brush$xmin, brush$ymin),c(brush$xmax, brush$ymax),relType))
	        cdn<-genCondition(c(i,j),c(xminSel, yminSel),c(xmaxSel, ymaxSel),relType)
	        cdnList<<-c(cdnList,"NEW_INC",genCondition(c(i,j),c(xminSel, yminSel),c(xmaxSel, ymaxSel),relType))
	      }else{
	        relType=c(0,0)
	        # cdnList<<-c(cdnList,"NEW_EXC",genCondition(c(i,j),c(brush$xmin, brush$ymin),c(brush$xmax, brush$ymax),relType))
	        cdn<-genCondition(c(i,j),c(xminSel, yminSel),c(xmaxSel, ymaxSel),relType)
	        cdnList<<-c(cdnList,"NEW_EXC",genCondition(c(i,j),c(xminSel, yminSel),c(xmaxSel, ymaxSel),relType))
	      }
	    }
	    
	    
	    lsTxt<<-"( "
	    
	    if (length(cdnList)>1){
	      prevCdn<-cdnList[1]
	      nxtCdn<-NULL
	      # if (!is.null(cdnCompleteString)){
	      if (length(EBCs2d)>0){
	        # cdnPartCounter<<-cdnCounter
	        cdnPartCounter<<-length(EBCs2d)
	      }else{
	        cdnPartCounter<<-0
	      }
	      # cdnList<-c("APPLIED",cdnList)
	      for (i in 2:length(cdnList)){
	        # if (cdnList[i]!="APPLIED"){
	        if ((cdnList[i]!="NEW_INC")&(cdnList[i]!="NEW_EXC")){
	          cdnPartCounter<<-cdnPartCounter+1
	          cdnTxt<-paste0(cdnTxt,"2D-EBC", cdnPartCounter, "= " ,cdnList[i],"\n")  
	          EBCs2d<<-c(EBCs2d,paste0("2D-EBC", cdnPartCounter, "= " ,cdnList[i]))
	          lsTxt<<-paste0(lsTxt," ( 2D-EBC",cdnPartCounter," )")
	          
	          if ((i<length(cdnList))&(prevCdn=="NEW_INC")){
	            lsTxt<<-paste0(lsTxt," & ")
	            # LogicalString<<-paste0(LogicalString," & ")
	          }
	          # else if((i<length(cdnList))&(cdnList[(i+1)]=="NEW")) {
	          else if ((i<length(cdnList))&(prevCdn=="NEW_EXC")){
	            lsTxt<<-paste0(lsTxt," | ")
	            # LogicalString<<-paste0(LogicalString," | ")
	          }
	          
	        }else if((cdnList[i]=="NEW_INC")|(cdnList[i]=="NEW_EXC")){
	          nxtCdn<-cdnList[i]
	          # LogicalString<<-substr(LogicalString,1,(nchar(LogicalString)-3))
	          lsTxt<<-substr(lsTxt,1,(nchar(lsTxt)-3))
	        }
	        if ((!is.null(nxtCdn))&(i<length(cdnList))){
	          if ((prevCdn=="NEW_INC")&(nxtCdn=="NEW_INC")){
	            # LogicalString<<-paste0(LogicalString," | ")
	            lsTxt<<-paste0(lsTxt," ) | ( ") 
	          }else if ((prevCdn=="NEW_INC")&(nxtCdn=="NEW_EXC")){
	            # LogicalString<<-paste0(LogicalString," & ")
	            lsTxt<<-paste0(lsTxt," ) & ( ") 
	          }else if ((prevCdn=="NEW_EXC")&(nxtCdn=="NEW_EXC")){
	            # LogicalString<<-paste0(LogicalString," & ")
	            lsTxt<<-paste0(lsTxt," ) & ( ") 
	          }else if ((prevCdn=="NEW_EXC")&(nxtCdn=="NEW_INC")){
	            # LogicalString<<-paste0(LogicalString," & ")
	            lsTxt<<-paste0(lsTxt," ) & ( ") 
	          }
	          prevCdn<-nxtCdn
	          nxtCdn<-NULL
	        }else if (i==length(cdnList)){
	          # LogicalString<<-paste0(LogicalString,")")
	          lsTxt<<-paste0(lsTxt," )")
	        }  
	        
	      }
	     
	      if (!is.null(cdnCompleteString)){
	        cdnTxt<-paste0(cdnCompleteString," \n",cdnTxt)
	      }else{
	        cdnTxt<-paste0("Conditions - 2D Filter: \n"  ,cdnTxt)  
	      }
	      
	      # if (!is.null(LogicalString2d)){
	      #   lsTxt<<-paste0("(( ",LogicalString2d,") & (",lsTxt," ))")
	      # }
	      
	      filterSelection<-ifelse(!is.null(isolate(input$extent)),isolate(input$extent),0)
	      
        ebccheckOutput<-checkEBCs(caller=2,filterSelection=filterSelection,
                                  EBCs1d=EBCs1d,LogicalString1d=LogicalString1d,
                                  EBCs2d=EBCs2d,LogicalString2d=LogicalString2d,
                                  EBCs=EBCs,LogicalString=LogicalString,
                                  cdnPartCounter=cdnPartCounter,cdnTxt=cdnTxt,lsTxt=lsTxt)

        EBCs2d<<-ebccheckOutput[[1]]
        LogicalString2d<<-ebccheckOutput[[2]]
        cdnTxt<-ebccheckOutput[[3]]
        dispcdnTxt<-ebccheckOutput[[4]]
        lsTxt<<-ebccheckOutput[[5]]
	      
	      cdnString<<-cdnTxt
	      
	    }
	    
	  }
	  cdnListShow$val<-dispcdnTxt
	})
	
	appK<-reactiveValues(val="No")
	returnCode<-NULL
	
	getVolume=function(df) {
	  #find triangular tesselation of (x,y) grid
	  res=delaunayn(as.matrix(df[,-3]),full=TRUE,options="Qz")
	  #calulates sum of truncated prism volumes
	  vol=sum(mapply(function(triPoints,A) A/3*sum(df[triPoints,"z"]),
	             split.data.frame(res$tri,seq_along(res$areas)),
	             res$areas))
	  ifelse(vol>1,1.00,vol)
	  return(vol)
	}
	
	probability<-NULL   # used to display prob value while downloading pdf
	xmin<-NULL
	xmax<-NULL
	ymin<-NULL
	ymax<-NULL
	denExtendX<-4
	denExtendY<-4
	output$hover_info <- renderUI({
	  req(plts$kernel2)
	  
	  # brush <- (input$plot2D_x_y_ker_den1_brush)
	  brush<-input$plot2D_x_y_ker_den2_brush
	  
	  if ((is.null(brush))|(isolate(input$probCalc)==2)) return()
	  if(typeofSignals=="Binary"){
	    showNotification("Probability calculation not available for binary signals",duration = 2)
	    return()
	  }
	  if (isolate(input$load_data==1)){
	    showNotification("Data not loaded.",duration=2)
	    return()
	  }
	  
    data<-data2d_xy_ker_den()
    ker_data<-data[[1]]
    xyrange<-data[[4]]
    colnames(ker_data)<-c("x","y","z","LOG10","st")
    ker_data<-ker_data[,-5]
    ker_data<-ker_data[,-4]
    
    ker_data<-ker_data[((ker_data[,1]>=brush$xmin)&(ker_data[,1]<=brush$xmax))&((ker_data[,2]>=brush$ymin)&(ker_data[,2]<=brush$ymax)),]
    
    i<-which(INPUT_VECTOR_VXX_Title_Type_Text==isolate(input$sig1))
    j<-which(INPUT_VECTOR_VXX_Title_Type_Text==isolate(input$sig2))
    
    xdata<-data[[4]]
    ydata<-data[[5]]
    xmin<<-min(xdata)  #range of data used while saving pdf, <<- operator updates variables defined in parent scope
    xmax<<-max(xdata)
    ymin<<-min(ydata)
    ymax<<-max(ydata)
  
    reCalc<-FALSE
    if (((brush$xmin<=min(xdata))&&(brush$xmax>=max(xdata))&&(brush$ymin<=min(ydata))&&(brush$ymax>=max(ydata)))) {
      
      id<<-NULL
      id<<-showNotification("Complete Dataset selected. Calculating Probability.",duration=0)
      
      reCalc<-TRUE
      my_n_x_max = 500
      my_n_y_max = 500
      
      my_n_x = my_n_x_max
      my_n_y = my_n_y_max
      
      xmin<-min(xdata)-1*((max(xdata)-min(xdata))/denExtendX) # variables in local scope of same name
      xmax<-max(xdata)+1*((max(xdata)-min(xdata))/denExtendX)
      ymin<-min(ydata)-1*((max(ydata)-min(ydata))/denExtendY)
      ymax<-max(ydata)+1*((max(ydata)-min(ydata))/denExtendY)
      # ymin<-min(ydata)
      # ymax<-max(ydata)
      
    }else if (dim(ker_data)[1]<100){
      reCalc<-TRUE
      my_n_x_max = 50
      my_n_y_max = 50

      my_n_x = my_n_x_max
      my_n_y = my_n_y_max
      # 
      xmin<-brush$xmin
      xmax<-brush$xmax
      ymin<-brush$ymin
      ymax<-brush$ymax
      
    }
    
    if (reCalc){
      
      MY_X_Y_COUNT <- SALI_PREPARE_CORRELATION_TABLE_X_Y_for_count_ANALYSIS_ENHANCED(xdata,ydata,3 , 3)
      
      my_hx = bandwidth.nrd(MY_X_Y_COUNT$x)
      my_hy = bandwidth.nrd(MY_X_Y_COUNT$y)
      
      my_density_estimation <- bkde2D(x=cbind(xdata,ydata), bandwidth=c(my_hx, my_hy), gridsize = c(my_n_x, my_n_y),range.x=list(c(xmin,xmax),c(ymin,ymax)))
      names(my_density_estimation)<-c("x","y","z")
      
      my_x_vector=expand.grid(x=my_density_estimation$y,y=my_density_estimation$x)$y
      my_y_vector=expand.grid(x=my_density_estimation$y,y=my_density_estimation$x)$x
      my_z_vector=as.vector(t(my_density_estimation$z))
      
      ker_data<-data.frame(x=my_x_vector,
                           y=my_y_vector,
                           z=my_z_vector)
    }
    
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    
    left_pct <- (brush$xmax - brush$domain$left) / (brush$domain$right - brush$domain$left)
    top_pct <- (brush$domain$top - brush$ymin) / (brush$domain$top - brush$domain$bottom)
    # 
    # # calculate distance from left and bottom side of the picture in pixels
   
    # 
    left_px <- brush$range$left + left_pct * (brush$range$right - brush$range$left)
    top_px <- brush$range$top + top_pct * (brush$range$bottom - brush$range$top)
    
   
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    
    # incProgress(0.5,detail = msg)
    # prob<-round(getVolume(ker_data),5)
    prob<-getVolume(ker_data)
    
	  probability<<-prob
    
    if (!is.null(id)){
      removeNotification(id)
      id<<-NULL
    }
   
	  # actual tooltip created as wellPanel
	  wellPanel(
	    style = style,
	    p(HTML(paste0("<b> Probability in region: </b>", prob , "<br/>"
	    )))
	  )  
	  
	  
	})
	
	PROJECT_DATA<-NULL
	
	observeEvent(input$plot2D_x_y_ker_den2_dblclick, {
	  brush <- input$plot2D_x_y_ker_den2_brush
	  if (!is.null(brush)) {
	    ranges2D_x_y_ker_den$x <- c(brush$xmin, brush$xmax)
	    ranges2D_x_y_ker_den$y <- c(brush$ymin, brush$ymax)
	  }else {
	    ranges2D_x_y_ker_den$x <- NULL
	    ranges2D_x_y_ker_den$y <- NULL
	  }
	  
	})
	output$click_info_2D_x_y_ker_den2 <- renderPrint({
	  data.frame(x=input$plot2D_x_y_ker_den2_click$x, y=input$plot2D_x_y_ker_den2_click$y)
	})
	
	output$brush_info_2D_x_y_ker_den2 <- renderPrint({
	  #data.frame(xmin=input$plot2D_x_y_ker_den2_brush$xmin,ymin=input$plot2D_x_y_ker_den2_brush$ymin,xmax=input$plot2D_x_y_ker_den2_brush$xmax,ymax=input$plot2D_x_y_ker_den2_brush$ymax)
	  ker_data<-data2d_xy_ker_den()
	  data<-ker_data[[2]]
	  cn<-colnames(ker_data[[2]])
	  brushedPoints(data, input$plot2D_x_y_ker_den2_brush, xvar=cn[1], yvar=cn[2])
	})
	
	output$plot2D_x_y_ker_den2_d<-downloadHandler(
	  filename = function(){
	    paste0("XY_K_Den_",input$sel1,"_",input$sel2,"_",input$sig1,"__", input$sig2,".pdf")
	  },
	  content=function(file){
	    pdf(file,width=12, height=10)
	    
	    if ((!is.null(input$plot2D_x_y_ker_den2_brush))&&(!is.null(probability))){
	      brush<-input$plot2D_x_y_ker_den2_brush
	      brushData<-data.frame(xmin=brush$xmin,xmax=brush$xmax,ymin=brush$ymin,ymax=brush$ymax)
	      if((brush$xmin<=xmin)&&(brush$xmax>=xmax)&&(brush$ymin<=ymin)&&(brush$ymax>=ymax)){ #comparing brush extents with data range
	        brushData<-data.frame(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax)
	      }
	      pltProb<-plts$kernel2 +
	        geom_rect(data=brushData,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,alpha=1),color="grey50",inherit.aes = FALSE,show.legend = FALSE)+
	        # geom_label(data=brushData,aes(x=xmax,y=ymin,label=paste0("Probability in region: \n", round(probability,4))),inherit.aes = FALSE)
	        # geom_tile(data=brushData,aes(x=(xmin+xmax)/2,y=(ymin+ymax)/2,alpha=1,width=(xmax-xmin), height=(ymax-ymin)),color="grey50",inherit.aes = FALSE,show.legend = FALSE)+
	        geom_label(data=brushData,aes(x=(xmin+xmax)/2,y=(ymin+ymax)/2,label=paste0("Probability in region: \n", round(probability,4))),inherit.aes = FALSE)
	      
	      # if (grepl("FILTERED",kerneldendataFilter$val,fixed=TRUE)){
	      pltProb<-pltProb+annotate("text", Inf, Inf, label = kerneldendataFilter$val, hjust = 1, vjust = 1)
	      pltProb<-pltProb+annotate("text", -Inf, Inf, label = paste0("Creation Date: ",Sys.Date()), hjust = 0, vjust = 1)
	      # }
	      
	      plot(pltProb)
	    }else{
	      
	      plt<-plts$kernel2
	      # if (grepl("FILTERED",kerneldendataFilter$val,fixed=TRUE)){
	      plt<-plt+annotate("text", Inf, Inf, label = kerneldendataFilter$val, hjust = 1, vjust = 1)
	      plt<-plt+annotate("text", -Inf, Inf, label = paste0("Creation Date: ",Sys.Date()), hjust = 0, vjust = 1)
	      # }
	      plot(plt)
	      # plot(plts$kernel1)  
	    }
	    dev.off()
	  }
	)
	
	
	
	}
	
	
	#2D Kernel Density Sensitivity
	{
	  xykdenStepval<-NULL
	  xykdenboundMin<-NULL
	  xykdenboundMax<-NULL
	  xykdenSteps<-NULL
	  dataManualxykdensens<-NULL
	  datasplitxykdensens<-NULL
	  xykdensensStepRange<-list()
	  xykden_high_val<-NULL
	  xykden_low_val<-NULL
	  upperX<-NULL
	  lowerX<-NULL
	  upperY<-NULL
	  lowerY<-NULL
	  typesigxykden<-"Binary"  #type of selected xy signals - binary or continuous
	  ranges2D_x_y_kden_sens<-reactiveValues(x=NULL,y=NULL)
	  my_density_estimation_xykden_sens<-NULL
	  PROJECT_DATA_VXX_COPY_xykden_sens<-NULL
	  xykden_sens_dat<-NULL
	  
	  xykdensensdataFilter<-reactiveValues(val=paste0("Data Source : FULL \n","Filtering Source : NONE"))
	  
	  xykdensensStep<-function(){
	    i<-which(INPUT_VECTOR_VXX_Title_Type_Text==isolate(input$xykden_sens))
	    xykdenboundMin <<- INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE[i]
	    xykdenboundMax <<- INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE[i]
	    
	    xykden_sens_Stepval <<- ((xykdenboundMax - xykdenboundMin)/input$xykden_interval)
	    xykden_sens_Steps<<-input$xykden_interval
	    
	    updateSliderInput(session,"xykden_sens_step",value=1,min=1,max=xykden_sens_Steps,step=1)
	    
	  }
	  
	  xykdentypeofsig<-function(signal1,signal2){
	    sigtype<-"Binary"
	    INPUT_X_ROUND_TO_NUMBER = INPUT_ROUND_TO_NUMBER
	    INPUT_Y_ROUND_TO_NUMBER = INPUT_ROUND_TO_NUMBER
	    i<-which(INPUT_VECTOR_VXX_Title_Type_Text==signal1)
	    j<-which(INPUT_VECTOR_VXX_Title_Type_Text==signal2)
	    
	    MY_X_Y_COUNT <- SALI_PREPARE_CORRELATION_TABLE_X_Y_for_count_ANALYSIS_ENHANCED(
	      PROJECT_DATA_VXX[[i]],
	      PROJECT_DATA_VXX[[j]],
	      INPUT_X_ROUND_TO_NUMBER , INPUT_Y_ROUND_TO_NUMBER)
	    
	    MY_X_Y_COUNT <- MY_X_Y_COUNT[!is.na(MY_X_Y_COUNT$x)]
	    MY_X_Y_COUNT <- MY_X_Y_COUNT[!is.na(MY_X_Y_COUNT$y)]
	    
	    my_aggregate_x =
	      aggregate(MY_X_Y_COUNT$x , by=list(sort(MY_X_Y_COUNT$x)), FUN=mean, na.rm=TRUE)
	    
	    
	    my_aggregate_y =
	      aggregate(MY_X_Y_COUNT$y , by=list(sort(MY_X_Y_COUNT$y)), FUN=mean, na.rm=TRUE)
	    
	    if (dim(my_aggregate_x)[1] > 2 ){
	      if (dim(my_aggregate_y)[1] > 2 ){
	        sigtype<-"Continuous"
	      }
	    }
	    return(sigtype)
	  }
	  
	  my_hx_full<-NULL
	  my_hy_full<-NULL
	  xmax_full<-NULL
	  xmin_full<-NULL
	  ymax_full<-NULL
	  ymin_full<-NULL
	  
	  prepxykdensensdata<-reactive({
	    
	    if (plt_xykden_sens$y=="No")return()
	    
	    id<<-showNotification("Checking data..",duration=NULL)
	    filterSelection<-ifelse(!is.null(isolate(input$extent)),input$extent,0)
	    signal1<-isolate(input$sig1)
	    signal2<-isolate(input$sig2)
	    xykdensig<-isolate(input$xykden_sens)
	    sourceSelection<-isolate(input$load_data)
	    xykdenInt<-isolate(input$xykden_interval)
	    # manual<-isolate(input$freqsens_manual)
	    
	    low_val<-isolate(input$xykden_sens_low)
	    high_val<-isolate(input$xykden_sens_high)
	    
	    typesigxykden<<-xykdentypeofsig(signal1,signal2)
	    if(!is.null(id)){
	      removeNotification(id)
	      id<<-NULL
	    }
	    if (typesigxykden=="Binary"){
	      showNotification("Signal1 & Signal2 should be continuous",duration=2)
	      return()
	    }
	    
	    data<-NULL
	    
	    if (xykdenInt<=0){
	      showNotification("Incorrect number of intervals.",duration=2)
	    }else if (signal1=="No Selection"){
	      showNotification("Please select Signal 1",duration=2)
	    }else if (signal2=="No Selection"){
	      showNotification("Please select Signal 2",duration=2)
	    }else if (((!is.na(low_val))&(is.na(high_val)))|((is.na(low_val))&(!is.na(high_val)))){
	      showNotification("Both Low/High values required for manual range setting.",duration=2)
	    }else if (((sourceSelection==2)|(sourceSelection==3))&(signal1!="No Selection")&(signal2!="No Selection")){
	      
	      id<<-showNotification("Preparing Data for Plots.....",duration=NULL)
	      
	      i<-which(INPUT_VECTOR_VXX_Title_Type_Text==signal1)
	      j<-which(INPUT_VECTOR_VXX_Title_Type_Text==signal2)
	      k<-which(INPUT_VECTOR_VXX_Title_Type_Text==xykdensig)
	      
	      xykdenboundMin <<- INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE[k]
	      xykdenboundMax <<- INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE[k]
	      
	      if (sourceSelection==2){
	        
	        filterEventResult<-dataFilterEventApply(filterSelection,dataFilter1D,dataFilter2D,
	                                                dataFilternD,i,j,PROJECT_DATA_VXX,
	                                                dataFilterEvents=NULL,appEvtName=NULL)
	        
	        PROJECT_DATA_VXX_COPY<-filterEventResult[[1]]
	        val<-filterEventResult[[2]]
	        
	      }else if (sourceSelection==3){
	        filterEventResult<-dataFilterEventApply(filterSelection,dataFilter1D,dataFilter2D,
	                                                dataFilternD,i,j,PROJECT_DATA_VXX,
	                                                dataFilterEvents,appEvtName)
	        
	        PROJECT_DATA_VXX_COPY<-filterEventResult[[1]]
	        val<-filterEventResult[[2]]
	      }
	      
	      MY_X_Y_COUNT <- SALI_PREPARE_CORRELATION_TABLE_X_Y_for_count_ANALYSIS_ENHANCED(PROJECT_DATA_VXX_COPY[[i]],
	                                                                                     PROJECT_DATA_VXX_COPY[[j]],3 , 3)
	      
	      # PROJECT_DATA_VXX_COPY_xykden_sens <<- PROJECT_DATA_VXX_COPY
	      my_hx <- bandwidth.nrd(MY_X_Y_COUNT$x)
	      my_hy <- bandwidth.nrd(MY_X_Y_COUNT$y)
	      
	      # if((length(unique(PROJECT_DATA_VXX_COPY[[i]]))>1)&(length(unique(PROJECT_DATA_VXX_COPY[[j]]))>1)){
	      #   diffx<-max(diff(PROJECT_DATA_VXX_COPY[[i]]))
	      #   diffy<-max(diff(PROJECT_DATA_VXX_COPY[[j]]))
	      #   
	      #   if(diffx>my_hx){
	      #     my_hx<-diffx
	      #   }
	      #   if(diffy>my_hy){
	      #     my_hy<-diffy
	      #   }
	      # }
	      
	      my_hx_full<<-my_hx
	      my_hy_full<<-my_hy
	      
	      xmax_full<<-max(PROJECT_DATA_VXX_COPY[[i]])
	      xmin_full<<-min(PROJECT_DATA_VXX_COPY[[i]])
	      ymax_full<<-max(PROJECT_DATA_VXX_COPY[[j]])
	      ymin_full<<-min(PROJECT_DATA_VXX_COPY[[j]])
	      
	      my_density_estimation_xykden_sens <<- bkde2D(x=cbind(PROJECT_DATA_VXX_COPY[[i]],PROJECT_DATA_VXX_COPY[[j]]), bandwidth=c(my_hx, my_hy), gridsize = c(500, 500))
	      
	      names(my_density_estimation_xykden_sens)<<-c("x","y","z")
	      
	      my_x_vector=expand.grid(x=my_density_estimation_xykden_sens$y,y=my_density_estimation_xykden_sens$x)$y
	      my_y_vector=expand.grid(x=my_density_estimation_xykden_sens$y,y=my_density_estimation_xykden_sens$x)$x
	      my_z_vector=as.vector(t(my_density_estimation_xykden_sens$z))
	      
	      my_density_estimation_xykden_sens<<-data.frame(x=my_x_vector,y=my_y_vector,z=my_z_vector)
	      
	      if(max(MY_X_Y_COUNT$x)>(max(PROJECT_DATA_VXX_COPY[[i]]))){
	        upperX<<-(max(MY_X_Y_COUNT$x))
	      }else{
	        upperX<<-(max(PROJECT_DATA_VXX_COPY[[i]]))
	      }
	      if (min(MY_X_Y_COUNT$x)<(min(PROJECT_DATA_VXX_COPY[[i]]))){
	        lowerX<<-(min(MY_X_Y_COUNT$x))  
	      }else{
	        lowerX<<-(min(PROJECT_DATA_VXX_COPY[[i]]))
	      }
	     
	      if(max(MY_X_Y_COUNT$y)>(max(PROJECT_DATA_VXX_COPY[[j]]))){
	        upperY<<-(max(MY_X_Y_COUNT$y))
	      }else{
	        upperY<<-(max(PROJECT_DATA_VXX_COPY[[j]]))
	      }
	      if(min(MY_X_Y_COUNT$y)<(min(PROJECT_DATA_VXX_COPY[[j]]))){
	        lowerY<<-(min(MY_X_Y_COUNT$y))
	      }else{
	        lowerY<<-(min(PROJECT_DATA_VXX_COPY[[j]]))
	      }
	      
	      xykdensensdataFilter$val<-val
	      if ((is.na(low_val))&(is.na(high_val))){
	        ii<-which(INPUT_VECTOR_VXX_Title_Type_Text==isolate(input$xykden_sens))
	        xykdenboundMin <<- INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE[ii]
	        xykdenboundMax <<- INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE[ii]
	        
	        xykdenStepval <<- ((xykdenboundMax - xykdenboundMin)/xykdenInt)
	        xykdenSteps<<-xykdenInt
	        datasplitxykdensens<<-NULL
	        if (is.null(datasplitxykdensens)){
	          updateSliderInput(session,"xykden_sens_step",value=0,min=0,max=xykdenSteps,step=1)
	          updateSliderInput(session,"xykden_sens_step",value=1,min=1,max=xykdenSteps,step=1)
	          xykden_low_val<<-xykdenboundMin
	          xykden_high_val<<-xykden_low_val+xykdenStepval  
	        }
	        # else{
	        #   xykden_low_val<<-xykdenboundMin+(isolate(input$xykden_sens_step)-1)*xykdenStepval
	        #   xykden_high_val<<-xykden_low_val+xykdenStepval
	        # }
	        
	      }else if ((!is.na(low_val))&(!is.na(high_val))){
	        
	        if (high_val<=low_val){
	          showNotification("Signal High should be greater than Signal Low.",duration=2,type="warning")
	          return()
	        }else if ((low_val<xykdenboundMin)|(high_val>xykdenboundMax)){
	          showNotification("Signal High/Low not in range.",duration=2,type="warning")
	          return()
	        }else{
	          dataManualxykdensens<<-NULL
	          xykden_high_val<<-high_val
	          xykden_low_val<<-low_val
	        }
	      }
	      
	      if ((is.na(low_val))&(is.na(high_val))){
	        if (is.null(datasplitxykdensens)){
	          xykdensensStepRange<-lapply(seq_len(xykdenInt),function(x,xykdenStepval,xykdenboundMin){
	            return(c(xykdenboundMin+(x-1)*xykdenStepval,xykdenboundMin+x*xykdenStepval))
	          },xykdenStepval,xykdenboundMin)
	          
	          
	          datainrange<-lapply(xykdensensStepRange,function(x,k){
	            return(PROJECT_DATA_VXX_COPY[(PROJECT_DATA_VXX_COPY[[k]]>=x[1])&(PROJECT_DATA_VXX_COPY[[k]]<x[2]),])
	          },k)
	          
	          my_hx <- bandwidth.nrd(PROJECT_DATA_VXX_COPY[[i]])
	          my_hy <- bandwidth.nrd(PROJECT_DATA_VXX_COPY[[j]])
	          countx<-length(unique(PROJECT_DATA_VXX_COPY[[i]]))
	          county<-length(unique(PROJECT_DATA_VXX_COPY[[j]]))
	          
	          maxcounti<-max(unlist(lapply(datainrange,function(x,i){
	            ct<-0
	            if (dim(x)[1]>0){
	              ct<-length(unique(x[[i]]))
	            }
	            return(ct)
	          },i)))
	          maxcountj<-max(unlist(lapply(datainrange,function(x,j){
	            ct<-0
	            if (dim(x)[1]>0){
	              ct<-length(unique(x[[j]]))
	            }
	            return(ct)
	          },j)))
	          my_hx<-my_hx*(countx/maxcounti)
	          my_hy<-my_hy*(county/maxcountj)
	          
	          data<-lapply(datainrange,function(x,i,j,my_hx,my_hy){
	            MY_X_Y_COUNT<-NULL
	            ker_data<-NULL
	            xdata<-x[[i]]
	            ydata<-x[[j]]
	            if ((length(xdata)!=0)&(length(ydata)!=0)){
	              xmin<-min(xdata)
	              xmax<-max(xdata)
	              ymin<-min(ydata)
	              ymax<-max(ydata)
	              MY_X_Y_COUNT <- SALI_PREPARE_CORRELATION_TABLE_X_Y_for_count_ANALYSIS_ENHANCED(xdata,ydata,3 , 3)
	              
	              # my_hx = bandwidth.nrd(xdata)
	              # my_hy = bandwidth.nrd(ydata)
	              if((length(unique(xdata))>1)&(length(unique(ydata))>1)){
	                diffx<-max(diff(x[[i]]))
	                diffy<-max(diff(x[[j]]))
	                
	                if(diffx>my_hx){
	                  my_hx<-diffx
	                }
	                if(diffy>my_hy){
	                  my_hy<-diffy
	                }
	              }
	              
	              my_density_estimation <- bkde2D(x=cbind(xdata,ydata), bandwidth=c(my_hx, my_hy), gridsize = c(100, 100))
	                                              # range.x=list(c(xmin,xmax),c(ymin,ymax)))
	              names(my_density_estimation)<-c("x","y","z")
	              
	              my_x_vector=expand.grid(x=my_density_estimation$y,y=my_density_estimation$x)$y
	              my_y_vector=expand.grid(x=my_density_estimation$y,y=my_density_estimation$x)$x
	              my_z_vector=as.vector(t(my_density_estimation$z))
	              
	              ker_data<-data.frame(x=my_x_vector,y=my_y_vector,z=my_z_vector)
	              
	              ker_data<-ker_data[(ker_data$x>=lowerX)&(ker_data$x<=upperX),]
	              ker_data<-ker_data[(ker_data$y>=lowerY)&(ker_data$y<=upperY),]
	              
	              # if (upperX<max(ker_data$x)){
	              #   upperX<<-max(ker_data$x)
	              # }
	              # if (lowerX>min(ker_data$x)){
	              #   lowerX<<-min(ker_data$x)
	              # }
	              # if (upperY<max(ker_data$y)){
	              #   upperY<<-max(ker_data$y)
	              # }
	              # if (lowerY>min(ker_data$y)){
	              #   lowerY<<-min(ker_data$y)
	              # }
	              
	            }
	            
	            return(list(MY_X_Y_COUNT,ker_data,data.frame(x=xdata,y=ydata)))
	            
	          },i,j,my_hx,my_hy)
	          
	        }else{
	          data<-datasplitxykdensens
	        }
	      }else if ((!is.na(low_val))&(!is.na(high_val))){
	        
	        datainrange<-PROJECT_DATA_VXX_COPY[(PROJECT_DATA_VXX_COPY[[k]]>=xyden_low_val)&(PROJECT_DATA_VXX_COPY[[k]]<xyden_high_val),]
	        xdata<-datainrange[i]
	        ydata<-datainrange[j]
	        xmin<-min(xdata)
	        xmax<-max(xdata)
	        ymin<-min(ydata)
	        ymax<-max(ydata)
	        MY_X_Y_COUNT <- SALI_PREPARE_CORRELATION_TABLE_X_Y_for_count_ANALYSIS_ENHANCED(xdata,ydata,3 , 3)
	        
	        my_hx = bandwidth.nrd(MY_X_Y_COUNT$x)
	        my_hy = bandwidth.nrd(MY_X_Y_COUNT$y)
	        
	        my_density_estimation <- bkde2D(x=cbind(xdata,ydata), bandwidth=c(my_hx, my_hy), gridsize = c(50, 50))
	        names(my_density_estimation)<-c("x","y","z")
	        
	        my_x_vector=expand.grid(x=my_density_estimation$y,y=my_density_estimation$x)$y
	        my_y_vector=expand.grid(x=my_density_estimation$y,y=my_density_estimation$x)$x
	        my_z_vector=as.vector(t(my_density_estimation$z))
	        
	        ker_data<-data.frame(x=my_x_vector,
	                             y=my_y_vector,
	                             z=my_z_vector)
	        data<-list(MY_X_Y_COUNT,ker_data,data.frame(x=xdata,y=ydata))
	      }
	    }
	    
	    return(data)
	    
	  })
	  
	  output$xykdensenslow_txt<-renderText({
	    if (input$xykden_sens_step==0)return()
	    xykden_low_val
	  })
	  
	  output$xykdensenshigh_txt<-renderText({
	    if (input$xykden_sens_step==0)return()
	    xykden_high_val
	  })
	  
	  output$xykdensensDatasource<-renderText({
	    
	    low_val<-isolate(input$xykden_sens_low)
	    high_val<-isolate(input$xykden_sens_high)
	    
	    if ((is.na(low_val))&(is.na(high_val))){
	      datasplitxykdensens<<-prepxykdensensdata()
	    }else if ((!is.na(low_val))&(!is.na(high_val))){
	      dataManualxykdensens<<-prepxykdensensdata()
	    }
	    
	    if(!is.null(id)){
	      removeNotification(id)
	      id<<-NULL
	      # showNotification("Please select signal to plot.",duration=2)
	    }
	    isolate(xykdensensdataFilter$val)
	  })
	  
	  recalculate_Kernel_Density<-function(xdata,ydata,my_n_x=200,my_n_y=200,xmin,xmax,ymin,ymax,extendXRange=0,extendYRange=0,dataType="partial",my_hx,my_hy){
	    
	    MY_X_Y_COUNT <- SALI_PREPARE_CORRELATION_TABLE_X_Y_for_count_ANALYSIS_ENHANCED(xdata,ydata,3 , 3)
	    
	    # my_hx = bandwidth.nrd(MY_X_Y_COUNT$x)
	    # my_hy = bandwidth.nrd(MY_X_Y_COUNT$y)
	    # 
	    # if((length(unique(xdata))>1)&(length(unique(ydata))>1)){
	    #   diffx<-max(diff(xdata))
	    #   diffy<-max(diff(ydata))
	    #   
	    #   if(diffx>my_hx){
	    #     my_hx<-diffx
	    #   }
	    #   if(diffy>my_hy){
	    #     my_hy<-diffy
	    #   }
	    # }
	    delx<-(xmax-xmin)
	    dely<-(ymax-ymin)
	    xmin<-xmin-extendXRange*(delx)
	    xmax<-xmax+extendXRange*(delx)
	    ymin<-ymin-extendYRange*(dely)
	    ymax<-ymax+extendYRange*(dely)
	    
	    my_density_estimation <- bkde2D(x=cbind(xdata,ydata), bandwidth=c(my_hx, my_hy), gridsize = c(my_n_x, my_n_y),range.x=list(c(xmin,xmax),c(ymin,ymax)))
	    names(my_density_estimation)<-c("x","y","z")
	    
	    my_x_vector=expand.grid(x=my_density_estimation$y,y=my_density_estimation$x)$y
	    my_y_vector=expand.grid(x=my_density_estimation$y,y=my_density_estimation$x)$x
	    my_z_vector=as.vector(t(my_density_estimation$z))
	    
	    ker_data<-data.frame(x=my_x_vector,
	                         y=my_y_vector,
	                         z=my_z_vector)
	  }
	  
	  xykden_sens_plotted_density<-NULL
	  
	  output$plot2D_xykden_sens<-renderPlot({
	    
	    if (input$xykden_sens_step==0)return()
	    
	    low_val<-isolate(input$xykden_sens_low)
	    high_val<-isolate(input$xykden_sens_high)
	    xyden_sens_dat<-NULL
	    req(xykdensensdataFilter$val)
	    if ((is.na(low_val))&(is.na(high_val))){
	      req(datasplitxykdensens)
	      if ((input$xykden_sens_step!=0)&(input$xykden_sens_step<=isolate(input$xykden_interval))) {
	        xykden_sens_dat<<-datasplitxykdensens[[input$xykden_sens_step]] 
	      }
	    }else if ((!is.na(low_val))&(!is.na(high_val))){
	      xykden_sens_dat<<-dataManualxykdensens
	    }
	    
	    kerdenPlt2<-ggplot()
	    i<-which(INPUT_VECTOR_VXX_Title_Type_Text==isolate(input$sig1))
	    j<-which(INPUT_VECTOR_VXX_Title_Type_Text==isolate(input$sig2))
	    
	    MY_X_Y_COUNT<-xykden_sens_dat[[1]]
	    data<-xykden_sens_dat[[2]]
	    
	    if ((!is.null(MY_X_Y_COUNT))&(!is.null(data))){
	      
	      my_legend_title_size = 20
	      my_legend_title_color = "black"
	      my_legend_title_face = "bold"
	      
	      #----- not working ... -----
	      my_legend_title_hjust = 0
	      my_legend_title_vjust = 0
	      #---------------------------
	      
	      my_legend_text_size = 20
	      my_legend_text_color = "black"
	      my_legend_text_angle = 0
	      
	      my_axis_text_size = 10
	      # my_axis_text_face = "bold"
	      
	      my_axis_title_size = 15
	      # my_axis_title_face = "bold"
	      
	      my_title_size = 15
	      # my_title_color = "black"
	      # my_title_face = "bold"
	      
        #--- point size
        #my_size = 2
        my_size = 0.01

        #--- contour size
        my_contour_thickness = 2

        #  used
        # nearly perfect 1
        # my_pch = 16

        # most perfect 5
        my_pch = "."

        # used
        my_density_geom = "tile"

        my_legend_key_size = 3
        my_legend_position = "right"

        my_n_x = my_n_x_max
        my_n_y = my_n_y_max

        my_hx = bandwidth.nrd(MY_X_Y_COUNT$x)
        my_hy = bandwidth.nrd(MY_X_Y_COUNT$y)
          
	      colnames(data)<-c("x","y","z")
	      MY_X_Y_KERNEL_DENSITY<-data.table(x=data$x,y=data$y,z=data$z)
	      
	      my_legend_title = paste("KERNEL-DENSITY :", "\n", "\n",sep="")

	      my_legend_breaks =  seq(from = min(MY_X_Y_KERNEL_DENSITY$z), to = max(MY_X_Y_KERNEL_DENSITY$z),length.out = 5)

	      my_legend_range=c(min(my_legend_breaks),max(my_legend_breaks))
	      
	      my_xlab = INPUT_VECTOR_VXX_Title_Type_Underscored_TEXT[i]
	      my_ylab = INPUT_VECTOR_VXX_Title_Type_Underscored_TEXT[j]
	      my_title<-paste0(INPUT_Analysis_Title,"\n",round(xykden_low_val,2),"<=",isolate(input$xykden_sens),
	                       "<",round(xykden_high_val,2))

	      my_colors_for_plot = my_colors_for_2D_KERNEL_DENSITY_X_Y
	      
	      densdf <- MY_X_Y_KERNEL_DENSITY
	      
	      xykden_sens_plotted_density<<-MY_X_Y_KERNEL_DENSITY
	      
	      # xmin<-min(MY_X_Y_COUNT$x)
	      # xmax<-max(MY_X_Y_COUNT$x)
	      # ymin<-min(MY_X_Y_COUNT$y)
	      # ymax<-max(MY_X_Y_COUNT$y)
	      # xmin<-xmin-(abs(upperX-lowerX)/2)
	      # xmax<-xmax+(abs(upperX-lowerX)/2)
	      # ymin<-ymin-(abs(upperY-lowerY)/2)
	      # ymax<-ymax+(abs(upperY-lowerY)/2)
	      # densdf<-densdf[(densdf$x>=xmin)&(densdf$x<=xmax),]
	      # densdf<-densdf[(densdf$y>=ymin)&(densdf$y<=ymax),]
	      
	      # upperX<-INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE[i]
	      # lowerX<-INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE[i]
	      # 
	      # upperY<-INPUT_VECTOR_DOMAIN_RIGHT_BOUNDARY_VALUE[j]
	      # lowerY<-INPUT_VECTOR_DOMAIN_LEFT_BOUNDARY_VALUE[j]
	      
	      # if (upperX<max(MY_X_Y_KERNEL_DENSITY$x)){
	      #   upperX<-max(MY_X_Y_KERNEL_DENSITY$x)
	      # }
	      # if (lowerX>min(MY_X_Y_KERNEL_DENSITY$x)){
	      #   lowerX<-min(MY_X_Y_KERNEL_DENSITY$x)
	      # }
	      # if (upperY<max(MY_X_Y_KERNEL_DENSITY$y)){
	      #   upperY<-max(MY_X_Y_KERNEL_DENSITY$y)
	      # }
	      # if (lowerY>min(MY_X_Y_KERNEL_DENSITY$y)){
	      #   lowerY<-min(MY_X_Y_KERNEL_DENSITY$y)
	      # }
	        
	            
    #---- prepare ratio for quadratic graph ---
    # ratio.display <- 1/1
    # ratio.values <- (max(MY_X_Y_COUNT$x)-min(MY_X_Y_COUNT$x))/
    #   (max(MY_X_Y_COUNT$y)-min(MY_X_Y_COUNT$y))
    
    #---- sp1: create kenel dnsity plot with contours ----
   
      kerdenPlt2<-ggplot(MY_X_Y_COUNT, aes(x = x, y = y)) +
        geom_point(pch=my_pch,size = my_size) +
        
        # coord_fixed(ratio.values / ratio.display) +
        geom_contour(aes(x=x,y=y,z=z, color=..level..),data=densdf,size = my_contour_thickness,inherit.aes = FALSE)	+
        scale_colour_gradient(low = "darkblue",high = "cyan",guide=FALSE) +
        geom_contour(aes(x=x,y=y,z=z),data=densdf,color= "white",size= 0.01,inherit.aes = FALSE)  +
        # geom_raster(aes(x=x,y=y,fill=z,alpha=z),data=densdf,show.legend=FALSE,inherit.aes = FALSE)+
        # stat_density2d(geom = my_density_geom,
        #                aes(fill=..density..,alpha=..density..),
        #                contour = FALSE,
        #                show.legend=FALSE,
        #                n = c(my_n_x, my_n_y),
        #                size = my_size) +
        # scale_fill_gradient(my_legend_title,low="darkblue",high="cyan",na.value=my_na_color_value,guide=FALSE) +
        theme(panel.grid.minor = element_line(colour = "white", linetype = "dotted", size = 1)) +
        # theme(legend.title = element_text(size= my_legend_title_size, colour=my_legend_title_color, hjust = my_legend_title_hjust, vjust = my_legend_title_vjust, face = my_legend_title_face)) +
        # theme(legend.position = my_legend_position ) +
        # theme(legend.background = element_rect(fill="NA"), legend.margin = unit(1, "cm")) +
        # theme(legend.key.size = unit(my_legend_key_size, "cm")) +
        # theme(legend.text = element_text(size = my_legend_text_size, colour = my_legend_text_color, angle = my_legend_text_angle)) +
        theme(axis.text=element_text(size=my_axis_text_size),axis.title=element_text(size=my_axis_title_size))+
        # theme(plot.title = element_text(size = my_title_size, colour = my_title_color, face=my_title_face))+
        theme(plot.title = element_text(size = my_title_size))+
        xlim(lowerX,upperX) +
        ylim(lowerY,upperY) +
        # scale_x_continuous() +
        # scale_y_continuous() +
        labs( title=my_title,x =my_xlab , y = my_ylab)+
        coord_cartesian(xlim=ranges2D_x_y_kden_sens$x, ylim=ranges2D_x_y_kden_sens$y)
	    }
	    
	    plts$xykden_sens<<-kerdenPlt2
	    isolate(plts$xykden_sens)
	  })
	  
	  output$xykden_sens_prob <- renderUI({
	    req(plts$xykden_sens)
	    
	    brush<-input$plot2D_xykden_sens_brush
	    
	    if ((is.null(brush))|(input$xykden_sens_prob_enable==1)) return() #input$xykden_sens_prob_enable==1 is disabled
	    if(typesigxykden=="Binary"){
	      showNotification("Probability calculation not available for binary signals",duration = 2)
	      return()
	    }
	    if (isolate(input$load_data==1)){
	      showNotification("Data not loaded.",duration=2)
	      return()
	    }
	    # if (is.null(xykden_sens_plotted_density)) return()
	    
	    # ker_data<-xykden_sens_plotted_density
	    if (is.null(xykden_sens_dat))return()
	    
      xydata<-xykden_sens_dat[[3]]
      MY_X_Y_COUNT <- xykden_sens_dat[[1]]
      ker_data<-xykden_sens_dat[[2]]
      
      if ((is.null(xydata))|(is.null(MY_X_Y_COUNT))|(is.null(ker_data)))return()
      
      my_hx = bandwidth.nrd(MY_X_Y_COUNT$x)
      my_hy = bandwidth.nrd(MY_X_Y_COUNT$y)
      
      xdata<-xydata$x
      ydata<-xydata$y
      xmin<-min(xdata)
      xmax<-max(xdata)
      ymin<-min(ydata)
      ymax<-max(ydata)
      ker_data<-recalculate_Kernel_Density(xdata,ydata,500,500,xmin_full,xmax_full,ymin_full,ymax_full,0,0,"partial",my_hx_full,my_hy_full)
	    reCalc<-FALSE
	    complete_data_selected<-FALSE
	    if (((brush$xmin<=min(xdata))&&(brush$xmax>=max(xdata))&&(brush$ymin<=min(ydata))&&(brush$ymax>=max(ydata)))) {

	      id<<-NULL
	      id<<-showNotification("Complete Dataset selected. Calculating Probability.",duration=0)

	      reCalc<-TRUE
	      complete_data_selected<-TRUE
	      # my_n_x_max = 200
	      # my_n_y_max = 200
	      # 
	      # my_n_x = my_n_x_max
	      # my_n_y = my_n_y_max
	      # 
	      # xmin<-min(xdata)-1*((max(xdata)-min(xdata)))
	      # xmax<-max(xdata)+1*((max(xdata)-min(xdata)))
	      # ymin<-min(ydata)-1*((max(ydata)-min(ydata)))
	      # ymax<-max(ydata)+1*((max(ydata)-min(ydata)))
	      
	      conditional_prob<-0
	      extendX<-1
	      extendY<-1
	      counter<-1
	      while(conditional_prob<0.95){
	        extendX<-extendX*counter
	        extendY<-extendY*counter
	        
	        ker_data<-recalculate_Kernel_Density(xdata,ydata,500,500,xmin,xmax,ymin,ymax,extendX,extendY,"partial",my_hx_full,my_hy_full)
	        
	        conditional_prob<-round(getVolume(as.data.frame(ker_data)),5)
	        counter<-counter+1
	      }
	      
	    }else if (dim(ker_data)[1]<100){
	      reCalc<-TRUE
	      my_n_x_max = 50
	      my_n_y_max = 50

	      my_n_x = my_n_x_max
	      my_n_y = my_n_y_max
	      #
	      xmin<-brush$xmin
	      xmax<-brush$xmax
	      ymin<-brush$ymin
	      ymax<-brush$ymax
	      
	      ker_data<-recalculate_Kernel_Density(xdata,ydata,50,50,xmin,xmax,ymin,ymax,0,0,"partial",my_hx_full,my_hy_full)

	    }

	    # if (reCalc){

	      # MY_X_Y_COUNT <- SALI_PREPARE_CORRELATION_TABLE_X_Y_for_count_ANALYSIS_ENHANCED(xdata,ydata,3 , 3)
	      # 
	      # if((length(unique(xdata))>1)&(length(unique(ydata))>1)){
	      #   diffx<-max(diff(xdata))
	      #   diffy<-max(diff(ydata))
	      #   
	      #   if(diffx>my_hx){
	      #     my_hx<-diffx
	      #   }
	      #   if(diffy>my_hy){
	      #     my_hy<-diffy
	      #   }
	      # }
	      # 
	      # my_density_estimation <- bkde2D(x=cbind(xdata,ydata), bandwidth=c(my_hx, my_hy), gridsize = c(my_n_x, my_n_y),range.x=list(c(xmin,xmax),c(ymin,ymax)))
	      # names(my_density_estimation)<-c("x","y","z")
	      # 
	      # my_x_vector=expand.grid(x=my_density_estimation$y,y=my_density_estimation$x)$y
	      # my_y_vector=expand.grid(x=my_density_estimation$y,y=my_density_estimation$x)$x
	      # my_z_vector=as.vector(t(my_density_estimation$z))
	      # 
	      # ker_data<-data.frame(x=my_x_vector,
	      #                      y=my_y_vector,
	      #                      z=my_z_vector)
	    # }
	    
	    #xyrange<-data[[4]]
	    
	    if (!complete_data_selected){
	      ker_data<-ker_data[((ker_data$x>=brush$xmin)&(ker_data$x<=brush$xmax))&((ker_data$y>=brush$ymin)&(ker_data$y<=brush$ymax)),]
	    }
	    
	    ker_data_complete<-my_density_estimation_xykden_sens[((my_density_estimation_xykden_sens$x>=brush$xmin)&(my_density_estimation_xykden_sens$x<=brush$xmax))
	                                                                 &((my_density_estimation_xykden_sens$y>=brush$ymin)&(my_density_estimation_xykden_sens$y<=brush$ymax)),]
	    
	    # calculate point position INSIDE the image as percent of total dimensions
	    # from left (horizontal) and from top (vertical)
	    
	    left_pct <- (brush$xmax - brush$domain$left) / (brush$domain$right - brush$domain$left)
	    top_pct <- (brush$domain$top - brush$ymin) / (brush$domain$top - brush$domain$bottom)
	    # 
	    # # calculate distance from left and bottom side of the picture in pixels
	    
	    # 
	    left_px <- brush$range$left + left_pct * (brush$range$right - brush$range$left)
	    top_px <- brush$range$top + top_pct * (brush$range$bottom - brush$range$top)
	    
	    
	    # create style property fot tooltip
	    # background color is set so tooltip is a bit transparent
	    # z-index is set so we are sure are tooltip will be on top
	    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
	                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
	    
	    
	    # conditional_prob<-round(getVolume(as.data.frame(ker_data)),5)
	    # total_prob<-round(getVolume(as.data.frame(ker_data_complete)),5)
	    
	    conditional_prob<-getVolume(as.data.frame(ker_data))
	    total_prob<-getVolume(as.data.frame(ker_data_complete))
	    
	    # probability<<-prob
	    
	    if (!is.null(id)){
	      removeNotification(id)
	      id<<-NULL
	    }
	    
	    # actual tooltip created as wellPanel
	    # wellPanel(
	    #   style = style,
	    #   p(HTML(paste0("<b> Conditional Probability: </b>", conditional_prob , "<br>",
	    #                 "<b> Total Probability: </b>", total_prob)))
	    # )  
	    
	    wellPanel(
	      style = style,
	      p(HTML(paste0("<b> Probability with Interval Data: </b>", conditional_prob , "<br>",
	                    "<b> Probability with Total Data: </b>", total_prob)))
	    )  
	    
	    
	    
	  })
	  
	  observeEvent(input$xykden_sens_step,ignoreInit = TRUE,{
	    
	    if((!is.na(input$xykden_sens_low))&(!is.na(input$xykden_sens_high))){
	      showNotification("Clear Low (Manual) & High (Manual) before animation.",duration = 2)
	    }
	    
	    low_val<-xykdenboundMin+(input$xykden_sens_step-1)*xykdenStepval
	    high_val<-low_val+xykdenStepval
	    
	    xykden_high_val<<-high_val
	    xykden_low_val<<-low_val
	   
	  })
	  
	  observeEvent(input$xykden_interval,ignoreInit = TRUE,{
	    datasplitxykdensens<<-NULL
	    xykdensensdataFilter$val<-NULL
	    
	  })
	  
	  observeEvent(input$plot2D_xykden_sens_dblclick, {
	    brush <- input$plot2D_xykden_sens_brush
	    if (!is.null(brush)) {
	      ranges2D_x_y_kden_sens$x <- c(brush$xmin, brush$xmax)
	      ranges2D_x_y_kden_sens$y <- c(brush$ymin, brush$ymax)
	    }else {
	      ranges2D_x_y_kden_sens$x <- NULL
	      ranges2D_x_y_kden_sens$y <- NULL
	    }
	    
	  })
	  
	  output$plot2D_xykden_sens_d<-downloadHandler(
	    filename = function(){
	      paste0(input$sel1,"_",input$sel2,input$sig1,"_",input$sig2,"_XY_KDEN_Sensitivity_Low_",input$xyden_sens_low,
	             "_High_",input$xykden_sens_high,"_",input$xykden_sens,".pdf")
	    },
	    content=function(file){
	      
	      pdf(file=file,width=15, height=7)
	      plt<-plts$xykden_sens
	      plt<-plt+annotate("text", Inf, Inf, label = xykdensensdataFilter$val, hjust = 1, vjust = 1)
	      plt<-plt+annotate("text", -Inf, Inf, label = paste0("Creation Date: ",Sys.Date()), hjust = 0, vjust = 1)
	      # for (i in 1:length(inserted)){
	        plot(plt)
	      # }
	      dev.off()
	    }
	  )
	}


}
shinyApp(ui=ui,server=server)




