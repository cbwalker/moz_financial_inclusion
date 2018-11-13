
#Summaries UI
library(shiny)
library(ggplot2)
library(leaflet)

#Source Files
#hiResTif<-('./data/F182013.v4c_web.avg_vis.tif')
# hiResLights<-readRDS('./data/lightFullCountry.rds')
# moz.d<-readRDS("./data/moz_d.rds")
# moz.r<-readRDS("./data/moz_r.rds")
# locations<-readRDS('./data/distcenters.rds')
summary_data<-readRDS('./data/summary_data.rds')

#UI Features
shinyUI(fluidPage(

	titlePanel("Financial Inclusion in Mozambique"),

	hr(),

	tabsetPanel(
		tabPanel("District Rankings", fluidRow(
			column(1,
				br()
				),
			column(5,
				plotOutput("summary_plot") #, height = "400px")
				),
			column(5,
				plotOutput("scatter_plot")
				),
			column(1,
				br()
				)
			), 
		hr(),
		hr(),
		fluidRow(
			column(2,
				selectInput('province', 'Choose Province', c('All', levels(summary_data$Province)))
				),
			column(2,
			br()
				),
			column(2,
				selectInput('rural', 'District type', c('Rural', 'Urban', 'Mix', 'All'))
				),
			column(2,
				sliderInput('branches', 'Current number of branches', 
					min=0, max=18, value=c(0,18), 
					step=, round=0),
				checkboxInput('bankButton', label='Filter by bank branches', value=FALSE)
				),
			column(2,
				sliderInput('popDens', 'Population Density', 
					min=sort(summary_data$Population.Density)[1], max=sort(summary_data$Population.Density)[length(sort(summary_data$Population.Density))-1], value=c(sort(summary_data$Population.Density)[1], sort(summary_data$Population.Density)[length(sort(summary_data$Population.Density))-1]), 
					step=10, round=0),
				checkboxInput('popDensButton', label='Filter by population density', value=FALSE)
				),
			column(2,
				sliderInput('pop', 'Population Level', 
					min=sort(summary_data$Population.Level)[1], max=sort(summary_data$Population.Level)[length(sort(summary_data$Population.Level))-1], value=c(sort(summary_data$Population.Level)[1], sort(summary_data$Population.Level)[length(sort(summary_data$Population.Level))-1]), 
					step=10, round=0),
				checkboxInput('popButton', label='Filter by population density', value=FALSE)
				)
			)), 
		tabPanel("Intra-district branch allocation", fluidRow(
			column(1,
				br()
				),
			column(4,
				plotOutput("light_plot", height = "400px")
				),
			column(2,
				br(),
				br(),
				br(),
				h5("Estimate of people covered:"),
				textOutput("coverageEstimate"),
				tags$head(tags$style("#coverageEstimate{color: red;
                                 font-size: 40px;
                                 text-indent: 35px;
                                 }"
                         )
              	),
				br(),
				h5("Average distance between branches (km):"),
				textOutput("avDistance"),
				tags$head(tags$style("#avDistance{color: red;
                                 font-size: 40px;
                                 text-indent: 35px;
                                 }"
                         )
              	),
				br(),
				br(),
				br(),
				br(),
				br(),
				actionButton("action", label = "Optimise Locations!")
				),
			column(4,
				leafletOutput("googlemap")
				),
			column(1,
				br()
				)
			),
		fluidRow(
			column(2,
				selectInput('province2', 'Choose Province', c('All', levels(summary_data$Province)))
				),
			column(2,
				uiOutput('districtsFiltered')
				),
			column(2,
				br()
				),
			column(2,
				sliderInput('coverage', 'Branch reach (km)', 
					min=1, max=20, value=10, 
					step=1)
				),
			column(2,
				numericInput('branchBudget', 'Number of branches in budget', 2)
				),
			column(2,
				sliderInput('ratio', 'Location:Light Ratio', 
					min=0, max=1, value=0, 
					step=0.1)
				))),
		tabPanel("Guide", 
			hr(),
			helpText("This app allows you to blah blah blah /n Note we have excluded Maputo City as a region and its districts since we are focussed on expanding services outside the city."))
		),

hr()

))