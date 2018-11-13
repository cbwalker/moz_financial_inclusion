
#Load Libraries
#install.packages("Rcpp")
library(dplyr)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(FactoClass)
library(leaflet)
library(raster)
library(geosphere)



#Source Files
#hiResTif<-('./data/F182013.v4c_web.avg_vis.tif')
lights<-raster("./data/lights.grd")
map<-readRDS("./data/moz_d.rds")
summary_data<-readRDS('./data/summary_data.rds')
source('helpers.R')


#Recode
#regions$NAME_1<-as.factor((regions$NAME_1))        


shinyServer(
	function(input, output) {

		#Reactive Filtering
		inputData<-reactive({
		#Inputs
		#summary_data<-readRDS('./data/summary_data.rds')
		province<-input$province
		district.type<-input$rural
		branches<-input$bankButton
		branchLo<-input$branches[1]
		branchHi<-input$branches[2]
		density<-input$popDensButton
		densityHi<-input$popDens[2]
		densityLo<-input$popDens[1]
		pop.level<-input$popButton
		popHi<-input$pop[2]
		popLo<-input$pop[1]
		#Filter
		return(filter_summary(summary_data, province, district.type, branches, branchLo, branchHi, density, densityHi,densityLo, pop.level, popHi, popLo))
		})

		#Plot the summary plot
		output$summary_plot<-renderPlot({
			data<-inputData()
			summary_plot(data)
			})

		#Plot the summary plot
		output$scatter_plot<-renderPlot({
			data<-inputData()
			scatter_plot(data)
			})

		
		#Reactive selectInputs from region to district
		provinceFiltered<-reactive({
			province_filter<-input$province2
			if(province_filter=='All'){
			districts<-	as.character(sort(summary_data$District))
			} else {
				districts<-as.character(sort(summary_data[summary_data$Province==province_filter, ]$District))
			}
			#districts<-summary_data %>% dplyr::filter(Province == province_filter) %>% dplyr::select(District)
			return(districts)
			})

		#reactive filter
		output$districtsFiltered<- renderUI({
			selectInput('district', 'Choose District', provinceFiltered())
			})

		#Collect reactive Map data
		mapData<-eventReactive( input$action, {
			dist<-input$district
			buildBranches<-input$branchBudget
			ratio<-input$ratio
			output<-plotOptimisedMap(map, lights, dist, ratio, buildBranches)
			return(output)
			})

		#Plot Light data
		######Problem AREA - Isolate to get to root cause
		output$light_plot<-renderPlot({
			input<-mapData()
			output<-input$g
			return(output)
			})

		# #Plot googlemap
		output$googlemap<-renderLeaflet({
			input<-mapData()
			coords<-input$coords
			leaflet() %>% addTiles() %>% 
			addMarkers(lng = coords$lng, lat = coords$lat)
			})


		
		#Coverage estimate output
		output$coverageEstimate<-renderText({
			coverageData<-mapData()
			totalLights<-coverageData$totalLights
			coords<-coverageData$coords
			radius<-input$coverage
			lights_df<-coverageData$df
			distPop<-summary_data %>% filter(District==input$district) %>% dplyr::select(Population.Level)
			distPop<-as.numeric(distPop$Population.Level)
			estimates<-estimateCoverage(totalLights, coords, radius, lights_df)
			paste(round(sum(estimates)*distPop, digits=0))	
			})

		output$avDistance<-renderText({
			coverageData<-mapData()
			matrix <- distm(coverageData$coords/1000)
			avDistance<-round(mean(matrix[lower.tri(matrix)]), digits=0)
			paste(avDistance)
			})


		


			})

