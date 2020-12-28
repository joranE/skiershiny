library(shiny)
library(config)
library(DBI)
library(pool)
library(RPostgres)
library(dplyr)
library(dbplyr)
library(DT)
library(ggplot2)
library(tidyr)
library(reactable)
library(glue)
library(patchwork)
library(readr)
library(shinythemes)
library(shinycssloaders)
library(stringr)
library(purrr)

source("ss_funs.R")
source("read_sql.R")
source("hth_funs.R")
source("dev_funs.R")
source("snap_funs.R")

#### DB Connection Prep ####
#Create connection pool
con_args <- config::get("dataconnection",config = "default")
con_pool <- pool::dbPool(RPostgres::Postgres(),
												 host = con_args$host,
												 dbname = con_args$dbname,
												 user = con_args$user,
												 password = con_args$password,
												 port = con_args$port,
												 sslmode = con_args$sslmode,
												 bigint = "integer")

skier <- dplyr::tbl(src = con_pool,"skier")

#Prepare skier name list
skier_names <- skier %>%
	select(name,fisid,compid) %>%
	collect() %>%
	mutate(name_fisid = paste(name," (",fisid,")",sep = "")) %>%
	sample_frac(size = 0.75)
skier_names <- as.list(setNames(skier_names$compid,skier_names$name_fisid))
skier_names <- c("Start typing..." = "",skier_names)

ui <- navbarPage(
	title = "Statistical Skier",
	id = "nav_tabs",
	theme = shinytheme("flatly"),
	
	#### SKIER SUMMARIES ####
	tabPanel(
		title = "Skier Summary",
		value = "ss_tab",
		
		sidebarLayout(
			
			#### Skier Summary Sidebar ####
			sidebarPanel(
				selectizeInput(
					inputId = "ss_skier_compid",
					label = "Skier",
					choices = NULL,
					multiple = FALSE
				),
				conditionalPanel(
					condition = "input.ss_tabs != 'ss_data'",
					radioButtons(
						inputId = "ss_maj_int",
						label = "Show",
						choices = list("All events" = "all","WC/TdS/OWG/WSC events" = "maj_int")
					),
					hr(),
					checkboxGroupInput(
						inputId = "ss_tech",
						label = "Technique",
						choices = list("Freestyle" = "F","Classic" = "C","Mixed" = "FC"),
						selected = c("F","C","FC")
					),
					conditionalPanel(
						condition = "input.ss_tabs == 'ss_dst'",
						sliderInput(
							inputId = "ss_dst_race_length",
							label = "Distance race length (km)",
							min = 2.5,
							max = 100,
							value = c(2.5,100),
							post = "km"
						),
						checkboxGroupInput(
							inputId = "ss_format",
							label = "Distance race format",
							choices = list("Interval" = "Interval",
														 "Mass" = "Mass",
														 "Skiathlon" = "Skiathlon",
														 "Pursuit" = "Pursuit",
														 "Pursuit Break" = "Pursuit Break"),
							selected = c("Interval","Mass","Skiathlon","Pursuit","Pursuit Break")
						)
					),
					numericInput(
						inputId = "ss_fis_pts_clip",
						label = "Clip FIS points to:",
						value = Inf
					)
				),
				actionButton(
					inputId = "ss_go",
					label = "Reload"
				),
				helpText("Select a plot region to display additional details",
								 "below the graph. Clicking on one of the resulting",
								 "table rows will display splits or heat times if",
								 "they are available."),
				helpText("Distance split times and sprint heat data",
								 "is incomplete and not available for many",
								 "races.")
			),
			
			#### Skier Summary Main Panel ####
			mainPanel(
				tabsetPanel(
					type = "tabs",
					id = "ss_tabs",
					
					tabPanel(
						title = "Distance",
						value = "ss_dst",
						
						plotOutput(outputId = "ss_dst_plot",
											 brush = "ss_dst_plot_brush",
											 height = "600px") %>% withSpinner(),
						conditionalPanel("input.ss_dst_plot_brush != null",
														 DTOutput("ss_dst_plot_brush_info") %>% withSpinner()),
						conditionalPanel("input.ss_dst_plot_brush_info_rows_selected != null",
														 plotOutput(outputId = "ss_dst_splits") %>% withSpinner())),
					
					tabPanel(
						title = "Sprint",
						value = "ss_spr",
						
						plotOutput(outputId = "ss_spr_plot",
											 brush = "ss_spr_plot_brush",
											 height = "600px") %>% withSpinner(),
						conditionalPanel("input.ss_spr_plot_brush != null",
														 DTOutput("ss_spr_plot_brush_info") %>% withSpinner())),
					
					tabPanel(
						title = "Stage",
						value = "ss_stg",
						
						reactableOutput(
							outputId = "ss_stg_tbl"
						) %>% withSpinner()),
					
					tabPanel(
						title = "Data",
						value = "ss_data",
						
						h4("Distance Results"),
						DTOutput(outputId = "ss_dst_tbl_data") %>% withSpinner(),
						br(),
						h4("Sprint Results"),
						DTOutput(outputId = "ss_spr_tbl_data") %>% withSpinner()
					)
					)
				)
			)
		),
	
	
	#### HEAD-TO-HEAD ####
	tabPanel(
		title = "Head-to-head",
		value = "hth_tab",
		
		sidebarLayout(
			
			#### Head-to-head sidebar ####
			sidebarPanel(
				selectizeInput(
					inputId = "hth_skier_compid",
					label = "Skier:",
					choices = NULL,
					multiple = FALSE
				),
				selectizeInput(
					inputId = "hth_opp_compid",
					label = "Opponents:",
					choices = NULL,
					multiple = TRUE
				),
				radioButtons(
					inputId = "hth_y_measure",
					label = "Measure difference in:",
					choices = c("Finishing Place" = "rnk_diff",
											"FIS Points" = "fp_diff",
											"PBM Points" = "pbm_diff"),
					selected = "fp_diff"
				),
				radioButtons(
					inputId = "hth_maj_int",
					label = "Show:",
					choices = list("All events" = "all","WC/TdS/OWG/WSC events" = "maj_int")
				),
				hr(),
				checkboxGroupInput(
					inputId = "hth_tech",
					label = "Technique:",
					choices = list("Freestyle" = "F","Classic" = "C","Mixed" = "FC"),
					selected = c("F","C","FC")
				),
				conditionalPanel(
					condition = "input.hth_tabs == 'hth_dst'",
					sliderInput(
						inputId = "hth_dst_race_length",
						label = "Distance race length (km):",
						min = 2.5,
						max = 100,
						value = c(2.5,100),
						post = "km"
					),
					checkboxGroupInput(
						inputId = "hth_format",
						label = "Distance race format:",
						choices = list("Interval" = "Interval",
													 "Mass" = "Mass",
													 "Skiathlon" = "Skiathlon",
													 "Pursuit" = "Pursuit",
													 "Pursuit Break" = "Pursuit Break"),
						selected = c("Interval","Mass","Skiathlon","Pursuit","Pursuit Break")
					)
				),
				actionButton(
					inputId = "hth_go",
					label = "Reload"
				),
				hr(),
				helpText("Select a plot region to display additional details",
								 "below the graph. Clicking on one of the resulting",
								 "table rows will display splits or heat times if",
								 "they are available."),
				helpText("Distance split times and sprint heat data",
								 "is incomplete and not available for many",
								 "races.")
			),
			
			#### Head-to-head main panel ####
			mainPanel(
				tabsetPanel(
					type = "tabs",
					id = "hth_tabs",
					
					tabPanel(
						title = "Distance",
						value = "hth_dst",
						
						plotOutput(outputId = "hth_dst_plot",
											 brush = "hth_dst_plot_brush",
											 height = "600px") %>% withSpinner(),
						conditionalPanel("input.hth_dst_plot_brush != null",
														 DTOutput("hth_dst_plot_brush_info") %>% withSpinner()),
						conditionalPanel("input.hth_dst_plot_brush_info_rows_selected != null",
														 plotOutput(outputId = "hth_dst_splits") %>% withSpinner())
					),
					tabPanel(
						title = "Sprint",
						value = "hth_spr",
						
						fluidRow(
							column(6,
										 h4("Overall"),
										 plotOutput(outputId = "hth_spr_plot",
										 					 brush = "hth_spr_plot_brush",
										 					 height = "600px") %>% withSpinner()),
							column(6,
										 h4("Within Heats"),
										 plotOutput(outputId = "hth_spr_heats",height = "600px") %>% withSpinner())
						),
						
						conditionalPanel("input.hth_spr_plot_brush != null",
														 DTOutput("hth_spr_plot_brush_info") %>% withSpinner())
					)
				)
			)
		)
		),
	
	
	#### FIS POINT DEVELOPMENT ####
	tabPanel(
		title = "Development Progress",
		value = "dev_tab",
		
		sidebarLayout(
			
			#### FIS Point Development Sidebar ####
			sidebarPanel(
				selectizeInput(
					inputId = "dev_skier_compid",
					label = "Skiers:",
					choices = NULL,
					multiple = TRUE
				),
				actionButton(
					inputId = "dev_go",
					label = "Reload"
				),
				hr(),
				helpText("Compare a skier's FIS points by age to",
								 "the 50th & 10th percentile FIS points by age",
								 "of skiers who have at least 2 major",
								 "international podiums in their career.")
			),
			
			#### FIS Point Development Main Panel ####
			mainPanel(
				tabsetPanel(
					type = "tabs",
					id = "dev_tabs",
					
					tabPanel(
						title = "Distance",
						outputId = "dev_dst",
						
						plotOutput(outputId = "dev_dst_plot") %>% withSpinner()
					),
					tabPanel(
						title = "Sprint",
						outputId = "dev_spr",
						
						plotOutput(outputId = "dev_spr_plot") %>% withSpinner()
					)
				)
			)
		)
		),
	
	#### RACE SUMMARIES ####
	tabPanel(
		title = "Race Snapshots",
		value = "snap_tab",
		
		sidebarLayout(
			
			sidebarPanel(
				dateInput(
					inputId = "snap_date",
					label = "Race date (yyyy-mm-dd):",
					value = as.character(Sys.Date())
				),
				sliderInput(
					inputId = "snap_top_pct",
					label = "Show top X% of skiers:",
					min = 10,
					max = 100,
					value = 30,
					step = 5,
					post = "%"
				),
				actionButton(
					inputId = "snap_go",
					label = "Reload"
				)
			),
			
			mainPanel(
				DTOutput(outputId = "snap_race_tbl") %>% withSpinner(),
				hr(),
				conditionalPanel(condition = "input.snap_race_tbl_rows_selected != null",
												 plotOutput(outputId = "snap_plot",height = "800px") %>% withSpinner())
			)
		)
	),
	
	#### ABOUT ####
	tabPanel(
		title = "About",
		value = "faq_tab",
		
		includeMarkdown("faq.md")
		)
)


#### SERVER ####
server <- function(input,output,session){
	
	#### SS DISTANCE PANEL ####
	updateSelectizeInput(session,"ss_skier_compid",choices = skier_names,server = TRUE)
	#SS Distance panel plot data
	ss_dst_data_r <- eventReactive(
		input$ss_go,
		{ss_dst_data(con_pool,input$ss_skier_compid,input$ss_maj_int,
								 input$ss_tech,input$ss_format,input$ss_dst_race_length,
								 input$ss_fis_pts_clip)}
	)
	
	#SS Distance panel plots
	output$ss_dst_plot <- renderPlot({ss_dst_plot(ss_dst_data_r())})
	
	#SS Distance panel brushed points data tables
	output$ss_dst_plot_brush_info <- renderDT({
		brushedPoints(ss_dst_data_r(),input$ss_dst_plot_brush) %>%
			spread(key = y_measure,value = y_value) %>%
			mutate(link = if_else(is.na(link),as.character(date),as.character(link)),
						 primary_tag = toupper(primary_tag)) %>%
		  select(Date = link,`Split Times` = splits_exist,Location = location,
		  			 Site = site,Event = primary_tag,Format = format,Tech = tech,
		  			 Length = length,last_col()) %>%
			arrange(Date)
	},rownames = FALSE,escape = FALSE)
	
	#SS Distance panel splits
	ss_dst_splits_data_r <- reactive({
		idx <- input$ss_dst_plot_brush_info_rows_selected
		if (!is.null(idx) && length(idx) > 0){
			ev_ids <- brushedPoints(ss_dst_data_r(),input$ss_dst_plot_brush) %>%
				slice(idx) %>%
				filter(splits_exist == "Y") %>%
				pull(eventid)
		} else {
			ev_ids <- integer(0)
		}
		ss_dst_splits_data(con_pool,ev_ids)
	})
	
	output$ss_dst_splits <- renderPlot({
		ss_dst_splits_plot(ss_dst_splits_data_r(),input$ss_skier_compid)
	})
	
	#### SS SPRINT PANEL ####
	#Sprint panel plot data
	ss_spr_data_r <- eventReactive(
		input$ss_go,
		{ss_spr_data(con_pool,input$ss_skier_compid,input$ss_maj_int,input$ss_tech,
								 input$ss_fis_pts_clip)}
	)
	
	#Sprint panel plots
	output$ss_spr_plot <- renderPlot({ss_spr_plot(ss_spr_data_r())})
	
	#Sprint panel brushed points data tables
	output$ss_spr_plot_brush_info <- renderDT({
		brushedPoints(ss_spr_data_r(),input$ss_spr_plot_brush) %>%
			spread(key = y_measure,value = y_value) %>%
			mutate(primary_tag = toupper(primary_tag)) %>%
			select(Qual = link_q,Fin = link_f,Date = date,Location = location,
						 Site = site,Event = primary_tag,Tech = tech,Length = length,
						 RankQual = rankqual,last_col()) %>%
			arrange(Date)
	},rownames = FALSE,escape = FALSE)
	
	#### SS STAGE PANEL ####
	ss_stg_data_r <- eventReactive(
		input$ss_go,
		{ss_stg_data(con_pool,input$ss_skier_compid,input$ss_maj_int)}
	)
	
	output$ss_stg_tbl <- renderReactable({
		stg <- ss_stg_data_r()
		outer_stg <- stg %>%
			select(id = ov_eventid,`Stg Date`,Event,`Overall Place`) %>%
			distinct()
		
		reactable(outer_stg,details = function(index){
			inner_stg <- stg[stg$ov_eventid == outer_stg$id[index],] %>%
				select(Date,Location,Site,Format,
							 Tech,Length,RankQual,`Stg Place`,`FIS Pts`,
							 `PBM Pts`)
			htmltools::div(stype = "padding: 10px",
										 reactable(inner_stg,outlined = TRUE))
		})
	})
	
	#### SS DATA PANEL ####
	ss_tbl_data_r <- eventReactive(
		input$ss_go,
		{ss_tbl_data(con_pool,input$ss_skier_compid)}
	)
	
	output$ss_dst_tbl_data <- renderDT({
		ss_tbl_data_r()$dst
	},extensions = c("Buttons","Scroller","FixedColumns"),
	rownames = FALSE,
	options = list(dom = "Bfrtip",
								 buttons = c("csv","excel"),
								 deferRender = TRUE,
								 scrollY = 200,
								 scroller = TRUE,
								 scrollX = TRUE,
								 fixedColumns = TRUE))
	
	output$ss_spr_tbl_data <- renderDT({
		ss_tbl_data_r()$spr
	},extensions = c("Buttons","Scroller","FixedColumns"),
	rownames = FALSE,
	options = list(dom = "Bfrtip",
								 buttons = c("csv","excel"),
								 deferRender = TRUE,
								 scrollY = 200,
								 scroller = TRUE,
								 scrollX = TRUE,
								 fixedColumns = TRUE))
	
	#### HTH PANEL ####
	updateSelectizeInput(session,"hth_skier_compid",choices = skier_names,server = TRUE)
	updateSelectizeInput(session,"hth_opp_compid",choices = skier_names,server = TRUE)
	
	#HTH Distance panel data
	hth_dst_data_r <- eventReactive(
		input$hth_go,
		{hth_dst_data(con_pool,input$hth_skier_compid,
									input$hth_opp_compid,input$hth_maj_int,
									input$hth_tech,input$hth_format,input$hth_dst_race_length)}
	)
	#HTH Distance panel plot
	output$hth_dst_plot <- renderPlot({
		hth_dst_plot(hth_dst_data_r(),input$hth_y_measure)
	})
	#HTH Distance panel brush data
	output$hth_dst_plot_brush_info <- renderDT({
		brushedPoints(hth_dst_data_r(),input$hth_dst_plot_brush) %>%
			select(Date = link,`Split Times` = splits_exist,Location = location,
						 Site = site,Event = primary_tag,Format = format,Tech = tech,
						 Length = length,matches(input$hth_y_measure)) %>%
			arrange(Date)
	},rownames = FALSE,escape = FALSE)
	
	hth_dst_splits_data_r <- reactive({
		idx <- input$hth_dst_plot_brush_info_rows_selected
		if (!is.null(idx) && length(idx) > 0){
			ev_ids <- brushedPoints(hth_dst_data_r(),input$hth_dst_plot_brush) %>%
				slice(idx) %>%
				filter(splits_exist == "Y") %>%
				pull(eventid)
		} else {
			ev_ids <- integer(0)
		}
		hth_dst_splits_data(con_pool,ev_ids,input$hth_skier_compid,input$hth_opp_compid)
	})
	
	output$hth_dst_splits <- renderPlot({
		hth_dst_splits_plot(hth_dst_splits_data_r(),c(input$hth_skier_compid,input$hth_opp_compid))
	})
	
	#HTH Sprint panel data
	hth_spr_data_r <- eventReactive(
		input$hth_go,
		{hth_spr_data(con_pool,input$hth_skier_compid,input$hth_opp_compid,
									input$hth_maj_int,input$hth_tech)}
	)
	#HTH Sprint panel plot
	output$hth_spr_plot <- renderPlot({
		hth_spr_plot(hth_spr_data_r(),input$hth_y_measure)
	})
	#HTH Sprint panel brush data
	output$hth_spr_plot_brush_info <- renderDT({
		brushedPoints(hth_spr_data_r(),input$hth_spr_plot_brush) %>%
			select(Qual = link_q,Fin = link_f,Date = date,`Heat Times` = heats_exist,Location = location,
						 Site = site,Event = primary_tag,Tech = tech,Length = length,
						 matches(input$hth_y_measure)) %>%
			arrange(Date)
	},rownames = FALSE,escape = FALSE)
	
	hth_spr_heats_data_r <- eventReactive(
		input$hth_go,{
		hth_spr_heats_data(con_pool,input$hth_skier_compid,input$hth_opp_compid)
	})
	
	output$hth_spr_heats <- renderPlot({
		hth_spr_heats_plot(hth_spr_heats_data_r())
	})
	
	#### DEV PANEL ####
	updateSelectizeInput(session,"dev_skier_compid",choices = skier_names,server = TRUE)
	dev_dst_data_r <- eventReactive(
		input$dev_go,
		{dev_dst_data(con_pool,input$dev_skier_compid)}
	)
	dev_spr_data_r <- eventReactive(
		input$dev_go,
		{dev_spr_data(con_pool,input$dev_skier_compid)}
	)
	
	output$dev_dst_plot <- renderPlot({dev_dst_plot(dev_dst_data_r())})
	output$dev_spr_plot <- renderPlot({dev_spr_plot(dev_spr_data_r())})
	
	#### RACE SNAPSHOT PANEL ####
	#Observe date picker and update race table options based on the entered date
	snap_get_events_r <- eventReactive(
		input$snap_go,
		{snap_get_events(con_pool,input$snap_date)}
	)
	output$snap_race_tbl <- renderDT({
		snap_get_events_r()
	},rownames = FALSE,selection = "single")
	
	snap_data_r <- reactive({
		idx <- input$snap_race_tbl_rows_selected
		if (!is.null(idx) && length(idx) == 1){
			ev_info <- snap_get_events_r() %>%
				slice(idx) %>%
				select(eventid,event_type,primary_tag)
		}else{
			ev_info <- NULL
		}
		snap_data(con_pool,ev_info,input$snap_top_pct)
	})
	output$snap_plot <- renderPlot({snap_plot(snap_data_r())})
}

shinyApp(ui = ui,server = server)