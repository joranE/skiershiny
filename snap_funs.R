snap_get_events <- function(con,date){
	evs <- dplyr::tbl(src = con,"v_event") %>%
		filter(date == local(date)) %>%
		select(eventid,event_type,primary_tag,location,site,gender,length,tech,format) %>%
		collect() 
	evs
}

shorten_names <- function(x){
	x %>%
		stringr::str_extract_all(string = .,pattern = "\\b[A-Z]+\\b") %>%
		purrr::map(.f = paste,collapse = " ") %>%
		stringr::str_to_title()
}

snap_data <- function(con,ev_info,top_pct){
	if (is.null(ev_info) || length(ev_info) == 0){
		return(NULL)
	}
	
	eventid <- ev_info$eventid
	ev_type <- ev_info$event_type
	ev_primary_tag <- ev_info$primary_tag
	
	src_tbl <- case_when(ev_type == "Distance" & ev_primary_tag %in% c("wc","tds","wsc","owg") ~ "v_distance_maj_int",
											 ev_type == "Distance" & !ev_primary_tag %in% c("wc","tds","wsc","owg") ~ "v_distance",
											 ev_type == "Sprint" & ev_primary_tag %in% c("wc","tds","wsc","owg") ~ "v_sprint_maj_int",
											 ev_type == "Sprint" & !ev_primary_tag %in% c("wc","tds","wsc","owg") ~ "v_sprint")
	
	cur_race <- tbl(src = con,src_tbl) %>%
		filter(eventid == local(eventid) & !is.na(rank)) %>%
		collect() %>%
		mutate(name1 = paste(shorten_names(name),rank)) %>%
		arrange(rank) %>%
		slice_head(prop = top_pct / 100)
	
	race_date <- cur_race$date[1]
	cutoff_date <- as.character(as.Date(race_date) - (365 * 4))
	
	if (ev_type == "Distance"){
		race_tech <- cur_race$tech[1]
		tech_label <- switch(race_tech,
												 'F' = 'Freestyle',
												 'C' = 'Classic',
												 'FC' = 'Skiathlon')
		race_format <- switch(cur_race$format[1],
													'Interval' = 'Interval',
													'Mass' = c('Mass','Skiathlon'),
													'Skiathlon' = c('Mass','Skiathlon'),
													'Pursuit' = 'Pursuit',
													'Pursuit Break' = 'Pursuit Break')
		format_label <- switch(cur_race$format[1],
													 'Interval' = 'Interval',
													 'Mass' = 'Mass',
													 'Skiathlon' = 'Mass',
													 'Pursuit' = 'Pursuit',
													 'Pursuit Break' = 'Pursuit Break')
		
		race_history <- tbl(src = con,src_tbl) %>%
			filter(compid %in% local(cur_race$compid) &
						 	date < local(race_date) &
						 	date >= local(cutoff_date)) %>%
			collect() %>%
			left_join(cur_race[,c('compid','name1')],by = 'compid') %>%
			mutate(same_tech = ifelse(tech == race_tech,'Yes','No'),
						 same_format = ifelse(format %in% race_format,'Yes','No')) %>%
			group_by(name) %>%
			mutate(nrace_overall = n()) %>%
			group_by(name,same_tech) %>%
			mutate(nrace_tech = n()) %>%
			group_by(name,same_format) %>%
			mutate(nrace_format = n()) %>%
			as.data.frame()
	}
	if (ev_type == "Sprint"){
		race_tech <- cur_race$tech[1]
		tech_label <- switch(race_tech,
												 'F' = 'Freestyle',
												 'C' = 'Classic')
		race_format <- NULL
		format_label <- NULL
		
		race_history <- tbl(src = con,src_tbl) %>%
			filter(compid %in% local(cur_race$compid) &
						 	date < local(race_date) &
						 	date >= local(cutoff_date)) %>%
			collect() %>%
			left_join(cur_race[,c('compid','name1')],by = 'compid') %>%
			mutate(same_tech = ifelse(tech == race_tech,'Yes','No')) %>%
			group_by(name) %>%
			mutate(nrace_overall = n()) %>%
			group_by(name,same_tech) %>%
			mutate(nrace_tech = n()) %>%
			as.data.frame()
	}
	
	return(list(ev_type = ev_type,cur_race = cur_race,race_history = race_history,
							format_label = format_label,tech_label = tech_label))
	
}

snap_plot <- function(race_data){
	if (is.null(race_data) || length(race_data) == 0 || is.null(race_data$ev_type)){
		return(NULL)
	}
	if (race_data$ev_type == "Distance"){
		p <- snap_plot_dst(race_data)
		return(p)
	}
	if (race_data$ev_type == "Sprint"){
		p <- snap_plot_spr(race_data)
		return(p)
	}else{
		return(NULL)
	}
	
}

snap_plot_dst <- function(race_data){
	cur_race <- race_data$cur_race
	race_history <- race_data$race_history
	tech_label <- race_data$tech_label
	format_label <- race_data$format_label
	
	title_template <- "{site}, {location} {gender}'s {tag} {length}km {tech} {format} - {date}"
	title <- glue::glue(title_template,
											site = cur_race$site[1],
											location = cur_race$location[1],
											gender = cur_race$gender[1],
											tag = toupper(cur_race$primary_tag[1]),
											tech = cur_race$tech[1],
											format = cur_race$format[1],
											length = cur_race$length[1],
											date = cur_race$date[1])
	
	ath_min_races_overall <- race_history %>%
		filter(nrace_overall < 10) %>%
		as.data.frame()
	ath_min_races_tech <- race_history %>%
		filter(same_tech == 'Yes' & nrace_tech < 10) %>%
		as.data.frame()
	ath_min_races_format <- race_history %>%
		filter(same_format == 'Yes' & nrace_format < 10) %>%
		as.data.frame()
	ath_min <- bind_rows(setNames(list(ath_min_races_overall,ath_min_races_tech,ath_min_races_format),
																c('Overall',tech_label,format_label)),
											 .id = 'facet_grp')
	
	ath_bars_overall <- race_history %>%
		filter(nrace_overall >= 10) %>%
		group_by(name1) %>%
		summarise(q25 = quantile(pbm,0.25,na.rm = TRUE),
							q75 = quantile(pbm,0.75,na.rm = TRUE))
	ath_bars_tech <- race_history %>%
		filter(same_tech == 'Yes' & nrace_tech >= 10) %>%
		group_by(name1) %>%
		summarise(q25 = quantile(pbm,0.25,na.rm = TRUE),
							q75 = quantile(pbm,0.75,na.rm = TRUE))
	ath_bars_format <- race_history %>%
		filter(same_format == 'Yes' & nrace_format >= 10) %>%
		group_by(name1) %>%
		summarise(q25 = quantile(pbm,0.25,na.rm = TRUE),
							q75 = quantile(pbm,0.75,na.rm = TRUE))
	ath_bars <- bind_rows(setNames(list(ath_bars_overall,ath_bars_tech,ath_bars_format),
																 c('Overall',tech_label,format_label)),
												.id = 'facet_grp')
	
	#Make block data
	n_race <- nrow(cur_race)
	n_block <- (n_race %/% 10) + ((n_race %% 10) > 0)
	
	mn_idx <- c(1,1+which(seq_len(n_race) %% 10 == 0))
	mx_idx <- c(which(seq_len(n_race) %% 10 == 0),n_race)
	
	block <- data.frame(ymn = cur_race$name1[mn_idx[seq_len(n_block)]],
											ymx = cur_race$name1[mx_idx[seq_len(n_block)]],
											xmn = rep(-Inf,n_block),
											xmx = rep(Inf,n_block),
											block = rep(c('block1','block2'),length.out = n_block))
	block <- bind_rows(setNames(list(block,block,block),
															c('Overall',tech_label,format_label)),
										 .id = 'facet_grp')
	
	name_order <- rev(cur_race$name1)
	cur_race <- bind_rows(setNames(list(cur_race,cur_race,cur_race),
																 c('Overall',tech_label,format_label)),
												.id = 'facet_grp')
	
	#Set name order
	cur_race$name1 <- factor(cur_race$name1,levels = name_order)
	ath_min$name1 <- factor(ath_min$name1,levels = name_order)
	ath_bars$name1 <- factor(ath_bars$name1,levels = name_order)
	block$ymn <- factor(block$ymn,levels = name_order)
	block$ymx <- factor(block$ymx,levels = name_order)
	
	#Set facet order
	cur_race$facet_grp <- factor(cur_race$facet_grp,
															 levels = c(tech_label,format_label,"Overall"))
	ath_min$facet_grp <- factor(ath_min$facet_grp,
															levels = c(tech_label,format_label,"Overall"))
	ath_bars$facet_grp <- factor(ath_bars$facet_grp,
															 levels = c(tech_label,format_label,"Overall"))
	block$facet_grp <- factor(block$facet_grp,
														levels = c(tech_label,format_label,"Overall"))
	
	p <- ggplot() +
		facet_wrap(~facet_grp,nrow = 1,scale = "free_x") +
		geom_blank(data = cur_race,aes(x = pbm,y = name1)) +
		geom_rect(data = block,
							aes(ymin = ymn,ymax = ymx,
									xmin = -Inf,xmax = Inf,
									fill = block),alpha = 0.25,show.legend = FALSE) +
		geom_segment(data = ath_bars,aes(x = q25,xend = q75,y = name1,yend = name1)) +
		geom_point(data = cur_race,aes(x = pbm,y = name1),color = "red") +
		geom_point(data = ath_min,aes(x = pbm,y = name1),alpha = 0.5) +
		scale_fill_manual(values = c('#778899','#2F4F4F')) +
		ggtitle(label = title,
						subtitle = "For >=10 prior races, bars represent 25th-75th percentile of past performance") +
		labs(x = '% Behind Median Skier',y = 'Athlete',
				 fill = "") +
		theme_bw()
	p
}

snap_plot_spr <- function(race_data){
	cur_race <- race_data$cur_race
	race_history <- race_data$race_history
	tech_label <- race_data$tech_label
	
	title_template <- "{site}, {location} {gender}'s {tag} {tech} Sprint - {date}"
	title <- glue::glue(title_template,
											site = cur_race$site[1],
											location = cur_race$location[1],
											gender = cur_race$gender[1],
											tag = toupper(cur_race$primary_tag[1]),
											tech = cur_race$tech[1],
											date = cur_race$date[1])

	ath_min_races_overall <- race_history %>%
		filter(nrace_overall < 10) %>%
		as.data.frame()
	ath_min_races_tech <- race_history %>%
		filter(same_tech == 'Yes' & nrace_tech < 10) %>%
		as.data.frame()
	ath_min <- bind_rows(setNames(list(ath_min_races_overall,ath_min_races_tech),
																c('Overall',tech_label)),
											 .id = 'facet_grp')
	
	ath_bars_overall <- race_history %>%
		filter(nrace_overall >= 10) %>%
		group_by(name1) %>%
		summarise(q25 = quantile(rank,0.25,na.rm = TRUE),
							q75 = quantile(rank,0.75,na.rm = TRUE))
	ath_bars_tech <- race_history %>%
		filter(same_tech == 'Yes' & nrace_tech >= 10) %>%
		group_by(name1) %>%
		summarise(q25 = quantile(rank,0.25,na.rm = TRUE),
							q75 = quantile(rank,0.75,na.rm = TRUE))
	ath_bars <- bind_rows(setNames(list(ath_bars_overall,ath_bars_tech),
																 c('Overall',tech_label)),
												.id = 'facet_grp')
	
	#Make block data
	n_race <- nrow(cur_race)
	n_block <- (n_race %/% 10) + ((n_race %% 10) > 0)
	
	mn_idx <- c(1,1 + which(seq_len(n_race) %% 10 == 0))
	mx_idx <- c(which(seq_len(n_race) %% 10 == 0),n_race)
	
	block <- data.frame(ymn = cur_race$name1[mn_idx[seq_len(n_block)]],
											ymx = cur_race$name1[mx_idx[seq_len(n_block)]],
											xmn = rep(-Inf,n_block),
											xmx = rep(Inf,n_block),
											block = rep(c('block1','block2'),length.out = n_block))
	block <- bind_rows(setNames(list(block,block),
															c('Overall',tech_label)),
										 .id = 'facet_grp')
	
	name_order <- rev(cur_race$name1)
	cur_race <- bind_rows(setNames(list(cur_race,cur_race),
																 c('Overall',tech_label)),
												.id = 'facet_grp')
	
	#Set name order
	cur_race$name1 <- factor(cur_race$name1,levels = name_order)
	ath_min$name1 <- factor(ath_min$name1,levels = name_order)
	ath_bars$name1 <- factor(ath_bars$name1,levels = name_order)
	block$ymn <- factor(block$ymn,levels = name_order)
	block$ymx <- factor(block$ymx,levels = name_order)
	
	#Set facet order
	cur_race$facet_grp <- factor(cur_race$facet_grp,
															 levels = c(tech_label,"Overall"))
	ath_min$facet_grp <- factor(ath_min$facet_grp,
															levels = c(tech_label,"Overall"))
	ath_bars$facet_grp <- factor(ath_bars$facet_grp,
															 levels = c(tech_label,"Overall"))
	block$facet_grp <- factor(block$facet_grp,
														levels = c(tech_label,"Overall"))
	
	p <- ggplot() +
		facet_wrap(~facet_grp,nrow = 1,scale = "free_x") +
		geom_blank(data = cur_race,aes(x = rank,y = name1)) +
		geom_rect(data = block,
							aes(ymin = ymn,ymax = ymx,
									xmin = -Inf,xmax = Inf,
									fill = block),alpha = 0.25,show.legend = FALSE) +
		geom_segment(data = ath_bars,aes(x = q25,xend = q75,y = name1,yend = name1)) +
		geom_point(data = ath_min,aes(x = rank,y = name1),alpha = 0.5) +
		geom_point(data = cur_race,aes(x = rank,y = name1),color = "red") +
		scale_fill_manual(values = c('#778899','#2F4F4F')) +
		ggtitle(label = title,
						subtitle = "For >=10 prior races, bars represent 25th-75th percentile of past performance") +
		labs(x = 'Finishing Place',y = 'Athlete',fill = "") +
		theme_bw()
	p
}