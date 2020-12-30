#### DISTANCE ####
hth_dst_data <- function(con,ath_compid,opp_compid,race_cat,race_tech,
												 race_format,race_length){
	if (length(ath_compid) == 0 || length(ath_compid) > 1){
		return(NULL)
	}
	if (length(opp_compid) < 1){
		return(NULL)
	}
	sql <- read_sql("sql/dst_hth.sql")
	sql <- glue::glue_sql(sql,
												ath_compid = ath_compid,
												opp_compid = opp_compid,
												.con = con)
	race_data <- RPostgres::dbGetQuery(con,sql) %>%
		mutate(date = as.Date(date),
					 rnk_diff = opp_rank - ath_rank,
					 fp_diff = opp_fispoints - ath_fispoints,
					 pbm_diff = opp_pbm_pts - ath_pbm_pts)
	
	if (race_cat == "maj_int"){
		race_data <- race_data %>%
			filter(primary_tag %in% c("wc","tds","wsc","owg"))
	}
	
	race_data <- race_data %>%
		filter(tech %in% race_tech & 
					 	format %in% race_format & 
					 	length >= race_length[1] & 
					 	length <= race_length[2])
	
	ev_ids <- unique(race_data$eventid)
	url_template <- '<a href="{link}" target="_blank" class="btn btn-primary">{val}</a>'
	urls <- dplyr::tbl(src = con,"v_event_url") %>%
		filter(eventid %in% local(ev_ids)) %>%
		select(eventid,date,url) %>%
		collect() %>%
		mutate(link = glue::glue(url_template,link = url,val = date)) %>%
		select(-date)
	
	race_data <- race_data %>%
		left_join(urls,by = "eventid")
	
	race_data
}

hth_dst_plot <- function(dst_data,y_measure = "rnk_diff"){
	if (is.null(dst_data) || nrow(dst_data) == 0){
		return(NULL)
	}
	nm <- dst_data$ath_name[1]
	y_lab <- switch(y_measure,
									"rnk_diff" = "Difference in Finishing Place",
									"fp_diff" = "Difference in FIS Points",
									"pbm_diff" = "Difference in PBM Points")
	y_measure <- rlang::sym(y_measure)
	p <- ggplot(data = dst_data,aes(x = date,!!y_measure)) + 
		facet_wrap(~opp_name,scales = "free_y") + 
		geom_hline(yintercept = 0,color = "blue") +
		geom_point() + 
		labs(x = "Date",y = y_lab,
				 title = paste(nm,"vs:"),
				 subtitle = glue("Positive values are wins for {nm}."))
	p
}

hth_dst_splits_data <- function(con,eventid,ath_compid,opp_compid){
	if (is.null(eventid) || length(eventid) == 0){
		return(NULL)
	}
	ath_ids <- c(ath_compid,opp_compid)
	
	dst_splits_all <- dplyr::tbl(src = con,"v_distance_splits") %>%
		filter(eventid %in% local(eventid) & !is.na(split_km) & !is.na(split_time)) %>%
		collect() %>%
		group_by(eventid,split_km) %>%
		mutate(split_pb = (split_time - min(split_time,na.rm = TRUE)) / min(split_time,na.rm = TRUE)) %>%
		group_by(eventid,compid) %>%
		arrange(split_km) %>%
		mutate(segment_time = c(split_time[1],diff(split_time)),
					 segment_length = c(split_km[1],diff(split_km)),
					 segment_pace = segment_time / segment_length) %>%
		ungroup() %>%
		mutate(race_key = paste(date,location,toupper(primary_tag)),
					 color_key = if_else(compid %in% ath_ids,name,NA_character_))
	
	dst_splits_red <- dst_splits_all %>%
		filter(compid %in% ath_ids) %>%
		group_by(eventid,split_km) %>%
		mutate(split_tb = split_time - min(split_time,na.rm = TRUE)) %>%
		ungroup() %>%
		mutate(race_key = paste(date,location,toupper(primary_tag)))
	list(all = dst_splits_all,red = dst_splits_red)
}

hth_dst_splits_plot <- function(split_data,ath_ids){
	if (is.null(split_data) || length(split_data) == 0){
		return(NULL)
	}
	dst_splits_all <- split_data$all
	dst_splits_red <- split_data$red
	
	rug_all <- dst_splits_all %>%
		select(race_key,split_km) %>%
		distinct()
	rug_red <- dst_splits_red %>%
		select(race_key,split_km) %>%
		distinct()
	
	p_all <- ggplot() + 
		facet_wrap(~race_key,scales = "free") + 
		geom_rug(data = rug_all,aes(x = split_km,y = NULL),sides = "t") +
		geom_line(data = dst_splits_all %>% filter(!compid %in% ath_ids),
							aes(x = split_km,y = segment_pace,group = compid),
							color = "grey",alpha = 0.5) + 
		geom_line(data = dst_splits_all %>% filter(compid %in% ath_ids),
							aes(x = split_km,y = segment_pace,color = color_key,group = compid),
							size = 1.25) + 
		#scale_y_continuous(labels = scales::percent_format()) +
		labs(x = "km",y = "Segment Pace (sec/km)",
				 color = "Skier",title = "Entire Field")
	
	p_red <- ggplot() + 
		facet_wrap(~race_key,scales = "free") +
		geom_rug(data = rug_red,aes(x = split_km,y = NULL),sides = "t") +
		geom_line(data = dst_splits_red,
							aes(x = split_km,y = split_tb,color = name,group = name)) +
		labs(x = "km",y = "Time Back (sec)",
				 color = "Skier",title = "Selected Skiers")
	
	p_all + p_red + patchwork::plot_layout(ncol = 1)
}

#### SPRINT ####
hth_spr_data <- function(con,ath_compid,opp_compid,race_cat,race_tech){
	if (length(ath_compid) == 0 || length(ath_compid) > 1){
		return(NULL)
	}
	if (length(opp_compid) < 1){
		return(NULL)
	}
	sql <- read_sql("sql/spr_hth.sql")
	sql <- glue::glue_sql(sql,
												ath_compid = ath_compid,
												opp_compid = opp_compid,
												.con = con)
	race_data <- RPostgres::dbGetQuery(con,sql) %>%
		mutate(date = as.Date(date),
					 rnk_diff = opp_rank - ath_rank,
					 fp_diff = opp_fispoints - ath_fispoints,
					 pbm_diff = opp_pbm_pts - ath_pbm_pts)
	
	ev_ids <- race_data %>%
		select(eventid,eventid_sf) %>%
		distinct() %>%
		mutate(eventid_sf = if_else(is.na(eventid_sf),"",eventid_sf))
	
	url_template_q <- '<a href="{link}" target="_blank" class="btn btn-primary">SPQ</a>'
	url_template_f <- '<a href="{link}" target="_blank" class="btn btn-primary">SPF</a>'
	
	urls <- dplyr::tbl(src = con,"v_event_url") %>%
		filter(eventid %in% local(ev_ids$eventid) & 
					 	(is.na(eventid_sf) | eventid_sf %in% local(ev_ids$eventid_sf))) %>%
		select(eventid,eventid_sf,date,url_type,url) %>%
		collect() %>%
		select(eventid,url_type,url) %>%
		mutate(url_type = factor(url_type,levels = c("SPQ","SPF"))) %>%
		spread(key = url_type,value = url,fill = NA_character_,drop = FALSE) %>%
		mutate(link_q = if_else(is.na(SPQ),"",as.character(glue::glue(url_template_q,link = SPQ))),
					 link_f = if_else(is.na(SPF),"",as.character(glue::glue(url_template_f,link = SPF)))) 
	
	race_data <- race_data %>%
		left_join(select(urls,eventid,link_q,link_f),by = "eventid")
	
	if (race_cat == "maj_int"){
		race_data <- race_data %>%
			filter(primary_tag %in% c("wc","tds","wsc","owg"))
	}
	
	race_data <- race_data %>%
		filter(tech %in% race_tech)
	
	
	race_data
}

hth_spr_plot <- function(spr_data,y_measure){
	if (is.null(spr_data) || nrow(spr_data) == 0){
		return(NULL)
	}
	nm <- spr_data$ath_name[1]
	y_lab <- switch(y_measure,
									"rnk_diff" = "Difference in Finishing Place",
									"fp_diff" = "Difference in FIS Points",
									"pbm_diff" = "Difference in PBM Points")
	y_measure <- rlang::sym(y_measure)
	
	n_skier <- n_distinct(spr_data$opp_compid)
	if (n_skier < 4) {
		fw <- facet_wrap(~opp_name,scales = "free_y",ncol = 1)
	} else {
		fw <- facet_wrap(~opp_name,scales = "free_y")
	}
	
	p <- ggplot(data = spr_data,aes(x = date,!!y_measure)) + 
		fw + 
		geom_hline(yintercept = 0,color = "blue") +
		geom_point() + 
		labs(x = "Date",y = y_lab,
				 title = paste(nm,"vs:"),
				 subtitle = glue::glue("Positive values are wins for {nm}."))
	p
}

hth_spr_heats_data <- function(con,ath_compid,opp_compid){
	if (length(ath_compid) == 0 || length(ath_compid) > 1){
		return(NULL)
	}
	if (length(opp_compid) < 1){
		return(NULL)
	}
	sql <- read_sql("sql/spr_heats_hth.sql")
	sql <- glue::glue_sql(sql,
												ath_compid = ath_compid,
												opp_compid = opp_compid,
												.con = con)
	result <- RPostgres::dbGetQuery(con,sql) %>%
		mutate(spr_round = case_when(substr(heat,1,1) == "1" ~ "Quarter",
																 substr(heat,1,1) == "2" ~ "Semi",
																 substr(heat,1,1) == "3" ~ "Final"),
					 spr_round = factor(spr_round,levels = c("Quarter","Semi","Final")),
					 heat_time_diff = opp_heat_time - ath_heat_time)
	result
}

hth_spr_heats_plot <- function(spr_heats_data){
	if (is.null(spr_heats_data) || nrow(spr_heats_data) == 0){
		return(NULL)
	}
	nm <- spr_heats_data$ath_name[1]
	m <- nrow(spr_heats_data)
	
	n_skier <- n_distinct(spr_heats_data$opp_compid)
	if (n_skier < 4) {
		fw <- facet_wrap(~opp_name,scales = "free_y",ncol = 1)
	} else {
		fw <- facet_wrap(~opp_name,scales = "free_y")
	}
	
	ggplot() + 
		fw + 
		geom_hline(yintercept = 0,color = "blue") +
		geom_jitter(data = spr_heats_data,aes(x = spr_round,y = heat_time_diff),width = 0.25) + 
		labs(x = "Round",y = "Time Difference (sec)",
				 title = paste(nm,"vs:"),
				 subtitle = glue::glue("Positive values are wins for {nm}."))
}