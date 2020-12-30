#### DISTANCE ####
ss_dst_data <- function(con,compid,race_cat,race_tech,
												race_format,race_length,fispoints_max){
	if (is.null(compid) || compid == ""){
		return(NULL)
	}
	if (race_cat == "maj_int"){
		dst_src <- dplyr::tbl(src = con,"v_distance_maj_int")
	} else {
		dst_src <- dplyr::tbl(src = con,"v_distance")
	}
	race_data <- dst_src %>%
		filter(compid == local(compid) &
					 	tech %in% local(race_tech) & 
					 	format %in% local(race_format) &
					 	length >= local(race_length[1]) & 
					 	length <= local(race_length[2]) &
					 	!is.na(rank))
	
	if (is.finite(fispoints_max)){
		race_data <- race_data %>%
			filter(fispoints <= local(fispoints_max))
	}
	
	race_data <- race_data %>%
		collect() %>%
		mutate(date = as.Date(date),
					 pbm_pts = round(pbm_pts,4))
	
	dst_splits <- dplyr::tbl(src = con,"v_distance_splits") %>%
		filter(eventid %in% local(race_data$eventid)) %>%
		select(eventid) %>%
		collect()
	
	race_data <- race_data %>%
		mutate(splits_exist = if_else(eventid %in% dst_splits$eventid,"Y","N")) %>%
		select(splits_exist,everything())
	
	race_data <- race_data %>%
		gather(key = "y_measure",value = "y_value",fispoints,rank,pbm_pts) %>%
		mutate(y_measure = case_when(y_measure == "fispoints" ~ "FIS Pts",
																 y_measure == "rank" ~ "Finishing Place",
																 y_measure == "pbm_pts" ~ "PBM Pts"))
	
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

ss_dst_plot <- function(race_data){
	if (is.null(race_data) || nrow(race_data) == 0){
		return(NULL)
	}
	p <- ggplot(data = race_data,aes(x = date,y = y_value)) + 
		facet_wrap(~y_measure,nrow = 3,scales = "free_y") +
		geom_point() +
		labs(x = "Date",y = NULL)
	
	p
}

ss_dst_splits_data <- function(con,eventid){
	if (is.null(eventid) || length(eventid) == 0){
		return(NULL)
	}
	
	dst_splits_all <- dplyr::tbl(src = con,"v_distance_splits") %>%
		filter(eventid %in% local(eventid) & !is.na(split_km)) %>%
		collect() %>%
		group_by(eventid,split_km) %>%
		mutate(split_pb = (split_time - min(split_time,na.rm = TRUE)) / min(split_time,na.rm = TRUE)) %>%
		group_by(eventid,compid) %>%
		arrange(split_km) %>%
		mutate(segment_time = c(split_time[1],diff(split_time)),
					 segment_length = c(split_km[1],diff(split_km)),
					 segment_pace = segment_time / segment_length) %>%
		ungroup() %>%
		mutate(race_key = paste(date,location,toupper(primary_tag)))
	
	dst_splits_all
}

ss_dst_splits_plot <- function(split_data,ath_ids){
	if (is.null(split_data) || nrow(split_data) == 0){
		return(NULL)
	}
	
	rug_dat <- split_data %>%
	select(race_key,split_km) %>%
		distinct()
	
	ggplot() + 
		facet_wrap(~race_key,scales = "free") +
		geom_rug(data = rug_dat,aes(x = split_km,y = NULL),sides = "t") +
		geom_line(data = split_data %>% filter(!compid %in% ath_ids),
							aes(x = split_km,y = segment_pace,group = compid),
							color = "grey",alpha = 0.5) + 
		geom_line(data = split_data %>% filter(compid %in% ath_ids),
							aes(x = split_km,y = segment_pace,group = compid),
							color = "blue",size = 1.25) + 
		labs(x = "km",y = "Segment Pace (sec/km)",
				 color = "Skier",title = "Entire Field")
	
}

#### SPRINT ####
ss_spr_data <- function(con,compid,race_cat = "all",
												race_tech = c("F","C","FC"),fispoints_max = Inf){
	if (is.null(compid) || compid == ""){
		return(NULL)
	}
	if (race_cat == "maj_int"){
		spr_src <- dplyr::tbl(src = con,"v_sprint_maj_int")
	} else {
		spr_src <- dplyr::tbl(src = con,"v_sprint")
	}
	race_data <- spr_src %>%
		filter(compid == local(compid) &
					 	tech %in% local(race_tech) & 
					 	(!is.na(rank) | !is.na(rankqual)))
	
	if (is.finite(fispoints_max)){
		race_data <- race_data %>%
			filter(fispoints <= local(fispoints_max))
	}
	
	race_data <- race_data %>%
		collect() %>%
		mutate(date = as.Date(date),
					 pbm_pts = round(pbm_pts,4))
	
	race_data <- race_data %>%
		gather(key = "y_measure",value = "y_value",fispoints,rank,pbm_pts) %>%
		mutate(y_measure = case_when(y_measure == "fispoints" ~ "FIS Pts",
																 y_measure == "rank" ~ "Finishing Place",
																 y_measure == "pbm_pts" ~ "PBM Pts"))
	
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
	
	race_data
}

ss_spr_plot <- function(race_data){
	if (is.null(race_data) || nrow(race_data) == 0){
		return(NULL)
	}
	p <- ggplot(data = race_data,aes(x = date,y = y_value)) + 
		facet_wrap(~y_measure,nrow = 3,scales = "free_y") +
		geom_point() +
		labs(x = "Date",y = NULL)
	
	p
}

#### STAGE ####
ss_stg_data <- function(con,compid,race_cat){
	if (compid == ""){
		return(NULL)
	}
	if (race_cat == "maj_int"){
		stg_src <- dplyr::tbl(src = con,"v_stage_maj_int")
	} else {
		stg_src <- dplyr::tbl(src = con,"v_stage")
	}
	
	race_data <- stg_src %>%
		filter(compid == local(compid) & 
					 	!is.na(rank)) %>%
		collect() %>%
		select(ov_eventid = eventid,`Stg Date` = date,Event = primary_tag,`Overall Place` = rank)
	
	ov_ids <- race_data$ov_eventid
	
	stg_ids <- dplyr::tbl(src = con,"stg_event_link") %>%
		filter(ov_eventid %in% local(ov_ids)) %>%
		collect()
	
	dst_stgs <- dplyr::tbl(src = con,"v_distance") %>%
		filter(compid == local(compid) & 
					 	eventid %in% local(stg_ids$stg_eventid)) %>%
		collect() %>%
		mutate(RankQual = NA,
					 pbm_pts = round(pbm_pts,4)) %>%
		select(eventid,Date = date,Location = location,Site = site,Format = format,
					 Tech = tech,Length = length,RankQual,`Stg Place` = rank,`FIS Pts` = fispoints,
					 `PBM Pts` = pbm_pts)
	spr_stgs <- dplyr::tbl(src = con,"v_sprint") %>%
		filter(compid == local(compid) & 
					 	eventid %in% local(stg_ids$stg_eventid)) %>%
		collect() %>%
		mutate(Format = NA,
					 pbm_pts = round(pbm_pts,4)) %>%
		select(eventid,Date = date,Location = location,Site = site,
					 Tech = tech,Length = length,RankQual = rankqual,`Stg Place` = rank,
					 `FIS Pts` = fispoints,`PBM Pts` = pbm_pts)
	
	all_stgs <- bind_rows(dst_stgs,spr_stgs) %>%
		left_join(stg_ids,by = c("eventid" = "stg_eventid")) %>%
		left_join(race_data,by = "ov_eventid") %>%
		mutate(Format = if_else(is.na(Format),"Sprint",Format),
					 Event = toupper(Event)) %>%
		arrange(`Stg Date`,Date)
	
}

#### DATA ####
ss_tbl_data <- function(con,compid){
	if (is.null(compid) || compid == ""){
		return(NULL)
	}
	dst_dat <- dplyr::tbl(src = con,"v_distance") %>%
		filter(compid == local(compid)) %>%
		collect()
	spr_dat <- dplyr::tbl(src = con,"v_sprint") %>%
		filter(compid == local(compid)) %>%
		collect()
	
	return(list(dst = dst_dat,spr = spr_dat))
}