spr_heat_prog_get_events <- function(con,date){
	evs <- dplyr::tbl(src = con,"v_sprint_heats") %>%
		filter(date == local(date)) %>%
		select(eventid,primary_tag,location,site,gender,length,tech) %>%
		collect() %>%
		distinct()
	evs
}

spr_heat_prog_data <- function(con,ev_info){
	if (is.null(ev_info) || length(ev_info) == 0){
		return(NULL)
	}
	race_info <- tbl(src = con,"v_event") %>%
		filter(eventid == local(ev_info$eventid)) %>%
		select(eventid,date,season,location,tech,length,gender) %>%
		collect()
	
	spr_qual <- tbl(src = con,"v_sprint") %>%
		filter(eventid == local(ev_info$eventid)) %>%
		collect() %>%
		select(eventid,name,nation,time,rank = rankqual) %>%
		mutate(heat = "qual",
					 heat = factor(heat,
					 							levels = c("qual","quarter","semi","final"),
					 							labels = c("Qual","QF","SF","Final")),
					 qf = NA_integer_,
					 sf = NA_integer_,
					 fn = NA_integer_)
	
	spr_heats_all <- tbl(src = con,"v_sprint_heats") %>%
		filter(eventid == local(ev_info$eventid)) %>%
		collect()
	spr_heats <- spr_heats_all %>%
		select(eventid,name,nation,
					 time = heat_time,
					 rank = heat_rank,qf,sf,fn,heat,ll) %>%
		mutate(heat = case_when(substr(heat,1,1) == "1" ~ "quarter",
														substr(heat,1,1) == "2" ~ "semi",
														substr(heat,1,1) == "3" ~ "final"),
					 heat = factor(heat,
					 							levels = c("qual","quarter","semi","final"),
					 							labels = c("Qual","QF","SF","Final")))
	lls <- spr_heats_all %>%
		filter(ll == "Y") %>%
		select(name,qf,sf) %>%
		tidyr::pivot_longer(cols = c("qf","sf"),names_to = "heat") %>%
		filter(!is.na(value)) %>%
		mutate(heat = toupper(heat)) %>%
		group_by(name) %>%
		summarise(ll_heat = paste(heat,collapse = ",")) %>%
		mutate(ll_name = paste(name,paste0("(LL: ",ll_heat,")"))) %>%
		select(name,ll_name)
	return(list(race_info = race_info,
							spr_qual = spr_qual,
							spr_heats = spr_heats,
							lls = lls))
}

spr_heat_prog_plot <- function(race_data,time_scale,clip = NULL){
	if (is.null(race_data) || length(race_data) == 0){
		return(NULL)
	}
	race_info <- race_data$race_info
	spr_qual <- race_data$spr_qual
	spr_heats <- race_data$spr_heats
	lls <- race_data$lls
	
	data_clean <- bind_rows(spr_qual,spr_heats) %>%
		mutate(heat_lab = as.character(coalesce(qf,sf)),
					 heat_lab = if_else(heat == "Qual",NA_character_,heat_lab))
	data_clean <- left_join(data_clean,
													lls,by = "name") %>%
		mutate(name = coalesce(ll_name,name))
	
	title <- paste(race_info$date,
								 race_info$location,
								 race_info$gender,
								 paste0(race_info$length,"km"),
								 race_info$tech)
	subtitle <- "QF & SF heats indicated by numbers on the plot."
	
	data_clean <- data_clean %>%
		group_by(name) %>%
		mutate(qual_only = all(heat == "Qual")) %>%
		ungroup() %>%
		filter(!qual_only) %>%
		mutate(adv_thresh = case_when(heat == "Qual" ~ max(time[heat == "Qual"],na.rm = TRUE),
																	heat == "QF" ~ max(time[ll == "Y" & heat == "QF"],na.rm = TRUE),
																	heat == "SF" ~ max(time[ll == "Y" & heat == "SF"],na.rm = TRUE),
																	heat == "Final" ~ max(time[heat == "Final" & rank == 3],na.rm = TRUE)))
	
	if (time_scale == "median"){
		data_clean <- data_clean %>%
			group_by(heat) %>%
			mutate(time_y = time - median(time,na.rm = TRUE))
		y_lab <- "Difference from median time (sec) within round"
	}
	if (time_scale == "raw") {
		data_clean <- data_clean %>%
			group_by(heat) %>%
			mutate(time_y = time)
		y_lab <- "Time (seconds)"
	}
	if (time_scale == "thresh"){
		data_clean <- data_clean %>%
			group_by(heat) %>%
			mutate(time_y = time - adv_thresh)
		y_lab <- "Difference from advancement time (sec) threshold"
	}
	
	spr_final <- data_clean %>%
		filter(name %in% data_clean$name[data_clean$fn == 1])
	name_lev_ord <- spr_final %>%
		filter(heat == "Final") %>%
		arrange(rank) %>%
		pull(name)
	spr_final$name <- factor(spr_final$name,levels = name_lev_ord)
	
	p <- ggplot(data = data_clean,aes(x = heat,y = time_y,group = name)) +
		geom_line(alpha = 0.5) +
		geom_line(data = spr_final,
							aes(color = name,group = name),
							size = 1.1) +
		geom_text(aes(label = heat_lab),
							hjust = rep(c(1,0),length.out = nrow(data_clean))) +
		scale_color_brewer(palette = "Set2") +
		labs(x = "Round",
				 y = y_lab,
				 color = NULL) +
		ggtitle(label = title,subtitle = subtitle) +
		theme_bw()
	
	if (!is.null(clip)){
		p <- p + coord_cartesian(ylim = clip)
	}
	p
}
