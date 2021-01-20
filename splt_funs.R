dst_split_get_events <- function(con,date){
	evs <- dplyr::tbl(src = con,"v_distance_splits") %>%
		filter(date == local(date)) %>%
		select(eventid,primary_tag,location,site,gender,length,tech) %>%
		collect() %>%
		distinct()
	evs
}

dst_split_data <- function(con,ev_info){
	if (is.null(ev_info) || length(ev_info) == 0){
		return(NULL)
	}
	
	dst_race <- tbl(src = con,"v_distance_splits") %>%
		filter(eventid == local(ev_info$eventid) & !is.na(split_km)) %>%
		collect() 
	
	short_seg_thresh <- 0.5
	split_km <- sort(unique(dst_race$split_km))
	short_seg <- which(diff(c(0,split_km)) < short_seg_thresh) - 1
	
	if (length(short_seg) > 0){
		omit_splits <- split_km[short_seg]
		dst_race <- dst_race %>%
			filter(!split_km %in% omit_splits)
	}
	
	dst_race <- dst_race %>%
		group_by(split_km) %>%
		mutate(time_back = split_time - min(split_time,na.rm = TRUE),
					 pct_back = time_back / min(split_time,na.rm = TRUE),
					 name = stringr::str_trim(name,side = "both")) %>%
		group_by(compid) %>%
		arrange(split_km) %>%
		mutate(seg_time = c(split_time[1],diff(split_time)),
					 seg_len = c(split_km[1],diff(split_km)),
					 seg_pace = seg_time / seg_len) %>%
		group_by(split_km) %>%
		mutate(seg_pace_pct = (seg_pace - min(seg_pace,na.rm = TRUE)) / min(seg_pace,na.rm = TRUE),
					 seg_time_back = seg_time - min(seg_time,na.rm = TRUE),
					 seg_time_back_scl = seg_time_back / seg_len) %>%
		as.data.frame()
	
	split_data <- bind_rows(dst_race %>% mutate(facet_label = "Overall Race % Back",y = pct_back),
													dst_race %>% mutate(facet_label = "% Behind Fastest Split Pace",y = seg_pace_pct),
													dst_race %>% mutate(facet_label = "Overall Race Position",y = split_rank),
													dst_race %>% mutate(facet_label = "Sec/Km Lost on Segment",y = seg_time_back_scl)) %>%
		mutate(facet_label = factor(facet_label,
																levels = c("Overall Race Position","Overall Race % Back",
																					 "% Behind Fastest Split Pace","Sec/Km Lost on Segment")))
	
	split_data
}

dst_split_plot <- function(race_data,show_top = NULL){
	if (is.null(race_data) || length(race_data) == 0){
		return(NULL)
	}
	
	if (!is.null(show_top)){
		last_split <- max(race_data$split_km)
		top_compid <- race_data %>%
			filter(split_km == last_split & 
						 	split_rank <= show_top) %>%
			pull("compid")
		race_data <- race_data %>%
			filter(compid %in% top_compid)
	}
	
	race_data_ly <- highlight_key(race_data,~name)
	
	p <- race_data_ly %>%
		ggplot(data = .,aes(x = split_km,y = y,group = name)) +
		facet_wrap(~facet_label,nrow = 2,scales = "free_y") +
		geom_line(alpha = 0.5,na.rm = TRUE) + 
		scale_x_continuous(breaks = unique(race_data$split_km),expand = c(0.01,0)) +
		labs(x = "km",y = NULL) + 
		theme(axis.text = element_text(size = 10))
	
	p_ly <- ggplotly(p,tooltip = "name") %>% config(displayModeBar = FALSE)
	p_ly_highlight <- highlight(p_ly,
															on = "plotly_click",
															off = "plotly_doubleclick",
															persistent = FALSE,
															color = "blue")
	p_ly_highlight
}