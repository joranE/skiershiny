DST_DEV <- readr::read_csv(file = "dst_fp_dev.csv",col_types = "dcdd") %>%
	rename(`50th` = q_upper,`10th` = q_lower) %>%
	tidyr::gather(key = "pct",value = "val",`50th`,`10th`)
SPR_DEV <- readr::read_csv(file = "spr_fp_dev.csv",col_types = "dcdd") %>%
	rename(`50th` = q_upper,`10th` = q_lower) %>%
	tidyr::gather(key = "pct",value = "val",`50th`,`10th`)

#### DISTANCE ####
dev_dst_data <- function(con,compid){
	if (is.null(compid) || compid == ""){
		return(NULL)
	}
	
	dst_src <- dplyr::tbl(src = con,"v_distance")
	
	dev_dst_data <- dst_src %>%
		filter(compid %in% local(compid) & !is.na(fispoints)) %>%
		select(eventid,compid,fisid,name,gender,age,date,fispoints) %>%
		collect()
	
	dev_dst_data
}

dev_dst_plot <- function(dev_dst_data){
	if (is.null(dev_dst_data) || nrow(dev_dst_data) == 0){
		return(NULL)
	}
	
	n_skier <- n_distinct(dev_dst_data$compid)
	if (n_skier < 4) {
		fw <- facet_wrap(~name,scales = "free",ncol = 1)
	} else {
		fw <- facet_wrap(~name,scales = "free")
	}
	
	dev_ref <- dev_dst_data %>%
		select(name,gender) %>%
		distinct() %>%
		left_join(DST_DEV,by = "gender") %>%
		filter(age <= max(dev_dst_data$age))
	
	ggplot() + 
		fw +
		geom_point(data = dev_dst_data,aes(x = age,y = fispoints),alpha = 0.5) + 
	  geom_line(data = dev_ref,aes(x = age,y = val,color = pct,group = pct)) + 
		scale_color_manual(values = c(`50th` = "blue",`10th` = "red")) +
		labs(x = "Age",y = "FIS Points",color = "Target FIS Point %-tile")
}

#### SPRINT ####
dev_spr_data <- function(con,compid){
	if (is.null(compid) || compid == ""){
		return(NULL)
	}
	
	spr_src <- dplyr::tbl(src = con,"v_sprint")
	
	dev_spr_data <- spr_src %>%
		filter(compid %in% local(compid) & !is.na(fispoints)) %>%
		select(eventid,compid,fisid,name,gender,age,date,fispoints) %>%
		collect()
	
	dev_spr_data
}

dev_spr_plot <- function(dev_spr_data){
	if (is.null(dev_spr_data) || nrow(dev_spr_data) == 0){
		return(NULL)
	}
	
	n_skier <- n_distinct(dev_spr_data$compid)
	if (n_skier < 4) {
		fw <- facet_wrap(~name,scales = "free",ncol = 1)
	} else {
		fw <- facet_wrap(~name,scales = "free")
	}
	
	dev_ref <- dev_spr_data %>%
		select(name,gender) %>%
		distinct() %>%
		left_join(SPR_DEV,by = "gender") %>%
		filter(age <= max(dev_spr_data$age))
	
	ggplot() + 
		fw +
		geom_point(data = dev_spr_data,aes(x = age,y = fispoints),alpha = 0.5) + 
		geom_line(data = dev_ref,aes(x = age,y = val,color = pct,group = pct)) + 
		scale_color_manual(values = c(`50th` = "blue",`10th` = "red")) +
		labs(x = "Age",y = "FIS Points",color = "Target FIS Point %-tile")
}