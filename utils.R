is_null_empty <- function(x){
	is.null(x) || length(x) == 0 || nrow(x) == 0
}