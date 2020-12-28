read_sql <- function(file){
	sql <- readLines(con = file)
	sql <- paste(sql,collapse = "\n")
	DBI::SQL(sql)
}