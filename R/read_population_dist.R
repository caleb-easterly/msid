#' @export
read_population_dist <- function(path){
    pd <- read.table(path, stringsAsFactors = FALSE)
    # todo: check that variables match constants

    return(pd)
}
