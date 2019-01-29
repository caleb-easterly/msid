#' @export
read_contact_matrix <- function(path){
    cm <- read.table(path, stringsAsFactors = FALSE)

    # replace any NAs with 0
    cm[is.na(cm)] <- 0

    # todo: check that matches constants and rownames
    # these are out of date
    r_gender <- "r_gender"
    rp_gender <- "rp_gender"
    r_sexid <- "r_sexid"
    rp_sexid <- "rp_sexid"
    r_sexact <- "r_sexact"
    rp_sexact <- "rp_sexact"
    partnerships <- "d_numnew12mo"

    return(cm)
}
