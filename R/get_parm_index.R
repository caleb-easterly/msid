#' todo: write test
#' @export
get_parm_index <- function(qsex, qsexid, qsexact, parm_indices){
    with(parm_indices, parm_uid[sexact == qsexact & sexid == qsexid & sex == qsex])
}
