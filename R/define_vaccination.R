#' @export
define_vaccination <- function(parms, m_vacc, w_vacc){
    make_parm_gender_vecs(mval = m_vacc,
                          wval = w_vacc,
                          parms$structural$parm_indices)
}
