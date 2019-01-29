# sample_spars <- function(nsamps){
#     df <- so_spar_data$SO_spar_df
#     newspar <- matrix(0, ncol = 8, nrow = nsamps)
#     for (i in 1:8){
#         par <- gamma_params_mom(mean = df$SPAR[i], sigma = df$SparSD[i], scale = F)
#         newspar[i, ] <- rgamma(nsamps, shape = par[1], rate = par[2])
#     }
#     newspar
# }

#' @export
make_parms_psa <- function(nsamps, bt_imis_parameters) {
    rows_to_keep <- sample(1:nrow(bt_imis_parameters), size = nsamps, replace = TRUE)
    imis <- bt_imis_parameters[rows_to_keep, ]
    other_parms <- psa_sample_params(nsamps)
    data.frame(imis, other_parms)
}

#' run analysis once with sampled parameters
#' 
#' @param imis_parameters
#' assumed to be back-transformed
#' 
#' @export
psa_analysis <- function(so,
                         psa_parms,
                         phi_base,
                         phi_comp,
                         poc = c("prev", "cases")){
    poc <- match.arg(poc)
    cl <- makeForkCluster(detectCores())
    result <- parLapply(cl, 1:nrow(psa_parms), function(i){
        if (poc == "cases"){
            cases <- cases_prevented(gammaM = psa_parms$gammaM[i], 
                                     gammaF = psa_parms$gammaF[i],
                                     deltaM = psa_parms$deltaM[i],
                                     deltaF = psa_parms$deltaF[i],
                                     sigma = psa_parms$sigma[i],
                                     eps = psa_parms$eps[i],
                                     betaMM = psa_parms$betaMM[i],
                                     betaMF = psa_parms$betaMF[i],
                                     betaFF = psa_parms$betaFF[i],
                                     tauM = psa_parms$tauM[i],
                                     tauF = psa_parms$tauF[i],
                                     theta = psa_parms$theta[i],
                                     so = so,
                                     phi_base = phi_base,
                                     phi_comp = phi_comp)
            cases$run <- i
            cases
        } else {
            prev_base <- estimate_steady_state_prevalence(gammaM = psa_parms$gammaM[i],
                                                          gammaF = psa_parms$gammaF[i],
                                                          deltaM = psa_parms$deltaM[i],
                                                          deltaF = psa_parms$deltaF[i],
                                                          sigma = psa_parms$sigma[i],
                                                          eps = psa_parms$eps[i],
                                                          betaMM = psa_parms$betaMM[i],
                                                          betaMF = psa_parms$betaMF[i],
                                                          betaFF = psa_parms$betaFF[i],
                                                          tauM = psa_parms$tauM[i],
                                                          tauF = psa_parms$tauF[i],
                                                          theta = psa_parms$theta[i],
                                                          so = so,
                                                          phi = phi_base)
            prev_comp <- estimate_steady_state_prevalence(gammaM = psa_parms$gammaM[i],
                                                          gammaF = psa_parms$gammaF[i],
                                                          deltaM = psa_parms$deltaM[i],
                                                          deltaF = psa_parms$deltaF[i],
                                                          sigma = psa_parms$sigma[i],
                                                          eps = psa_parms$eps[i],
                                                          betaMM = psa_parms$betaMM[i],
                                                          betaMF = psa_parms$betaMF[i],
                                                          betaFF = psa_parms$betaFF[i],
                                                          tauM = psa_parms$tauM[i],
                                                          tauF = psa_parms$tauF[i],
                                                          theta = psa_parms$theta[i],
                                                          so = so,
                                                          phi = phi_comp)
            data.frame("base" = prev_base$gsoPrev,
                       "comp" = prev_comp$gsoPrev,
                       "gso" = define_gso(),
                       "run" = i)
        }
    })
    stopCluster(cl)
    return(result)
}

#' @export
sample_delta <- function(nsamps){
    # Schiller 2012. doi: 10.1016/j.vaccine.2012.04.108
    # Protection of Gardasil against 6-month persistent HPV6/11/16/18 infection. 
    # according to protocol (women 24-45)
    mu_deltaF <- 0.896
    lb_deltaF <- 0.793
    ub_deltaF <- 0.954
    se_deltaF <- (ub_deltaF - lb_deltaF) / (1.96*2)
    p_deltaF <- dampack::beta_params(mean = mu_deltaF, sigma = se_deltaF)
    
    # males (from Schiller 2012, also)
    # Protection of Gardasil against 6-month persistent genital HPV16  infection
    # according to protocol
    # men 16 - 26
    mu_deltaM <- 0.787
    lb_deltaM <- 0.555
    ub_deltaM <- 0.909
    se_deltaM <- (ub_deltaM - lb_deltaM) / (1.96*2)
    p_deltaM <- dampack::beta_params(mean = mu_deltaM, sigma = se_deltaM)
    
    # sample
    deltaM <- rbeta(nsamps, p_deltaM$alpha, p_deltaM$beta)
    deltaF <- rbeta(nsamps, p_deltaF$alpha, p_deltaF$beta)
    
    cbind(deltaM, deltaF)
}

#' @export 
psa_sample_params <- function(nsamps){
    delta <- sample_delta(nsamps)
    sigma <- 1/runif(nsamps, min = 20, max = 100)
    cbind(delta,
          sigma)
}