get_choice_prob = function(study_list, probes, dst, model, pars){
     d = NA
     study_list_idx = sapply(1:length(study_list),function(x) which(colnames(dst)==study_list[x]))
     probe_list_idx = sapply(1:length(probes), function(x) which(colnames(dst)==probes[x]))
     for(i in 1:length(probes)){
          d[i] = sum(sapply(1:length(study_list_idx), function(x) dst[probe_list_idx[i],study_list_idx[x]]))
     }
     p = model(d,pars)
     return(p)
}

get_list = function(vet, dst, model, pars){
     response_list = list()
     study_lists = vet$study
     probe_lists = vet$probe
     for(i in 1:length(study_lists)){
          response_list[[i]] = sapply(1:nrow(probe_lists[[i]]), function(x) get_choice_prob(study_lists[[i]],probe_lists[[i]][x,],dst,model,pars))
     }
     categories = names(study_lists)
     response_list = Map(cbind,accuracy=response_list,categories=categories)
     response_df = data.frame(do.call(rbind,response_list))
     response_df$accuracy = as.numeric(as.character(response_df$accuracy))
     return(response_df)
}

#' Simulate the Vanderbilt Expertise Test
#'
#' \code{run_vet} gets the probability of choosing the target image for all trials on the VET
#'
#' @param vet The preprocessed VET test list made with \code{\link{make_vet_list}}
#' @param dst A distance matrix or list of distance matrices. Each element (i,j) is the distance between image i and j.
#' The name of row i and column j correspond to the name of image i and name of image j, respectively.
#' @param model A function that returns the probability of choosing the target probe.
#' Its first argument is a 3 element vector containing the summed distances for a given probe and all study items.
#' Its second argument is a named vector of parameters used to convert the distance into a probabilty of picking the target.
#' See the example below for further explanation.
#' @param par A data frame of parameter values
#' @return A data frame where row i correspond to the predicted probability of choosing the target on trial i.
#' @examples
#' #make a list of parameters
#' par_list = make_parameter_list(num_subjects=10,par_names=c('c','beta'),
#'                                lower=c(0,0),upper=c(1,1))
#'
#' # set the model function
#' model = function(d,pars) {
#'     c = pars$c
#'     beta = pars$beta
#'     p = sapply(d, function(d) exp(-c*d) / (exp(-c*d) + beta))
#'     p = p[1] / sum(p) #probability of picking target
#' }
#'
#' #run the simulation
#' response_lists = run_vet(vet_list, dst, model, par_list)
#'
#' #get the correlation matrix
#' get_corr_matrix(response_lists)
#' @export
run_vet = function(vet, dst, model, par){
     if (is.matrix(dst)) { #if we are running a single distance matrix
          if (nrow(par)==1) { #if we are running a single parameter set
               response_lists = get_list(vet, dst, model, par)
          }else if (ncol(par)==1) { #if we are running multiple parameters with single distance matrix with a single parameter
               par = cbind(par,par)
               response_lists = plyr::llply(1:nrow(par),function(x) get_list(vet, dst, model, par[x,]), .progress = 'win')
          }else{
               response_lists = plyr::llply(1:nrow(par),function(x) get_list(vet, dst, model, par[x,]), .progress = 'win')
          }
     }else{ #if we are running multiple distance matrices
          if(nrow(par)==1){ #if we are running a single parameter set
               response_lists = plyr::llply(1:length(dst),function(x) get_list(vet, dst[[x]], model, par), .progress = 'win')
          }else{ #if we are running multiple parameters with multiple distance matrices
               response_lists = plyr::llply(1:length(dst),function(x) get_list(vet, dst[[x]], model, par[x,]), .progress = 'win')
          }
     }
     return(response_lists)
}
