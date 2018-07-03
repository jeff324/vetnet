#' Makes a list of parameters
#'
#' \code{make_parameter_list} makes a list of parameters for input to \code{\link{run_vet}}
#'
#' @param num_subjects The number of subject parameters
#' @param par_names A character vector of parameter names
#' @param lower The lower bound on the uniform distribution
#' @param upper The upper bound on the uniform distribution
#' @examples
#' #make parameters for 10 subjects
#' par_list = make_parameter_list(num_subjects=10,par_names=c('c','beta'),lower=c(0,0),upper=c(1,1))
#' @export
make_parameter_list = function(num_subjects,par_names,lower,upper){
     x = NULL
     for(i in 1:length(par_names)){
          y = stats::runif(num_subjects,lower[i],upper[i])
          x = cbind(x,y)
     }
     x = as.data.frame(x)
     names(x) = par_names
     return(x)
}


load_file = function(file_name){
     load(file_name)
     obj_names = ls()
     obj_names = obj_names[obj_names != 'file_name']
     if(length(obj_names) > 1){
          #return a list of objects
          dat = sapply(obj_names, function(x)get(x), simplify=FALSE, USE.NAMES=TRUE)
     }else{
          #return a single object
          dat = get(obj_names)
     }
     return(dat)
}
