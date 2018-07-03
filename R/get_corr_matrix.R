#' Correlation matrix from response probabilities
#'
#' \code{get_cor_mat} gets the correlation matrix where each element corresponds
#' to the correlation between two category's predicted accuracy
#'
#' @param response_lists The output from \code{\link{run_vet}}
#' @return A correlation matrix
#' @export
get_corr_matrix = function(response_lists){
     response_lists = Map(cbind,response_lists,subject = 1:length(response_lists))
     response_df = do.call(rbind,response_lists)
     agg_response = stats::aggregate(accuracy ~ subject * categories, response_df, mean)
     agg_response_mat = reshape2::dcast(data = agg_response,value.var = 'accuracy',formula = subject ~ categories)
     agg_response_mat = stats::na.omit(agg_response_mat)
     cor_mat = stats::cor(agg_response_mat[2:ncol(agg_response_mat)],agg_response_mat[2:ncol(agg_response_mat)])
     return(cor_mat)
}
