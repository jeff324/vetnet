#' Example distance matrix
#'
#' A distance matrix of all VET images created with VGG-16
#'
#' @format A 613 x 613 matrix with column and row names corresponding to VET images
#'
"dst"


#' The preprocessed VET test list
#'
#' @format A list with two elements: study and probe
#' \describe{
#'   \item{study}{study list for each of the 8 categories}
#'   \item{probe}{a matrix where each row is the probe names for the given trial,
#'   the first element is the target}
#'   ...
#' }
#'
#' @format A 613 x 613 matrix with column and row names corresponding to VET images
#'
"vet_list"
