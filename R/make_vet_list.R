#' Prepare the VET test list
#'
#' \code{make_vet_list} Takes the raw VET and prepares it for use in \code{\link{run_vet}}
#'
#' @param  vet_test_file the VET 1.0 test list
#' @return The processed VET that can be used in \code{run_vet}
#'
#' @examples
#' vet_list = make_vet_list(system.file('extdata','vet_1.0_test_list.csv',package='vetnet'))
#' @export
make_vet_list = function(vet_test_file){
     vet_test = utils::read.csv(vet_test_file,sep = '\t')
     #do some preprocessing on the matrix and the test list
     #dst = dst / max(dst) #normalize distances
     vet_test = vet_test[vet_test$TrialType != 'Study',]
     vet_test$TargetName = gsub(pattern = ' ', replacement = '', x = as.character(vet_test$TargetName))
     vet_test$Distractor1Name = gsub(pattern = ' ', replacement = '', x = as.character(vet_test$Distractor1Name))
     vet_test$Distractor2Name = gsub(pattern = ' ', replacement = '', x = as.character(vet_test$Distractor2Name))

     #make some data structures that will be helpful for making the final data structures
     categories = as.character(unique(vet_test$Category))
     probes = cbind(vet_test$TargetName,vet_test$Distractor1Name,vet_test$Distractor2Name)

     ######### these data structures are inputs to the main function, get_list()
     probe_lists = lapply(1:length(categories),function(x) probes[vet_test$Category == categories[[x]],])
     study_lists = lapply(1:length(categories),function(x) vet_test$TargetName[vet_test$Category == categories[[x]]][1:6])

     categories = unlist(lapply(categories,function(x) gsub(pattern = ' ', replacement = '', x = x)))
     names(probe_lists) = categories
     names(study_lists) = categories

     vet_list = list(probe = probe_lists, study=study_lists)
     return(vet_list)
}

