#' Determine parent row in demo of Meuwissen-Luo algorithm
#'
#' Given a pedigree the demo of determining the relationship
#' matrix using Meuwissen-Luo writes the parents above the
#' label-row of the relationship matrix
#'
#' @param pPed the pedigree
#' @return a character vector with the same length as the pedigree containing the parents
#' @export
sGetParentRow <- function(pPed){
  return(sapply(1:length(pPed@label),
         function(x)
           ifelse(is.na(pPed@sire[x]) & is.na(pPed@dam[x]),
                  "",
                  paste(ifelse(is.na(pPed@sire[x]),"NA",as.character(pPed@sire[x])),
                        ifelse(is.na(pPed@dam[x]),"NA",as.character(pPed@dam[x])),
                        sep=" - ")
                  )
         )
  )
}


sGetParentRow <- function(pPed){
  return(sapply(1:length(pPed@label),
                function(x) {
                  if(is.na(pPed@sire[x]) & is.na(pPed@dam[x])){
                    return("")
                  } else {
                    return(paste(as.character(pPed@sire[x]),as.character(pPed@dam[x]), sep=" - "))
                  }
                })
  )
}
