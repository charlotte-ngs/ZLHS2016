#' Function returning a matrix as tex output
#'
#' The matrix specified by pmatAMatrix is converted to TeX
#' using function xtable::xtable. This produces a tabular TeX-object.
#' The output from xtable::xtable is converted to a TeX-array. Some
#' lines at the beginning and at the end are ignored. The number
#' of lines that are ignored can be specified using the parameters
#' pnOutStartLine and pnEndIgnoreLines.
#'
#' @param  pmatAMatrix        Matrix to be represented in tex format
#' @param  pnOutStartLine     line index where output should start, default = 5
#' @param  pnEndIgnoreLines   number of lines to be ignored at the end of the output, default = 1
#' @return string containing tex representation of matrix
sGetTexMatrix <- function(pmatAMatrix, pnOutStartLine = 5, pnEndIgnoreLines = 1) {
  sResultTexMatrix <- capture.output(print(xtable::xtable(pmatAMatrix),
                                           include.rownames = FALSE,
                                           include.colnames = FALSE,
                                           hline.after = NULL,
                                           sanitize.text.function=identity))
  ### # do some replacements
  sResultTexMatrix <- gsub("tabular", "array",
                           sResultTexMatrix[pnOutStartLine:(length(sResultTexMatrix)-pnEndIgnoreLines)],
                           fixed = TRUE)
  return(sResultTexMatrix)
}

#' Matrix with character elements and column and row indices
#'
#' @param psBaseElement   character prefix shown in front of indices
#' @param pnNrRow         number of rows
#' @param pnNrCol         number of columns
matGetMatElem <- function(psBaseElement, pnNrRow, pnNrCol){
  return(matrix(sapply(0:(pnNrRow*pnNrCol-1),
                       function(x,y) paste(psBaseElement, x%/%y+1, x%%y+1, sep = ""),
                       pnNrCol),
                nrow = pnNrRow,
                ncol = pnNrCol,
                byrow = TRUE))
}

#' Lower triangular matrix
matLowerTri <- function(psBaseElement, pnNrRow, pnNrCol, pvecDiag = NULL) {
  matResult <- matGetMatElem(psBaseElement = psBaseElement, pnNrRow = pnNrRow, pnNrCol = pnNrCol)
  matResult[upper.tri(matResult)] <- "0"
  if (!is.null(pvecDiag)) diag(matResult) <- pvecDiag
  return(matResult)
}

#' Diagonal matrix
matDiag <- function(psBaseElement, pnNrRow, pnNrCol) {
  matResult <- matGetMatElem(psBaseElement = psBaseElement, pnNrRow = pnNrRow, pnNrCol = pnNrCol)
  matResult[upper.tri(matResult) | lower.tri(matResult)]  <- "0"
  return(matResult)
}
