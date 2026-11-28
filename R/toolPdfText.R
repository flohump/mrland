#' @title toolPdfText
#' @description Text of a PDF file by page, in the layout of the page. The text comes from the file of the same
#' name with the extension txt (pages separated by form feeds). If that file is missing it is written first, with
#' the command line tool pdftotext (poppler). The download functions call this function, so that reading a source
#' needs no PDF software.
#' @param pdf name of the PDF file
#' @return character vector, one element per page
#' @author Florian Humpenoeder
toolPdfText <- function(pdf) {
  txt <- sub("\\.pdf$", ".txt", pdf)
  if (!file.exists(txt)) {
    if (!nzchar(Sys.which("pdftotext"))) {
      stop("no file ", txt, " and no command line tool pdftotext to write it")
    }
    stopifnot(system2("pdftotext", c("-layout", shQuote(pdf), shQuote(txt))) == 0)
  }
  pages <- paste(readLines(txt, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  return(strsplit(pages, "\f", fixed = TRUE)[[1]])
}
