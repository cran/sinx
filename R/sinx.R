#' Read sayings from spread sheets.
#'
#' @param file a character string giving a sinx sayings database in csv format (in UTF-8 encoding). By default all csv files in the data directory of the sinx package are used.
#' @param lib library name of the sayings.
#' @param sep seperator of the columns. See '?read.table()'.
#'
#' @return a data frame of sayings, each row contains:
#' - quote:	the quote, main part of the sayings,
#' - author: the author of the quote,
#' - context: the context in which it was quoted (if available, otherwise NA),
#' - source	: where it was quoted (if available, otherwise NA),
#' - date:	when it was quoted (if available, otherwise NA).
#' @importFrom utils read.table
#' @export
#'
#' @examples
#' libs <- read.sinxs()
#' libs_ts <- read.sinxs(system.file('sinxs/tangshi.csv', package = 'sinx'))
read.sinxs <- function(file = NULL,
                       sep = ',',
                       lib = 'sinxs') {
  os <- Sys.info()['sysname']
  if (os == 'Windows') {
    old_loc <- Sys.getlocale("LC_CTYPE")
    on.exit(Sys.setlocale("LC_CTYPE", old_loc))
    # Sys.setlocale("LC_CTYPE", "English")
  }
  if (!is.null(file)) {
    sinxs <- file[file.exists(file)]
  } else {
    sep <- ','
    allfiles <- dir(system.file("sinxs", package = "sinx"))
    firstname <- gsub('\\.csv|\\.md', '', allfiles)
    lastname <- gsub('.*(\\.csv|\\.md)$', '\\1', allfiles)
    loc <- firstname %in% lib
    sinxs <-
      system.file("sinxs", paste0(firstname[loc], lastname[loc]), package = "sinx")
  }

  rval <- NULL
  nfile <- length(sinxs)
  sep <- rep_len(sep, nfile)
  for (i in 1:nfile) {
    afile <- sinxs[i]
    atable <- NULL
    if(grepl('\\.csv$', afile)){
      if (os == 'Windows')
        Sys.setlocale("LC_CTYPE", "English")
      atable <- read.table(
        sinxs[i],
        header = TRUE,
        sep = sep[i],
        quote = "\"",
        colClasses = "character",
        encoding = 'UTF-8'
      )
      if (os == 'Windows')
        Sys.setlocale("LC_CTYPE", old_loc)
    }

    if(grepl('\\.md$', afile))
      atable <- md2df(sinxs[i])
    rval <- rbind(rval,atable)
  }
  rval
}

sinxs.env <- new.env()

#' Sino Xmen's sayings the R community.
#'
#' @param which an integer specifying the row number of `sinxs.data`. Alternatively `which`` can be a character and `grep`` is used to try to find a suitable row.
#' @param sinxs.data data frame containing a saying in each row. By default the data from the 'sinx' package are used.
#' @param fixed logical passed to `grep` if `which`` is a character, indicating if it should work (if `TRUE`, as by default) with a simple character string or (if `FALSE`) with regular expressions.
#' @param showMatches if `which` is character, a logical indicating if `sinx()` should print all the row numbers of `sinxs.data` which match the `grep` search.
#' @param author a character string to match (via `grep`) to the "authors" column of `sinxs.data`.
#' @param ... potential further arguments passed to `grep`.
#'
#' @return an object of class "sinx" which is a row from a data frame of sayings (like those read in from read.sinxs).
#' @export
#'
#' @examples
#' sinx()
#'
#' libs_ts <- read.sinxs(system.file('sinxs/tangshi.csv', package = 'sinx'))
#' sinx(sinxs.data = libs_ts)
sinx <- function(which = NULL,
                 sinxs.data = NULL,
                 fixed = TRUE,
                 showMatches = FALSE,
                 author = character(),
                 ...)
{
  if (is.null(sinxs.data)) {
    if (is.null(sinxs.env$sinxs.data))
      sinxs.env$sinxs.data <- read.sinxs()
    sinxs.data <- sinxs.env$sinxs.data
  }

  if (is.null(which) && !length(author)) {
    which <- sample.int(nrow(sinxs.data), 1)
  } else if (is.character(which) || length(author)) {
    if (length(author)) {
      if (is.null(fd.auth <- sinxs.data[, "author"])) {
        warning("'sinxs.data' does not have an \"author\" column")
      } else {
        sinxs.data <-
          sinxs.data[grep(author,
                          fd.auth,
                          useBytes = TRUE,
                          fixed = fixed), ]
      }
    }
    if (is.character(which)) {
      fort <- apply(sinxs.data, 1, function(x)
        paste(x, collapse = " "))
      which1 <-
        grep(which, fort, useBytes = TRUE, fixed = fixed, ...)
      if (length(which1) < 1)
        which1 <-
        grep(tolower(which),
             tolower(fort),
             useBytes = TRUE,
             fixed = TRUE,
             ...)
    } else {
      which1 <- seq_len(nrow(sinxs.data))
    }
    if (showMatches)
      cat("Matching row numbers:", paste(which1, collapse = ", "), "\n")
    which <- which1
    if (length(which) > 1)
      which <- sample(which, size = 1)
  }
  if (length(which) > 0 &&
      which %in% seq(along = rownames(sinxs.data))) {
    structure(sinxs.data[which, ], class = "sinx")
  } else {
    character(0)
  }
}

#' Print R sinx sayings.
#'
#' @param x an object of class "sinx", usually a single row from `sinxs.data`.
#' @param ... potential further arguments passed to `grep`.
#'
#' @return print.
#' @export
print.sinx <- function(x, ...){
  x$context <-
    if (is.na(x$context) | x$context == '')
      ""
  else
    paste(" (", x$context, ")", sep = "")
  if (is.na(x$source) | x$source == '')
    x$source <- ""
  x$date <-
    if (is.na(x$date) | x$date == '')
      ""
  else
    x$date <- paste(" (", x$date, ")", sep = "")
  if (is.na(x$author) | x$author == '')
    x$author <- "unkown"
  if (anyNA(x))
    stop("'quote' is required")

  line1 <- x$quote
  line2 <- paste("   -- ", x$author, x$context, sep = "")
  line3 <- paste("      ", x$source, x$date, sep = "")

  cat(paste("\n", line1, "\n\n\n",
            line2, "\n",
            line3, "\n\n", sep = ""))
}

