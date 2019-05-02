#' Create a book/slides from a sinx library
#'
#' @param lib library name of the sayings. See `?read.sinxs`.
#' @param title title of the book
#' @param author author's name
#' @param section name of a sinx libary column, in which the characters are the sections in the book
#' @param subsection name of a sinx libary column, in which the characters are the subsections in the book
#' @param file file name of source spread sheet. See '?read.sinxs()'.
#' @param if_render if render the book
#' @param style 'pagedown' or 'bookdown' or 'xaringan'
#' @param bookdown_template template name. only valid when the style is 'bookdown'. See '?bookdownplus'.
#' @param to the book directory.
#'
#' @return a book
#' @importFrom xaringan inf_mr
#' @importFrom bookdownplus bookdownplus
#' @export
#'
#' @examples
#' \dontrun{
#' secx()
#'
#' secx(lib = 'sinxs', title = 'cosx', subsection = 'date')
#' }
secx <-
  function(lib = 'tangshi',
           file = NULL,
           title = 'A Sinx Book',
           author = 'dapeng',
           section = 'author',
           subsection = 'context',
           style = c('pagedown', 'bookdown', 'xaringan'),
           bookdown_template = 'demo_zh',
           to = 'sinx_book',
           if_render = TRUE) {
    filename <- gsub(' ', '_', title)
    body_df <- read.sinxs(lib = lib, file = file)
    for(i in 1:ncol(body_df)) body_df[, i] <- clean_txt(body_df[, i])
    body_df$section_subsection <-
      paste(body_df[, section], body_df[, subsection], sep = '_')
    body_df <- body_df[order(body_df$section_subsection),]

    body_df[, section][duplicated(body_df[, section])] <- ''
    body_df[, subsection][duplicated(body_df$section_subsection)] <-
      ''
    body_df$section <-
      ifelse(body_df[, section] == '',
             '\n',
             paste('# ', body_df[, section]))
    body_df$subsection <-
      ifelse(body_df[, subsection] == '',
             '\n---\n',
             paste('## ', body_df[, subsection]))
    rmd_body <-
      paste(
        body_df$section,
        body_df$subsection,
        body_df$quote,
        sep = '\n\n',
        collapse = '\n\n'
      )
    rmd_body <- strsplit(rmd_body, '\\\\n')[[1]]
    rmd_body2 <- rep('', length(rmd_body) * 2)
    rmd_body2[seq(1, by = 2, length.out = length(rmd_body))] <-
      rmd_body
    if ('pagedown' %in% style) {
      rmd_new <- c(
        "---",
        paste0("title: ", title),
        paste0("author: ", author),
        "output:",
        "  pagedown::html_paged:",
        "    toc: true",
        "    self_contained: false",
        "paged-footnotes: true",
        "---",
        rmd_body2
      )
      filermd <- paste0(filename, '.Rmd')
      writeLines(rmd_new, filermd, useBytes = TRUE)
      if (if_render)
        xaringan::inf_mr(filermd)
    }
    if ('bookdown' %in% style) {
      bookdownplus::bookdownplus(
        template = bookdown_template,
        more_output = bookdownplus::get_output(),
        title = title,
        author = author,
        render = FALSE,
        rproj = TRUE,
        output_name = filename,
        to = to
      )
      writeLines(rmd_body2, file.path(to, 'body.Rmd'), useBytes = TRUE)
      if (if_render)
        bookdownplus::bookdownplus(
          to = to,
          render = TRUE,
          more_output = bookdownplus::get_output(),
          new = FALSE
        )
    }
    if ('xaringan' %in% style) {
      rmd_body <-
        paste(
          body_df$section,
          body_df$subsection,
          body_df$quote,
          sep = '\n\n',
          collapse = '\n---\n'
        )
      rmd_body <- strsplit(rmd_body, '\\\\n')[[1]]
      rmd_body2 <- rep('', length(rmd_body) * 2)
      rmd_body2[seq(1, by = 2, length.out = length(rmd_body))] <-
        rmd_body

      rmd_new <- c(
        "---",
        paste0("title: ", title),
        paste0("author: ", author),
        "output:",
        "  xaringan::moon_reader:",
        "    css: [default, zh-CN.css]",
        "    lib_dir: libs",
        "    nature:",
        "      highlightStyle: github",
        "      highlightLines: true",
        "      countIncrementalSlides: false",
        "---",
        "layout: true",
        "class: inverse, center, middle",
        "---",
        rmd_body2
      )
      filename <- gsub(' ', '_', title)
      filermd <- paste0(filename, '_xaringan.Rmd')
      writeLines(rmd_new, filermd, useBytes = TRUE)
      if (if_render)
        xaringan::inf_mr(filermd)
    }
  }

clean_txt <- function(x){
  x <- gsub('^ *', '', x)
  x <- gsub(' *$', '', x)
  gsub('@', '\\\\@', x)
}
