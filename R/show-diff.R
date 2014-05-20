#' Create a diff with git and export to html
#' 
#' @param file one or two files to create diff for. If exactly two paths are given and at least one points outside the current repository, \code{git diff} will compare the two files.
#' @param commit one or two revisions hashes of the git repository or "HEAD" (and a revision hash). If one revision is provided, the diff will be between the revision (old, A) and your local file (new, B). If there are two revisions, the diff is between the first revision (old, A) and the second revision (new, B). Se the git diff manaual page for more information. (Run \code{git diff --help} in your console.)
#' @param context How mutch context should the output contain? \code{full} for the whole file, \code{auto} for git standard (3 lines), or any number used in the \code{git diff option --unified=context} 
#' @param git.options string, contains options passed to the git command. The following options are already set and cannot be overwritten: \code{--color=always}, \code{--color-words}, and \code{--word-diff}, \code{--unified=context}
#' @param output string, output of the html code produced by the function, \code{viewer} to show the diff in the RStudio viewer, \code{string} to return as a string with the html body part (without the body-tag), or \code{file} for saving a standalone html version to a file. If you choose \code{file} you must provide the file name in the parameter \code{output.file}
#' @param output.file a connection, or a character string naming the file to write to
#' @param jquery url to the jQuery javascript library, NULL uses the internal bundled version
#' 
#' @export
#' 
show_diff <- function (
  file, 
  commit = "HEAD", 
  context = c("full", "auto"),
  git.options = "--ignore-space-change --ignore-blank-lines --minimal", 
  clean = TRUE, 
  output = c("viewer", "string", "file"), 
  output.file = NULL, 
  template = NULL, 
  css = NULL, 
  jquery = NULL) {
  
  output <- match.arg(output)
  if (output == "file" && is.null(output.file)) {
    stop("You must provide a connection or a file name, if you set output to 'file'.")
  }
  
  ### prepare path, options, working directory, coloring
  file <- normalizePath(file)
  
  # options
  if (!is.numeric(context)) {
    context <- match.arg(context)
  }
  if (context == "full") {
    unified <- paste0("--unified=", sum(sapply(file, function(x){length(readLines(x, warn = FALSE))}), na.rm = TRUE))
  } else if (context == "auto") {
    unified <- ""
  } else {
    unified <- paste0("--unified=", context)
  }
  
  git.options <- paste("--color=always --color-words --word-diff=color", unified, git.options)
  
  # set working directory so that git will work
  wd <- getwd()
  setwd(dirname(file))
  
  # save git colors and set to standard colors: 
  # se http://ascii-table.com/ansi-escape-sequences.php for more information about coloring
  color.diff <- data.frame(name = c("color.diff.meta", 
                                    "color.diff.frag", 
                                    "color.diff.func", 
                                    "color.diff.old", 
                                    "color.diff.new", 
                                    "color.diff.commit", 
                                    "color.diff.whitespace", 
                                    "color.diff.plain"), 
                           standard = c("bold", 
                                        "cyan", 
                                        "magenta", 
                                        "red", 
                                        "green", 
                                        "yellow", 
                                        "blue", 
                                        ""), 
                           pattern = c("\033\\[1m(.+?)\033\\[m", 
                                       "\033\\[36m(.+?)\033\\[m", 
                                       "\033\\[35m(.+?)\033\\[m", 
                                       "\033\\[31m(.+?)\033\\[m", 
                                       "\033\\[32m(.+?)\033\\[m", 
                                       "\033\\[33m(.+?)\033\\[m", 
                                       "\033\\[34m(.+?)\033\\[m", 
                                       "(.*?)\033\\[m"),
                           replacement = c("<span class=\"meta\">\\1</span>",
                                           "<span class=\"frag\">\\1</span>",
                                           "<span class=\"func\">\\1</span>",
                                           "<del>\\1</del>",
                                           "<ins>\\1</ins>",
                                           "<span class=\"commit\">\\1</span>",
                                           "<span class=\"whitespace\">\\1</span>", 
                                           "\\1"),
                           old = rep("", 8), 
                           stringsAsFactors = FALSE)
  
  for (color.diff.name in color.diff[["name"]]) {
    # save old coloring information
    color <- suppressWarnings(system(paste("git config --global", color.diff.name), intern = TRUE))
    if (!is.null(attr(color, "status"))) {
      color <- ""
    }
    color.diff[color.diff[["name"]] == color.diff.name, "old"] <- color
    
    # set standard coloring
    standard.color <- color.diff[color.diff[["name"]] == color.diff.name, "standard"]
    if (standard.color == "") {
      if (color != "") {
        system(paste("git config --global --unset", color.diff.name))
      }
    } else {
      system(paste("git config --global", color.diff.name, standard.color))
    }
  }

  
  ### run git command
  command <- paste("git diff", 
                   git.options, 
                   commit, 
                   ifelse(commit[1] != "", "--", ""), 
                   paste(shQuote(file), collapse = " ")) 
  diff <- system(command, intern = TRUE)
  
  ### restore coloring and working directory
  # set back git colors
  for (color.diff.name in color.diff[["name"]]) {
    old.color <- color.diff[color.diff[["name"]] == color.diff.name, "old"]
    if (old.color == "") {
      if (color.diff[color.diff[["name"]] == color.diff.name, "standard"] != "") {
        system(paste("git config --global --unset", color.diff.name))
      }
    } else {
      system(paste("git config --global", color.diff.name, old.color))
    }
  }
  
  #set back working directory
  setwd(wd)
  
  # If there are no differences, throw a warning and return
  if (identical(diff, character(0))) {
    warning("There are no differences between the files.")
    return()
  }
  
  ### replace coloring with html tags
  for (color.diff.name in color.diff[["name"]]){
    pattern <- color.diff[color.diff[["name"]] == color.diff.name, "pattern"]
    replacement <- color.diff[color.diff[["name"]] == color.diff.name, "replacement"]
    diff <- gsub(pattern, replacement, diff)
  }
  
  
  ### output
  diff <- paste0("<p>", diff, "</p>")
  
  if (output == "string") {
    return(diff)
  } else {
    
    if (is.null(template)) {
      template <- system.file("html", "template.html", package = getPackageName())
    }
    if (is.null(css)) {
      css <- system.file("css", "style.css", package = getPackageName())
    }
    if (is.null(jquery)) {
      jquery <- system.file("js", "jquery-1.11.1.min.js", package = getPackageName())
    }
    
    whisker.template <- paste(readLines(template, warn = FALSE), collapse = "\n")
    
    html <- whisker.render(whisker.template, data = list(css = css, 
                                                         jquery = jquery, 
                                                         body = paste(diff, collapse="\n")))
    if (output == "viewer") {
      output.file <- tempfile(fileext=".html")
    }
    con <- file(output.file, "w+", encoding = "UTF-8")
    writeLines(html, con = con)
    close(con)
    if (output == "viewer" && exists("viewer", envir=getNamespace("rstudio"))) {
      rstudio::viewer(output.file)
    }
  }
  
}
