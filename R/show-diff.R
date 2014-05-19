#' Create a diff with git and export to html
#' 
#' @param file file to create diff for
#' @param revision one or two revisions of the git repository. If one revision is provided, the diff will be between the revision (old, A) and your local file (new, B). If there are two revisions, the diff is between the first revision (old, A) and the second revision (new, B). 
#' @param context How mutch context should the output contain? \code{full} for the whole file, \code{auto} for git standard (3 lines), or any number used in the \code{git diff option --unified=context} 
#' @param git.options string, contains options passed to the git command. The following options are already set and cannot be overwritten: \code{--color=always}, \code{--color-words}, and \code{--word-diff}, \code{--unified=context}
#' @param output 
#' @param output.file
#' 
#' @export
#' 
show_diff <- function (
  file, 
  revision = NULL, 
  context = c("full", "auto")
  git.options = "--ignore-space-change --ignore-blank-lines --minimal", 
  clean = TRUE, 
  output = c("viewer", "return", "file"), 
  output.file = NULL) {
  
  
  ### prepare path, options, working directory, coloring
  file <- normalizePath(file)
  git.options <- paste("--color=always --color-words --word-diff", git.options)
  
  # set working directory so that git will work
  wd <- getwd()
  setwd(dirname(file))
  
  # save git colors and set to standard colors: 
  color.diff <- data.frame(name = c("color.diff.plain", 
                                    "color.diff.meta", 
                                    "color.diff.frag", 
                                    "color.diff.func", 
                                    "color.diff.old", 
                                    "color.diff.new", 
                                    "color.diff.commit", 
                                    "color.diff.whitespace"), 
                           standard = c("", 
                                        "bold", 
                                        "cyan", 
                                        "magenta", 
                                        "red", 
                                        "green", 
                                        "yellow", 
                                        "blue"), 
                           pattern = c("(.*?)\\[m", 
                                       "\033\\[1m(.+?)\\[m", 
                                       "\033\\[36m(.+?)\\[m", 
                                       "\033\\[35m(.+?)\\[m", 
                                       "\033\\[31m(.+?)\\[m", 
                                       "\033\\[32m(.+?)\\[m", 
                                       "\033\\[33m(.+?)\\[m", 
                                       "\033\\[34m(.+?)\\[m"),
                           replacement = c("\\1", 
                                           "<span class=\"meta\">\\1</span>",
                                           "<span class=\"frag\">\\1</span>",
                                           "<span class=\"func\">\\1</span>",
                                           "<del>\\1</del>",
                                           "<ins>\\1</ins>",
                                           "<span class=\"commit\">\\1</span>",
                                           "<span class=\"whitespace\">\\1</span>"),
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
      system(paste("git config --global --unset", color.diff.name))
    } else {
      system(paste("git config --global", color.diff.name, standard.color))
    }
  }

  
  # do stuff

  
  
  ### restore coloring and working directory
  # set back git colors
  for (color.diff.name in color.diff[["name"]]) {
    old.color <- color.diff[color.diff[["name"]] == color.diff.name, "old"]
    if (color.diff.standard[[color.diff.name]] == "") {
      system(paste("git config --global --unset", color.diff.name))
    } else {
      system(paste("git config --global", color.diff.name, old.color))
    }
  }
  
  #set back working directory
  setwd(wd)
  
}

# get git config diff color
# set git config diff colors to standard
# git config --global color.diff.new "green"
# color.diff
# color.diff.<slot>
#   
#   Use customized color for diff colorization. <slot> specifies which part of the patch to use the specified color, and is one of plain (context text), meta (metainformation), frag (hunk header), func (function in hunk header), old (removed lines), new (added lines), commit (commit headers), or whitespace (highlighting whitespace errors). The values of these variables may be specified as in color.branch.<slot>.

ins = "\033\\[32m(.+?)\033\\[m" 
del = "\033\\[31m(.+?)\033\\[m"

# run git command

# set back git config diff colors

commit <- ""
file <- ""
command <- paste("git diff --color=always --color-words --word-diff=plain --ignore-space-change --ignore-blank-lines --minimal  --unified=1000000", commit, "--", file)
diff <- system(command, intern = TRUE)




#deletions:
diff <- gsub("\033\\[31m(.+?)\033\\[m", "<del>\\1</del>", diff)

#insertitions:
diff <- gsub("\033\\[32m(.+?)\033\\[m", "<ins>\\1</ins>", diff)

#remove other diff stuff, e.g. \033[m
diff <- gsub("\033\\[m", "", diff)

output <- paste0("<p>", diff, "</p>")

#cat(paste(output, collapse = "\n"))

html.head <- "<!DOCTYPE html>
<html>
<head>
<meta charset=\"UTF-8\" />
<link href=\"http://d2c5utp5fpfikz.cloudfront.net/3_1_1/css/bootstrap.min.css\" rel=\"stylesheet\">
<style>
ins {color: green;}
del {color: red;}
.no-highlighting{
color: black;
text-decoration: none; 
}
.no-decoration {
text-decoration: none;
}
.nodisp {display: none;}
code {color: black;}
body{padding: 5px;}

#buttons{position: fixed; 
top: 5px;
right: 15px;
}
</style>
<script src=\"http://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js\"></script>
<script>

function showHide(what) {

if (what == 'diff') {
$( 'del, ins').each(function( i ){
this.style.display = 'inline';
$ ( this ).removeClass( 'no-decoration' );
$ (this ).toggleClass('no-highlighting');
});
$ ( '#toggle' ).addClass( 'nodisp');
$ ( '#ins' ).removeClass( 'nodisp no-decoration');
$ ( '#del' ).removeClass( 'nodisp no-decoration');
$ ( '#diff' ).addClass( 'nodisp');

} else if (what == 'ins'){
$( 'ins' ).each(function( i ) {
this.style.display = 'inline';
$ ( this ).addClass( 'no-decoration' );
});
$( 'del' ).each(function( i ) {
this.style.display = 'none';
$ ( this ).removeClass( 'no-decoration' );
});
$ ( '#toggle' ).removeClass( 'nodisp');
$ ( '#del' ).removeClass( 'nodisp');
$ ( '#ins' ).addClass( 'nodisp');
$ ( '#diff' ).removeClass( 'nodisp');

} else {
$( 'del' ).each(function( i ) {
this.style.display = 'inline';
$ ( this ).addClass( 'no-decoration' );
});
$( 'ins' ).each(function( i ) {
this.style.display = 'none';
$ ( this ).removeClass( 'no-decoration' );
});
$ ( '#toggle' ).removeClass( 'nodisp');
$ ( '#ins' ).removeClass( 'nodisp');
$ ( '#del' ).addClass( 'nodisp');
$ ( '#diff' ).removeClass( 'nodisp');

}

}

function highlighting(){
$( 'del, ins').each(function( i ){
$ (this ).toggleClass('no-highlighting');
});
}



</script>
</head>
<body>
<div id=\"buttons\">
<button id=\"del\" type=\"button\" onclick=\"showHide('del')\">A</button>
<button id=\"ins\" type=\"button\" onclick=\"showHide('ins')\">B</button>
<button id=\"diff\" class=\"nodisp\" type=\"button\" onclick=\"showHide('diff')\">Diff</button>
<button id=\"toggle\" class=\"nodisp\" type=\"button\" onclick=\"highlighting()\">Color</button>
</div>
<code>
"

html.foot <- "</code>
</body>
</html>"


# con <- file("diff.html", "w+", encoding = "UTF-8")
# writeLines(c(html.head, output, html.foot), con = con)
# close(con)

tmpfile <- htmlFile <- tempfile(fileext=".html")

con <- file(tmpfile, "w+", encoding = "UTF-8")
writeLines(c(html.head, output, html.foot), con = con)
close(con)

rstudio::viewer(tmpfile)