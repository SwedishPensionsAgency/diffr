#' Create a diff with git and export to html
#' 
#' @param file
#' @param revision
#' @param ins 
#' @param del
#' @param git.options
#' @param clean
#' @param output
#' 
#' 
show_diff <- function (
  file, 
  revision = NULL, 
  git.options = "--color-words --word-diff --ignore-space-change --ignore-blank-lines --minimal", 
  clean = TRUE, 
  output = c("viewer", "return")) {
  
  
}

# get git config diff color
# set git config diff colors to standard
# git config --global color.diff.new "green"
# color.diff
# 
# Whether to use ANSI escape sequences to add color to patches. If this is set to always, git-diff(1), git-log(1), and git-show(1) will use color for all patches. If it is set to true or auto, those commands will only use color when output is to the terminal. Defaults to false.
# 
# This does not affect git-format-patch(1) or the git-diff-* plumbing commands. Can be overridden on the command line with the --color[=<when>] option.
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