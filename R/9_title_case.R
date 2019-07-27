
x <- readLines("~/Dokumente/Bibliography/lit.bib")

title_pos <- grep("title", x)

titles <- gsub(".*= {(.*)},", "\\1", x[title_pos], perl = TRUE)
writeLines(titles, "txt/titles.txt")
# Use http://titlecase.com/
titles_cap <- readLines("txt/titles_case.txt")

i <- 1
for(pos in title_pos) {
  x[pos] <- gsub(titles[i], titles_cap[i], x[pos], perl = TRUE)
  i <- i + 1
}

writeLines(x, "~/Dokumente/Bibliography/lit.bib")
