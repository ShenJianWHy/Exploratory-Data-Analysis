strSort <- function(x) {sapply(lapply(strsplit(x, NULL), sort), paste, collapse="")%>% gsub(pattern = " ", replacement = "")}
