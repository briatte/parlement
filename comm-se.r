# add committee co-memberships

sponsors = dir("raw_se/senateur", pattern = "html$", full.names = TRUE)
raw = data_frame()

s = read.csv("data/sponsors-se.csv", stringsAsFactors = FALSE)

for (i in sponsors) {
  
  h = htmlParse(i, encoding = "UTF-8")
  n = xpathSApply(h, "//li[contains(text(), 'ommission')]", xmlValue)
  n = tolower(str_clean(gsub("(.*)(ommission)(.*)", "c\\2\\3", n)))
  
  if (length(n))
    raw = rbind(raw, data_frame(i, n))
  
}

raw$i = gsub("raw_se/senateur/|\\.html", "", raw$i)
raw$n = gsub("\\s\\(président\\)$|\\)$", "", raw$n)

# save flat list
write.csv(summarise(group_by(raw, n), members = n()) %>% 
            arrange(n), "data/committees-se.csv", row.names = FALSE)

comm = data_frame(u = unique(raw$n))

# add sponsor columns
for (i in sponsors)
  comm[, gsub("raw_se/senateur/|\\.html", "", i) ] = 0

for (i in colnames(comm)[ -1 ])
  comm[ , i ] = as.numeric(comm$u %in% raw$n[ raw$i == i ])

stopifnot(gsub("http://www.senat.fr/senateur/|\\.html$", "", s$url) %in% names(comm[, -1]))

# assign co-memberships to networks
for (i in ls(pattern = "^net_fr_se")) {
  
  n = get(i)
  cat(i, ":", network.size(n), "nodes")
  
  sp = network.vertex.names(n)
  names(sp) = gsub("http://www.senat.fr/senateur/|\\.html$", "", n %v% "url")
  
  stopifnot(names(sp) %in% colnames(comm))
  
  m = comm[ , names(sp) ]
  
  cat(" :", nrow(m), "committees", ncol(m), "MPs")
  M = m
  
  m = t(as.matrix(m)) # sponsors in rows, committees in columns
  m = m %*% t(m) # adjacency matrix
  
  stopifnot(ncol(m) == network.size(n))
  
  colnames(m) = sp[ colnames(m) ]
  rownames(m) = sp[ rownames(m) ]
  
  e = data_frame(i = n %e% "source", j = n %e% "target")
  e$committee = NA
  
  for (j in 1:nrow(e))
    e$committee[ j ] = m[ e$i[ j ], e$j[ j ] ]
  
  cat(" co-memberships:",
      str_pad(paste0(range(e$committee), collapse = "-"), 6, "right"),
      sum(e$committee == 0), "null,",
      sum(e$committee == 1), "single,",
      sum(e$committee > 1), "> 1\n")
  
  nn = network(e[, 1:2], directed = FALSE)
  nn %e% "committee" = e$committee
  
  print(table(nn %e% "committee", exclude = NULL))
  stopifnot(!is.na(nn %e% "committee"))
  
  n %e% "committee" = e$committee
  assign(i, n)
  
  nn %n% "committees" = as.table(rowSums(M))
  assign(paste0("co", i), nn)
  
}
