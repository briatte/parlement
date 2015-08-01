# add committee co-memberships

load("data/net_fr_an.rda")
sponsors = dir("raw_an/mps")
raw = data_frame()

s = read.csv("data/sponsors-an.csv", stringsAsFactors = FALSE)

cat("Legislature 14\n")
h = htmlParse("http://www.assemblee-nationale.fr/14/tribun/xml/liste_commissions.asp", encoding = "UTF-8")
n = xpathSApply(h, "//div[@id='corps']//ul[@class='liste']/li/a", xmlValue)
n = str_clean(n)
l = xpathSApply(h, "//div[@id='corps']//ul[@class='liste']/li/a/@href")

for(i in l[ grepl("Commission", n) ]) {
  
  cat(n[ l == i ], "\n")
  h = htmlParse(paste0("http://www.assemblee-nationale.fr", i), encoding = "UTF-8")
  a = xpathSApply(h, "//div[@id='organe']//a[contains(@href, 'tribun/fiches')]/@href")
  a = paste0("http://www.assemblee-nationale.fr", a)
  raw = rbind(raw, data.frame(
    y = 14,
    i = unique(s$url[ s$url_an %in% a ]),
    n = n[ which(l == i) ],
    l = i,
    stringsAsFactors = FALSE))
  
}

cat("Legislature 13\n")
h = htmlParse("http://www.assemblee-nationale.fr/13/tribun/xml/liste_commissions.asp", encoding = "UTF-8")
n = xpathSApply(h, "//div[@id='corps']//ul[@class='liste']/li/a", xmlValue)
n = str_clean(n)
l = xpathSApply(h, "//div[@id='corps']//ul[@class='liste']/li/a/@href")

for(i in l) {
  
  cat(n[ l == i ], "\n")
  h = htmlParse(paste0("http://www.assemblee-nationale.fr", i), encoding = "UTF-8")
  a = xpathSApply(h, "//div[@id='organe']//a[contains(@href, 'tribun/fiches')]/@href")
  a = paste0("http://www.assemblee-nationale.fr", a)
  raw = rbind(raw, data.frame(
    y = 13,
    i = unique(s$url[ s$url_an %in% a ]),
    n = n[ which(l == i) ],
    l = i,
    stringsAsFactors = FALSE))
  
}

cat("Legislature 12\n")
h = htmlParse("http://www.assemblee-nationale.fr/12/tribun/commissions.asp", encoding = "UTF-8")
n = xpathSApply(h, "//a[contains(@href, 'gene3')]", xmlValue)
n = str_clean(n)
l = xpathSApply(h, "//a[contains(@href, 'gene3')]/@href")

for(i in l) {
  
  cat(n[ l == i ], "\n")
  h = htmlParse(paste0("http://www.assemblee-nationale.fr/12/tribun/", i), encoding = "UTF-8")
  a = xpathSApply(h, "//table[@width='90%']//a[contains(@href, 'fiches_id')]/@href")
  a = gsub("../fiches_id", "http://www.assemblee-nationale.fr/12/tribun", a)
  raw = rbind(raw, data.frame(
    y = 12,
    i = unique(s$url[ s$url_an %in% a ]),
    n = n[ l == i ],
    l = i,
    stringsAsFactors = FALSE))
  
}

cat("Legislature 11\n")
h = htmlParse("http://www.assemblee-nationale.fr/11/tribun/commissions.asp", encoding = "UTF-8")
n = xpathSApply(h, "//a[contains(@href, 'gene3')]", xmlValue)
n = str_clean(n)
l = xpathSApply(h, "//a[contains(@href, 'gene3')]/@href")

for(i in l) {
  
  cat(n[ l == i ], "\n")
  h = htmlParse(paste0("http://www.assemblee-nationale.fr/11/tribun/", i), encoding = "UTF-8")
  a = xpathSApply(h, "//a[contains(@href, 'fiches_id')]/@href")
  a = paste0("http://www.assemblee-nationale.fr/11/tribun/", a)
  raw = rbind(raw, data.frame(
    y = 11,
    i = unique(s$url[ s$url_an %in% a ]),
    n = n[ l == i ],
    l = i,
    stringsAsFactors = FALSE))
  
}

raw$n = tolower(raw$n)
raw = subset(raw, grepl("commission", n))

write.csv(summarise(group_by(raw[, c(1, 3) ], y, n), members = n()) %>% 
            arrange(y, n) %>% 
            data.frame, "data/committees-an.csv", row.names = FALSE)

# unique legislature-committee pairings
raw$u = paste(raw$y, raw$n)

comm = data_frame(u = unique(raw$u))

# add sponsor columns
for(i in sponsors)
  comm[, gsub("\\.html", "", i) ] = 0

for(i in colnames(comm)[ -1 ])
  comm[ , i ] = as.numeric(comm$u %in% raw$u[ raw$i == i ])

stopifnot(s$url %in% names(comm[, -1]))

# assign co-memberships to networks
for(i in ls(pattern = "^net_fr_an1[1-4]")) {
  
  n = get(i)
  cat(i, ":", network.size(n), "nodes")
  
  sp = network.vertex.names(n)
  names(sp) = n %v% "url"
  
  stopifnot(names(sp) %in% colnames(comm))
  
  m = comm[ substr(comm$u, 1, 2) == gsub("\\D", "", i), names(sp) ]
  
  cat(" :", nrow(m), "committees", ncol(m), "MPs")
  M = m
  
  m = t(as.matrix(m)) # sponsors in rows, committees in columns
  m = m %*% t(m) # adjacency matrix
  
  stopifnot(ncol(m) == network.size(n))
  
  colnames(m) = sp[ colnames(m) ]
  rownames(m) = sp[ rownames(m) ]
  
  e = data_frame(i = n %e% "source", j = n %e% "target")
  e$committee = NA
  
  for(j in 1:nrow(e))
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

save(list = ls(pattern = "^((co)?net|edges|bills)_fr_an\\d{1,2}$"), 
     file = "data/net_fr_an.rda")
