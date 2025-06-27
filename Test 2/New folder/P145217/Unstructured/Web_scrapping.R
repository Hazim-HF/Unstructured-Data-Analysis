# Part 2: Web scrapping
eg6<-readLines("https://en.wikipedia.org/wiki/Data_science")
eg6<-readLines("https://www.pathofexile.com/")

eg6[grep("\\h2",eg6)]
eg6[grep("\\p",eg6)] #paragraph

#Using library XML
library(XML)
doc<-htmlParse(eg6)
doc.text<-unlist(xpathApply(doc,'//p',xmlValue))
unlist(xpathApply(doc,'//h2',xmlValue))

#Using library httr
library(httr)
eg7<-GET("https://www.edureka.co/blog/what-is-data-science/")
doc<-htmlParse(eg7)
doc.text<-unlist(xpathApply(doc,'//p',xmlValue))

#Part 2: Web scrapping
#Using library rvest
library(rvest)
eg8<-read_html("https://www.edureka.co/blog/what-is-data-science/")
nodes<-html_nodes(eg8,'.color-4a div span , .btn-become-profesional-link+ p')
texts<-html_text(nodes)
texts

eg8<-read_html("https://www.pathofexile.com/")
nodes<-html_nodes(eg8,'.FontinBold')
texts<-html_text(nodes)
texts

#Selecting multiple pages
pages<-paste0('https://www.amazon.co.jp/s?k=skincare&crid=28HIW1TYLV9UM&sprefix=skincare%2Caps%2C268&ref=nb_sb_noss_1&page=',0:9)
eg10<-read_html(pages[1])
nodes<-html_nodes(eg10,'.a-price-whole')
texts<-html_text(nodes)
Price<-function(page){
  url<-read_html(page)
  nodes<-html_nodes(url ,'.a-price-whole ')
  html_text(nodes)}
sapply(pages,Price)
do.call("c",lapply(pages,Price))

base_link = "https://www.amazon.co.jp/s?i=digital-music&srs=3534638051&bbn=3534638051&rh=n%3A3534638051%2Cn%3A2128134051&dc&language=en&qid=1744000033&rnid=2321267051&xpid=EWY9FT7MgyKhn&ref=sr_pg_2&page="
pages<-paste0(base_link,1:5)
eg10<-read_html(pages[1])
css_selector_string = '.a-color-base.a-text-normal span , .a-price-whole'
nodes<-html_nodes(eg10, css_selector_string)
texts<-html_text(nodes)
Price<-function(page){
  url<-read_html(page)
  nodes<-html_nodes(url , css_selector_string)
  html_text(nodes)}
sapply(pages,Price)
do.call("c",lapply(pages,Price))


