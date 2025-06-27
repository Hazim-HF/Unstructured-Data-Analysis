##grep functions (without use any package)
ww<-c("statistics","estate","castrate","catalyst","Statistics")
ss<-c("I like statistics","I like bananas","Estates and statues are expensive")
#1st function - grep() -give the location of pattern
grep(pattern="stat",x=ww) #x is the document, will return the location only
grep(pattern="stat",x=ww,ignore.case=T) #ignore the capital/small letter, will return the location only
grep(pattern="stat",x=ww,ignore.case=T,value=T) #ignore the capital/small letter, return to that particular words

#2nd function - grepl() - give logical expression
grepl(pattern="stat",x=ww) #Return true/false
grepl(pattern="stat",x=ss)


#3rd function - regexpr()
#return a vector of two attributes; position of the first match and its length
#if not, it returns -1
regexpr(pattern="stat",ww)
regexpr(pattern="stat",ss)
#4th function - gregexpr()
gregexpr(pattern="stat",ss)
#5th function - regexec()
regexec(pattern="(st)(at)",ww)
#6th function - sub()
sub("stat","STAT",ww,ignore.case=T)
sub("stat","STAT",ss,ignore.case=T)
#7th function - gsub()
gsub("stat","STAT",ss,ignore.case=T)

