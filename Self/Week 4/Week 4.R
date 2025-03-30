## Text Exploration

# Regular Expression - a language to identify pattern/sequence of character

#grep function (without using any packages)

#1st function - grep()
ww<-c("statistics","estate","castrate","catalyst","Statistics")
ss<-c("I like statistics","I like bananas","Estates and statues are expensive")

grep(pattern="stat",x=ww) #x is document, will return the location only
grep(pattern="stat",x=ww, ignore.case=T) #ignore  the capital/small latter. will return the location only
grep(pattern="stat",x=ww, ignore.case=T,value=T) #ignore  the capital/small latter. Return to particular words

#2nd function - grepl()
grepl(pattern = "stat", x=ww) #return  true/false
grepl(pattern = "stat", x=ss) #return  true/false

#3rd function - regexpr()
# return a vector of two attributes; position of the first match and length
# if not it returns to -1

regexpr(pattern="stat",ww)
regexpr(pattern="stat",ss)

#4th function - gregexpr()
gregexpr(pattern="stat",ss) ##lebih details sebab dia boleh trace dua perkataan

#5th function - regexec()
regexec(pattern="(st)(at)", ww) #gabungan dua words

#6th function - sub()
sub("stat","STAT", ww, ignore.case=T)
sub("stat","STAT", ss, ignore.case=T) #convert first word sahaja

#7th function - gsub()
gsub("stat","STAT",ss,ignore.case=T) #convert whole word sahaja


library(stringr)
words #dataset related to words
fruit #dataset fruit available in package stringr
sentences #dataset sentences

#common function in package stringr

str_length("This is STQD6114") #give length of string
str_split(sentences," ") #split the function by space & return the list
str_split(sentences," ", simplify=T) #TRUE return to the matrices
str_c("a","b","c") #combine string to become long list
str_c("A",c("li","bu","ngry")) #combine "A" to each vector
str_c("one for all","All for one", sep=",")
str_c("one for all","All for one", collapse=",") #combine the string to be one sentences

#str_sub() gives subset

x<-c("Apple","Banana","Pear")
str_sub(x,1,3) #gives from 1st to 3rd letter
str_sub(x,-3,-1) #gives the last three letter
str_to_upper(x) #return the string to upper case letter
str_to_lower(x) #return the string to lower case letter
str_to_title("unstructured data analytics") #upper case letter of each word
str_view(fruit,"an") #view the pattern of dataset in the browser by highlighting - the first pattern in a word
str_view_all(fruit,"an") #view the pattern of dataset in the browser by highlighting - the whole pattern in a word
str_view_all(fruit, "an", match=T) #tunjuk yang ada an je
str_view(fruit, "an", match=T) #tunjuk yang ada an jugak


# "." refers to anything except newline
str_view(fruit,".a.", match=T) #sebelum & selepas huruf a ada huruf lain
str_view(sentences,".a.", match=T) #the first "a" in the sentence
str_view_all(sentences,".a.", match=T) #semua yang ada ".a." before and after 
str_view_all(sentences,"\'", match=T) #find the symbol ('), put (\)

#Anchors - ^ refers to the start of a string, $ refers to the end of a string
str_view(x,"^A") #perkataan yang start dengan A
str_view(fruit,"^a")
str_view(fruit,"a$", match=T) #perkataan ending dgn a
str_view_all(fruit,"^...$",match=T) #fruit dengan 3 perkataan. Tak kisah start&ending huruf apa

ee<-c("sum","summarize","rowsum","summary")
str_view(ee,"sum")
str_view(ee,"\\bsum") #perkataan "sum" in the front of perkataan
str_view(ee,"sum\\b") #tamat dengan sum
#note: b- boundary, d-digits, s- white space (space, tab,newline)

str_view(sentences,"\\d", match=T) #takde digit dalam sentences
ss<-c("This is a class with students", "There are 18 students", "This class is from 11.00 am")
str_view(ss,"\\d", match=T) #tunjukkan first digit
str_view_all(ss,"\\d", match=T) #tunjukkan semua digits
str_view_all(ss,"\\s", match=T) #tunjuk semua space

str_view_all(fruit,"[abc]", match=T) #[abc] match to all a/b/c even dalam satu perkataan
str_view_all(fruit,"^[abc]", match=T) #start with a/b/c bagi setiap perkataan
str_view_all(fruit,"[^abc]", match=T) #letters except a/b/c
str_view_all(fruit,"^[^abc]", match=T) #start selain a/b/c
str_view_all(fruit,"^(g|h)", match=T) #sama dengan ^[gh]

#Repetition
#? means 0 or 1 repetition
#+ means 1 or more repitition
#* means 0 or more repitition
#{n} means n repitition
#{,m} means m or less times repitition
#{n,m} means between n to m times repitition

ex<-"aabbbccddddeeeee"
str_view(ex,"aab?") #0 or 1 repitition
str_view(ex,"aac?") #output give aa because can be 0
str_view_all(ex,"(a|d){2}") #find a or d sebanyak 2 kali
str_view_all(ex,"de+") #find the letters d and e, e can repeat more than once
str_view_all(ex,"de+?") #find d and e, and gives the shortest because "?" gives 1 rep
str_view_all(ss,"\\d+") #find digit at least once
str_view_all(ss,"\\d{2,}")


#Grouping and backreferencing
str_view(fruit,"(a).\\1", match=T) #find a, after any letter (1 dot= 1 letter),then repeat once
str_view(fruit,"(a).+\\1", match=T) #between 'a' must have more than 1 repeatition
str_view(fruit,"(a)(.)+\\1\\2", match=T) #find a, then followed by character, then repeat a again, then repeat the first one
str_view(fruit,"(.)(.)+\\2\\1", match=T) #find a pair of characters that is repeat in that word (will end with that pair of words)

str_view(words,"^(.).*\\1$", match=T) #start any character, and hv any character inside (can be 0/more bcoz use *), end with the start letter
str_view(words,"(..).*\\1", match=T) #start with pair.. tengah tu boleh 0/more repeat. ending with same pair masa start


#other packages in packages str
str_detect(fruit,"e") #detect true or false
str_detect(fruit,"[aeiou]$")
str_count(fruit,"e") #count letter e in word
fruit[5] #element ke-5
str_replace(fruit,"^a","A")
str_replace_all(fruit,"^a"="A", "^e"="E") #tak boleh run
str_locate(fruit) #tak boleh run
