#Scrapeo de Recetas de Chela en R, duplicado en python
#######################################################

#Cargar librerias
library(XML)
library(RCurl)
library(stringr)
library(dplyr)

#Encontrar y Parsear root
root = "http://www.beersmithrecipes.com/styles"
SOURCE <- getURL(root)
parsed_root<-htmlParse(SOURCE)


#Extraer links útiles de los estilos
styles_links_names<-xpathSApply(parsed_root,"//span[@class='subtitle']/a",xmlValue)
styles_links<-xpathSApply(parsed_root,"//span[@class='subtitle']/a/@href")
df<-data.frame(Estilo=styles_links_names,Link=styles_links)


#I want only all grain recipes, initially at least
#Extract useful words from the style tag and paste them properly
#To later create the appropiate URLs
#Lazy regex exp,  I know

pattern<-"[A-z]{2,12}"
estilos<-select(df,Estilo)
aux1<-apply(estilos,1, function(x) str_extract_all(x,pattern,simplify=TRUE))
aux2<-lapply(aux1, paste, collapse="+")
filtered_first_branch <- paste("http://beersmithrecipes.com/searchrecipe?uid=&term=",aux2,"&sort=Best+Match&allgrain=1&rated=0&submit=Apply",sep="")

#Create ALL urls
#Since there is different pages having only the root one isnt good enough

lurls<-vector("list",nrow(df))
names(lurls)<-df[,1]
j=1

##Starts from 2 because beersmith link did not properly write the light american lager link

for (i in 2:length(filtered_first_branch)){
running <- TRUE
rootb<-filtered_first_branch[i]
nextpage<- xpathSApply(htmlParse(getURL(rootb)),"//div[@class='pagelinks']/a",xmlValue)
np<-"Next Page >>"
	while (running) {
		if (np %in% nextpage){
			lurls[[i]][j]<- tail(xpathSApply(htmlParse(getURL(rootb)),"//div[@class='pagelinks']/a/@href"),n=1)
			rootb<-lurls[[i]][j]
			nextpage<- xpathSApply(htmlParse(getURL(rootb)),"//div[@class='pagelinks']/a",xmlValue)
			j<-j+1
		} else {
			running<-FALSE
			j<-1
		}
	}
}

#Merge lists for final links to all existing pages containing recipes
all_pages_links<-mapply(c,lurls,filtered_first_branch,SIMPLIFY=FALSE)
names(all_pages_links)<-df[,1]

#Start empty list to put individual recipes links in it
recipes<-rep(list(rep(0,0)),98)

#Get invididual recipes links for each style
for (i in 1:length(all_pages_links)) {
	recipes[[i]]<-xpathSApply(htmlParse(sapply(all_pages_links[[i]],getURL)),"//h4/a/@href")
	
}


