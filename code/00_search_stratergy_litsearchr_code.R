#-------------------------- Installing required packages----------------------------------------
# Remotes package is required to allow you to install litsearchr package
# Since it is only available through Github and not CRAN
# Skip this part if you have already installed litsearchr 


install.packages("remotes")
library(remotes)
#to install litsearchr from Github
install_github("elizagrames/litsearchr", ref="main")

#-----------------------------------------------------------------------------------------------

#Loading required packaged if they are already installed
pacman::p_load(litsearchr,igraph,ggplot2)

#---------------------------------- Naive search file import and deduplication---------------------------------------------
 
# Before using litsearchr to refine your search terms, we can perform a naive search on the topic on WOS
# The references generated from this naive search can then be used to refine the search terms using litsearchr
# In this case we used references that were present in our own library (through previous searches)

# We can import .bib, .ris or .txt files using 
# import_results function of litsearchr (which is from synthesizer package)
# The function can also be used in case the searches were performed in more the one data base...
# ...e.g.  =c("savedrecs.bib","scopus.bib"))


# In our case we use .bib file "own_library_references.bib" from our own set of references
search_directory="data/01_systematic_search/01_search_strategy/data_litsearchr"
naiveimport<- litsearchr::import_results(directory=search_directory,
                                         file = "own_library_references.bib")


#remove_duplicates function can be used to deduplicate the list based on titles 
# One can use different methods to deduplicate such as exact search or fuzzy search

# In our case, we do not need to deduplicate since we have a short list of references 
# of 15 papers that we have looked at manually already.. 
# However we perform deduplication to show as an example with exact title comparision..
deduplicated_naiveimport<- litsearchr::remove_duplicates(naiveimport, field= "title", method = "exact")

#----------------------------to extract/search for keywords for the naive search------------------------------------------

#extract_terms is used to extract the search string words from our data base
#here we look for search string words in the title, abstract and keywords of the paper
#we use the fakerake method which is the default method of litsearchr that is similar to rapid automatic keyword extraction
#min_freq is the minimum number of times these words much occur in each paper
#min_n sets the minimum phrase that search string can be.. 
#in our case it can be a single word at minimum and maximum a two phrase word

rakedkeywords<-
  litsearchr::extract_terms(text = c(deduplicated_naiveimport$title,
                                     deduplicated_naiveimport$abstract,
                                     deduplicated_naiveimport$keywords),
    method = "fakerake",
    min_freq = 1,
    ngrams = TRUE,
    min_n=1,
    max_n = 2,
    language = "English"
  )

#--------------------counting the occurance of searched keywords across studies in our database----------------------------

#create_dfm counts the occurrence of the searched keywords across the studies and produces a matrix
naive_dfm<-litsearchr::create_dfm(elements = paste(dedup$keywords,dedup$title,dedup$abstracts),
                                 features = rakedkeywords)



#creating the network graph showing co-occurrence between the words (note that one can change
#the numbers of studies and minimal occurence)
naivegraph<-litsearchr:: create_network(search_dfm = naive_dfm, min_studies = 3, 
                                        min_occ = 2)

#------------------------------------plots to visualise the searched keywords----------------------------------------------

#it shows the grap with the co-occurence of words
plot(naivegraph)

#The ‘strength’ of each term in the network is the number of other terms that it appears together with
strengths <- strength(naivegraph)

data.frame(term=names(strengths), strength=strengths, row.names=NULL) %>%
  mutate(rank=rank(strength, ties.method="min")) %>%
  arrange(strength) ->
  term_strengths

#term_strengths

# plot to show the strength of each term to set a cutoff and include the important terms that occur multiple times
cutoff_fig <- ggplot(term_strengths, aes(x=rank, y=strength, label=term)) +
  geom_line() +
  geom_point() +
  geom_text(data=filter(term_strengths, rank>5), hjust="right", nudge_y=20, check_overlap=TRUE)

cutoff_fig

#find_cutoff defines the cutoff. i.e.changing the percentage will return more or less...
#... keywords (the highter the percentage the more keywords)
cutoff_cum <- find_cutoff(naivegraph, method="cumulative", percent=0.90)
cutoff_cum
cutoff_fig +
  geom_hline(yintercept=cutoff_cum, linetype="dashed")


#this line simplifies the graph
reducedgraph<- litsearchr::reduce_graph(naivegraph, 
                                        cutoff_strength = cutoff_cum)
#shows the simplified graph
plot(reducedgraph)

searchterms <- litsearchr::get_keywords(reducedgraph)


write.csv(searchterms, "data/01_systematic_search/01_search_strategy/litsearchr_search_terms.csv")
