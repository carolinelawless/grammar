remove(list=ls())
library(rjson)
source("simulate_2mcfg(functions).R")

number_sentences<- 500
grammar<- "cf"
#gamma1<- 3 #parameter for Poisson over #arguments of functions
#gamma2<- 0.5 #parameter for Poisson over length of strings
#gamma2<- 0.5 #parameter for binomial for the additional strings
alpha1<- 3 #scaling parameter for DP over nonterminals
alpha2<- 3 #scaling parameter for DP over rules
terminals<- c('a','b','c','d')
p<- 0.5 #probability of emission
sentence_length_max<- 1e2

##the following 5 objects store elements of all previously drawn rules
string1_list_all<- list() #first dim of output of functions
string2_list_all<- list() #second dim of output of functions
nonterminals_vec_all<- vector() #vector of nonterminal nodes
nonterminals_list_all<- list() #list of nonterminal children of nonterminal nodes
number_symbols_vec_all<- vector() #number of terminal symbols introduced (useful for calculating sentence length)

n<- length(terminals)

parameters <- list(n)
names(parameters)<- "n"

observations<- list()

for(i in 1:number_sentences){
  sl<- 0
  while(sl<=1){
  TT<- tree_random(gamma1,gamma2,alpha1,p,terminals,alpha2,string1_list_all,string2_list_all,nonterminals_vec_all,nonterminals_list_all,number_symbols_vec_all,grammar)

  N_list<- TT[[1]]
  string1_list<- TT[[2]]
  string2_list<- TT[[3]]
  N_list
  
  string1_list_all<- TT[[5]]
  string2_list_all<- TT[[6]]
  nonterminals_vec_all<- TT[[7]]
  nonterminals_list_all<- TT[[8]]
  number_symbols_vec_all<- TT[[9]]
  N_list
  sentence_length<- length(string1_list)
  
  sentence<- sentence_construction(N_list,string1_list,string2_list)
  sl<- length(sentence)
}
  observations[[i]]<- sentence
  print(i)
}
observations

data<- list(parameters,observations)

names(data)<- c("theta","y")

jsonData <- toJSON(data)

name<- paste0("simulated_",grammar,"(",length(terminals),").json")
  
write(jsonData,name)
