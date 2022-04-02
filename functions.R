remove(list=ls())

##Draw from the DP of nonterminals
draw_nt<- function(alpha1,nonterminals_vec,n){
  nt<- vector(length=n)
  ntv<-nonterminals_vec
  for(i in 1:n){
    if(length(ntv)==0){
      nt[i]<- 1
    }else{
      draw<- sample(c("new","old"),1,prob=c(alpha1,length(ntv)))
      if(draw=="old"){
        s<- sample(1:length(ntv),1)
        nt[i]<- ntv[s]
      }else if(draw=="new"){
        nt[i]<- max(ntv)+1
      }
    }
    ntv<- c(ntv,nt[i])
  }
  return(nt)
}

##Random base production rule
base_production_random<- function(gamma1,gamma2,alpha1,nonterminals_vec,terminals,grammar){
  if(grammar=="g0"){
    N<- rpois(1,gamma1)+1
    alpha_length<- 4*N+2
    #n<- rpois(alpha_length,gamma2)
    n<- rbinom(alpha_length,1,0.5)
    alpha<- vector(length=alpha_length)
    for(i in 1:alpha_length){
      alphas<- sample(terminals,n[i],replace=TRUE)
      alpha[i]<- paste(alphas,collapse="")
    }
    sigma<- sample(c(1:(2*N),rep('',2*N)),4*N,replace=FALSE)
  
    string1<- c(rbind(alpha[1:(2*N)],sigma[1:(2*N)]),alpha[(4*N+1)])
    string2<- c(rbind(alpha[(2*N+1):(4*N)],sigma[(2*N+1):(4*N)]),alpha[(4*N+2)])
    nonterminals<- draw_nt(alpha1,nonterminals_vec,N)
    number_symbols<- sum(n)
    rule_type<- 1
  }else if(grammar== "g1"){
    ss<- sample(terminals,1)
    string1<- c(ss,1)
    string2<- c(ss,2)
    nonterminals<- 2
    number_symbols<- 2
    rule_type<- 1
  }else if(grammar=="g2"){
    string1<- c("a",1,"b")
    string2<- c("c",2,"d")
    nonterminals<- 2
    number_symbols<- 4
    rule_type<- 1
  }else if(grammar== "g3"){
    N<- 1
    string1<- c("a",1)
    string2<- c(2,"b")
    nonterminals<- draw_nt(alpha1,nonterminals_vec,N)
    number_symbols<- 2
    rule_type<- 1
  }else if(grammar== "alpha^2"){
    N<- 1
    ts<- sample(terminals,1)
    string1<- c(ts,1)
    string2<- c(ts,2)
    nonterminals<- draw_nt(alpha1,nonterminals_vec,N)
    number_symbols<- 2
    rule_type<- 1
  }else if(grammar== "cf"){
    N<- 2
    string1<- 1:2
    string2<- 3:4
    nonterminals<- draw_nt(alpha1,nonterminals_vec,N)
    number_symbols<- 0
    rule_type<- 1
  }else if(grammar== "regular"){
    N<- 1
    string1<- sample(terminals,1)
    string2<- 1:2
    nonterminals<- draw_nt(alpha1,nonterminals_vec,N)
    number_symbols<- 1
    rule_type<- 1
  }
  
  r<- list(string1,string2,nonterminals,number_symbols,rule_type)
  return(r)
}

##Random base rule
base_rule_random<- function(gamma1,gamma2,alpha1,nonterminals_vec,p,terminals,grammar){
  type<- sample(c("emission","production"),1,prob=c(p,1-p))
  if(type=="emission"){
    if(grammar=="g0"){
    string1<- sample(terminals,1)
    string2<- sample(terminals,1)
    nonterminals<- 0
    number_symbols<- 2
    rule_type<- 2
    }else if(grammar== "g1"){
    ss<- sample(terminals,1)
    string1<- ss
    string2<- ss
    nonterminals<- 0
    number_symbols<- 2
    rule_type<- 2
    }else if(grammar=="g2"){
    string1<- ""
    string2<- ""
    nonterminals<- 0
    number_symbols<- 0
    rule_type<- 2
    }else if(grammar=="g3"){
    string1<- "a"
    string2<- "b"
    nonterminals<- 0
    number_symbols<- 2
    rule_type<- 2
    }else if(grammar== "alpha^2"){
    terminal_symbol<- sample(terminals,1)
    string1<- terminal_symbol
    string2<- terminal_symbol
    nonterminals<- 0
    number_symbols<- 2
    rule_type<-2
    }else if(grammar=="cf"){
    string1<- sample(terminals,1)
    string2<- ""
    nonterminals<- 0
    number_symbols<- 1
    rule_type<- 2
    }else if(grammar=="regular"){
    string1<- sample(terminals,1)
    string2<- ""
    nonterminals<- 0
    number_symbols<- 1
    rule_type<- 2
    }
    r<- list(string1,string2,nonterminals,number_symbols,rule_type)
  }else if(type=="production"){
    r<- base_production_random(gamma1,gamma2,alpha1,nonterminals_vec,terminals,grammar)
  }
  return(r)
}

##Random DP rule
dp_rule_random<- function(gamma1,gamma2,alpha1,p,alpha2,nonterminal,string1_list,string2_list,nonterminals_vec,nonterminals_list,number_symbols_vec,grammar){
  nonterminals_vec_short<- nonterminals_vec[1:length(nonterminals_list)]
  index<- which(nonterminals_vec_short== nonterminal)
  L<- length(index)
  draw<- sample(c("new","old"),1, p=c(alpha2,L))
  if(draw=="new"){
    r<- base_rule_random(gamma1,gamma2,alpha1,nonterminals_vec,p,terminals,grammar)
  }else if(draw=="old"){
    j<- sample(index,1)
    ss1<- string1_list[[j]]
    ss2<- string2_list[[j]]
    nt<- nonterminals_list[[j]]
    number_symbols<- number_symbols_vec[j]
    rule_type<- 3
    r<- list(ss1,ss2,nt,number_symbols,rule_type)
  }
  return(r)
}

##Simulate a random tree
tree_random<- function(gamma1,gamma2,alpha1,p,terminals,alpha2,string1_list_all,string2_list_all,nonterminals_vec_all,nonterminals_list_all,number_symbols_vec_all,grammar){
  
  N_list<- list()
  string1_list<- list()
  string2_list<- list()
  
  nonterminal<- draw_nt(alpha1,nonterminals_vec_all,1)
  nonterminals_vec<- nonterminal
  
  nonterminals_list<- list() 
  sentence_length<- 0
  rule_types<- vector()
  
  
  
  ##rr<- base_production_random(gamma1,gamma2,alpha1,nonterminals_vec_all,terminals,grammar)
  rr<- dp_rule_random(gamma1,gamma2,alpha1,p,alpha2,nonterminal,string1_list_all,string2_list_all,nonterminals_vec_all,nonterminals_list_all,number_symbols_vec_all,grammar)
  nonterminals_vec_all<- c(nonterminals_vec_all,nonterminals_vec)
  
  sentence_length<- sentence_length + rr[[4]]
  rule_types<- c(rule_types,rr[[5]])
  number_symbols_vec_all<- c(number_symbols_vec_all,rr[[4]])
  
  N_list[[1]]<- length(rr[[3]])
  string1_list[[1]]<- rr[[1]]
  string1_list_all[[length(string1_list_all)+1]]<- rr[[1]]
  string2_list[[1]]<- rr[[2]]
  string2_list_all[[length(string2_list_all)+1]]<- rr[[2]]
  if( sum(rr[[3]]!=0)>0){
    nonterminals_vec<- c(nonterminals_vec,rr[[3]][which(rr[[3]]!=0)])
    nonterminals_vec_all<- c(nonterminals_vec_all,rr[[3]][which(rr[[3]]!=0)])
  }
  nonterminals_list[[1]]<- rr[[3]]
  nonterminals_list_all[[length(nonterminals_list_all)+1]]<-rr[[3]]
  
  
  nts<- nonterminals_list[[1]]
  
  while(sum(nts)>0 && sentence_length<sentence_length_max){
    nnts<- vector()
    nl<- vector()
    for(i in 1:length(nts)){
      nonterminal<- nts[i]
      if(nonterminal !=0){
        rr<- dp_rule_random(gamma1,gamma2,alpha1,p,alpha2,nonterminal,string1_list_all,string2_list_all,nonterminals_vec_all,nonterminals_list_all,number_symbols_vec_all,grammar)
        rule_types<- c(rule_types,rr[[5]])
        sentence_length<- sentence_length + rr[[4]]
        number_symbols_vec_all<- c(number_symbols_vec_all,rr[[4]])
        if( sum(rr[[3]]!=0)>0){
          nonterminals_vec<- c(nonterminals_vec,rr[[3]][which(rr[[3]]!=0)])
          nonterminals_vec_all<- c(nonterminals_vec_all,rr[[3]][which(rr[[3]]!=0)])
          nl<- c(nl,length(rr[[3]][which(rr[[3]]!=0)]))
        }else{
          nl<- c(nl,0)
        }
        string1_list[[length(string1_list)+1]]<- rr[[1]]
        string1_list_all[[length(string1_list_all)+1]]<- rr[[1]]
        string2_list[[length(string2_list)+1]]<- rr[[2]]
        string2_list_all[[length(string2_list_all)+1]]<- rr[[2]]
        nonterminals_list[[length(nonterminals_list)+1]]<- rr[[3]]
        nonterminals_list_all[[length(nonterminals_list_all)+1]]<- rr[[3]]
        
        nnts<- c(nnts,rr[[3]])
      }
    }
    nts<- nnts
    N_list[[length(N_list)+1]]<- nl
  }
  
  if(sentence_length>=sentence_length_max){
    sentence_length<- 'inf'
  }
  
  TT<- list(N_list,string1_list,string2_list, sentence_length ,string1_list_all,string2_list_all, nonterminals_vec_all, nonterminals_list_all,number_symbols_vec_all,rule_types)
  return(TT)
}


##Determinalistcally evaluate a function in a tree (useful for constructing sentences)
evaluate<- function(s,x1,x2,ind,N){
  ###
  ###
  for(i in 1:N){
    if(sum(s== (2*i-1))>0){
      w<- which(s==(2*i-1))
      s<- append(s,x1[[ind+i]],after= w)
      s<- s[-w]
      }
    if(sum(s== 2*i)>0){
      w<- which(s==(2*i))
      s<- append(s,x2[[ind+i]],after= w)
      s<- s[-w]
      }
  }
  
  #ss<- paste(ss,collapse="")
  return(s)
}

##Determinalistically construct a sentence given a tree

sentence_construction<- function(N_list,r1,r2){
  
  L<- length(N_list)
  
  if(L==1){
  ss<- c(r1[[1]],r2[[1]])
  }else{
  
  width<- vector(length=L)
  for(i in 1:L){
    width[i]=length(N_list[[i]])
  }
  
  j<- L
  len<- width[j]
  index<- sum(width[1:(j-1)])
  x1<- list()
  x2<- list()
  
  for(i in 1:len){
    x1[[i]]<- r1[[index+i]]
    x2[[i]]<- r2[[index+i]]
  }
  xx1<- x1
  xx2<- x2
  
  for(j in (L-1):1){
    
    len<- width[j]
    if(j>1){
      index<- sum(width[1:(j-1)])
    }else if(j==1){
      index<-0
    }
    x1<- list()
    x2<- list()
    
    ind<-0
    for(i in 1:len){
      
      N<- N_list[[j]][i]
      if(N==0){
        x1[[i]]<- r1[[index+i]]
        x2[[i]]<- r2[[index+i]]
      }else if(N>0){
        s1<- r1[[index+i]]
        s2<- r2[[index+i]]

        x1[[i]]<- evaluate(s1,xx1,xx2,ind,N)
        x2[[i]]<- evaluate(s2,xx1,xx2,ind,N)
      }
      ind<- ind+N
    }
    xx1<- x1
    xx2<- x2
  }
  
  #sentence<- paste(c(x1,x2),collapse="")
  ss<- c(x1[[1]],x2[[1]])
  }
  
  ww<- which(ss== "")
  if(length(ww>0)){ ss<- ss[-ww]}
  
  for(i in 1:length(terminals)){
  ss[which(ss==terminals[i])]<- i
  }
  ss<- as.numeric(ss)
  
  return(ss)
  
}
