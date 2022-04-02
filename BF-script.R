remove(list=ls())
setwd("//wsl$/Ubuntu/home/caroline/birchgrammar/Grammar/output/")
library(rjson)

filebase = "simulated_cf(4)"

f.cf = paste0(filebase, "-cf-s5-p10000-sent500_1samp(b).json")
f.reg = paste0(filebase, "-reg-s10-p10000-sent500_1samp(a).json")

#f.cf = file.choose()
#f.reg = file.choose()

d.cf = fromJSON(file = f.cf)
d.reg = fromJSON(file = f.reg)

l.cf = length(d.cf)
l.reg = length(d.reg)

ml.cf = rep(NA, l.cf)
ml.reg = rep(NA, l.reg)

for(i in 1:l.cf){
  ml.cf[i] = tail(d.cf[[i]]$lnormalize, 1)
}

for(i in 1:l.reg){
  ml.reg[i] = tail(d.reg[[i]]$lnormalize, 1)
}
# sort(ml.cf)
# sort(ml.reg)

# positive: favours CF
c(max(ml.cf) - min(ml.reg), mean(ml.cf - ml.reg), min(ml.cf) - max(ml.reg))

# test

var.cf<- var(ml.cf)
var.reg<- var(ml.reg)

t<- (mean(ml.cf) - mean(ml.reg))/ sqrt(var.cf/l.cf + var.reg/l.reg)

df<- (var.cf/l.cf + var.reg/l.reg)^2/( (var.cf/l.cf)^2/(l.cf-1) + (var.reg/l.reg)^2/(l.reg-1) )

t.test(ml.cf,ml.reg)
t

