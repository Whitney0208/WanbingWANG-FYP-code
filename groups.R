B=100
n<-8161
#set.seed(123456)
index_all<-matrix(0,100,8161)

for(bb in 1:B){
  ind<-sample(1:8161,size = n,replace = T)
  index_all[bb,]<-ind
}

index_all<-matrix(0,100,8161)
index_all[1:50,]<-index_50
index_all[51:100,]<-index_50_2

results_all<-append(result50[1:1200],result50_2[1:1200])

#index overlap
#1和2-100
overlap1<-numeric()
for(i in 2:100){
  overlap1[i-1]<-sum(as.integer(index_all[1,])%in%intersect(as.integer(index_all[1,]),as.integer(index_all[i,])))/8161
}

sum(as.integer(index_all[2,])%in%intersect(as.integer(index_all[2,]),as.integer(index_all[3,])))/8161
sum(as.integer(index_all[2,])%in%intersect(as.integer(index_all[2,]),as.integer(index_all[4,])))/8161


s1<-seq(5,2400,24)
cluster_boo<-results_all[s1]

#分组重合率
coincidence_rate<-list()

clu_origin<-case1[["cluster"]][1,1,]
#a<-which(clu_origin==1)
#b<-which(clu_origin==2)
#c<-which(clu_origin==3)

order_row<-matrix(0,100,3)

for(k in 1:100){
  clu<-cluster_boo[k][[1]][1,1,]
  #index1<-which(clu==1)
  #index2<-which(clu==2)
  #index3<-which(clu==3)
  
  coincidence_matrix<-matrix(0,3,3)
  for(i in 1:3){
    for(j in 1:3){
      sample_grouping_index<-as.integer(which(clu==i))
      coincidence_matrix[i,j]<-sum(as.integer(index_all[k,sample_grouping_index])%in%intersect(as.integer(index_all[k,sample_grouping_index]),as.integer(which(clu_origin==j))))/length(as.integer(index_all[k,sample_grouping_index]))
    }
  }
  
  coincidence_rate<-append(coincidence_rate,list(coincidence_matrix))
  
  index1<-which(coincidence_matrix==max(coincidence_matrix),arr.ind = TRUE)
  coincidence_matrix[index1[1],]<-0
  coincidence_matrix[,index1[2]]<-0
  
  index2<-which(coincidence_matrix==max(coincidence_matrix),arr.ind = TRUE)
  coincidence_matrix[index2[1],]<-0
  coincidence_matrix[,index2[2]]<-0  
  
  index3<-which(coincidence_matrix==max(coincidence_matrix),arr.ind = TRUE)
  coincidence_matrix[index3[1],]<-0
  coincidence_matrix[,index3[2]]<-0 
    
  index_matrix<-rbind(index1,index2,index3)
  
  row_rank1<-as.integer(rank(index_matrix[,1]))
  
  desired_order<-order(row_rank1)
  
  row_rank2<-index_matrix[,2][desired_order]
  order_row[k,]<-as.integer(row_rank2)
}

#summary_y
#for(i in 1:100){
#  clu<-cluster_boo[i][[1]]
#  index1<-which(clu==1)
#  index2<-which(clu==2)
#  index3<-which(clu==3)
  
#  sample_data<-newdata[index_all[i,],]
#  y<-sample_data[,26]
  
#  y1<-y[index1]
#  y2<-y[index2]
#  y3<-y[index3]
  
#  groups<-list("g1"=summary(y1),"g2"=summary(y2),"g3"=summary(y3))
#  summary_y<-append(summary_y,groups)
#}

#order_row<-matrix(0,100,3)
#for(i in seq(1,300,3)){
#  m1<-summary_y[i][[1]][4]
#  m2<-summary_y[i+1][[1]][4]
#  m3<-summary_y[i+2][[1]][4]
  
#  row_rank<-as.integer(rank(c(m1,m2,m3)))
#  order_row[(i-1)/3+1,]<-row_rank
#}

#order_row
s<-seq(2,2400,24)
coef_y<-results_all[s]
coef_y_order<-list()
for(i in 1:100){
  desired_order<-order(order_row[i,])
  mat<-coef_y[i][[1]][1,1,,]
  coef_y_order<-append(coef_y_order,list(mat[desired_order,]))
}

#nonzero mean sd
#nonzero
coef_all<-list()
for(i in 1:3){
  for(j in 1:25){
    x1<-list()
    x2<-numeric()
    for(k in 1:100){
      x2<-append(x2,coef_y_order[k][[1]][i,j])
    }
    x1<-append(x1,list(x2))
    coef_all<-append(coef_all,x1)
  }
}

num<-numeric()
for(i in 1:75){
  num<-append(num,sum(coef_all[i][[1]]!=0))
}
num1<-matrix(num,3,25,byrow = T)
colnames(num1)<-colnames(cbind(X,M,C))

#mean
SUM<-numeric()
for(i in 1:75){
  SUM<-append(SUM,sum(coef_all[i][[1]]))
  m<-SUM/num
}
m[is.na(m)]=0
m1<-matrix(m,3,25,byrow = T)
colnames(m1)<-colnames(cbind(X,M,C))

#se<-matrix(0,3,25)
se<-numeric()

for(i in 1:75){
  index_nonzero<-which(coef_all[i][[1]]!=0)
  coef_nonzero<-coef_all[i][[1]][index_nonzero]
  se<-append(se,sd(coef_nonzero))
}
se1<-matrix(se,3,25,byrow = T)
colnames(se1)<-colnames(cbind(X,M,C))

#coef_m
s<-seq(1,2400,24)
coef_m<-results_all[s]
coef_m_order<-list()
for(i in 1:100){
  desired_order<-order(order_row[i,])
  mat<-coef_m[i][[1]][1,1,,,]
  coef_m_order<-append(coef_m_order,list(mat[desired_order,,]))
}

#nonzero mean sd
#nonzero
g1<-list()
for(i in 1:100){
  g1<-append(g1,list(coef_m_order[i][[1]][1,,]))
}
  
g2<-list()
for(i in 1:100){
  g2<-append(g2,list(coef_m_order[i][[1]][2,,]))
}
  
g3<-list()
for(i in 1:100){
  g3<-append(g3,list(coef_m_order[i][[1]][3,,]))
}


g1_coef<-list()
for(i in 1:4){
  for(j in 1:21){
    x1<-list()
    x2<-numeric()
    for(k in 1:100){
      x2<-append(x2,g1[k][[1]][i,j])
    }
    x1<-append(x1,list(x2))
    g1_coef<-append(g1_coef,x1)
  }
}

g2_coef<-list()
for(i in 1:4){
  for(j in 1:21){
    x1<-list()
    x2<-numeric()
    for(k in 1:100){
      x2<-append(x2,g2[k][[1]][i,j])
    }
    x1<-append(x1,list(x2))
    g2_coef<-append(g2_coef,x1)
  }
}

g3_coef<-list()
for(i in 1:4){
  for(j in 1:21){
    x1<-list()
    x2<-numeric()
    for(k in 1:100){
      x2<-append(x2,g3[k][[1]][i,j])
    }
    x1<-append(x1,list(x2))
    g3_coef<-append(g3_coef,x1)
  }
}

#num_nonzero
#g1
num<-numeric()
for(i in 1:84){
  num<-append(num,sum(g1_coef[i][[1]]!=0))
}
num_g1<-matrix(num,4,21,byrow = T)
colnames(num_g1)<-colnames(cbind(X,M,C))

#g2
num<-numeric()
for(i in 1:84){
  num<-append(num,sum(g2_coef[i][[1]]!=0))
}
num_g2<-matrix(num,4,21,byrow = T)
colnames(num_g2)<-colnames(cbind(X,M,C))

#g3
num<-numeric()
for(i in 1:84){
  num<-append(num,sum(g3_coef[i][[1]]!=0))
}
num_g3<-matrix(num,4,21,byrow = T)
colnames(num_g3)<-colnames(cbind(X,M,C))

#mean
#g1
SUM<-numeric()
for(i in 1:84){
  SUM<-append(SUM,sum(g1_coef[i][[1]]))
  m<-SUM/num
}
m[is.na(m)]=0
m_g1<-matrix(m,4,21,byrow = T)
colnames(m_g1)<-colnames(cbind(X,M,C))

#g2
SUM<-numeric()
for(i in 1:84){
  SUM<-append(SUM,sum(g2_coef[i][[1]]))
  m<-SUM/num
}
m[is.na(m)]=0
m_g2<-matrix(m,4,21,byrow = T)
colnames(m_g2)<-colnames(cbind(X,M,C))

#g3
SUM<-numeric()
for(i in 1:84){
  SUM<-append(SUM,sum(g3_coef[i][[1]]))
  m<-SUM/num
}
m[is.na(m)]=0
m_g3<-matrix(m,4,21,byrow = T)
colnames(m_g3)<-colnames(cbind(X,M,C))

#se_g1
se<-numeric()
for(i in 1:84){
  index_nonzero<-which(g1_coef[i][[1]]!=0)
  g1_nonzero<-g1_coef[i][[1]][index_nonzero]
  se<-append(se,sd(g1_nonzero))
}
se_g1<-matrix(se,4,21,byrow = T)
colnames(se_g1)<-colnames(cbind(X,M,C))

#g2_se
se<-numeric()
for(i in 1:84){
  index_nonzero<-which(g2_coef[i][[1]]!=0)
  g2_nonzero<-g2_coef[i][[1]][index_nonzero]
  se<-append(se,sd(g2_nonzero))
}
se_g2<-matrix(se,4,21,byrow = T)
colnames(se_g2)<-colnames(cbind(X,M,C))

#g3_se
se<-numeric()
for(i in 1:84){
  index_nonzero<-which(g1_coef[i][[1]]!=0)
  g1_nonzero<-g1_coef[i][[1]][index_nonzero]
  se<-append(se,sd(g1_nonzero))
}
se_g3<-matrix(se,4,21,byrow = T)
colnames(se_g3)<-colnames(cbind(X,C))


#plot
index1<-which(clu_origin==1)
index2<-which(clu_origin==2)
index3<-which(clu_origin==3)

x<-c(0,1)
y1<-as.integer(table(X[index1]))
y2<-as.integer(table(X[index2]))
y3<-as.integer(table(X[index3]))
plot(x,y1,type = 'b',col='red',ylab ="edu",ylim = c(1000,2000))
points(x,y2,col='blue')
lines(x,y2,col='blue')
points(x,y3,col='green')
lines(x,y3,col='green')

legend(x = 'topleft', 
       legend = c(expression(1), expression(2),expression(3)),
       lty = 1,
       col = c('red', 'blue','green'))


x<-c(0.25,0.5,0.75)
y1<-as.integer(summary(y[index1])[c(2,3,5)])
y2<-as.integer(summary(y[index2])[c(2,3,5)])
y3<-as.integer(summary(y[index3])[c(2,3,5)])
plot(x,y1,type = 'b',col='red',xlab = "quantile",ylab ="depressive score",ylim =c(1,30))
points(x,y2,col='blue')
lines(x,y2,col='blue')
points(x,y3,col='green')
lines(x,y3,col='green')

legend(x = 'topleft', 
       legend = c(expression(1), expression(2),expression(3)),
       lty = 1,
       col = c('red', 'blue','green'))

x<-c(0.25,0.5,0.75)
y1<-as.integer(summary(income.per[index1,])[c(2,3,5)])
y2<-as.integer(summary(income.per[index2,])[c(2,3,5)])
y3<-as.integer(summary(income.per[index3,])[c(2,3,5)])
plot(x,y1,type = 'b',col='red',xlab = "quantile",ylab ="income.per",ylim=c(1400,23000))
points(x,y2,col='blue')
lines(x,y2,col='blue')
points(x,y3,col='green')
lines(x,y3,col='green')

legend(x = 'topleft', 
       legend = c(expression(1), expression(2),expression(3)),
       lty = 1,
       col = c('red', 'blue','green'))

x<-c(0,1,2)
y1<-as.integer(table(family.support[index1,]))
y2<-as.integer(table(family.support[index2,]))
y3<-as.integer(table(family.support[index3,]))
plot(x,y1,type = 'b',col='red',ylab ="family.support",ylim = c(500,2500))
points(x,y2,col='blue')
lines(x,y2,col='blue')
points(x,y3,col='green')
lines(x,y3,col='green')

legend(x = 'topleft', 
       legend = c(expression(1), expression(2),expression(3)),
       lty = 1,
       col = c('red', 'blue','green'))

x<-0:7
y1<-as.integer(table(commu.support[index1,]))
y2<-as.integer(table(commu.support[index2,]))
y3<-as.integer(table(commu.support[index3,]))
plot(x,y1,type = 'b',col='red',ylab ="community.support",ylim = c(50,1500))
points(x,y2,col='blue')
lines(x,y2,col='blue')
points(x,y3,col='green')
lines(x,y3,col='green')

legend(x = 'topright', 
       legend = c(expression(1), expression(2),expression(3)),
       lty = 1,
       col = c('red', 'blue','green'))

x<-c(0,1,2)
y1<-as.integer(table(public.support[index1,]))
y2<-as.integer(table(public.support[index2,]))
y3<-as.integer(table(public.support[index3,]))
plot(x,y1,type = 'b',col='red',ylab ="public.support",ylim = c(100,3000))
points(x,y2,col='blue')
lines(x,y2,col='blue')
points(x,y3,col='green')
lines(x,y3,col='green')

legend(x = 'topleft', 
       legend = c(expression(1), expression(2),expression(3)),
       lty = 1,
       col = c('red', 'blue','green'))

#confounder
x<-c(0,1)
y1<-as.integer(table(smoke[index1,]))
y2<-as.integer(table(smoke[index2,]))
y3<-as.integer(table(smoke[index3,]))
plot(x,y1,type = 'b',col='red',ylab ="smoke",ylim = c(800,2000))
points(x,y2,col='blue')
lines(x,y2,col='blue')
points(x,y3,col='green')
lines(x,y3,col='green')

legend(x = 'topright', 
       legend = c(expression(1), expression(2),expression(3)),
       lty = 1,
       col = c('red', 'blue','green'))


x<-c(0,1)
y1<-as.integer(table(alc[index1,]))
y2<-as.integer(table(alc[index2,]))
y3<-as.integer(table(alc[index3,]))
plot(x,y1,type = 'b',col='red',ylab ="alc",ylim = c(500,2500))
points(x,y2,col='blue')
lines(x,y2,col='blue')
points(x,y3,col='green')
lines(x,y3,col='green')

legend(x = 'topright', 
       legend = c(expression(1), expression(2),expression(3)),
       lty = 1,
       col = c('red', 'blue','green'))


x<-c(0,1)
y1<-as.integer(table(social[index1,]))
y2<-as.integer(table(social[index2,]))
y3<-as.integer(table(social[index3,]))
plot(x,y1,type = 'b',col='red',ylab ="social",ylim = c(1000,2000))
points(x,y2,col='blue')
lines(x,y2,col='blue')
points(x,y3,col='green')
lines(x,y3,col='green')

legend(x = 'topright', 
       legend = c(expression(1), expression(2),expression(3)),
       lty = 1,
       col = c('red', 'blue','green'))

x<-c(0,1)
y1<-as.integer(table(hyp[index1,]))
y2<-as.integer(table(hyp[index2,]))
y3<-as.integer(table(hyp[index3,]))
plot(x,y1,type = 'b',col='red',ylab ="hyp",ylim = c(500,2500))
points(x,y2,col='blue')
lines(x,y2,col='blue')
points(x,y3,col='green')
lines(x,y3,col='green')

legend(x = 'topright', 
       legend = c(expression(1), expression(2),expression(3)),
       lty = 1,
       col = c('red', 'blue','green'))

x<-c(0,1)
y1<-as.integer(table(dysl[index1,]))
y2<-as.integer(table(dysl[index2,]))
y3<-as.integer(table(dysl[index3,]))
plot(x,y1,type = 'b',col='red',ylab ="dysl",ylim = c(200,3000))
points(x,y2,col='blue')
lines(x,y2,col='blue')
points(x,y3,col='green')
lines(x,y3,col='green')

legend(x = 'topright', 
       legend = c(expression(1), expression(2),expression(3)),
       lty = 1,
       col = c('red', 'blue','green'))

x<-c(0,1)
y1<-as.integer(table(diab[index1,]))
y2<-as.integer(table(diab[index2,]))
y3<-as.integer(table(diab[index3,]))
plot(x,y1,type = 'b',col='red',ylab ="diab",ylim = c(200,3100))
points(x,y2,col='blue')
lines(x,y2,col='blue')
points(x,y3,col='green')
lines(x,y3,col='green')

legend(x = 'topright', 
       legend = c(expression(1), expression(2),expression(3)),
       lty = 1,
       col = c('red', 'blue','green'))

x<-c(0,1)
y1<-as.integer(table(lung[index1,]))
y2<-as.integer(table(lung[index2,]))
y3<-as.integer(table(lung[index3,]))
plot(x,y1,type = 'b',col='red',ylab ="lung",ylim = c(200,3100))
points(x,y2,col='blue')
lines(x,y2,col='blue')
points(x,y3,col='green')
lines(x,y3,col='green')

legend(x = 'topright', 
       legend = c(expression(1), expression(2),expression(3)),
       lty = 1,
       col = c('red', 'blue','green'))

x<-c(0,1)
y1<-as.integer(table(heart[index1,]))
y2<-as.integer(table(heart[index2,]))
y3<-as.integer(table(heart[index3,]))
plot(x,y1,type = 'b',col='red',ylab ="heart",ylim = c(200,3100))
points(x,y2,col='blue')
lines(x,y2,col='blue')
points(x,y3,col='green')
lines(x,y3,col='green')

legend(x = 'topright', 
       legend = c(expression(1), expression(2),expression(3)),
       lty = 1,
       col = c('red', 'blue','green'))

x<-c(0,1)
y1<-as.integer(table(kidn[index1,]))
y2<-as.integer(table(kidn[index2,]))
y3<-as.integer(table(kidn[index3,]))
plot(x,y1,type = 'b',col='red',ylab ="kidn",ylim = c(200,3100))
points(x,y2,col='blue')
lines(x,y2,col='blue')
points(x,y3,col='green')
lines(x,y3,col='green')

legend(x = 'topright', 
       legend = c(expression(1), expression(2),expression(3)),
       lty = 1,
       col = c('red', 'blue','green'))

x<-c(0,1)
y1<-as.integer(table(digest[index1,]))
y2<-as.integer(table(digest[index2,]))
y3<-as.integer(table(digest[index3,]))
plot(x,y1,type = 'b',col='red',ylab ="digest",ylim = c(200,3100))
points(x,y2,col='blue')
lines(x,y2,col='blue')
points(x,y3,col='green')
lines(x,y3,col='green')

legend(x = 'topright', 
       legend = c(expression(1), expression(2),expression(3)),
       lty = 1,
       col = c('red', 'blue','green'))

x<-c(0,1)
y1<-as.integer(table(arth[index1,]))
y2<-as.integer(table(arth[index2,]))
y3<-as.integer(table(arth[index3,]))
plot(x,y1,type = 'b',col='red',ylab ="arth",ylim = c(200,3100))
points(x,y2,col='blue')
lines(x,y2,col='blue')
points(x,y3,col='green')
lines(x,y3,col='green')

legend(x = 'topright', 
       legend = c(expression(1), expression(2),expression(3)),
       lty = 1,
       col = c('red', 'blue','green'))

x<-c(0,1)
y1<-as.integer(table(phys.dis[index1,]))
y2<-as.integer(table(phys.dis[index2,]))
y3<-as.integer(table(phys.dis[index3,]))
plot(x,y1,type = 'b',col='red',ylab ="phys.dis",ylim = c(200,3100))
points(x,y2,col='blue')
lines(x,y2,col='blue')
points(x,y3,col='green')
lines(x,y3,col='green')

legend(x = 'topright', 
       legend = c(expression(1), expression(2),expression(3)),
       lty = 1,
       col = c('red', 'blue','green'))

x<-c(0,1)
y1<-as.integer(table(pain[index1,]))
y2<-as.integer(table(pain[index2,]))
y3<-as.integer(table(pain[index3,]))
plot(x,y1,type = 'b',col='red',ylab ="pain",ylim = c(200,3100))
points(x,y2,col='blue')
lines(x,y2,col='blue')
points(x,y3,col='green')
lines(x,y3,col='green')

legend(x = 'topright', 
       legend = c(expression(1), expression(2),expression(3)),
       lty = 1,
       col = c('red', 'blue','green'))

x<-c(0,1)
y1<-as.integer(table(adl.limit[index1,]))
y2<-as.integer(table(adl.limit[index2,]))
y3<-as.integer(table(adl.limit[index3,]))
plot(x,y1,type = 'b',col='red',ylab ="adl.limit",ylim = c(100,3100))
points(x,y2,col='blue')
lines(x,y2,col='blue')
points(x,y3,col='green')
lines(x,y3,col='green')

legend(x = 'topright', 
       legend = c(expression(1), expression(2),expression(3)),
       lty = 1,
       col = c('red', 'blue','green'))

x<-c(0,1)
y1<-as.integer(table(area[index1,]))
y2<-as.integer(table(area[index2,]))
y3<-as.integer(table(area[index3,]))
plot(x,y1,type = 'b',col='red',ylab ="area",ylim = c(200,3100))
points(x,y2,col='blue')
lines(x,y2,col='blue')
points(x,y3,col='green')
lines(x,y3,col='green')

legend(x = 'topright', 
       legend = c(expression(1), expression(2),expression(3)),
       lty = 1,
       col = c('red', 'blue','green'))

x<-c(0,1)
y1<-as.integer(table(sex[index1,]))
y2<-as.integer(table(sex[index2,]))
y3<-as.integer(table(sex[index3,]))
plot(x,y1,type = 'b',col='red',ylab ="sex",ylim = c(1000,2000))
points(x,y2,col='blue')
lines(x,y2,col='blue')
points(x,y3,col='green')
lines(x,y3,col='green')

legend(x = 'topright', 
       legend = c(expression(1), expression(2),expression(3)),
       lty = 1,
       col = c('red', 'blue','green'))

x<-c(0.25,0.5,0.75)
age<-newdata[,22]
y1<-as.integer(summary(age[index1])[c(2,3,5)])
y2<-as.integer(summary(age[index2])[c(2,3,5)])
y3<-as.integer(summary(age[index3])[c(2,3,5)])
plot(x,y1,type = 'b',col='red',xlab = "quantile",ylab ="age")
points(x,y2,col='blue')
lines(x,y2,col='blue')
points(x,y3,col='green')
lines(x,y3,col='green')

legend(x = 'topleft', 
       legend = c(expression(1), expression(2),expression(3)),
       lty = 1,
       col = c('red', 'blue','green'))

x<-c(0,1)
y1<-as.integer(table(mari[index1,]))
y2<-as.integer(table(mari[index2,]))
y3<-as.integer(table(mari[index3,]))
plot(x,y1,type = 'b',col='red',ylab ="mari",ylim = c(500,2500))
points(x,y2,col='blue')
lines(x,y2,col='blue')
points(x,y3,col='green')
lines(x,y3,col='green')

legend(x = 'topleft', 
       legend = c(expression(1), expression(2),expression(3)),
       lty = 1,
       col = c('red', 'blue','green'))

x<-c(0.25,0.5,0.75)
bmi<-newdata[,24]
y1<-as.integer(summary(bmi[index1])[c(2,3,5)])
y2<-as.integer(summary(bmi[index2])[c(2,3,5)])
y3<-as.integer(summary(bmi[index3])[c(2,3,5)])
plot(x,y1,type = 'b',col='red',xlab = "quantile",ylab ="bmi",ylim = c(10,30))
points(x,y2,col='blue')
lines(x,y2,col='blue')
points(x,y3,col='green')
lines(x,y3,col='green')

legend(x = 'topleft', 
       legend = c(expression(1), expression(2),expression(3)),
       lty = 1,
       col = c('red', 'blue','green'))

x<-c(0,1)
y1<-as.integer(table(retire[index1,]))
y2<-as.integer(table(retire[index2,]))
y3<-as.integer(table(retire[index3,]))
plot(x,y1,type = 'b',col='red',ylab ="retire",ylim = c(300,3000))
points(x,y2,col='blue')
lines(x,y2,col='blue')
points(x,y3,col='green')
lines(x,y3,col='green')

legend(x = 'topright', 
       legend = c(expression(1), expression(2),expression(3)),
       lty = 1,
       col = c('red', 'blue','green'))

