data<-data.base.complete

colnames(data)

library(glmnet)
library(igraph)

set.seed(123456)

edu<-as.data.frame(rep(0,nrow(data)))
edu[which(data["edu"]==TRUE),1]=1
edu[which(data["edu"]==FALSE),1]=0

X<-edu
colnames(X)<-"edu"

M<-cbind(data[,27],data[,24:26],data[,29:37])

live.with.all.child<-as.data.frame(rep(0,nrow(data)))
live.with.all.child[which(M["live.with.all.child"]==1),1]=1
live.with.all.child[which(M["live.with.all.child"]==0),1]=0

weekly.contact.child<-as.data.frame(rep(0,nrow(data)))
weekly.contact.child[which(M["weekly.contact.child"]==1),1]=1
weekly.contact.child[which(M["weekly.contact.child"]==0),1]=0

trans.from.child<-as.data.frame(rep(0,nrow(data)))
trans.from.child[which(M["trans.from.child"]==TRUE),1]=1
trans.from.child[which(M["trans.from.child"]==FALSE),1]=0

pension<-as.data.frame(rep(0,nrow(data)))
pension[which(M["pension"]==TRUE),1]=1
pension[which(M["pension"]==FALSE),1]=0

med.insur<-as.data.frame(rep(0,nrow(data)))
med.insur[which(M["med.insur"]==1),1]=1
med.insur[which(M["med.insur"]==0),1]=0

commu.dance<-as.data.frame(rep(0,nrow(data)))
commu.dance[which(M["commu.dance"]==TRUE),1]=1
commu.dance[which(M["commu.dance"]==FALSE),1]=0

commu.card.chess<-as.data.frame(rep(0,nrow(data)))
commu.card.chess[which(M["commu.card.chess"]==TRUE),1]=1
commu.card.chess[which(M["commu.card.chess"]==FALSE),1]=0

commu.out.exer<-as.data.frame(rep(0,nrow(data)))
commu.out.exer[which(M["commu.out.exer"]==TRUE),1]=1
commu.out.exer[which(M["commu.out.exer"]==FALSE),1]=0

commu.care.center<-as.data.frame(rep(0,nrow(data)))
commu.care.center[which(M["commu.care.center"]==TRUE),1]=1
commu.care.center[which(M["commu.care.center"]==FALSE),1]=0

commu.assoc<-as.data.frame(rep(0,nrow(data)))
commu.assoc[which(M["commu.assoc"]==TRUE),1]=1
commu.assoc[which(M["commu.assoc"]==FALSE),1]=0

commu.act.center<-as.data.frame(rep(0,nrow(data)))
commu.act.center[which(M["commu.act.center"]==TRUE),1]=1
commu.act.center[which(M["commu.act.center"]==FALSE),1]=0

commu.org.help<-as.data.frame(rep(0,nrow(data)))
commu.org.help[which(M["commu.org.help"]==TRUE),1]=1
commu.org.help[which(M["commu.org.help"]==FALSE),1]=0

income.per<-data["income.per"]
family.support<-live.with.all.child+weekly.contact.child+trans.from.child
commu.support<-commu.dance+commu.card.chess+commu.out.exer+commu.care.center+commu.assoc+commu.act.center+commu.org.help
public.support<-pension+med.insur

M<-cbind(income.per,family.support,commu.support,public.support)
colnames(M)<-c("income.per","family.support","commu.support","public.support")


C<-cbind(data[,4:20],data[,22:23],data["retire"])
smoke<-as.data.frame(rep(0,nrow(data)))
smoke[which(C["smoke"]==TRUE),1]=1
smoke[which(C["smoke"]==FALSE),1]=0

alc<-as.data.frame(rep(0,nrow(data)))
alc[which(C["alc"]==TRUE),1]=1
alc[which(C["alc"]==FALSE),1]=0

hyp<-as.data.frame(rep(0,nrow(data)))
hyp[which(C["hyp"]==TRUE),1]=1
hyp[which(C["hyp"]==FALSE),1]=0

dysl<-as.data.frame(rep(0,nrow(data)))
dysl[which(C["dysl"]==TRUE),1]=1
dysl[which(C["dysl"]==FALSE),1]=0

diab<-as.data.frame(rep(0,nrow(data)))
diab[which(C["diab"]==TRUE),1]=1
diab[which(C["diab"]==FALSE),1]=0

lung<-as.data.frame(rep(0,nrow(data)))
lung[which(C["lung"]==TRUE),1]=1
lung[which(C["lung"]==FALSE),1]=0

heart<-as.data.frame(rep(0,nrow(data)))
heart[which(C["heart"]==TRUE),1]=1
heart[which(C["heart"]==FALSE),1]=0

kidn<-as.data.frame(rep(0,nrow(data)))
kidn[which(C["kidn"]==TRUE),1]=1
kidn[which(C["kidn"]==FALSE),1]=0

digest<-as.data.frame(rep(0,nrow(data)))
digest[which(C["digest"]==TRUE),1]=1
digest[which(C["digest"]==FALSE),1]=0

arth<-as.data.frame(rep(0,nrow(data)))
arth[which(C["arth"]==TRUE),1]=1
arth[which(C["arth"]==FALSE),1]=0

pain<-as.data.frame(rep(0,nrow(data)))
pain[which(C["pain"]==TRUE),1]=1
pain[which(C["pain"]==FALSE),1]=0

area<-as.data.frame(rep(0,nrow(data)))
area[which(C["area"]==TRUE),1]=1
area[which(C["area"]==FALSE),1]=0

sex<-as.data.frame(rep(0,nrow(data)))
sex[which(C["sex"]==TRUE),1]=1
sex[which(C["sex"]==FALSE),1]=0

mari<-as.data.frame(rep(0,nrow(data)))
mari[which(C["mari"]==TRUE),1]=1
mari[which(C["mari"]==FALSE),1]=0

retire<-as.data.frame(rep(0,nrow(data)))
retire[which(C["retire"]==TRUE),1]=1
retire[which(C["retire"]==FALSE),1]=0

social<-as.data.frame(rep(0,nrow(data)))
social[which(C["social"]==1),1]=1
social[which(C["social"]==0),1]=0

phys.dis<-as.data.frame(rep(0,nrow(data)))
phys.dis[which(C["phys.dis"]==1),1]=1
phys.dis[which(C["phys.dis"]==0),1]=0

adl.limit<-as.data.frame(rep(0,nrow(data)))
adl.limit[which(C["adl.limit"]==1),1]=1
adl.limit[which(C["adl.limit"]==0),1]=0

C<-cbind(smoke,alc,social,hyp,dysl,diab,lung,heart,kidn,digest,arth,phys.dis,
         pain,adl.limit,area,sex,data["age"],mari,data["bmi"],retire)
colnames(C)<-colnames(cbind(data[,4:20],data[,22:23],data["retire"]))

y<-data["cesd"]
newdata<-cbind(X,M,C,y)

B<-5
n<-8161
set.seed(123456)

for(bb in 1:B){
  ind<-sample(1:n,size = n,replace = T)
  sample_data<-newdata[ind,]
  X<-sample_data[,1]
  M<-sample_data[,2:5]
  C<-sample_data[,6:25]
  y<-sample_data[,26]
  
  M<-as.numeric(unlist(M))
  M<-matrix(M,length(X),4,byrow = F)
  
  C<-as.numeric(unlist(C))
  C<-matrix(C,length(X),20,byrow = F)
  
  k<-3
  n<-length(X)
  lambda1=NULL 
  lambda2=0.05
  lambda3=NULL 
  rho=1e-4 
  eps1 = 1e-3 #3
  eps2 = 1e-2 #2
  eps3=1e-2 #2
  eps4 = 1e-3 #3
  max.iter = 300 #1000
  #lambda.min=ifelse(n>p,.001,.05) 
  lambda1.max=NULL
  lambda1.max.ratio=0.2 
  nlam=100
  beta0_m=NULL 
  beta0_y=NULL 
  cluster0=NULL 
  weight_m=diag(1, dim(M)[2], dim(M)[2]) 
  weight_y=1
  c0=0.5 
  t0=1 
  standardy=TRUE 
  a=3.7
  
  dims=dim(M)
  n=dims[1]              #sample size
  pT=1                   #### Number of treatment is one
  pM=dims[2]             #### Number of potential mediators
  if (!is.null(C)) {
    pC=dim(C)[2]           #### Number of confounders
  } else {
    pC=0
  }
  p=pT+pM+pC             #### Number of all variables except y
  lambda.min=ifelse(n>p,.001,.05) 
  
  n_pa = ((pT)*k+pC)*pM + ((pT+pM)*k+pC)   #### Number of total parameters
  location_m = matrix(0,k, (pT+pC)*pM)         #### Location of parameters for mediator models in the largest (final) pararmeter vector
  location_y = matrix(0,k, (pT+pM+pC))         #### Location of parameters for the outcome model in the largest pararmeter vector
  temp1 = k*pM*(pT)         ##### Number of all treatment coefficients in mediator models
  temp2 = temp1 + pM*pC         ##### Number of all the coefficients in mediator models
  temp3 = temp2 + k*(pT+pM)         ##### Number of all the coefficients except for coefficients of confounders in outcome models
  if (pC>0) {
    for (i in 1:k) {
      before=(i-1)*((pT)*pM)
      for (j in 1:pM) {
        location_m[i,((j-1)*(pT+pC)+1):((j-1)*(pT+pC)+pT)]=(before+(j-1)*(pT)+1):(before+(j)*(pT))
        location_m[i,((j-1)*(pT+pC)+pT+1):((j-1)*(pT+pC)+pT+pC)]=(temp1+(j-1)*pC+1):(temp1+(j)*pC)
      }
      location_y[i,1:(pT+pM)]=(temp2+(i-1)*(pT+pM)+1):(temp2+i*(pT+pM))
      location_y[i,(pT+pM+1):(pT+pM+pC)]=(temp3+1):(temp3+pC)
    }
  } else {
    for (i in 1:k) {
      before=(i-1)*((pT)*pM)
      for (j in 1:pM) {
        location_m[i,((j-1)*(pT+pC)+1):((j-1)*(pT+pC)+pT)]=(before+(j-1)*(pT)+1):(before+(j)*(pT))
        #location_m[i,((j-1)*(1+pT+pC)+1+pT+1):((j-1)*(1+pT+pC)+1+pT+pC)]=(temp1+(j-1)*pC+1):(temp1+(j)*pC)
      }
      location_y[i,1:(pT+pM)]=(temp2+(i-1)*(pT+pM)+1):(temp2+i*(pT+pM))
      #location_y[i,(1+pT+pM+1):(1+pT+pM+pC)]=(temp3+1):(temp3+pC)
    }
  }
  
  index_m=array(0, c(k, pM, pT+pC))     ## (Subgroup number, Mediator number, Predictor (Treatment+Confouncder) number)
  index_y=matrix(0, k, pT+pM+pC)        ## (Subgroup number, Predictor (Treatment+Mediator+Confouncder) number)
  
  for (i in 1:k) {
    before=(i-1)*((pT)*pM)
    index_m[i,,1:(pT)]=t(matrix((before+1):(before+(pT)*pM), (pT), pM))
    if (pC>0) {
      index_m[i,,(pT+1):(pT+pC)]=t(matrix((temp1+1):(temp1+pC*pM), pC, pM))
    }
    index_y[i, 1:(pT+pM)]=(temp2+(i-1)*(pT+pM)+1):(temp2+i*(pT+pM))
    if (pC>0){
      index_y[i, (pT+pM+1):(pT+pM+pC)]=(temp3+1):(temp3+pC)
    }
  }
  
  W=kronecker(weight_m, diag(1,(pT+pC)))
  
  ########## Standardize ###########
  centerx=0
  scalex=1
  #XX=(X-centerx)/scalex
  XX=as.data.frame(X)
  XX<-as.numeric(XX[,1])
  
  
  centerm=apply(M,2,mean)
  scalem=apply(M,2,sd)
  MM=M
  for (i in 1:pM) {
    MM[,i]=(M[,i]-centerm[i])/scalem[i]
  }
  
  #if (pC>1) {
  #  centerc=apply(C,2,mean)
  #  scalec=apply(C,2,sd)
  #  CC=C
  #  for (i in 1:pC) {
  #    CC[,i]=(C[,i]-centerc[i])/scalec[i]
  #  }
  #} else if (pC==1) {
  #  centerc=mean(C)
  #  scalec=sd(C)
  #  CC=(C-centerc)/scalec
  #}
  if (pC>1) {
    #centerc=apply(C,2,mean)
    #scalec=apply(C,2,sd)
    CC=C
    for (i in c(17,19)) {
      CC[,i]=(C[,i]-mean(C[,i]))/sd(C[,i])
    }
  } else if (pC==1) {
    centerc=mean(C)
    scalec=sd(C)
    CC=(C-centerc)/scalec
  }
  centerc=matrix(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,mean(C[,17]),0,mean(C[,19]),0),1,20)
  scalec=matrix(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,sd(C[,17]),1,sd(C[,19]),1),1,20)
  
  centery=mean(y)
  if (standardy) {
    scaley=sd(y)
  } else {
    scaley=1
  }
  yy=(y-centery)/scaley
  
  
  ###### Initial values
  if (is.null(cluster0)) {
    cluster0=kmeans(cbind(XX,MM,yy), k)$cluster        #### Initial cluster labels based on k-means
  }
  
  if (pC>0) {
    data_m=cbind(XX, CC)               ### All predictor varaibles in mediator model
    data_y=cbind(XX, MM, CC)           ### All predictor varaibles in outcome model
  } else {
    data_m=cbind(XX)               ### All predictor varaibles in mediator model
    data_y=cbind(XX, MM)           ### All predictor varaibles in outcome model
  }
  
  if (is.null(beta0_m) & is.null(beta0_y)) {
    ind1<-sample(1:length(X),600,replace = F)
    initial_data=cbind(XX[ind1],MM[ind1,],yy[ind1])
    initial_data<-as.matrix(initial_data)
    #initial_data<-as.matrix(initial_data, drop = TRUE)
    inter=initial_data%*%t(initial_data)
    inter2=cov2cor(inter)         #correlation covariance matrix
    diag(inter2)=0
    cor_thre=0.5
    for (cc in 1:11) {
      inter3=(abs(inter2)>cor_thre)  
      g1 <- graph_from_adjacency_matrix(inter3, mode = "undirected" ) 
      subgroup2=largest_cliques(g1)
      if (length(subgroup2[[1]])<=max(10,(pT+pC))) {
        cor_thre=cor_thre-0.05
      } else {
        break
      }
    }
    
    
    
    beta0_m=array(0, c(k, pM, pT+pC))     ## (Subgroup number, Mediator number, Predictor (Treatment+Confouncder) number)
    beta0_y=matrix(0, k, pT+pM+pC)        ## (Subgroup number, Predictor (Treatment+Mediator+Confouncder) number)
    
    res0_m=array(0, c(k, n, pM))
    res0_y=matrix(0, k, n)
    ploss0=matrix(0, k, n)
    loss0_m=matrix(0, k, n)
    loss0_y=matrix(0, k, n)
    n_index=round(n/k)
    
    for (i in 1:k) {
      if (i==1) {
        #index=as.numeric(subgroup2[[1]])
        #index=as.numeric(cluster0)
        index=ind1[as.numeric(subgroup2[[1]])]
      }
      if (k==1) {
        index=1:n
      }
      # index=which(cluster0==i)         #### Index for samples in the h-th group
      if (length(index)>(1+pT+pC)) {
        if (pC>0) {
          beta0_m[i,,]=t(lm(as.matrix(MM[index,])~XX[index]+CC[index,] -1)$coef)
        } else {
          beta0_m[i,,]=t(lm(as.matrix(MM[index,])~XX[index] -1)$coef)
        }
      } else {
        # cvfit <- cv.glmnet(cbind(XX[index], CC[index,]), MM[index,], family="gaussian")
        # fit <- cvfit$glmnet.fit
        # beta0=coef(fit, s=cvfit$lambda.min)
        for (j in 1:pM) {
          #beta0_m[i,j,]=t(as.numeric(beta0[j]))     #### Cannot convert 'list' to 'numeric'
          if (pC>0) {
            cvfit <- cv.glmnet(as.matrix(cbind(XX[index], CC[index,])), MM[index,j], nfolds=3, family="gaussian", intercept=FALSE)
          } else {
            cvfit <- cv.glmnet(as.matrix(XX[index]), MM[index,j], nfolds=3, family="gaussian", intercept=FALSE)
          } #cv=cross validation
          
          fit <- cvfit$glmnet.fit
          beta0_m[i,j,]=t(as.numeric(coef(fit, s=cvfit$lambda.min))[-1])
        }
      }
      if (length(index)>(1+pT+pM+pC)) {
        if (pC>0) {
          beta0_y[i,]=lm(yy[index]~XX[index]+MM[index,]+CC[index,] -1)$coef
        } else {
          beta0_y[i,]=lm(yy[index]~XX[index]+MM[index,] -1)$coef
        }
      } else {
        if (pC>0) {
          cvfit <- cv.glmnet(as.matrix(cbind(XX[index], MM[index,], CC[index,])), yy[index], nfolds=3, family="gaussian", intercept=FALSE)
        } else {
          cvfit <- cv.glmnet(as.matrix(cbind(XX[index], MM[index,])), yy[index], nfolds=3, family="gaussian", intercept=FALSE)
        }
        fit <- cvfit$glmnet.fit
        beta0_y[i,]=t(as.numeric(coef(fit, s=cvfit$lambda.min))[-1])
      }
      res0_m[i,,]=MM-data_m%*%t(beta0_m[i,,])
      res0_y[i,]=yy-data_y%*%beta0_y[i,]
      # loss0_m=diag(res0_m%*%weight_m%*%t(res0_m))
      # loss0_y=weight_y*(res0_y)^2
      ploss0[i,]=diag(res0_m[i,,]%*%weight_m%*%t(res0_m[i,,]))+weight_y*(res0_y[i,])^2#+pw[h]=diag(res0_m%*%weight_m%*%t(res0_m))+weight_y*(res0_y)^2#+pw[h]
      if (i==1) {
        index=order(ploss0[1,], decreasing = TRUE)[1:n_index]
      } else {
        index=order(apply(ploss0[1:i,],2,sum), decreasing = TRUE)[1:n_index]
      }
    }
  }
  #n_pen=n_pa-k*(pM+1)                ### Number of parameters which are penalized
  #index_pen=unique(c(as.vector(t(index_m[,,-1])), as.vector(t(index_y[,-1]))))  ### Index of parameters which are penalized
  #index_row=NULL                     ### Index of rows in D which will be used to solve beta from the constrain equation
  n_bep=pT+pM+(pT)*pM                  ### Number of parameters in between-group penalty
  n_D=n_bep*k*(k-1)/2
  #n_pen+n_bep*k*(k-1)/2          ### Number of total rows in D
  D=matrix(0, n_D, n_pa)
  
  #lambda2=0.5    #### tuning parameter before fused lasso is lambda2*lambda1
  count_D=0
  count_k=0
  between_ratio=1 #weight_y
  if (k>1) {
    for (i in 1:(k-1)) {
      #D[(pM*(k*pT+pC)+count_k*(pT+pM)+pT+1):(pM*(k*pT+pC)+(count_k+1)*(pT+pM)), index_y[i,(1+pT+1):(1+pT+pM)]]=weight_y*diag(1, pM, pM) #diag(1/abs(beta0_m[i,,2]), pM, pM)  # Adjust penalty of mediator coefficints in the outcome model
      #D[(pM*(k*pT+pC)+count_k*(pT+pM)+pT+1):(pM*(k*pT+pC)+(count_k+1)*(pT+pM)), index_y[i,(1+pT+1):(1+pT+pM)]]=diag(min(abs(beta0_m[,,2]))/abs(beta0_m[i,,2]), pM, pM)  # Adjust penalty of mediator coefficints in the outcome model
      #D[(pM*(k*pT+pC)+count_k*(pT+pM)+pT+1):(pM*(k*pT+pC)+(count_k+1)*(pT+pM)), index_y[i,(1+pT+1):(1+pT+pM)]]=diag(exp(-abs(beta0_m[i,,2])), pM, pM)  # Adjust penalty of mediator coefficints in the outcome model
      for (j in (i+1):k) {
        # D[(count_D*n_bep+1):(count_D*n_bep+n_bep), c(index_m[i,,2], index_y[i,2:(1+pT+pM)])]=lambda2[1]*diag(c(rep(1, pM), rep(weight_y, (pT+pM))), n_bep, n_bep)
        # D[(count_D*n_bep+1):(count_D*n_bep+n_bep), c(index_m[j,,2], index_y[j,2:(1+pT+pM)])]=lambda2[1]*diag(c(rep(-1, pM), rep(-weight_y, (pT+pM))), n_bep, n_bep)
        D[(count_D*n_bep+1):(count_D*n_bep+n_bep), c(as.vector(t(index_m[i,,1:(pT)])), index_y[i,1:(pT+pM)])]=diag(c(rep(1, (pT)*pM), rep(between_ratio, (pT+pM))), n_bep, n_bep)
        D[(count_D*n_bep+1):(count_D*n_bep+n_bep), c(as.vector(t(index_m[j,,1:(pT)])), index_y[j,1:(pT+pM)])]=diag(c(rep(-1, (pT)*pM), rep(-between_ratio, (pT+pM))), n_bep, n_bep)
        #index_row=c(index_row, (n_pen+count_D*n_bep+1):(n_pen+count_D*n_bep+n_bep))
        count_D=count_D+1
      }
      #count_k=count_k+1
    }
    #D[(pM*(k*pT+pC)+count_k*(pT+pM)+pT+1):(pM*(k*pT+pC)+(count_k+1)*(pT+pM)), index_y[k,(1+pT+1):(1+pT+pM)]]=weight_y*diag(1, pM, pM) #diag(1/abs(beta0_m[k,,2]), pM, pM)
    #D[(pM*(k*pT+pC)+count_k*(pT+pM)+pT+1):(pM*(k*pT+pC)+(count_k+1)*(pT+pM)), index_y[k,(1+pT+1):(1+pT+pM)]]=diag(min(abs(beta0_m[,,2]))/abs(beta0_m[k,,2]), pM, pM)
    #D[(pM*(k*pT+pC)+count_k*(pT+pM)+pT+1):(pM*(k*pT+pC)+(count_k+1)*(pT+pM)), index_y[k,(1+pT+1):(1+pT+pM)]]=diag(exp(-abs(beta0_m[k,,2])), pM, pM)
  } #else {
  #   D[(pM*(k*pT+pC)+count_k*(pT+pM)+pT+1):(pM*(k*pT+pC)+(count_k+1)*(pT+pM)), index_y[k,(1+pT+1):(1+pT+pM)]]=weight_y*diag(1, pM, pM) #diag(1/abs(beta0_m[k,,2]), pM, pM)
  #   #D[(pM*(k*pT+pC)+count_k*(pT+pM)+pT+1):(pM*(k*pT+pC)+(count_k+1)*(pT+pM)), index_y[k,(1+pT+1):(1+pT+pM)]]=diag(min(abs(beta0_m[,,2]))/abs(beta0_m[k,,2]), pM, pM)
  #   #D[(pM*(k*pT+pC)+count_k*(pT+pM)+pT+1):(pM*(k*pT+pC)+(count_k+1)*(pT+pM)), index_y[k,(1+pT+1):(1+pT+pM)]]=diag(exp(-abs(beta0_m[k,,2])), pM, pM)
  # }
  
  
  #user=TRUE
  ##### Not sure how to define maximum of lambda
  if (is.null(lambda1)) {
    if (is.null(lambda1.max)) {
      temp1_lambda1=0#-Inf
      temp2_lambda1=0#-Inf
      for (h in 1:k) {
        index=1:n
        #which(cluster0==h) 
        temp1_lambda1=temp1_lambda1+abs(2*weight_m%*%t(MM[index, ,drop=FALSE])%*%data_m[index, ,drop=FALSE])
        #max(temp1_lambda1, abs(2*weight_m%*%t(MM[index, ,drop=FALSE])%*%data_m[index, ,drop=FALSE]))
        temp2_lambda1=temp2_lambda1+abs(2*weight_y%*%t(yy[index])%*%as.matrix(data_y[index, ,drop=FALSE]))
        #max(temp2_lambda1, abs(2*weight_y%*%t(yy[index])%*%as.matrix(data_y[index, ,drop=FALSE])))
      }
      #temp1_lambda1=max(abs(2*weight_m%*%t(MM)%*%data_m))
      #temp2_lambda1=max(abs(2*weight_y%*%t(yy)%*%data_y))
      # if (is.null(lambda1.max.ratio)) {
      #   lambda1.max=max(temp1_lambda1/n, temp2_lambda1/n)
      # } else {
      lambda1.max=lambda1.max.ratio*max(temp1_lambda1/n, temp2_lambda1/n)
      # }
    }
    #max(temp1_lambda1, temp2_lambda1)
    lambda1=exp(seq(log(lambda.min*lambda1.max),log(lambda1.max),len=nlam))
    #user=FALSE
  } 
  
  #lambda1<-quantile(lambda1,probs=c(0.45,0.55))
  #lambda1<-as.double(lambda1)
  lambda1<-0.0166923645
  
  
  L=length(lambda1)
  L2=length(lambda2)                      #### lambda2 should be from small to large
  beta_m=array(0, c(L2, L, k, pM, pT+pC))   #### Parameters for mediator model and k subgroups
  beta_y=array(0, c(L2, L, k, pT+pM+pC))    #### Parameters for outcome model
  beta_m_out=array(0, c(L2, L, k, pM, pT+pC))   #### Parameters for mediator model and k subgroups
  beta_y_out=array(0, c(L2, L, k, pT+pM+pC))    #### Parameters for outcome model
  intercept_m=array(0, c(L2, L, k, pM))
  intercept_y=array(0, c(L2, L, k))
  cluster=array(0, c(L2, L, n))                 #### Subgrouping labels
  ncluster=matrix(0, L2, L)
  res_m=array(0, c(k, n, pM))
  res_y=matrix(0, k, n)
  pw=rep(0, k)
  ploss=matrix(0, k, n)
  loss_m=matrix(0, k, n)
  loss_y=matrix(0, k, n)
  fdev_m=array(0, c(k, pM, pT+pC))    ### First derivative for mediator model
  fdev_y=matrix(0, k, pT+pM+pC)       ### First derivative for outcome model
  
  
  #l=1
  #A0=rho*t(D)%*%D
  A0=matrix(0, n_pa, n_pa)
  #A=A0 #matrix(0,n_pa, n_pa)    #### the coefficients of the linear system in ADMM
  b00=rep(0,n_pa)             ####the right-hand side(s) of the linear system in ADMM
  nu_pre=rep(0,n_pa)
  #beta0_m[h,,1:(1+pT)]
  for (h in 1:k) {
    A0[location_m[h,], location_m[h,]] = A0[location_m[h,], location_m[h,]] + 2*W%*%kronecker(diag(1,pM,pM), t(data_m)%*%data_m)          #### Dimension: (pM*(1+pT+pC)) x (pM*(1+pT+pC))
    b00[location_m[h,]] = b00[location_m[h,]] + as.vector(t(2*weight_m%*%t(MM)%*%data_m))
    nu_pre[as.vector(t(index_m[h,,]))]=as.vector(t(beta0_m[h,,]))
    
    A0[location_y[h,], location_y[h,]] = A0[location_y[h,], location_y[h,]] + 2*weight_y*t(data_y)%*%data_y
    b00[location_y[h,]] = b00[location_y[h,]] + 2*weight_y%*%t(yy)%*%data_y
    nu_pre[index_y[h,]]=beta0_y[h,]
  }  
  
  #nu_pre has NA
  #nu_pre[is.na(nu_pre)]=0
  
  
  # v_pre=rep(0,n_D) 
  # mu_pre=rep(0,n_D)               #### Lagrangian
  lrss=array(0, c(L2, L, 2))
  bic1=matrix(0, L2, L)
  objective=array(0, c(L2, L, 2))
  bic2=matrix(0, L2, L)
  objective2=array(0, c(L2, L, 2))
  gcv1=matrix(0, L2, L)
  gcv2=matrix(0, L2, L)
  # bic3=matrix(0, L2, L)
  # objective3=array(0, c(L2, L, 2))
  # vhat=v_pre                      #### Fast ADMM
  # muhat=mu_pre
  u0=0.99
  tau=0.999
  #t=1
  alpha=0.0001
  
  
  converge=TRUE
  for (l2 in 1:L2) {
    for (l in 1:L) {
      if (l2==1 & l==1) {
        # if (k>1) {
        beta_m_pre=beta0_m
        beta_y_pre=beta0_y
        cluster_pre=cluster0
        # } else {
        #   beta_m_pre=as.matrix(beta0_m[1,, ], pM, pT+pC)
        #   beta_y_pre=beta0_y[1, ]
        #   cluster_pre=cluster0
        # }
      } 
      for (i in 1:max.iter) {
        # if (k>1) {
        for (h in 1:k) {
          if (length(dim(beta_m_pre))<3) {
            print('Error')
          }
          res_m[h,,]=MM-data_m%*%t(beta_m_pre[h,,])
          res_y[h,]=yy-data_y%*%beta_y_pre[h,]
          loss_m[h,]=diag(res_m[h,,]%*%weight_m%*%t(res_m[h,,]))
          loss_y[h,]=weight_y*(res_y[h,])^2
          ploss[h,]=diag(res_m[h,,]%*%weight_m%*%t(res_m[h,,]))+weight_y*(res_y[h,])^2#+pw[h]
        }
        if (k>1) {
          cluster[l2, l,]=apply(ploss, 2, which.min)
        } else {
          cluster[l2, l,]=1
        }
        
        pd_m=matrix(0, pM, pT+pC)
        pd_y=rep(0, pT+pM+pC)
        b0=rep(0,n_pa)
        #b00             ####the right-hand side(s) of the linear system in ADMM
        # if (k>1) {
        for (h in 1:k) {
          index=which(cluster[l2, l,]==h)        #### Index for samples in the h-th group
          
          pd_m[,1:pT]=beta_m_pre[h,,1:pT]
          if (dim(as.matrix(res_m[h,-index,]))[1]==pM) {
            b0[location_m[h,]] = b0[location_m[h,]] + as.vector(t(2*weight_m%*%as.matrix(res_m[h,-index,])%*%(-data_m[-index,]) + n*lambda1[l]*4*c0*pd_m))
          } else {
            b0[location_m[h,]] = b0[location_m[h,]] + as.vector(t(2*weight_m%*%t(res_m[h,-index,])%*%(-data_m[-index, ,drop=FALSE]) + n*lambda1[l]*4*c0*pd_m))
          }
          
          pd_y[(pT+1):(pT+pM)]=n*lambda1[l]*4*c0*beta_y_pre[h,(pT+1):(pT+pM)]
          pd_y[1:pT]=n*2*beta_y_pre[h,1:pT]/(a-1)
          if (n-length(index)==1) {
            b0[location_y[h,]] = b0[location_y[h,]] + 2*weight_y*t(-data_y[-index,])*res_y[h,-index] + pd_y
          } else {
            b0[location_y[h,]] = b0[location_y[h,]] + t(2*weight_y*t(-data_y[-index, ,drop=FALSE])%*%t(res_y[h,-index, drop=FALSE])) + pd_y
          }
        }
        
        for (j in 1:max.iter) {
          b=-(b00+b0) + A0%*%nu_pre #+rho*t(D)%*%as.matrix(vhat)-t(D)%*%muhat             ####the right-hand side(s) of the linear system in ADMM
          eta_star=D%*%nu_pre/rho                                                  ### first derivative of between-group penalty (difference)
          eta_star[which(eta_star>1)]=1
          eta_star[which(eta_star< -1)]=-1
          b=b+n*lambda1[l]*lambda2[l2]*t(D)%*%eta_star      #λ0=λ1*λ2
          
          pec=rep(0,n_pa)                                                          ### first derivative of penalty for confounders
          if (pC>0 & lambda1[l]!=0) {
            for (ii in 1:pM) {
              for (jj in 1:pC) {
                index_temp=index_m[1,ii,pT+jj]
                #location_m[1,(ii-1)*(1+pT+pC)+1+pT+jj]
                u=abs(nu_pre[index_temp])
                if (u==0) {
                  if (b[index_temp]>n*lambda1[l]) {
                    pec[index_temp]=-n*lambda1[l]
                  } else if (b[index_temp]< -n*lambda1[l]) {
                    pec[index_temp]=n*lambda1[l]
                  } else {
                    pec[index_temp]=-b[index_temp]
                  }
                } else {
                  pec[index_temp]=n*lambda1[l]*sign(nu_pre[index_temp])
                }
              }
            }
            for (ii in 1:pC) {
              index_temp=index_y[1,pT+pM+ii]
              #location_y[1,1+pT+pM+ii]
              u=abs(nu_pre[index_temp])
              if (u<eps1) {
                if (b[index_temp]>n*lambda1[l]) {
                  pec[index_temp]=-n*lambda1[l]
                } else if (b[index_temp]< -n*lambda1[l]) {
                  pec[index_temp]=n*lambda1[l]
                } else {
                  pec[index_temp]=-b[index_temp]
                  nu_pre[index_temp]=0
                }
              } else {
                pec[index_temp]=n*lambda1[l]*sign(nu_pre[index_temp])
              }
            }
          }
          b=b+pec
          
          ped=rep(0, n_pa)                                                 ### first derivative of l2 penalty for mediation coefficients proposed in DC
          for (h in 1:k) {
            ped[index_m[h,,1:pT]]=n*lambda1[l]*4*c0*nu_pre[index_m[h,,1:pT]]
            ped[index_y[h,1:pT]]=n*2*nu_pre[index_y[h,1:pT]]/(a-1)
            ped[index_y[h,(pT+1):(pT+pM)]]=n*lambda1[l]*4*c0*nu_pre[index_y[h,(pT+1):(pT+pM)]]
          }
          b=b+ped
          
          lambda3=lambda1[l]
          #sqrt(lambda1[l])                  #### tuning parameter for treatment in outcome model
          pew=rep(0, n_pa)                                                 ### first derivative of within-group mediation penalty
          for (h in 1:k) {
            index_temp=index_y[h,pT]
            u=abs(nu_pre[index_temp])
            if (u<eps1) {
              if (b[index_temp]>n*lambda3) {
                pew[index_temp]= -n*lambda3
              } else if (b[index_temp]< -n*lambda3) {
                pew[index_temp]= n*lambda3
              } else {
                pew[index_temp]=-b[index_temp]
                nu_pre[index_temp]=0
              }
            } else if (u <= lambda3) {
              pew[index_temp]=n*lambda3*sign(nu_pre[index_temp])
            } else if (u <= a*lambda3) {
              pew[index_temp]=n*(a*lambda3-u)*sign(nu_pre[index_temp])/(a-1)
            }
            tempM1=1+c0*abs(nu_pre[index_m[h, ,pT]])
            tempM2=1+c0*abs(nu_pre[index_y[h,(pT+1):(pT+pM)]])
            index_tempM1=index_m[h, ,pT]
            index_tempM2=index_y[h,(pT+1):(pT+pM)]
            lambdaM1=n*lambda1[l]/(tempM1^2*tempM2)
            lambdaM2=n*lambda1[l]/(tempM1*tempM2^2)
            temp_result1=apply(cbind(lambda=lambdaM1, dev=b[index_tempM1], funvalue=nu_pre[index_tempM1]), 1, soft, c1=1, c2=2, c3=3, eps1=eps1)
            temp_result2=apply(cbind(lambda=lambdaM2, dev=b[index_tempM2], funvalue=nu_pre[index_tempM2]), 1, soft, c1=1, c2=2, c3=3, eps1=eps1)
            # print(index_tempM1[temp_result1[2,]==1])
            # print(index_tempM2[temp_result2[2,]==1])
            pew[index_tempM1]=temp_result1[1,]
            nu_pre[index_tempM1[temp_result1[2,]==1]]=0
            pew[index_tempM2]=temp_result2[1,]
            nu_pre[index_tempM2[temp_result2[2,]==1]]=0
          }
          b=b+pew
          
          temp_m=array(nu_pre[1:temp1], dim=c((pT), pM, k))
          for (h in 1:k) {
            beta_m[l2, l,h,,1:(pT)]=t(temp_m[,,h])
            if (pC>0) {
              beta_m[l2, l,h,,(pT+1):(pT+pC)]=t(matrix(nu_pre[(temp1+1):temp2], pC, pM))
              beta_y[l2, l,h,(pT+pM+1):(pT+pM+pC)]=nu_pre[(temp3+1):n_pa]
            }
          }
          beta_y[l2, l,,1:(pT+pM)]=t(matrix(nu_pre[(temp2+1):temp3], (pT+pM), k))
          for (h in 1:k) {
            res_m[h,,]=MM-data_m%*%t(beta_m[l2, l, h, , ])
            res_y[h,]=yy-data_y%*%(beta_y[l2, l, h, ])
            
            loss_m[h,]=diag(res_m[h,,]%*%weight_m%*%t(res_m[h,,]))
            loss_y[h,]=weight_y*(res_y[h,])^2
            # ploss[h,]=diag(res_m[h,,]%*%weight_m%*%t(res_m[h,,]))+weight_y*(res_y[h,])^2#+pw[h]
          }
          eta_star=D%*%nu_pre/rho                                                  ### first derivative of between-group penalty (difference)
          eta_star[which(eta_star>1)]=1
          eta_star[which(eta_star< -1)]=-1
          penalty_d=sum(beta_y[l2, l, ,(pT)]^2)/(a-1) + 2*c0*lambda1[l]*(sum(beta_m[l2, l, , ,(pT)]^2) + sum(beta_y[l2, l, ,(pT+1):(pT+pM)]^2))
          penalty_b=lambda1[l]*lambda2[l2]*(t(eta_star)%*%D%*%nu_pre - rho*t(eta_star)%*%eta_star/2)
          penalty_w=sum(as.data.frame(lapply(abs(beta_y[l2, l, ,pT]), SCADp, lambda=lambda3, a=a))) + lambda1[l]*(k*p/c0 - sum(1/(c0*(1+c0*abs(beta_m[l2, l, , ,(pT)]))*(1+c0*abs(beta_y[l2, l, ,(pT+1):(pT+pM)])))))
          penalty_c=0
          if (pC>0) {
            penalty_c=lambda1[l]*(sum(abs(beta_m[l2, l, , ,(pT+1):(pT+pC)])) + sum(abs(beta_y[l2, l, ,(pT+pM+1):(pT+pM+pC)])))
          }
          object_pre = sum(loss_m) + sum(loss_y) + n*(penalty_w + penalty_b + penalty_d + penalty_c) - t(nu_pre)%*%b0
          
          t=t0
          test_x=NULL
          test_y=NULL
          for (ii in 1:20) {
            t=0.5*t
            nu=nu_pre-t*b
            test_x=c(test_x, t)
            
            temp_m=array(nu[1:temp1], dim=c((pT), pM, k))
            for (h in 1:k) {
              beta_m[l2, l,h,,1:(pT)]=t(temp_m[,,h])
              if (pC>0) {
                beta_m[l2, l,h,,(pT+1):(pT+pC)]=t(matrix(nu[(temp1+1):temp2], pC, pM))
                beta_y[l2, l,h,(pT+pM+1):(pT+pM+pC)]=nu[(temp3+1):n_pa]
              }
            }
            beta_y[l2, l,,1:(pT+pM)]=t(matrix(nu[(temp2+1):temp3], (pT+pM), k))
            for (h in 1:k) {
              res_m[h,,]=MM-data_m%*%t(beta_m[l2, l, h, , ])
              res_y[h,]=yy-data_y%*%(beta_y[l2, l, h, ])
              
              loss_m[h,]=diag(res_m[h,,]%*%weight_m%*%t(res_m[h,,]))
              loss_y[h,]=weight_y*(res_y[h,])^2
              #ploss[h,]=diag(res_m[h,,]%*%weight_m%*%t(res_m[h,,]))+weight_y*(res_y[h,])^2#+pw[h]
            }
            eta_star=D%*%nu/rho                                                  ### first derivative of between-group penalty (difference)
            eta_star[which(eta_star>1)]=1
            eta_star[which(eta_star< -1)]=-1
            penalty_d=sum(beta_y[l2, l, ,(pT)]^2)/(a-1) + 2*c0*lambda1[l]*(sum(beta_m[l2, l, , ,(pT)]^2) + sum(beta_y[l2, l, ,(pT+1):(pT+pM)]^2))
            penalty_b=lambda1[l]*lambda2[l2]*(t(eta_star)%*%D%*%nu - rho*t(eta_star)%*%eta_star/2)
            penalty_w=sum(as.data.frame(lapply(abs(beta_y[l2, l, ,pT]), SCADp, lambda=lambda3, a=a))) + lambda1[l]*(k*p/c0 - sum(1/(c0*(1+c0*abs(beta_m[l2, l, , ,(pT)]))*(1+c0*abs(beta_y[l2, l, ,(pT+1):(pT+pM)])))))
            penalty_c=0
            if (pC>0) {
              penalty_c=lambda1[l]*(sum(abs(beta_m[l2, l, , ,(pT+1):(pT+pC)])) + sum(abs(beta_y[l2, l, ,(pT+pM+1):(pT+pM+pC)])))
            }
            object = sum(loss_m) + sum(loss_y) + n*(penalty_w + penalty_b + penalty_d + penalty_c) - t(nu)%*%b0
            # print(object)
            # if (object>object_pre) {
            #   print('break')
            # }
            test_y=c(test_y, object)
            
            if (object<object_pre-alpha*t*t(b)%*%b) {
              break
            }
            
            if (max(abs(t*b))<eps1) {
              break
            }
          }
          # print(object)
          
          #print(paste('ii=', ii))
          
          
          if (ii==20 & object>object_pre) {
            print('GD May not decrease')
          }
          # print(ii)
          # print(object)
          # print(beta_y[l2, l, , ])
          # print(t(matrix(nu[(temp2+1):temp3], (1+pT+pM), k)))
          
          
          # if (sum(abs(nu)>10)>0) {
          #   print(nu)
          # }
          #print(beta_y[l,,])
          #diff_ADMM=max(abs(mu-mu_pre), as.vector(abs(beta_m[l, , , ]-beta_m_pre_ADMM)), as.vector(abs(beta_y[l, , ]-beta_y_pre_ADMM)), abs(v-v_pre))
          diff_Nadam=max(abs(nu-nu_pre))
          if (is.na(diff_Nadam)) {
            print(diff_Nadam)
          }
          #print(max(abs(nu-nu_pre)))
          # print(which.max(abs(nu-nu_pre)))
          # print(round(as.numeric(nu), digits=3))
          if (diff_Nadam<eps1) {
            #nu[index_pen]=solve(D[index_row, index_pen], v[index_row])
            nu[abs(nu)<eps1]=0
            diff_Nadam2=max(abs(nu-nu_pre))
            if (diff_Nadam2<eps2) {
              nu_pre=nu
              # object_pre2=object_pre
              object_pre=object
              # convergeNadam=TRUE
              # if (nu[n_pa]==0) {
              #   print(nu[n_pa])
              # }
              break
            }
          }
          if (j==max.iter) {
            print(j)
          }
          nu_pre=nu
          # v1_pre=v1
          # v2_pre=v2
          object_pre=object
          # beta_y_pre0=beta_y[l2, l, , ]
          # beta_m_pre0=beta_m[l2, l, , , ]
          loss_m_pre=loss_m
          loss_y_pre=loss_y
          penalty_w_pre=penalty_w
          penalty_b_pre=penalty_b
          penalty_d_pre=penalty_d
          penalty_c_pre=penalty_c
          fm_pre=t(nu)%*%b0
        }
        
        #print(paste('j=', j))
        
        if (j==max.iter & diff_Nadam>eps1) {
          convergeNadam=FALSE
          converge=FALSE
          print('Nadam does not converge')
          print(l)
          print(i)
        }
        
        temp_m=array(nu[1:temp1], dim=c((pT), pM, k))
        for (h in 1:k) {
          beta_m[l2, l, h,,1:(pT)]=t(temp_m[,,h])
          if (pC>0) {
            beta_m[l2, l, h,,(pT+1):(pT+pC)]=t(matrix(nu[(temp1+1):temp2], pC, pM))
            beta_y[l2, l, h,(pT+pM+1):(pT+pM+pC)]=nu[(temp3+1):n_pa]
          }
        }
        beta_y[l2, l, ,1:(pT+pM)]=t(matrix(nu[(temp2+1):temp3], (pT+pM), k))
        
        diff=max(abs(as.vector(beta_m[l2, l, , , ])-as.vector(beta_m_pre)), 
                 as.vector(abs(beta_y[l2, l, , ]-beta_y_pre)))
        
        # print(c(beta_m[l2, l, 1, 1, 1], beta_y[l2, l, 1, 1]))
        # print(diff)
        if (diff<eps1 & sum(cluster[l2, l,]==cluster_pre)==n) {
          #converge=TRUE
          beta_m_pre=array(beta_m[l2, l, , , ], dim = c(k, pM, pT+pC))
          beta_y_pre=array(beta_y[l2, l, , ], dim = c(k, pT+pM+pC))
          cluster_pre=cluster[l2, l,]
          nu_pre=nu
          break
        }
        beta_m_pre=array(beta_m[l2, l, , , ], dim = c(k, pM, pT+pC))
        beta_y_pre=array(beta_y[l2, l, , ], dim = c(k, pT+pM+pC))
        cluster_pre=cluster[l2, l,]
        nu_pre=nu
      }
      
      #print(paste('i=', i))
      
      if (k>1) {
        for (iii in 1:(pT+pM)) {
          adjm=as.matrix(dist(beta_y[l2, l, , iii]))
          g2=graph_from_adjacency_matrix(adjm<=eps4, mode = "undirected")
          cd=components(g2)
          for (kkk in 1:cd$no) {
            index_cd=which(cd$membership==kkk)
            beta_y[l2, l, index_cd, iii]=mean(beta_y[l2, l, index_cd, iii])
          }
          if (sum((dist(beta_y[l2, l, , iii])<=eps4)&(dist(beta_y[l2, l, , iii])>0))>0) {
            print('Error')
          }
          if (iii<=pM) {
            for (jjj in 1:(pT)) {
              adjm=as.matrix(dist(beta_m[l2, l, , iii, jjj]))
              g2=graph_from_adjacency_matrix(adjm<=eps4, mode = "undirected")
              cd=components(g2)
              for (kkk in 1:cd$no) {
                index_cd=which(cd$membership==kkk)
                beta_m[l2, l, index_cd, iii, jjj]=mean(beta_m[l2, l, index_cd, iii, jjj])
              }
              if (sum((dist(beta_m[l2, l, , iii, jjj])<=eps4)&(dist(beta_m[l2, l, , iii, jjj])>0))>0) {
                print('Error')
              }
            }
          }
        }
      }
      
      if (i==max.iter & diff>eps1) {
        converge=FALSE
        print('The algorithm does not converge')
      }
      
      #print(paste('l=',l, 'l2=', l2))
      
      for (h in 1:k) {
        res_m[h,,]=MM-data_m%*%t(beta_m[l2, l, h, , ])
        res_y[h,]=yy-data_y%*%(beta_y[l2, l, h, ])
        
        loss_m[h,]=diag(res_m[h,,]%*%weight_m%*%t(res_m[h,,]))
        loss_y[h,]=weight_y*(res_y[h,])^2
        ploss[h,]=diag(res_m[h,,]%*%weight_m%*%t(res_m[h,,]))+weight_y*(res_y[h,])^2#+pw[h]
      }
      cluster[l2, l, ]=apply(ploss, 2, which.min)
      for (h in 1:k) {
        index=which(cluster[l2, l, ]==h)
        # lrss[l,h]=log(sum(diag(res_m[h,index,]%*%weight_m%*%t(res_m[h,index,])))/length(index))+weight_y*log(sum((res_y[h,index])^2)/length(index))
        # objective[l,1]=objective[l,1]+length(index)*lrss[l,h]
        # objective[l,2]=objective[l,2]+(sum(beta_m[l, h, , ]!=0)+weight_y*sum(beta_y[l, h, ]!=0))*log(length(index))
        if (length(index)==1) {
          lrss[l2, l, 1]=lrss[l2, l,1]+sum(diag(res_m[h,index,]%*%weight_m%*%as.matrix(res_m[h,index,])))
        } else {
          lrss[l2, l, 1]=lrss[l2, l,1]+sum(diag(res_m[h,index,]%*%weight_m%*%t(res_m[h,index,])))
        }
        lrss[l2, l, 2]=lrss[l2, l,2]+sum((res_y[h,index])^2)
      }
      objective[l2, l, 1]=n*(log(lrss[l2, l, 1]/n)+log(weight_y*lrss[l2, l, 2]/n))
      objective2[l2, l, 1]=objective[l2, l, 1]
      # objective3[l2, l, 1]=objective[l2, l, 1]
      if (k>1) {
        num_m=sum(as.vector(beta_m[l2, l, , ,])!=0)-sum(apply(beta_m[l2, l, , ,pT], 2, function(x) (sum(x!=0)-sum(unique(x)!=0))))
        num_y=sum(as.vector(beta_y[l2, l, , ])!=0)-sum(apply(beta_y[l2, l, ,1:(pT+pM)], 2, function(x) (sum(x!=0)-sum(unique(x)!=0))))
      } else {
        num_m=sum(as.vector(beta_m[l2, l, , ,])!=0)
        num_y=sum(as.vector(beta_y[l2, l, , ])!=0)
      }
      
      num_m2=sum(as.vector(beta_m[l2, l, , ,])!=0)
      num_y2=sum(as.vector(beta_y[l2, l, , ])!=0)
      
      
      # if (k>1) {
      #   num_m3=sum(as.vector(beta_m[l2, l, , ,-1])!=0)-sum(apply(beta_m[l2, l, , ,2], 2, function(x) (sum(x!=0)-sum(unique(x)!=0))))
      #   num_y3=sum(as.vector(beta_y[l2, l, , -1])!=0)-sum(apply(beta_y[l2, l, ,2:(1+pT+pM)], 2, function(x) (sum(x!=0)-sum(unique(x)!=0))))
      # } else {
      #   num_m3=sum(as.vector(beta_m[l2, l, , ,-1])!=0)
      #   num_y3=sum(as.vector(beta_y[l2, l, , -1])!=0)
      # }
      
      objective[l2, l, 2]=log(n)*(num_m+num_y)  #weight_y*
      objective2[l2, l, 2]=log(n)*(num_m2+num_y2)
      # objective3[l2, l, 2]=log(n)*(num_m3+num_y3)
      bic1[l2, l]=objective[l2, l, 1]+objective[l2, l, 2]
      bic2[l2, l]=objective2[l2, l, 1]+objective2[l2, l, 2]
      gcv1[l2, l] = (lrss[l2, l, 1]+weight_y*lrss[l2, l, 2])/(n-(num_m+num_y))^2
      gcv2[l2, l] = (lrss[l2, l, 1])/(n-(num_m))^2 + (weight_y*lrss[l2, l, 2])/(n-(num_y))^2
      # bic3[l2, l]=objective3[l2, l, 1]+objective3[l2, l, 2]
      #objective[l,1]=n*lrss[l]
      #objective[l,2]=(sum(beta_m[l, , , ]!=0)+sum(beta_y[l, , ]!=0))*log(n)
      
      beta_m_out[l2, l, , , ]=beta_m[l2, l, , , ]
      beta_y_out[l2, l, , ]=beta_y[l2, l, , ]
      if (pC>0) {
        for (h in 1:k) {
          for (j in 1:pM) {
            beta_m_out[l2, l, h,j,1:(pT+pC)]=scalem[j]*beta_m[l2, l, h,j,1:(pT+pC)]/c(scalex, scalec)
            intercept_m[l2, l, h, j] = centerm[j]-crossprod(c(centerx, centerc), beta_m_out[l2, l, h,j,1:(pT+pC)])
          }
          beta_y_out[l2, l, h,1:(pT+pM+pC)]=scaley*beta_y[l2, l, h,1:(pT+pM+pC)]/c(scalex, scalem, scalec)
          intercept_y[l2, l, h] = centery-crossprod(c(centerx, centerm, centerc), beta_y_out[l2, l, h,1:(pT+pM+pC)])
        }
      } else {
        for (h in 1:k) {
          beta_m_out[l2, l, h, ,1:pT]=scalem*beta_m[l2, l, h, ,1:pT]/scalex
          intercept_m[l2, l, h, ] = centerm - beta_m_out[l2, l, h, ,1:pT]*centerx
          
          beta_y_out[l2, l, h,1:(pT+pM)]=scaley*beta_y[l2, l, h,1:(pT+pM)]/c(scalex, scalem)
          intercept_y[l2, l, h] = centery-crossprod(c(centerx, centerm), beta_y_out[l2, l, h,1:(pT+pM)])
        }
      }
      ncluster[l2, l] = length(unique(cluster[l2, l,]))
    }
  }
  returnlist=list("beta_m"=beta_m_out, "beta_y"=beta_y_out, "beta_m0"=beta_m, "beta_y0"=beta_y, 'cluster'=cluster, 'ncluster'=ncluster,
                  'lambda1'=lambda1, 'lambda2'=lambda2, 'bic1'=bic1, 'bic2'=bic2, 'gcv1'=gcv1, 'gcv2'=gcv2, #'bic3'=bic3, 
                  'objective'=objective, 'objective2'=objective2, #'objective3'=objective3, 
                  'lrss'=lrss, 'res_m'=res_m, 'res_y'=res_y, "converge"=converge, 
                  "XX"=XX, "yy"=yy, 'centerx'=centerx,'scalex'=scalex, 'intercept_m'=intercept_m, 'intercept_y'=intercept_y)
  
  if(bb==1){
    sample1<-sample_data
    result1<-returnlist
    save(result1,file = "sample_boo1.RData")
  }
  if(bb==2){
    sample2<-sample_data
    result2<-returnlist
    save(result2,file = "sample_boo2.RData")
  }
  if(bb==3){
    sample3<-sample_data
    result3<-returnlist
    save(result3,file = "sample_boo3.RData")
  }
  if(bb==4){
    sample4<-sample_data
    result4<-returnlist
    save(result4,file = "sample_boo4.RData")
  }
  if(bb==5){
    sample5<-sample_data
    result5<-returnlist
    save(result5,file = "sample_boo5.RData")
  }
}

soft <- function (data, c1, c2, c3, eps1) {
  lambda=data[c1]
  dev=data[c2] 
  funvalue=data[c3] 
  u=abs(funvalue)
  zero=FALSE
  if (u<eps1) {
    if (dev > lambda) {
      pew=-lambda
    } else if (dev< -lambda) {
      pew=lambda
    } else {
      pew=-dev
      zero=TRUE
      #nu_pre[index_tempM1]=0
    }
  } else {
    pew=lambda*sign(funvalue)
  }
  returnlist=c('pew'=pew, 'zero'=zero)
  return(returnlist)
}


###### The value of SCAD penalty function
SCADp <- function (beta, lambda, a) {
  if (abs(beta)<=lambda) {
    result=lambda*abs(beta)
  } else if (abs(beta)<=a*lambda) {
    result=(beta^2-2*a*lambda*abs(beta)+lambda^2)/(2*(1-a))
  } else {
    result=lambda^2*(a+1)/2
  } 
  return(result)
}


#sd
rb<-rbind(result1[["beta_y"]][1,1,1,],result2[["beta_y"]][1,1,1,],result3[["beta_y"]][1,1,1,],result4[["beta_y"]][1,1,1,],result5[["beta_y"]][1,1,1,])
round(apply(rb,2,sd),2)

#intersect

#boot1  213
index1<-which(result1[["cluster"]][1,1,]==2)
index2<-which(result1[["cluster"]][1,1,]==1)
index3<-which(result1[["cluster"]][1,1,]==3)

sample11<-sample1[index1,]
sample22<-sample1[index2,]
sample33<-sample1[index3,]

summary(sample11[,26])
summary(sample22[,26])
summary(sample33[,26])

length(as.integer(rownames(sample8161[which(returnlist[["cluster"]][1,1,]==1),])))

length(intersect(unique(as.integer(rownames(sample11))),as.integer(rownames(sample8161[which(returnlist[["cluster"]][1,1,]==1),]))))/length(unique(as.integer(rownames(sample11))))
length(intersect(unique(as.integer(rownames(sample22))),as.integer(rownames(sample8161[which(returnlist[["cluster"]][1,1,]==2),]))))/length(unique(as.integer(rownames(sample22))))
length(intersect(unique(as.integer(rownames(sample33))),as.integer(rownames(sample8161[which(returnlist[["cluster"]][1,1,]==3),]))))/length(unique(as.integer(rownames(sample33))))
#length(intersect(as.integer(rownames(sample11)),which(returnlist[["cluster"]][1,1,]==1)))/length(which(returnlist[["cluster"]][1,1,]==1))
#length(intersect(as.integer(rownames(sample22)),which(returnlist[["cluster"]][1,1,]==2)))/length(which(returnlist[["cluster"]][1,1,]==2))
#length(intersect(as.integer(rownames(sample33)),which(returnlist[["cluster"]][1,1,]==3)))/length(which(returnlist[["cluster"]][1,1,]==3))

#boot2  321
index1<-which(result2[["cluster"]][1,1,]==3)
index2<-which(result2[["cluster"]][1,1,]==2)
index3<-which(result2[["cluster"]][1,1,]==1)

sample11<-sample2[index1,]
sample22<-sample2[index2,]
sample33<-sample2[index3,]

summary(sample11[,26])
summary(sample22[,26])
summary(sample33[,26])

length(intersect(unique(as.integer(rownames(sample11))),as.integer(rownames(sample8161[which(returnlist[["cluster"]][1,1,]==1),]))))/length(unique(as.integer(rownames(sample11))))
length(intersect(unique(as.integer(rownames(sample11))),as.integer(rownames(sample8161[which(returnlist[["cluster"]][1,1,]==2),]))))/length(unique(as.integer(rownames(sample11))))
length(intersect(unique(as.integer(rownames(sample11))),as.integer(rownames(sample8161[which(returnlist[["cluster"]][1,1,]==3),]))))/length(unique(as.integer(rownames(sample11))))

length(intersect(unique(as.integer(rownames(sample22))),as.integer(rownames(sample8161[which(returnlist[["cluster"]][1,1,]==1),]))))/length(unique(as.integer(rownames(sample22))))
length(intersect(unique(as.integer(rownames(sample22))),as.integer(rownames(sample8161[which(returnlist[["cluster"]][1,1,]==2),]))))/length(unique(as.integer(rownames(sample22))))
length(intersect(unique(as.integer(rownames(sample22))),as.integer(rownames(sample8161[which(returnlist[["cluster"]][1,1,]==3),]))))/length(unique(as.integer(rownames(sample22))))

length(intersect(unique(as.integer(rownames(sample33))),as.integer(rownames(sample8161[which(returnlist[["cluster"]][1,1,]==1),]))))/length(unique(as.integer(rownames(sample33))))
length(intersect(unique(as.integer(rownames(sample33))),as.integer(rownames(sample8161[which(returnlist[["cluster"]][1,1,]==2),]))))/length(unique(as.integer(rownames(sample33))))
length(intersect(unique(as.integer(rownames(sample33))),as.integer(rownames(sample8161[which(returnlist[["cluster"]][1,1,]==3),]))))/length(unique(as.integer(rownames(sample33))))

#boot3 231
index1<-which(result3[["cluster"]][1,1,]==2)
index2<-which(result3[["cluster"]][1,1,]==3)
index3<-which(result3[["cluster"]][1,1,]==1)

sample11<-sample3[index1,]
sample22<-sample3[index2,]
sample33<-sample3[index3,]

summary(sample11[,26])
summary(sample22[,26])
summary(sample33[,26])

length(intersect(unique(as.integer(rownames(sample11))),as.integer(rownames(sample8161[which(returnlist[["cluster"]][1,1,]==1),]))))/length(unique(as.integer(rownames(sample11))))
length(intersect(unique(as.integer(rownames(sample22))),as.integer(rownames(sample8161[which(returnlist[["cluster"]][1,1,]==2),]))))/length(unique(as.integer(rownames(sample22))))
length(intersect(unique(as.integer(rownames(sample33))),as.integer(rownames(sample8161[which(returnlist[["cluster"]][1,1,]==3),]))))/length(unique(as.integer(rownames(sample33))))

#boot4 231
index1<-which(result4[["cluster"]][1,1,]==2)
index2<-which(result4[["cluster"]][1,1,]==3)
index3<-which(result4[["cluster"]][1,1,]==1)

sample11<-sample4[index1,]
sample22<-sample4[index2,]
sample33<-sample4[index3,]

summary(sample11[,26])
summary(sample22[,26])
summary(sample33[,26])

length(intersect(unique(as.integer(rownames(sample11))),as.integer(rownames(sample8161[which(returnlist[["cluster"]][1,1,]==1),]))))/length(unique(as.integer(rownames(sample11))))
length(intersect(unique(as.integer(rownames(sample11))),as.integer(rownames(sample8161[which(returnlist[["cluster"]][1,1,]==2),]))))/length(unique(as.integer(rownames(sample11))))
length(intersect(unique(as.integer(rownames(sample11))),as.integer(rownames(sample8161[which(returnlist[["cluster"]][1,1,]==3),]))))/length(unique(as.integer(rownames(sample11))))

length(intersect(unique(as.integer(rownames(sample22))),as.integer(rownames(sample8161[which(returnlist[["cluster"]][1,1,]==1),]))))/length(unique(as.integer(rownames(sample22))))
length(intersect(unique(as.integer(rownames(sample22))),as.integer(rownames(sample8161[which(returnlist[["cluster"]][1,1,]==2),]))))/length(unique(as.integer(rownames(sample22))))
length(intersect(unique(as.integer(rownames(sample22))),as.integer(rownames(sample8161[which(returnlist[["cluster"]][1,1,]==3),]))))/length(unique(as.integer(rownames(sample22))))

length(intersect(unique(as.integer(rownames(sample33))),as.integer(rownames(sample8161[which(returnlist[["cluster"]][1,1,]==1),]))))/length(unique(as.integer(rownames(sample33))))
length(intersect(unique(as.integer(rownames(sample33))),as.integer(rownames(sample8161[which(returnlist[["cluster"]][1,1,]==2),]))))/length(unique(as.integer(rownames(sample33))))
length(intersect(unique(as.integer(rownames(sample33))),as.integer(rownames(sample8161[which(returnlist[["cluster"]][1,1,]==3),]))))/length(unique(as.integer(rownames(sample33))))

#boot5  213
index1<-which(result5[["cluster"]][1,1,]==2)
index2<-which(result5[["cluster"]][1,1,]==1)
index3<-which(result5[["cluster"]][1,1,]==3)

sample11<-sample5[index1,]
sample22<-sample5[index2,]
sample33<-sample5[index3,]

summary(sample11[,26])
summary(sample22[,26])
summary(sample33[,26])

length(intersect(unique(as.integer(rownames(sample11))),as.integer(rownames(sample8161[which(returnlist[["cluster"]][1,1,]==1),]))))/length(unique(as.integer(rownames(sample11))))
length(intersect(unique(as.integer(rownames(sample22))),as.integer(rownames(sample8161[which(returnlist[["cluster"]][1,1,]==2),]))))/length(unique(as.integer(rownames(sample22))))
length(intersect(unique(as.integer(rownames(sample33))),as.integer(rownames(sample8161[which(returnlist[["cluster"]][1,1,]==3),]))))/length(unique(as.integer(rownames(sample33))))






