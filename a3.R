library(Diagnostics)

learn = function (hist)
{
  
  #Pneumonia probability matrix
  probPn = matrix(0,1,2)
  probPn[1,1] = 1 - sum(hist$Pn)/length(hist$Pn) # no pneumonia
  probPn[1,2] = 1 - probPn[1,1] # pneumonia
  
  #Visited TB spot probability matrix
  probVTB = matrix(0,1,2)
  probVTB[1,1] = 1-sum(hist$VTB)/length(hist$VTB) # no VTB
  probVTB[1,2] = 1 - probVTB[1,1] # VTB
  
  #Smokes probability matrix
  probSm = matrix(0,1,2)
  probSm[1,1] = 1-sum(hist$Sm)/length(hist$Sm) # no smoke
  probSm[1,2] = 1- probSm[1,1] # smoke
  
  #Probability Temperature given Pneumonia
  probTe = matrix(0,2,2)
  probTe[1,1] = mean(hist$Te[hist$Pn==0]) # no pneumonia temperature mean
  probTe[1,2] = sd(hist$Te[hist$Pn==0]) # no pnemonia temperature std
  probTe[2,1] = mean(hist$Te[hist$Pn==1]) # pneumonia temperature mean
  probTe[2,2] = sd(hist$Te[hist$Pn==1]) # pneumonia temperature std
  
  #Probability TB given VTB
  probTB = matrix(0,2,2)
  probTB[1,1] = 1-sum(hist$TB[hist$VTB==0])/length(hist$TB[hist$VTB==0]) # no VTB no TB
  probTB[1,2] = 1-probTB[1,1] # no VTB, TB
  probTB[2,1] = 1-sum(hist$TB[hist$VTB==1])/length(hist$TB[hist$VTB==1]) # VTB, no TB
  probTB[2,2] = 1 - probTB[2,1] # VTB, TB
  
  #Probability lung cancer given smokes
  probLC = matrix(0,2,2)
  probLC[1,1] = 1-sum(hist$LC[hist$Sm==0])/length(hist$LC[hist$Sm==0]) # no smoke, no lung cancer
  probLC[1,2] = 1-probLC[1,1] # no smoke, lung cancer
  probLC[2,1] = 1-sum(hist$LC[hist$Sm==1])/length(hist$LC[hist$Sm==1]) # smoke, no lung cancer
  probLC[2,2] = 1 - probLC[2,1] # smoke, lung cancer
  
  #Probability bronchitis given smokes
  probBr = matrix(0,2,2)
  probBr[1,1] = 1-sum(hist$Br[hist$Sm==0])/length(hist$Br[hist$Sm==0]) # no smoke, no brochitis
  probBr[1,2] = 1-probBr[1,1] # no smoke, brochitis
  probBr[2,1] = 1-sum(hist$Br[hist$Sm==1])/length(hist$Br[hist$Sm==1]) # smoke, no brochitis
  probBr[2,2] = 1 - probBr[2,1] # smoke, brochitis
  
  
  
  #Probability dyspnea given lungcancer and bronchitis
  probDy = matrix(0,4,2)
  probDy[1,1] = 1-sum(hist$Dy[(hist$Br==0)&(hist$LC==0)])/length(hist$Dy[(hist$Br==0)  & (hist$LC==0)])
  probDy[1,2] = 1-probDy[1,1]
  probDy[2,1] = 1-sum(hist$Dy[(hist$Br==1)&(hist$LC==0)])/length(hist$Dy[(hist$Br==1)&(hist$LC==0)])
  probDy[2,2] = 1-probDy[2,1]
  probDy[3,1] = 1-sum(hist$Dy[(hist$Br==0)&(hist$LC==1)])/length(hist$Dy[(hist$Br==0)&(hist$LC==1)])
  probDy[3,2] = 1-probDy[3,1]
  probDy[4,1] = 1-sum(hist$Dy[(hist$Br==1)&(hist$LC==1)])/length(hist$Dy[(hist$Br==1)&(hist$LC==1)])
  probDy[4,2] = 1-probDy[4,1]
  
  #Probability X-Ray given lungcancer, tuberculosis, pneumonia
  probXR = matrix(0,8,2)
  probXR[1,1] = 1 - sum(hist$XR[hist$LC==0 & hist$TB==0 & hist$Pn==0])/length(hist$XR[hist$LC==0 & hist$TB==0 & hist$Pn==0])
  probXR[1,2] = 1 - probXR[1,1]
  probXR[2,1] = 1 - sum(hist$XR[hist$LC==1 & hist$TB==0 & hist$Pn==0])/length(hist$XR[hist$LC==1 & hist$TB==0 & hist$Pn==0])
  probXR[2,2] = 1 - probXR[2,1]
  
  probXR[3,1] = 1 - sum(hist$XR[hist$LC==0 & hist$TB==1 & hist$Pn==0])/length(hist$XR[hist$LC==0 & hist$TB==1 & hist$Pn==0])
  probXR[3,2] = 1 - probXR[3,1]
  probXR[4,1] = 1 - sum(hist$XR[hist$LC==1 & hist$TB==1 & hist$Pn==0])/length(hist$XR[hist$LC==1 & hist$TB==1 & hist$Pn==0])
  probXR[4,2] = 1 - probXR[4,1]
  
  probXR[5,1] = 1 - sum(hist$XR[hist$LC==0 & hist$TB==0 & hist$Pn==1])/length(hist$XR[hist$LC==0 & hist$TB==0 & hist$Pn==1])
  probXR[5,2] = 1 - probXR[5,1]
  probXR[6,1] = 1 - sum(hist$XR[hist$LC==1 & hist$TB==0 & hist$Pn==1])/length(hist$XR[hist$LC==1 & hist$TB==0 & hist$Pn==1])
  probXR[6,2] = 1 - probXR[6,1]
  
  probXR[7,1] = 1 - sum(hist$XR[hist$LC==0 & hist$TB==1 & hist$Pn==1])/length(hist$XR[hist$LC==0 & hist$TB==1 & hist$Pn==1])
  probXR[7,2] = 1 - probXR[7,1]
  probXR[8,1] = 1 - sum(hist$XR[hist$LC==1 & hist$TB==1 & hist$Pn==1])/length(hist$XR[hist$LC==1 & hist$TB==1 & hist$Pn==1])
  probXR[8,2] = 1 - probXR[8,1]
  
  network = list(Pn = probPn, Br = probBr, Dy = probDy, LC = probLC, Sm = probSm,
                 VTB = probVTB, TB = probTB, XR = probXR, Te = probTe)
  
  return(network)
}

diagnose = function(network, cases)
{
  
  result_probs = matrix(0,10,4)
  
  # iterating for each case
  for (x in 1:10) {
    
    samples = data.frame(Pn = rep(0,1000), Te = rep(0,1000), VTB = rep(0,1000), TB = rep(0,1000), Sm = rep(0,1000),
                         LC = rep(0,1000), Br = rep(0,1000), XR = rep(0,1000), Dy = rep(0,1000))
    
    samples[1,] = cases[x,]
    samples$Pn[1] = sample(0:1,1)
    samples$TB[1] = sample(0:1,1)
    samples$LC[1] = sample(0:1,1)
    samples$Br[1] = sample(0:1,1)
    
    # loop generating 100 samples
    for (i in 2:1000) {
      
      temp = samples[i-1,]
      
      for (j in c("Pn","TB","LC","Br")) {
        # evaluate the configuration with changed parameter j
        probTemp_old = 0
        probTemp_old = network$Pn[temp$Pn+1]*network$Sm[temp$Sm+1]*network$VTB[temp$VTB+1]
        probTemp_old = probTemp_old*dnorm(temp$Te,network$Te[temp$Pn+1,1],network$Te[temp$Pn+1,2])
        probTemp_old = probTemp_old*network$TB[temp$VTB+1,temp$TB+1]
        probTemp_old = probTemp_old*network$LC[temp$Sm+1,temp$LC+1]
        probTemp_old = probTemp_old*network$Br[temp$Sm+1,temp$Br+1]
        probTemp_old = probTemp_old*network$Dy[(temp$LC)*2+temp$Br+1,temp$Dy+1]
        probTemp_old = probTemp_old*network$XR[(temp$Pn)*4 + (temp$TB)*2+temp$LC+1,temp$XR+1]

        
        # assign a new value to the unknown parameter j
        if(samples[[j]][i-1] == 0){
          temp[[j]] = 1
        }else{
          temp[[j]] = 0
        }
        
        # evaluate the configuration with changed parameter j
        probTemp_new = 0
        probTemp_new = network$Pn[temp$Pn+1]*network$Sm[temp$Sm+1]*network$VTB[temp$VTB+1]
        probTemp_new = probTemp_new*dnorm(temp$Te,network$Te[temp$Pn+1,1],network$Te[temp$Pn+1,2])
        probTemp_new = probTemp_new*network$TB[temp$VTB+1,temp$TB+1]
        probTemp_new = probTemp_new*network$LC[temp$Sm+1,temp$LC+1]
        probTemp_new = probTemp_new*network$Br[temp$Sm+1,temp$Br+1]
        probTemp_new = probTemp_new*network$Dy[(temp$LC)*2+temp$Br+1,temp$Dy+1]
        probTemp_new = probTemp_new*network$XR[(temp$Pn)*4 + (temp$TB)*2+temp$LC+1,temp$XR+1]

        
        if(probTemp_new < probTemp_old){
          if(runif(1) > probTemp_new/probTemp_old ){
            if(temp[[j]] == 0){
              temp[[j]] = 1
            }else{
              temp[[j]] = 0
            }
          }
        }
        samples[i,] = temp
      }
      
    }
    
    result_probs[x,1] = sum(samples$Pn[101:1000])/900
    result_probs[x,2] = sum(samples$TB[101:1000])/900
    result_probs[x,3] = sum(samples$LC[101:1000])/900
    result_probs[x,4] = sum(samples$Br[101:1000])/900
    
  }
  return(result_probs)
}

runDiagnostics(learn, diagnose, verbose = 2)