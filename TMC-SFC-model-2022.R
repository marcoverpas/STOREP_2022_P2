#This R code reproduces the experiments discussed at STOREP Conference,
#May 27 2022, Session 19: "Monetary theories and tools". Presentation title:
#"It is not la vie en rose. New insights from Graziani's theory of monetary circuit"

#Created on May 27 2022 by Marco Veronese Passarella


################################################################################

#Clear Environment
rm(list=ls(all=TRUE))

#Clear Plots
if(!is.null(dev.list())) dev.off()

#Clear Console
cat("\014")

#Import (self-generated) steady-state values
data00 <- read.csv("https://www.dropbox.com/s/k4desu52d3couxa/steady_state_values.csv?dl=1") 

#Select type of investment function 
inv_opt = 1                                 #Note: 0 = standard sfc; 1 = circuit 

#Number of periods
nPeriods = 200

#Number of scenarios
nScenarios=6 

#Set parameters and exogenous variables 
delta=0.1                                              #Depreciation rate of capital stock
pr=1                                                   #Labor productivity
r_bar=0.04                                             #Policy rate
w=0.72                                                 #Wage rate 
alpha1w=0.757621 #(0.70)                               #Propensity to consume out of wages
alpha2w=0.1                                            #Propensity to consume out of workers' wealth
alpha1z=0.69     #(0.70)                               #Propensity to consume out of profits 
alpha2z=0.1                                            #Propensity to consume out of capitalists' wealth
lambdaw=0.5                                            #Workers' share of deposits to total wealth (liquidity preference)  
lambdaz=0.5                                            #Capitalists' share of deposits to total wealth (liquidity preference)  
gamma=0.15                                             #Speed of adjustment of current investment to target level
psi0=0                                                 #Coefficient of adaptive expectations: fixed
psi1=0.1                                               #Coefficient of adaptive expectations: correction

#Set values of coefficients that are shocked
alpha0w=matrix(data=23,nrow=nScenarios,ncol=nPeriods)               #Autonomous consumption of workers
alpha0z=matrix(data=2.5,nrow=nScenarios,ncol=nPeriods)              #Autonomous consumption of capitalists
beta=matrix(data=0.10,nrow=nScenarios,ncol=nPeriods)                #Target investment share to total income
sigma=matrix(data=1,nrow=nScenarios,ncol=nPeriods)                  #Coefficient of real supply function (note: 1 = full adjustment to demand; 0 = fully exogenous)
iota=matrix(data=1,nrow=nScenarios,ncol=nPeriods)                   #Coefficient of additional interest payments (0 = no extra payments; 1 = extra payments)
kappa=matrix(data=1,nrow=nScenarios,ncol=nPeriods)                  #Target capital to output ratio
par_id=matrix(data=1,nrow=nScenarios,ncol=nPeriods)                 #Coefficient defining type of investment (1 = endogenous; 0 = exogenous)
alpha3=matrix(data=0,nrow=nScenarios,ncol=nPeriods)                 #Exogenous investment
gammay=matrix(data=0.15,nrow=nScenarios,ncol=nPeriods)              #Speed of adjustment of potential output to current one  

#Define and set initial value of variables
y_n=matrix(data=data00$y_r_0,nrow=nScenarios,ncol=nPeriods)         #Real output: exogenous supply
p=matrix(data=1,nrow=nScenarios,ncol=nPeriods)                      #Unit price

#Define variables
af=matrix(data=data00$af_0,nrow=nScenarios,ncol=nPeriods)           #Amortization funds
c=matrix(data=data00$c_0,nrow=nScenarios,ncol=nPeriods)             #Total demand of consumption goods
cw=matrix(data=data00$cw_0,nrow=nScenarios,ncol=nPeriods)           #Consumption goods demanded by workers
cz=matrix(data=data00$cz_0,nrow=nScenarios,ncol=nPeriods)           #Consumption goods demanded by capitalists
da=matrix(data=data00$da_0,nrow=nScenarios,ncol=nPeriods)           #Depreciation allowances (real)
k=matrix(data=data00$k_0,nrow=nScenarios,ncol=nPeriods)             #Actual stock of capital
kt=matrix(data=data00$k_0,nrow=nScenarios,ncol=nPeriods)            #Target stock of capital
ld=matrix(data=data00$ld_0,nrow=nScenarios,ncol=nPeriods)           #Demand for bank loans 
ls=matrix(data=data00$ls_0,nrow=nScenarios,ncol=nPeriods)           #Supply of bank loans 
id=matrix(data=data00$id_0,nrow=nScenarios,ncol=nPeriods)           #Demand for investment
ms=matrix(data=data00$mh_0,nrow=nScenarios,ncol=nPeriods)           #Supply of bank deposits
n=matrix(data=data00$n_0,nrow=nScenarios,ncol=nPeriods)             #Employed workers (direct labor time)
wb=matrix(data=data00$wb_0,nrow=nScenarios,ncol=nPeriods)           #Wage bill 
y=matrix(data=data00$y_0,nrow=nScenarios,ncol=nPeriods)             #Total income
ydw=matrix(data=data00$ydw_0,nrow=nScenarios,ncol=nPeriods)         #Disposal income of workers
ydz=matrix(data=data00$ydz_0,nrow=nScenarios,ncol=nPeriods)         #Disposal income of capitalists
ydw_e=matrix(data=data00$ydw_0,nrow=nScenarios,ncol=nPeriods)       #Expected disposal income of workers
ydz_e=matrix(data=data00$ydz_0,nrow=nScenarios,ncol=nPeriods)       #Expected disposal income of capitalists
yd=matrix(data=data00$yd_0,nrow=nScenarios,ncol=nPeriods)           #Total disposal income of households
fin_i=matrix(data=data00$fin_i_0,nrow=nScenarios,ncol=nPeriods)     #Initial finance to production
fin_f=matrix(data=data00$fin_f_0,nrow=nScenarios,ncol=nPeriods)     #Final finance
bs=matrix(data=data00$bs_0,nrow=nScenarios,ncol=nPeriods)           #Supply for private securities
rb=matrix(data=r_bar,nrow=nScenarios,ncol=nPeriods)                 #Rate of return on private securities
rl=matrix(data=r_bar,nrow=nScenarios,ncol=nPeriods)                 #Rate of interest on banks loans
rm=matrix(data=r_bar,nrow=nScenarios,ncol=nPeriods)                 #Rate of interest on bank deposits
vw=matrix(data=data00$vw_0,nrow=nScenarios,ncol=nPeriods)           #Wealth of workers
bw=matrix(data=data00$bw_0,nrow=nScenarios,ncol=nPeriods)           #Demand for private securities by workers
mw=matrix(data=data00$mw_0,nrow=nScenarios,ncol=nPeriods)           #Bank deposits held by workers by workers
vz=matrix(data=data00$vz_0,nrow=nScenarios,ncol=nPeriods)           #Wealth of capitalists
bz=matrix(data=data00$bz_0,nrow=nScenarios,ncol=nPeriods)           #Demand for private securities by capitalists
mz=matrix(data=data00$mz_0,nrow=nScenarios,ncol=nPeriods)           #Bank deposits held by workers by capitalists
vh=matrix(data=data00$vh_0,nrow=nScenarios,ncol=nPeriods)           #Wealth of households
bh=matrix(data=data00$bh_0,nrow=nScenarios,ncol=nPeriods)           #Total demand for private securities
mh=matrix(data=data00$mh_0,nrow=nScenarios,ncol=nPeriods)           #Total bank deposits held by workers
idt=matrix(data=data00$idt_0,nrow=nScenarios,ncol=nPeriods)         #Target investment
p_e=matrix(data=data00$p_e_0,nrow=nScenarios,ncol=nPeriods)         #Expected price
paym_l=matrix(data=data00$paym_l_0,nrow=nScenarios,ncol=nPeriods)   #Interest payments on loans
paymw_m=matrix(data=data00$paymw_m_0,nrow=nScenarios,ncol=nPeriods) #Interest payments on deposits paid to workers
paymz_m=matrix(data=data00$paymz_m_0,nrow=nScenarios,ncol=nPeriods) #Interest payments on deposits paid to capitalists
paym_m=matrix(data=data00$paym_m_0,nrow=nScenarios,ncol=nPeriods)   #Total interest payments on deposits
paymw_b=matrix(data=data00$paymw_b_0,nrow=nScenarios,ncol=nPeriods) #Interest payments on private securities paid to workers
paymz_b=matrix(data=data00$paymz_b_0,nrow=nScenarios,ncol=nPeriods) #Interest payments on private securities paid to capitalists
paym_b=matrix(data=data00$paym_m_0,nrow=nScenarios,ncol=nPeriods)   #Total interest payments on securities
pb=matrix(data=data00$pb_0,nrow=nScenarios,ncol=nPeriods)           #Bank profit
cw_r=matrix(data=data00$cw_r_0,nrow=nScenarios,ncol=nPeriods)       #Real consumption of workers
cw_r_t=matrix(data=data00$cw_r_t_0,nrow=nScenarios,ncol=nPeriods)   #Expected real consumption of workers
cz_r=matrix(data=data00$cz_r_0,nrow=nScenarios,ncol=nPeriods)       #Real consumption of capitalists
c_r=matrix(data=data00$c_r_0,nrow=nScenarios,ncol=nPeriods)         #Real consumption of households
c_r_t=matrix(data=cw_r_t+cz_r,nrow=nScenarios,ncol=nPeriods)        #Total expected real consumption 
id_r=matrix(data=data00$id_r_0,nrow=nScenarios,ncol=nPeriods)       #Real investment
y_r=matrix(data=data00$y_r_0,nrow=nScenarios,ncol=nPeriods)         #Real output
y_s=matrix(data=data00$y_s_0,nrow=nScenarios,ncol=nPeriods)         #Real output: actual supply
y_g=matrix(data=data00$y_g_0,nrow=nScenarios,ncol=nPeriods)         #Real output gap
pf=matrix(data=data00$pf_0,nrow=nScenarios,ncol=nPeriods)           #Profit of firms
slt=matrix(data=data00$slt_0,nrow=nScenarios,ncol=nPeriods)         #Surplus labor
nlt=matrix(data=data00$nlt_0,nrow=nScenarios,ncol=nPeriods)         #Necessary labor time
expl=matrix(data=data00$expl_0,nrow=nScenarios,ncol=nPeriods)       #Exploitation rate

#Begin the model

#Choose scenario
for (j in 1:nScenarios){
  
  #Define time loop
  for (i in 2:nPeriods){
    
    #Define iterations
    for (iterations in 1:200){
      
      #Shock to autonomous consumption
      if (i>=10 && j==2){alpha0w[j,i]=24
      alpha0z[j,i]=3.5}
      
      #Shock to investment (quantity adjustment)
      if (i>=10 && j==3){alpha3[j,i]=25
      par_id[j,i]=0}            
        
      #Shock to investment (mixed adjustment)
      if (i>=10 && j==4){alpha3[j,i]=25
      par_id[j,i]=0
      sigma[j,i]=0}
      
      #Shock to investment (price adjustment)
      if (i>=10 && j==5){alpha3[j,i]=25
      par_id[j,i]=0
      sigma[j,i]=0
      gammay[j,i]=0
      }
      
      #Different baseline when interest payments are calculated in standard way
      if (j==6){iota[j,i]=0}
      
      
      #STEP 5: Model     
      
      #Households: workers and capitalists
      ydw[j,i] = wb[j,i] + paymw_m[j,i] + paymw_b[j,i]                         #Disposable income of workers
      ydz[j,i] =  pb[j,i] + pf[j,i] + paymz_m[j,i] + paymz_b[j,i]              #Disposable income of capitalists
      yd[j,i] =  ydw[j,i] + ydz[j,i]                                           #Total disposable income of households
      vw[j,i] = vw[j,i-1] + ydw[j,i] - cw[j,i]                                 #Stock of wealth of workers
      vz[j,i] = vz[j,i-1] + ydz[j,i] - cz[j,i]                                 #Stock of wealth of capitalists
      vh[j,i] = vw[j,i-1] + vz[j,i]                                            #Total stock of wealth of workers
      bw[j,i] = (1-lambdaw)*vw[j,i]                                            #Demanded stock of private securities by workers
      bz[j,i] = (1-lambdaz)*vz[j,i]                                            #Demanded stock of private securities by capitalists
      bh[j,i] = bw[j,i] + bz[j,i]                                              #Total demanded stock of private securities
      mw[j,i] = vw[j,i] - bw[j,i]                                              #Demanded stock of deposits (hoarding) by workers 
      mz[j,i] = vz[j,i] - bz[j,i]                                              #Demanded stock of deposits (hoarding) by capitalists 
      mh[j,i] = mw[j,i] + mz[j,i]                                              #Total demanded stock of deposits (hoarding)
      cw_r_t[j,i] = alpha0w[j,i] + alpha1w*ydw_e[j,i]/p_e[j,i] + alpha2w*vw[j,i-1]/p[j,i-1] #Expected real consumption of workers
      c_r_t[j,i] = cw_r_t[j,i] + cz_r[j,i]                                     #Total expected real consumption 
      if (sigma[j,i]==1){cw_r[j,i] = cw_r_t[j,i]}                                           #Real consumption (fixed price) of workers
      else{cw_r[j,i] = y_r[j,i] - cz_r[j,i] - id_r[j,i]}                                    #Real consumption (market-clearing price) of workers
      cz_r[j,i] = alpha0z[j,i] + alpha1z*ydz_e[j,i]/p_e[j,i] + alpha2z*vz[j,i-1]/p[j,i-1]   #Real consumption of capitalists
      c_r[j,i] = cw_r[j,i] + cz_r[j,i]                                         #Total real consumption of households
      cw[j,i] = cw_r[j,i]*p[j,i]                                               #Nominal consumption by workers
      cz[j,i] = cz_r[j,i]*p[j,i]                                               #Nominal consumption by capitalists
      c[j,i] = cw[j,i] + cz[j,i]                                               #Total nominal consumption 
      
      #Firms
      if (sigma[j,i]==1){y_r[j,i] = c_r[j,i] + id_r[j,i]}                      #Real output: expenditure approach
      else{y_r[j,i] = y_s[j,i]}
      y[j,i] = c[j,i] + id[j,i]                                                #Nominal demand
      if (inv_opt==0){
        
        #Standard SFC investment function
        kt[j,i] = kappa[j,i]*y_r[j,i-1]                                        #Target capital stock
        id_r[j,i] = par_id[j,i]*(gamma*(kt[j,i] - k[j,i-1]) + da[j,i]/p[j,i]) + (1-par_id[j,i])*alpha3[j,i] } #Real gross current investment (including capital depreciation)
      
      else{  
        #Circuit-based investment function
        idt[j,i] = beta[j,i]*y_r[j,i]                                          #Real target investment
        id_r[j,i] = par_id[j,i]*(id_r[j,i-1] + gamma*(idt[j,i] - id_r[j,i-1])) + (1-par_id[j,i])*alpha3[j,i] } #Real gross current investment (without capital depreciation)
      
      id[j,i] = id_r[j,i]*p[j,i]                                               #Nominal gross current investment 
      da[j,i] = delta*k[j,i-1]                                                 #Real capital depreciation
      af[j,i] = da[j,i]*p[j,i]                                                 #Nominal amortization funds (retained profits)
      k[j,i] = k[j,i-1] + id_r[j,i] - da[j,i]                                  #Real capital stock
      pf[j,i] = y[j,i] - paym_l[j,i] - af[j,i] - paymz_b[j,i] - paymw_b[j,i] - wb[j,i]         #Total profit
      bs[j,i] = bs[j,i-1] + bh[j,i] - bh[j,i-1]                                #Stock of securities issued by firms
      rb[j,i] = r_bar                                                          #Rate of interest on private securities
      
      #Market-clearing price and supply adjusting to demand
      y_s[j,i] = sigma[j,i]*y_r[j,i] + (1-sigma[j,i])*y_n[j,i]                 #Real supply
      p[j,i] = (c_r_t[j,i]*p_e[j,i])/c_r[j,i]                                  #Unit price of output
      y_n[j,i] = y_n[j,i-1] + gammay[j,i]*(y[j,i-1] - y_n[j,i-1])              #Potential output (endogenous)
      y_g[j,i] = y_s[j,i-1] - y_n[j,i]                                         #Output gap
      
      #Banks, initial finance and final finance (funding)
      fin_i[j,i] = wb[j,i]                                                      #Initial finance to production 
      fin_f[j,i] = c[j,i] + (bs[j,i] - bs[j,i-1]) - (paym_l[j,i] + paymw_b[j,i] + paymz_b[j,i] + pf[j,i])   #Final finance obtained by firms 
      ld[j,i] = ld[j,i-1] + fin_i[j,i] - fin_f[j,i]                            #Stock of debt (bank loans) of firms at the end of the period
      ls[j,i] = ls[j,i-1] + (ld[j,i] - ld[j,i-1])                              #Supply of bank loans
      ms[j,i] = ms[j,i-1] + (ls[j,i] - ls[j,i-1])                              #Supply of bank deposits    
      pb[j,i] = paym_l[j,i] - paymw_m[j,i] - paymz_m[j,i]                      #Bank profit
      rm[j,i] = r_bar                                                          #Rate of interest on deposits
      rl[j,i] = r_bar                                                          #Rate of interest on bank loans
      
      #Employment and wages
      wb[j,i] = w*n[j,i]        #Wage bill  
      n[j,i] = y_r[j,i]/pr                                                     #Labor demand
      
      #Circuit-based interest payments 
      paym_l[j,i] = rl[j,i-1]*ld[j,i-1] + iota[j,i]*rl[j,i-1]*fin_f[j,i-1]/2   #Interest payments on bank loans (including average interest payments on repaid share of loans)
      paymw_m[j,i] = rm[j,i-1]*mw[j,i-1] + iota[j,i]*(mw[j,i-1]/mh[j,i-1])*rm[j,i-1]*fin_f[j,i-1]/2  #Interest payments on bank deposits (see above) to workers
      paymz_m[j,i] = rm[j,i-1]*mz[j,i-1]                                       #Interest payments on bank deposits (see above) to capitalists
      paym_m[j,i] = paymw_m[j,i] + paymz_m[j,i]                                #Total interest payments on bank deposits
      paymw_b[j,i] = rb[j,i-1]*bw[j,i-1]                                       #Interest payments on private securities to workers
      paymz_b[j,i] = rb[j,i-1]*bz[j,i-1]                                       #Interest payments on private securities to capitalists
      paym_b[j,i] = paymw_b[j,i] + paymz_b[j,i]                                #Total interest payments on private securities
      
      #Labor-value categories
      nlt[j,i] = cw_r[j,i]/pr                                                  #Necessary labor time
      slt[j,i] = (cz_r[j,i]+id_r[j,i])/pr                                      #Surplus labor time
      expl[j,i] = slt[j,i]/nlt[j,i]                                            #Exploitation rate
      
      #Expectations
      if(i<=3){p_e[j,i] = 1}
      else{p_e[j,i] = p[j,i-1] + psi0 + psi1*(p[j,i-1] - p_e[j,i-1])}          #Expected price (adaptive)
    
      if(i<=3){ydw_e[j,i] = ydw[j,i-1]}
      else{ydw_e[j,i] = ydw[j,i-1] + psi0 + psi1*(ydw[j,i-1] - ydw_e[j,i-1])}  #Expected disposable income of workers (adaptive)
      
      if(i<=3){ydz_e[j,i] = ydz[j,i-1]}
      else{ydz_e[j,i] = ydz[j,i-1] + psi0 + psi1*(ydz[j,i-1] - ydz_e[j,i-1])}  #Expected disposable income of workers (adaptive)
      
      
    }
  }
}

#Save steady-state values
ydw_0 = ydw[1,nPeriods]
ydz_0 = ydz[1,nPeriods]
yd_0 = yd[1,nPeriods]
vw_0 = vw[1,nPeriods]
vz_0 = vz[1,nPeriods]
vh_0 = vh[1,nPeriods]
bw_0 = bw[1,nPeriods]
bz_0 = bz[1,nPeriods]
bh_0 = bh[1,nPeriods]
mw_0 = mw[1,nPeriods]
mz_0 = mz[1,nPeriods]
mh_0 = mh[1,nPeriods]
cw_r_t_0 = cw_r_t[1,nPeriods]
cw_r_0 = cw_r[1,nPeriods]
cz_r_0 = cz_r[1,nPeriods]
c_r_0 = c_r[1,nPeriods]
cw_0 = cw[1,nPeriods]
cz_0 = cz[1,nPeriods]
c_0 = c[1,nPeriods]
y_r_0 = y_r[1,nPeriods]
y_0 = y[1,nPeriods]
idt_0 = idt[1,nPeriods]
id_r_0 = id_r[1,nPeriods]
id_0 = id[1,nPeriods]#
da_0 = da[1,nPeriods]
af_0 = af[1,nPeriods]
k_0 = k[1,nPeriods]
pf_0 = pf[1,nPeriods]
bs_0 = bs[1,nPeriods]
rb_0 = rb[1,nPeriods]
y_s_0 = y_s[1,nPeriods]
p_0 = p[1,nPeriods]
y_n_0 = y_n[1,nPeriods]
y_g_0 = y_g[1,nPeriods]
fin_i_0 = fin_i[1,nPeriods]
fin_f_0 = fin_f[1,nPeriods]
ld_0 = ld[1,nPeriods]
ls_0 = ls[1,nPeriods]
ms_0 = ms[1,nPeriods]
pb_0 = pb[1,nPeriods]
rm_0 = rm[1,nPeriods]
rl_0 = rl[1,nPeriods]
wb_0 = wb[1,nPeriods]
n_0 = n[1,nPeriods]
paym_l_0 = paym_l[1,nPeriods]
paymw_m_0 = paymw_m[1,nPeriods]
paymz_m_0 = paymz_m[1,nPeriods]
paym_m_0 = paym_m[1,nPeriods]
paymw_b_0 = paymw_b[1,nPeriods]
paymz_b_0 = paymz_b[1,nPeriods]
paym_b_0 = paym_b[1,nPeriods]
nlt_0 = nlt[1,nPeriods]
slt_0 = slt[1,nPeriods]
expl_0 = expl[1,nPeriods]
p_e_0 = p_e[1,nPeriods]

#Consistency check (redundant equation)
plot(mh[1,2:nPeriods]-ms[1,2:nPeriods], type="l", col=1,lwd=5,lty=1, font.main=1,cex.main=1,main="Consistency check: demand for deposits - supply of deposits", ylab = '$',xlab = '', ylim = range(-0.1,0.1))
lines(mh[2,2:nPeriods]-ms[2,2:nPeriods], type="l",col=2,lwd=5,lty=2)
lines(mh[3,2:nPeriods]-ms[3,2:nPeriods], type="l",col=3,lwd=5,lty=3)
lines(mh[4,2:nPeriods]-ms[4,2:nPeriods], type="l",col=4,lwd=5,lty=4)
lines(mh[5,2:nPeriods]-ms[5,2:nPeriods], type="l",col=5,lwd=5,lty=5)
lines(mh[6,2:nPeriods]-ms[6,2:nPeriods], type="l",col=5,lwd=5,lty=6)

################################################################################

# Create BS and TFM

#Upload libraries
library(knitr)

#Choose a period
yr=15

#Choose the scenario
j=2

#BS matrix

#Create row names for BS matrix
rownames <-c( "Bank deposits",
              "Loans",
              "Fixed capital",
              "Corporate securities",
              "Balance (net worth)",
              "Column_Total"
)

#Create workers aggregates
Workers <- c( round(mw[j,yr],digits=2) ,                                                                    
              0,
              0,
              round(bw[j,yr],digits=2),
              round(-vw[j,yr],digits=2),
              round(mw[j,yr]+mw[j,yr]-vw[j,yr],digits=2)
)                                                                    

#Create table of results
WorkDataBS<-as.data.frame(Workers,row.names=rownames)
kable(WorkDataBS)

#Create capitalists aggregates
Capitalists <- c( round(mz[j,yr],digits=2) ,                                                                    
                  0,
                  0,
                  round(bz[j,yr],digits=2),
                  round(-vz[j,yr],digits=2),
                  round(mz[j,yr]+mz[j,yr]-vz[j,yr],digits=2)
)                                                                    

#Create table of results
CapDataBS<-as.data.frame(Capitalists,row.names=rownames)
kable(CapDataBS)

#Create firms aggregates
Firms <- c(0,
           round(-ld[j,yr],digits=2),
           round(k[j,yr],digits=2),
           round(-bs[j,yr],digits=2),
           0,
           round(-ld[j,yr]+k[j,yr]-bs[j,yr],digits=2)
)                                                                    

#Create table of results
FirmsDataBS<-as.data.frame(Firms,row.names=rownames)
kable(FirmsDataBS)

#Create banks aggregates
Banks <- c(round(-ms[j,yr],digits=2),
           round(ls[j,yr],digits=2),
           0,
           0,
           0,
           round(-ms[j,yr]+ls[j,yr],digits=2)
)                                                                    

#Create table of results
BanksDataBS<-as.data.frame(Banks,row.names=rownames)
kable(BanksDataBS)

#Create total aggregates
Row_Total <- c(round(mw[j,yr]+mz[j,yr]-ms[j,yr],digits=2),                                                                    
               round(ld[j,yr]-ls[j,yr],digits=2),
               round(k[j,yr],digits=2),
               round(bw[j,yr]+bz[j,yr]-bs[j,yr],digits=2),
               round(-vw[j,yr]-vz[j,yr],digits=2),
               round(-vw[j,yr]-vz[j,yr]+k[j,yr],digits=2)
)                                                                    

#Create table of results
TotDataBS<-as.data.frame(Row_Total,row.names=rownames)
kable(TotDataBS)

#Create BS matrix
BS_Matrix<-cbind(WorkDataBS,CapDataBS,FirmsDataBS,BanksDataBS,TotDataBS)
kable(BS_Matrix,align='ccccc') 

#####################

#TFM matrix

#Create row names for TFM matrix
rownames <-c( "Consumption",
              "Investment",
              "[Memo: Production]",
              "Wages",
              "Depreciation / Amortisation",
              "Firms' profit",
              "Banks' profit",
              "Interests on loans",
              "Interests on deposits",
              "Interests on securities",
              "Change in loans",
              "Change in deposits",
              "Change in securities",
              "Column_Total"
)


#Create workers aggregates
Workers <- c(round(-cw[j,yr], digits = 2),                                                                    
             0,
             0,
             round(wb[j,yr], digits = 2),
             0,
             0,
             0,
             0,
             round(paymw_m[j,yr], digits = 2),
             round(paymw_b[j,yr], digits = 2),
             0,
             round((-mw[j,yr]+mw[j,yr-1]), digits = 2),
             round((-bw[j,yr]+mw[j,yr-1]), digits = 2),
             round((-cw[j,yr]+wb[j,yr]+paymw_m[j,yr]+paymw_b[j,yr]-mw[j,yr]+mw[j,yr-1]-bw[j,yr]+bw[j,yr-1]), digits = 2)
)

#Create table of results
WorkDataTFM<-as.data.frame(Workers,row.names=rownames)
kable(WorkDataTFM)

#Create capitalists aggregates
Capitalists <- c(round(-cz[j,yr], digits = 2),                                                                    
                 0,
                 0,
                 0,
                 0,
                 round(pf[j,yr], digits = 2),
                 round(pb[j,yr], digits = 2),
                 0,
                 round(paymz_m[j,yr], digits = 2),
                 round(paymz_b[j,yr], digits = 2),
                 0,
                 round((-mz[j,yr]+mw[j,yr-1]), digits = 2),
                 round((-bz[j,yr]+mw[j,yr-1]), digits = 2),
                 round((-cz[j,yr]+pf[j,yr]+pb[j,yr]+paymz_m[j,yr]+paymz_b[j,yr]-mz[j,yr]+mz[j,yr-1]-bz[j,yr]+bz[j,yr-1]), digits = 2)
)

#Create table of results
CapDataTFM<-as.data.frame(Capitalists,row.names=rownames)
kable(CapDataTFM)

#Create firms aggregates
Firms_current <- c(round(c[j,yr], digits = 2),                                                                    
                   round(id[j,yr], digits = 2),
                   paste("[",round(y[j,yr], digits = 2),"]"),
                   round(-wb[j,yr], digits = 2),
                   round(-af[j,yr], digits = 2),
                   round(-pf[j,yr], digits = 2),
                   0,
                   round(-paym_l[j,yr], digits = 2),
                   0,
                   round(-paym_b[j,yr], digits = 2),
                   0,
                   0,
                   0,
                   round(c[j,yr]+id[j,yr]-wb[j,yr]-af[j,yr]-pf[j,yr]-paym_l[j,yr]-paym_b[j,yr], digits=2)
)

#Create table of results
FirmsDataTFM<-as.data.frame(Firms_current,row.names=rownames)
kable(FirmsDataTFM)

#Create firms (capital) aggregates
Firms_capital <- c(0,                                                                    
                   round(-id[j,yr], digits = 2),
                   0,
                   0,
                   round(af[j,yr], digits = 2),
                   0,
                   0,
                   0,
                   0,
                   0,
                   round(ld[j,yr]-ld[j,yr-1], digits = 2),
                   0,
                   round(bs[j,yr]-bs[j,yr-1], digits = 2),
                   round(-id[j,yr]+af[j,yr]+ld[j,yr]-ld[j,yr-1]+bs[j,yr]-bs[j,yr-1], digits=2)
)

#Create table of results
CapitalDataTFM<-as.data.frame(Firms_capital,row.names=rownames)
kable(CapitalDataTFM)


#Create banks aggregates
Banks   <- c(0,                                                                    
             0,
             0,
             0,
             0,
             0,
             round(-pb[j,yr], digits = 2),
             round(paym_l[j,yr], digits = 2),
             round(-paym_m[j,yr], digits = 2),
             0,
             round(-ls[j,yr]+ls[j,yr-1],digits = 2),
             round(ms[j,yr]-ms[j,yr-1],digits = 2),
             0,
             round(-pb[j,yr]+paym_l[j,yr]-paym_m[j,yr]-ls[j,yr]+ls[j,yr-1]+ms[j,yr]-ms[j,yr-1], digits=2)
)

#Create table of results
BanksDataTFM<-as.data.frame(Banks,row.names=rownames)
kable(BanksDataTFM)

#Create total aggregates
Row_Total  <- c( round(-cw[j,yr]-cz[j,yr]+c[j,yr], digits = 2),                                                                    
                 0,
                 0,
                 0,
                 0,
                 0,
                 0,
                 0,
                 0,
                 0,
                 round(ld[j,yr]-ld[j,yr-1]-ls[j,yr]+ls[j,yr-1], digits = 2),
                 round(-mw[j,yr]+mw[j,yr-1]-mz[j,yr]+mz[j,yr-1]+ms[j,yr]-ms[j,yr-1], digits = 2),
                 round(-bw[j,yr]+bw[j,yr-1]-bz[j,yr]+bz[j,yr-1]+bs[j,yr]-bs[j,yr-1], digits = 2),
                 round(ld[j,yr]-ld[j,yr-1]-ls[j,yr]+ls[j,yr-1]-mw[j,yr]+mw[j,yr-1]-mz[j,yr]+mz[j,yr-1]+ms[j,yr]-ms[j,yr-1]-bw[j,yr]+bw[j,yr-1]-bz[j,yr]+bz[j,yr-1]+bs[j,yr]-bs[j,yr-1], digits = 2)
)

#Create table of results
TotDataTFM<-as.data.frame(Row_Total,row.names=rownames)
kable(TotDataTFM)

#Create TFM matrix
TFM_Matrix<-cbind(WorkDataTFM,CapDataTFM,FirmsDataTFM,CapitalDataTFM,BanksDataTFM,TotDataTFM)
kable(TFM_Matrix,align='cccccc') 

################################################################################

#Create charts

################################################################################

#Create Sankey diagram

#Upload libraries for Sankey diagram
library(networkD3)
library(htmlwidgets)
library(htmltools)

#Create nodes: source, target and flows
nodes = data.frame("name" = 
                     c("Firms outflow (current)", # Node 0
                       "Workers outflow", # Node 1
                       "Capitalists outflow", # Node 2
                       "Banks outflow", # Node 3
                       "Firms outflow (capital)", # Node 4
                       "Firms inflow (current)", # Node 5
                       "Workers inflow", # Node 6
                       "Capitalists inflow", # Node 7
                       "Banks inflow", # Node 8
                       "Firms inflow (capital)", # Node 9
                       "Wages", # Node 10
                       "Firms profit", # Node 11
                       "Consumption", # Node 12
                       "Interests on loans", # Node 13
                       "Depreciation", # Node 14
                       "Interests on securities", # Node 15
                       "Interests on deposits", # Node 16
                       "Deposits (change)", # Node 17
                       "Securities (change)", # Node 18
                       "Loans (change)", # Node 19
                       "Investment")) # Node 20

#Create the flows
links = as.data.frame(matrix(c(
  0, 10, wb[3,10],  
  0, 11, pf[3,10],   
  0, 13, paym_l[3,10],
  0, 14, af[3,10],
  0, 15, paym_b[3,10],
  1, 12, cw[3,10],
  1, 17, mw[3,10]-mw[3,9],
  1, 18, bw[3,10]-bw[3,9],
  2, 12, cz[3,10],
  2, 17, mz[3,10]-mz[3,9],
  2, 18, bz[3,10]-bz[3,9],
  3, 16, paym_m[3,10],
  3, 19, ls[3,10]-ls[3,9],
  4, 20, id[3,10],
  10, 6, wb[3,10],   
  11, 7, pf[3,10],
  12, 5, c[3,10],
  13, 8, paym_l[3,10],
  14, 9, af[3,10],
  15, 6, paymw_b[3,10],
  15, 7, paymz_b[3,10],
  16, 6, paymw_m[3,10],
  16, 7, paymz_m[3,10],
  17, 8, ms[3,10]-ms[3,9],
  18, 9, bs[3,10]-bs[3,9],
  19, 9, ld[3,10]-ld[3,9],
  20, 5, id[3,10]

), 

#Note: each row represents a link. The first number represents the node being
#connected from. The second number represents the node connected to. The third
#number is the value of the node.  


byrow = TRUE, ncol = 3))
names(links) = c("source", "target", "value")
my_color <- 'd3.scaleOrdinal() .domain([]) .range(["chartreuse","red","green","yellow","dodgerblue","darkorchid","khaki","peru","violet","cyan","pink","orange","beige"])'

#Create and plot the network
sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name", colourScale=my_color,
              fontSize= 20, nodeWidth = 30)


################################################################################

#Figure 1 - Baseline

layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
par(mar = c(5, 4, 4, 4) + 0.3)  

#Figure 1a
plot(y_r[2,2:100],type="l",col=1,lwd=2,lty=1,font.main=1.5,cex.main=1.5,cex.axis=1.5,cex.lab=1.5,main="(a) Production, consumption, and investment",ylab = '$ (const. p)',xlab = '',ylim=range(min(id[2,2:100]),max(y_r[2,2:100])))
abline(h=data00$y_r_0,col=1,lwd=1,lty=2)
lines(c_r[2,2:100],type="l",col=2,lwd=2,lty=1)
abline(h=data00$c_r_0,col=2,lwd=1,lty=2)
lines(id_r[2,2:100],type="l",col=3,lwd=2,lty=1)
abline(h=data00$id_r_0,col=3,lwd=1,lty=2)
legend("right",c("Real output","Real consumption","Real investment"),  bty = "n", cex = 1.5, lty=c(1,1,1), lwd=c(2,2,2,1), col = c(1,2,3), box.lwd=0)

#Figure 1b
plot(fin_i[2,2:100],type="l",col=4,lwd=2,lty=1,font.main=1.5,cex.main=1.5,cex.axis=1.5,cex.lab=1.5,main="(b) The monetary circuit",ylab = '$ (flow)',xlab = '',ylim=range(min(fin_i[2,2:100]),max(fin_i[2,2:100])))
abline(h=data00$fin_i_0,col=8,lwd=1,lty=2)
lines(fin_f[2,2:100],type="l",col=5,lwd=2,lty=2)
par(new = TRUE)
plot(ld[2,2:100],type="l",col=8,lwd=2,lty=1,axes = FALSE, bty = "n", xlab = "", ylab = "",  ylim=range(min(ld[2,2:100]),max(ld[2,2:100])))
axis(4)
mtext("$ (stock)", side=4, line=3)
legend("bottomright",c("Initial finance","Final finance","Outstanding debt (right axis)"),  bty = "n", cex = 1.5, lty=c(1,2,1), lwd=c(2,2,2,1), col = c(4,5,8), box.lwd=0)

#Figure 1c
plot(p[1,2:100],type="l",col=2,lwd=2,lty=1,font.main=1.5,cex.main=1.5,cex.axis=1.5,cex.lab=1.5,main="(c) Price level",ylab = '$',xlab = '',ylim=range(0.97,1.03))
legend("bottomright",c("Unit price of output"),  bty = "n", cex = 1.5, lty=c(1), lwd=c(2), col = c(2), box.lwd=0)

#Figure 1d
#plot(mh[1,2:100]-ms[1,2:100],type="l",col="#009999",lwd=2,lty=1,font.main=1.5,cex.main=1.5,cex.axis=1.5,cex.lab=1.5,main="(d) Redundant equation",ylab = '$',xlab = '',ylim=range(-1,1))
#legend("bottomright",c("Supply of deposits - demand for deposits"),  bty = "n", cex = 1.5, lty=c(1), lwd=c(2), col = c("#009999"), box.lwd=0)

#Figure 1d - alternative
plot(100*cw_r[2,2:100]/cw_r[1,2:100],type="l",col="purple",lwd=2,lty=1,font.main=1.5,cex.main=1.5,cex.axis=1.5,cex.lab=1.5,main="(d) Real consumption of workers",ylab = 'Index',xlab = '',ylim=range(100,110))
lines(100*cw_r_t[2,2:100]/cw_r_t[1,2:100],type="l",col="orange",lwd=2,lty=2)
lines(100*cw_r_t[2,2:100]/cw_r_t[1,2:100],type="l",col="red1",lwd=2,lty=3)
legend("right",c("Actual","Expected","Planned"),  bty = "n", cex = 1.5, lty=c(1,2,3), lwd=c(2,2,2), col = c("purple","orange","red1"), box.lwd=0)

################################################################################

#Figure 2 - Shock to investment (quantity adjustment)

layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
par(mar = c(5, 4, 4, 4) + 0.3)  

#Figure 2a
plot(100*y_r[3,2:100]/y_r[1,2:100],type="l",col=1,lwd=2,lty=1,font.main=1.5,cex.main=1.5,cex.axis=1.5,cex.lab=1.5,main="(a) Production, consumption, and investment (real)",ylab = 'Index',xlab = '',ylim=range(96,126))
lines(100*cw_r[3,2:100]/cw_r[1,2:100],type="l",col=2,lwd=2,lty=1)
lines(100*cz_r[3,2:100]/cz_r[1,2:100],type="l",col=3,lwd=2,lty=1)
lines(100*id_r[3,2:100]/id_r[1,2:100],type="l",col=4,lwd=2,lty=1)
legend("topright",c("Output","Workers' consumption","Capitalists' consumption","Real investment"),  bty = "n", cex = 1.5, lty=c(1,1,1,1), lwd=c(2,2,2,2), col = c(1,2,3,4), box.lwd=0)

#Figure 2b
plot(100*fin_i[3,2:100]/fin_i[1,2:100],type="l",col=4,lwd=2,lty=1,font.main=1.5,cex.main=1.5,cex.axis=1.5,cex.lab=1.5,main="(b) The monetary circuit",ylab = 'Index (flow)',xlab = '',ylim=range(100,115))
lines(100*fin_f[3,2:100]/fin_f[1,2:100],type="l",col=5,lwd=2,lty=2)
par(new = TRUE)
plot(100*ld[3,2:100]/ld[1,2:100],type="l",col=8,lwd=2,lty=1,axes = FALSE, bty = "n", xlab = "", ylab = "")
axis(4)
mtext("Index (stock)", side=4, line=3)
legend("right",c("Initial finance","Final finance","Outstanding debt (right axis)"),  bty = "n", cex = 1.5, lty=c(1,2,1), lwd=c(2,2,2,1), col = c(4,5,8), box.lwd=0)

#Figure 2c
plot(p[3,2:100],type="l",col=2,lwd=2,lty=1,font.main=1.5,cex.main=1.5,cex.axis=1.5,cex.lab=1.5,main="(c) Price level",ylab = '$',xlab = '',ylim=range(0.98,1.02))
legend("bottomright",c("Unit price of output"),  bty = "n", cex = 1.5, lty=c(1), lwd=c(2), col = c(2), box.lwd=0)

#Figure 2d
plot(100*cw_r[3,2:100]/cw_r[1,2:100],type="l",col="purple",lwd=2,lty=1,font.main=1.5,cex.main=1.5,cex.axis=1.5,cex.lab=1.5,main="(d) Real consumption of workers",ylab = 'Index',xlab = '',ylim=range(100,115))
lines(100*cw_r_t[3,2:100]/cw_r_t[1,2:100],type="l",col="orange",lwd=2,lty=2)
lines(100*cw_r_t[3,2:100]/cw_r_t[1,2:100],type="l",col="red1",lwd=2,lty=3)
legend("right",c("Actual","Expected","Planned"),  bty = "n", cex = 1.5, lty=c(1,2,3), lwd=c(2,2,2), col = c("purple","orange","red1"), box.lwd=0)

################################################################################

#Figure 3 - Shock to investment (mixed adjustment)

layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
par(mar = c(5, 4, 4, 4) + 0.3)  

#Figure 3a
plot(100*y_r[4,2:100]/y_r[1,2:100],type="l",col=1,lwd=2,lty=1,font.main=1.5,cex.main=1.5,cex.axis=1.5,cex.lab=1.5,main="(a) Production, consumption, and investment (real)",ylab = 'Index',xlab = '',ylim=range(90,150))
lines(100*cw_r[4,2:100]/cw_r[1,2:100],type="l",col=2,lwd=2,lty=1)
lines(100*cz_r[4,2:100]/cz_r[1,2:100],type="l",col=3,lwd=2,lty=1)
lines(100*id_r[4,2:100]/id_r[1,2:100],type="l",col=4,lwd=2,lty=1)
legend("topright",c("Output","Workers' consumption","Capitalists' consumption","Investment"),  bty = "n", cex = 1.5, lty=c(1,1,1,1), lwd=c(2,2,2,2), col = c(1,2,3,4), box.lwd=0)

#Figure 3b
plot(100*fin_i[4,2:100]/fin_i[1,2:100],type="l",col=4,lwd=2,lty=1,font.main=1.5,cex.main=1.5,cex.axis=1.5,cex.lab=1.5,main="(b) The monetary circuit",ylab = 'Index (flow)',xlab = '',ylim=range(98,130))
lines(100*fin_f[4,2:100]/fin_f[1,2:100],type="l",col=5,lwd=2,lty=2)
par(new = TRUE)
plot(100*ld[4,2:100]/ld[1,2:100],type="l",col=8,lwd=2,lty=1,axes = FALSE, bty = "n", xlab = "", ylab = "",ylim=range(98,130))
axis(4)
mtext("Index (stock)", side=4, line=3)
legend("bottomright",c("Initial finance","Final finance","Outstanding debt (right axis)"),  bty = "n", cex = 1.5, lty=c(1,2,1), lwd=c(2,2,2,1), col = c(4,5,8), box.lwd=0)

#Figure 3c
plot(p[4,2:100],type="l",col=2,lwd=2,lty=1,font.main=1.5,cex.main=1.5,cex.axis=1.5,cex.lab=1.5,main="(c) Price level",ylab = '$',xlab = '',ylim=range(1,1.1))
legend("right",c("Unit price of output"),  bty = "n", cex = 1.5, lty=c(1), lwd=c(2), col = c(2), box.lwd=0)

#Figure 3d
plot(100*cw_r[4,2:100]/cw_r[1,2:100],type="l",col="purple",lwd=2,lty=1,font.main=1.5,cex.main=1.5,cex.axis=1.5,cex.lab=1.5,main="(d) Real consumption of workers",ylab = 'Index',xlab = '',ylim=range(93,115))
lines(100*cw_r_t[4,2:100]/cw_r_t[1,2:100],type="l",col="orange",lwd=2,lty=2)
lines(100*cw_r_t[3,2:100]/cw_r_t[1,2:100],type="l",col="red1",lwd=2,lty=3)
legend("right",c("Actual","Expected","Planned"),  bty = "n", cex = 1.5, lty=c(1,2,3), lwd=c(2,2,2), col = c("purple","orange","red1"), box.lwd=0)

################################################################################

#Figure 4 - Shock to investment (price adjustment)

layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
par(mar = c(5, 4, 4, 4) + 0.3)  

#Figure 4a
plot(100*y_r[5,2:100]/y_r[1,2:100],type="l",col=1,lwd=2,lty=1,font.main=1.5,cex.main=1.5,cex.axis=1.5,cex.lab=1.5,main="(a) Production, consumption, and investment (real)",ylab = 'Index',xlab = '',ylim=range(70,250))
lines(100*cw_r[5,2:100]/cw_r[1,2:100],type="l",col=2,lwd=2,lty=1)
lines(100*cz_r[5,2:100]/cz_r[1,2:100],type="l",col=3,lwd=2,lty=1)
lines(100*id_r[5,2:100]/id_r[1,2:100],type="l",col=4,lwd=2,lty=1)
legend("right",c("Output","Workers' consumption","Capitalists' consumption","Investment"),  bty = "n", cex = 1.5, lty=c(1,1,1,1), lwd=c(2,2,2,2), col = c(1,2,3,4), box.lwd=0)

#Figure 4b
plot(100*fin_i[5,2:100]/fin_i[1,2:100],type="l",col=4,lwd=2,lty=1,font.main=1.5,cex.main=1.5,cex.axis=1.5,cex.lab=1.5,main="(b) The monetary circuit",ylab = 'Index (flow)',xlab = '',ylim=range(98,130))
lines(100*fin_f[5,2:100]/fin_f[1,2:100],type="l",col=5,lwd=2,lty=2)
par(new = TRUE)
plot(100*ld[5,2:100]/ld[1,2:100],type="l",col=8,lwd=2,lty=1,axes = FALSE, bty = "n", xlab = "", ylab = "",ylim=range(98,130))
axis(4)
mtext("Index (stock)", side=4, line=3)
legend("right",c("Initial finance","Final finance","Outstanding debt (right axis)"),  bty = "n", cex = 1.5, lty=c(1,2,1), lwd=c(2,2,2,1), col = c(4,5,8), box.lwd=0)

#Figure 4c
plot(p[5,2:100],type="l",col=2,lwd=2,lty=1,font.main=1.5,cex.main=1.5,cex.axis=1.5,cex.lab=1.5,main="(c) Price level",ylab = '$',xlab = '',ylim=range(1,1.30))
legend("right",c("Unit price of output"),  bty = "n", cex = 1.5, lty=c(1), lwd=c(2), col = c(2), box.lwd=0)

#Figure 4d
plot(100*cw_r[5,2:100]/cw_r[1,2:100],type="l",col="purple",lwd=2,lty=1,font.main=1.5,cex.main=1.5,cex.axis=1.5,cex.lab=1.5,main="(d) Real consumption of workers",ylab = 'Index',xlab = '',ylim=range(75,115))
lines(100*cw_r_t[5,2:100]/cw_r_t[1,2:100],type="l",col="orange",lwd=2,lty=2)
lines(100*cw_r_t[3,2:100]/cw_r_t[1,2:100],type="l",col="red1",lwd=2,lty=3)
legend("right",c("Actual","Expected","Planned"),  bty = "n", cex = 1.5, lty=c(1,2,3), lwd=c(2,2,2), col = c("purple","orange","red1"), box.lwd=0)

################################################################################

#Figure 5

layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
par(mar = c(5, 4, 4, 4) + 0.3)  

#Figure 5a
plot(100*cw_r[5,2:100]/cw_r[1,2:100],type="l",col="purple",lwd=2,lty=1,font.main=1.5,cex.main=1.5,cex.axis=1.5,cex.lab=1.5,main="(a) Consumption of workers",ylab = 'Index',xlab = '',ylim=range(75,115))
lines(100*cw[5,2:100]/cw[1,2:100],type="l",col="orange",lwd=2,lty=2)
lines(100*cw_r[5,2:100]/cw_r[1,2:100],type="l",col="purple",lwd=2,lty=1)
legend("topright",c("Real","Nominal"),  bty = "n", cex = 1.5, lty=c(1,2), lwd=c(2,2), col = c("purple","orange"), box.lwd=0)

#Figure 5b
plot((ydw[5,2:100]-cw[5,2:100])/p[5,2:100]-(ydw[1,2:100]-cw[1,2:100])/p[1,2:100],type="l",col="purple",lwd=2,lty=1,font.main=1.5,cex.main=1.5,cex.axis=1.5,cex.lab=1.5,main="(b) Saving of workers (difference with baseline)",ylab = '$',xlab = '',ylim=range(-3,2))
lines((ydw[5,2:100]-cw[5,2:100])-(ydw[1,2:100]-cw[1,2:100]),type="l",col="orange",lwd=2,lty=2)
lines((ydw[5,2:100]-cw[5,2:100])/p[5,2:100]-(ydw[1,2:100]-cw[1,2:100])/p[1,2:100],type="l",col="purple",lwd=2,lty=1)
legend("topright",c("Real","Nominal"),  bty = "n", cex = 1.5, lty=c(1,2), lwd=c(2,2), col = c("purple","orange"), box.lwd=0)

#Figure 5c
plot(100*cz_r[5,2:100]/cz_r[1,2:100],type="l",col="purple",lwd=2,lty=1,font.main=1.5,cex.main=1.5,cex.axis=1.5,cex.lab=1.5,main="(c) Consumption of capitalists",ylab = 'Index',xlab = '',ylim=range(100,300))
lines(100*cz[5,2:100]/cz[1,2:100],type="l",col="orange",lwd=2,lty=2)
lines(100*cz_r[5,2:100]/cz_r[1,2:100],type="l",col="purple",lwd=2,lty=1)
legend("bottomright",c("Real","Nominal"),  bty = "n", cex = 1.5, lty=c(1,2), lwd=c(2,2), col = c("purple","orange"), box.lwd=0)

#Figure 4d
plot((ydz[5,2:100]-cz[5,2:100])/p[5,2:100]-(ydz[1,2:100]-cz[1,2:100])/p[1,2:100],type="l",col="purple",lwd=2,lty=1,font.main=1.5,cex.main=1.5,cex.axis=1.5,cex.lab=1.5,main="(d) Saving of capitalists (difference with baseline)",ylab = '$',xlab = '',ylim=range(0,6))
lines((ydz[5,2:100]-cz[5,2:100])-(ydz[1,2:100]-cz[1,2:100]),type="l",col="orange",lwd=2,lty=2)
lines((ydz[5,2:100]-cz[5,2:100])/p[5,2:100]-(ydz[1,2:100]-cz[1,2:100])/p[1,2:100],type="l",col="purple",lwd=2,lty=1)
legend("topright",c("Real","Nominal"),  bty = "n", cex = 1.5, lty=c(1,2), lwd=c(2,2), col = c("purple","orange"), box.lwd=0)

################################################################################

#Figure 6

layout(matrix(c(1,2), 1, 2, byrow = TRUE))
par(mar = c(5, 4, 4, 4) + 0.3)  

#Figure 6a
plot(slt[1,2:49], type="l",lwd=2,lty=1,font.main=1.5,cex.main=1.5,cex.axis=1.5,cex.lab=1.5,main="(a) Surplus-labour time",ylab = 'N',xlab = '',ylim=range(min(slt[1,2:nPeriods]),max(slt[5,2:49])))
lines(slt[2,2:49], type="l",col=2,lwd=2,lty=1)
lines(slt[3,2:49], type="l",col=3,lwd=2,lty=1)
lines(slt[4,2:49], type="l",col=4, lwd=2,lty=1)
lines(slt[5,2:49], type="l",col="orange", lwd=2,lty=1)
lines(slt[1,2:49], type="l",col=1, lwd=2,lty=1)
legend("right",c("Baseline","Scenario 2","Scenario 3","Scenario 4","Scenario 5"),  bty = "n", cex = 1.5, lty=c(1,1,1,1,1), lwd=c(2,2,2,2,2), col = c(1,2,3,4,"orange"), box.lwd=0)

#Figure 6b
plot(100*expl[1,2:49], type="l",lwd=2,lty=1,font.main=1.5,cex.main=1.5,cex.axis=1.5,cex.lab=1.5,main="(b) Exploitation rate",ylab = '%',xlab = '',ylim=range(100*min(expl[1,2:nPeriods]),100*max(expl[5,2:49])))
lines(100*expl[2,2:49], type="l",col=2,lwd=2,lty=1)
lines(100*expl[3,2:49], type="l",col=3,lwd=2,lty=1)
lines(100*expl[4,2:49], type="l",col=4, lwd=2,lty=1)
lines(100*expl[5,2:49], type="l",col="orange", lwd=2,lty=1)
lines(100*expl[1,2:49], type="l",col=1, lwd=2,lty=1)
legend("right",c("Baseline","Scenario 2","Scenario 3","Scenario 4","Scenario 5"),  bty = "n", cex = 1.5, lty=c(1,1,1,1,1), lwd=c(2,2,2,2,2), col = c(1,2,3,4,"orange"), box.lwd=0)
