hypergeometric_distribution<-function(dane, CI=0.95, wykres=TRUE, proporcja=TRUE, factor_effective=1) {
  # iterate between minumum possible number of ants from the upper part of the nest
  # and maximum possible number
  for (i in max(removed_total-N2,removed_1):min((removed_total-removed_2),N1)){
    counter<-counter+1
    proportion1[counter]<-round(i/n, digits=3)
    N1<-i
    N2<-n-i
    # z twierdzenia Bayesa mamy, że P(N0|r1,r2)=(P(r1|N0)*P(r2|N0)*P(N0))/(P(r1)*P(r2))
    probab[counter]<-dhyper(r1, m1, group1-m1, N1)*dhyper(r2, m2, group2-m2, N2)
    dane_zbiorcze<-rbind(dane_zbiorcze, data.frame("probability"=probab[counter],
                                                   "N_1"=N1, "marked_1"=r1, "N_2"=N2, "marked_2"=r2))
  }
  #normalizacja prawdopodobieństw do 1
  dane_zbiorcze[,"probability"]<-dane_zbiorcze[,"probability"]/sum(na.omit(dane_zbiorcze[,"probability"])) 
  prob_max<-which.max(dane_zbiorcze$probability)
  cumprob<-confidence_interval(dane_zbiorcze, alpha=1-CI)
  #wybieramy wartość N1, dla której prawdopodobieństwo łączne jest największe
  N1_optim<-dane_zbiorcze[prob_max,"N_1"] 
  N2_optim<-dane_zbiorcze[prob_max,"N_2"]
  r1_optim=round((N1_optim/group1)*m1)
  r2_optim=round((N2_optim/group2)*m2)
  if (wykres==TRUE) plot(proportion1, probab)
  #oblicza ile razy prawdopodobienstwo wylosowania dowolnego osobnika z grupy 1 jest mniejsze/wieksze niz prawdopodobienstwa wylosowania osobnika z grupy 
  #2 w sytuacji, gdy liczebnosc grup jest rowna; relative_prob=(przewidywana wielkosc kolonii -  
  #wielkosc  grupy 2)/wielkosc grupy 1
  relative_prob<-((group2*(1/(abs(1-(cumprob$proportion-10^-10)))))-group2)/group1 
  
  cumprob<-cbind(cumprob, "wzgledne_prawdopodobienstwo"=relative_prob)
  if (proporcja)
    lista<-list("wykres_dane"=dane_zbiorcze, "szacowane_N1"=N1_optim, "szacowane_N2"=N2_optim,
                "przedział ufności tabela"=cumprob,"przedział ufności"=range(cumprob$proportion), 
                "proporcja"=group1/(group1+group2),r1_optim=r1_optim, r2_optim=r2_optim, r1=r1, r2=r2,
                "suma prawdopodobienstw"=sum(probab), factor_effective=factor_effective)
  else
    lista<-list("wykres_dane"=dane_zbiorcze, "szacowane_N1"=N1_optim, "szacowane_N2"=N2_optim,
                "przedział ufności tabela"=cumprob,"przedzial_ufnosci"=range(cumprob$wzgledne_prawdopodobienstwo),
                "oczekiwane_wgledne_prawdopodobienstwo"=cumprob$wzgledne_prawdopodobienstwo[1])
  
  lista
}