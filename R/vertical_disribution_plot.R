pdf('vertical_distribution_plot.pdf', width = 6, height = 8)
#dev.new(width = 10, height = 5)
plot.new()
plot.window(c(0,14), c(-5,30))


for(i in 0:200){
  val=exp(-i/200)-((1-(exp(-i/200)))/(1-exp(-1)))*exp(-1)
  print(val)
  
  if(i==0){text(2.8, 0.05, paste(as.character(i*0.05),'%', 
    sep=''), cex=1.2)
    rect(c(0.3,0.3), c(-0.5,-0.5), c(2,2), c(0.1, 0.1), 
         col=rgb(val, val, val), lwd=0.1, border=rgb(val, val, val))
    lines(c(2,2.15), c(0.05,0.05))
    }
  else {
    if(i==200)
    {text(3.4, i*0.1+0.05, paste('>= ',as.character(i*0.05), 
                            '%',
                            sep=''), cex=1.2)
      rect(c(0.3,0.3), c(20,20), c(2,2), c(20.6,20.6), 
           col=rgb(val, val, val), lwd=0.1, border=rgb(val, val, val))
      lines(c(2,2.15), c(i*0.1+0.05,i*0.1+0.05))
      }
  else{
      if (i%%20==0)
        {text(2.8, i*0.1+0.05, paste(as.character(i*0.05), '% ', 
                             sep=''), cex=1.2)
        lines(c(2,2.15), c(i*0.1+0.05,i*0.1+0.05))
        }
      rect(c(0.3,0.3), c(i*0.1,i*0.1), c(2,2), c(i*0.1+0.1,i*0.1+0.1), 
           col=rgb(val, val, val), lwd=0.1, border=rgb(val, val, val))
      
      }
  }
}
rect(c(0.3,0.3), c(-0.5,-0.5), c(2,2), c(20.6, 20.6), 
     col=rgb(0,0,0, alpha=0), lwd=1.2, border='black')
points(1.15, -2, col='#bc0900', cex=2.2,pch=21,bg='#fb8b06')
text(2.3, -2, 'queen', cex=1.2, pos=4)
for (i in seq(0.3,2, by=0.14)){
  points(i, -4, pch=15, cex=0.3, col='#00AB25')
}
text(2.3, -4.8, 'mean depth \nof workers', cex=1.2, pos=4)
dev.off()







wykres<-function(dane,kolonia, krolowe, srednie){
  dane<-dane[dane$kolonia==kolonia,]
  krolowe<-krolowe[krolowe$kolonia==kolonia,]
  gl_srednia<-srednie[match(kolonia, srednie[,2]),1]

  if (kolonia=='18-14'){
    for (i in 1:nrow(dane)){
      val=exp(-dane$prop[i]*20)-((1-(exp(-dane$prop[i]*20)))/(1-exp(-2)))*exp(-2)
      if (val<0) val=0
      
      rect(c(0,0), c(65-i,65-i), c(10,10), c(66-i,66-i), border=rgb(val, val, val), col=rgb(val, val, val), lwd=0.1)
      if (val<0.5) text_col=1
      else text_col=0
      if (dane$prop[i]>0)
      text(5,65.5-i,as.character(round(dane$prop[i]*100, digits=1)),cex=0.9,col=rgb(text_col,text_col,text_col))
      
    } 
    text(5, 68, kolonia, cex=1.7)
    rect(c(0,0), c(0,0), c(10,10), c(65,65), lwd=1.3)
    text(5, -3, as.character(dane$sum[1]), cex=1.9)
  }
  
  else{

  for (i in 1:nrow(dane)){
    val=exp(-dane$prop[i]*10)-((1-(exp(-dane$prop[i]*10)))/(1-exp(-1)))*exp(-1)
    if (val<0) val=0
    if (val<0.5) text_col=1
    else text_col=0
    
    rect(c(0,0), c(60-i,60-i), c(10,10), c(61-i,61-i), border=rgb(val, val, val), col=rgb(val, val, val), lwd=0.1)
    if (dane$prop[i]>0.0007)
      text(5,60.5-i,as.character(round(dane$prop[i]*100, digits=1)),cex=0.9,col=rgb(text_col,text_col,text_col))
  } 
  text(5, -3, as.character(dane$sum[1]), cex=1.9)
  
  text(5, 63, kolonia, cex=1.7)
  rect(c(0,0), c(0,0), c(10,10), c(60,60), lwd=1.3)
  }
  for (i in seq(0.2,10, by=0.8)){
    points(c(i), c(60-gl_srednia), pch=15, cex=0.5, col='#00AB25')
    }
  for (i in 1:nrow(krolowe)){
    if (i<nrow(krolowe)){
      if(krolowe$glebokosc_krolowej[i]==krolowe$glebokosc_krolowej[i+1])
        points(c(1.5, 8.5), rep(60-krolowe$glebokosc_krolowej[i],2), 
               col='#bc0900', cex=3,pch=21,bg='#fb8b06')
      else points(2, 60-krolowe$glebokosc_krolowej[i], col='#bc0900', cex=3,pch=21,bg='#fb8b06')}
    else {
      if (i>1){
        if(krolowe$glebokosc_krolowej[i-1]==krolowe$glebokosc_krolowej[i])
          next
        else
          points(2, 60-krolowe$glebokosc_krolowej[i], col='#bc0900', cex=3,pch=21,bg='#fb8b06')
      }
      else{
        points(2, 60-krolowe$glebokosc_krolowej[i], col='#bc0900', cex=3,pch=21,bg='#fb8b06')
      }
      
    }
  }
}



pdf('depths2.pdf', width = 10, height = 7.5)
par(mfrow=c(1,12))

par(mar=rep(0.3,4))
plot.new()
plot.window(c(-6,4), c(-5,72))
lines(c(3,3), c(0,60), lwd=1)
for (i in 0:6){
  lines(c(2.5,3), c(60-(i*10), 60-(i*10)), lwd=1)
  text(0,60-(i*10), as.character(i*10), cex=1.9)
}
text(-4, 33, 'depth [cm]', cex=2.1, srt=90)
text(-1, -3, 'Number of \n workers', cex=1.6)
text(-1.4, 63, 'Colony ID', cex=1.6)

for (kolonia in colonies_order$kolonia){
  par(mar=rep(0.3,4))
  plot.new()
  plot.window(c(-2,11), c(-5,72))
  wykres(glebokosci_2, kolonia ,krolowe, glebokosc_srednie_wazone)
}
dev.off()
