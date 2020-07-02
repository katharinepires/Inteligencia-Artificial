load(file.choose())
jogodavelha <- function(jogada)
{
  
  verificavitoria = function()
  {
    #Para identificar o vencedor
    decvit = paste(estadojogo,collapse="");   
   
    if ( 
      (substr(decvit,1,3) %in% c("XXX","BBB"))   | ( substr(decvit,3,6) %in% c("XXX","BBB"))   | (substr(decvit,7,9) %in% c("XXX","BBB")) | 
      (paste(substr(decvit,1,1),substr(decvit,4,4),substr(decvit,7,7),sep="") %in% c("XXX","BBB")) |   
      (paste(substr(decvit,2,2),substr(decvit,5,5),substr(decvit,8,8),sep="") %in% c("XXX","BBB")) |
      (paste(substr(decvit,3,3),substr(decvit,6,6),substr(decvit,9,9),sep="") %in% c("XXX","BBB")) |
      (paste(substr(decvit,1,1),substr(decvit,5,5),substr(decvit,9,9),sep="") %in% c("XXX","BBB")) |  
      (paste(substr(decvit,7,7),substr(decvit,5,5),substr(decvit,3,3),sep="") %in% c("XXX","BBB")) 
    )   warning("Vitória!")
  }
      
  #PARA INICIAR O JOGO:
  if (jogada==0)
  {
    
    estadojogo <<- c(replicate(9,"." ))
    plot(0,type='n',axes=FALSE,ann=FALSE)
    abline(v=.87)
    abline(v=1.14)
    abline(h=-.34)
    abline(h=.32) 
    
    #Para a Inteligencia Artificial jogar primeiro, de maneira aleatória
    pj = sample(9,1)
    estadojogo[pj] <<- "X"   
    
  }
  else
  {
    
    #Para a sua jogada
    if (estadojogo[jogada] != "."  ) {
      stop("Jogada Ilegal!")
    }
    estadojogo[jogada] <<- "B"
    verificavitoria();
     
    #Vez da Intelig/encia Artificial
    jogada =  modelottt$Policy[paste(estadojogo,collapse="")]
    jogada = as.integer(substr(jogada,2,2))
   
    if (is.na(jogada) | estadojogo[jogada] != "." )
    {
      
      print(paste0("Jogada ilegal: ", ifelse(is.na(jogada)," NA ", " Inválida, ")    ,paste(estadojogo,collapse="")))
      
      jogada = regexpr(".", paste0(estadojogo,collapse=""), fixed=T)[1]
      
    }
     
    estadojogo[jogada] <<- "X"
    
    verificavitoria()
    
  }
    
  #Para a impressão do jogo
  text(0.735,.8,labels=ifelse(estadojogo[1]=="."," ",estadojogo[1]),cex=7)
  text(1,.8,labels=ifelse(estadojogo[2]=="."," ",estadojogo[2]),cex=7)
  text(1.257,.8,labels=ifelse(estadojogo[3]=="."," ",estadojogo[3]),cex=7)
  text(0.735,0,labels=ifelse(estadojogo[4]=="."," ",estadojogo[4]),cex=7)
  text(1,0,labels=ifelse(estadojogo[5]=="."," ",estadojogo[5]),cex=7)
  text(1.257,0,labels=ifelse(estadojogo[6]=="."," ",estadojogo[6]),cex=7)
  text(0.735,-.73,labels=ifelse(estadojogo[7]=="."," ",estadojogo[7]),cex=7)
  text(1,-.73,labels=ifelse(estadojogo[8]=="."," ",estadojogo[8]),cex=7)
  text(1.257,-.73,labels=ifelse(estadojogo[9]=="."," ",estadojogo[9]),cex=7)
   
  #Para indentificar o fim de jogo
  fim = regexpr(".", paste0(estadojogo,collapse=""), fixed=T)[1]
  if (fim==-1)
  {
    warning("Fim de Jogo!")
    
  }
    
}
