documentos_func=matrix(data = character(),nrow = 3000,ncol =5,byrow = FALSE)
colnames(documentos_func)<- c("valor","tipo","data-expedição","data-envio","fk-func")
set.seed(123)
i<-1
linha<-0
 while (i<3001) {
   linha<-linha+1
   documentos_func[linha,1]=grCpf()
   documentos_func[linha,2]="CPF"
   documentos_func[linha,3]=as.character(data_exp()) 
   documentos_func[linha,4]=as.character(data_env()) 
   documentos_func[linha,5]=i
   linha<-linha+1
   documentos_func[linha,1]=grRg()
   documentos_func[linha,2]="RG"
   documentos_func[linha,3]=as.character(data_exp()) 
   documentos_func[linha,4]=as.character(data_env()) 
   documentos_func[linha,5]=i
   linha<-linha+1
   documentos_func[linha,1]=grTitulo()
   documentos_func[linha,2]="Titulo"
   documentos_func[linha,3]=as.character(data_exp()) 
   documentos_func[linha,4]=as.character(data_env()) 
   documentos_func[linha,5]=i
   
   i<-i+1
 }

View(documentos_func)


######################################################### Funções ################################



grCpf <- function() {
  c<-1
  cpf<-""
  while (c<13) {
    if(c>9 && c<11)
    {
      cpf<-paste(cpf,"-",sep = "")
    }
    else
    {
      n<-round(runif(1,1,9))
      cpf<-paste(cpf,n,sep = "")
    }
    c<-c+1
    
  }
  
    return(cpf)
  
}


grRg <- function(){
  c<-1
  rg<-""
  while (c<10) {
    if(c>7 && c<9)
    {
      rg<-paste(rg,"-",sep = "")
    }
    else
    {
      n<-round(runif(1,1,9))
      rg<-paste(rg,n,sep = "")
    }
    c<-c+1
  }
  
  return(rg)
}


grTitulo <- function(){
  c<-1
  titulo<-""
  while (c<13) {
    
      n<-round(runif(1,1,9))
      titulo<-paste(titulo,n,sep = "")
    
    c<-c+1
  }
  
  return(titulo)
}


data_exp<-function()
{
  n_exp<-runif(1,1,length(datas_exp))
  datas_exp    <- seq(from=as.Date("2005-01-01"), to=as.Date("2020-12-31"), by="day")
  x<-datas_exp[n_exp]
  return(x)
}

data_env<-function()
{
  datas_env<- seq(from=as.Date("2021-01-01"), to=as.Date("2021-05-3"), by="day")
  n_env<-runif(1,1,length(datas_env))
  x<-datas_env[n_env]
  
  return(x)
}

write.csv(documentos_func,"C:\\Users\\kurok\\OneDrive\\Documentos\\Band-Tec\\Projeto-Inovação\\Bunny\\dados\\documentos-func.csv",row.names=FALSE)

