Nomes_Gerados <- read.csv("~/Band-Tec/Projeto-Inovação/Bunny/dados/API_NomeGen/Nomes_Gerados.csv",encoding = "UTF-8")
dados_func=matrix(data = character(),nrow = 1000,ncol =4,byrow = FALSE)
colnames(dados_func)<- c("nome","email","login","senha")
View(dados_func)
count<-1
set.seed(123)
tag=c("@gmail.com","@hotmail.com","@outlook.com","@bol.com.br","@msn.com","@yahoo.com")
while (count<1001) {
  n<-abs(round(rnorm(1,3,1),0))
  list(n)
  nome<-Nomes_Gerados$nome[count]
  dados_func[count,1]=nome
  loginp<-strsplit(nome,' ')[[1]][1]  
  logind<-strsplit(nome,' ')[[1]][length(strsplit(nome,' ')[[1]])]
  login<-tolower(paste(loginp , logind, sep = "."))
  dados_func[count,3]=login
login=login[1:1]
  if(n==0)
  {
    n=n+1
  }
  email<-paste(login,tag[n],sep = "")
  dados_func[count,2]=email

  count=count+1
}

count=1

while(count<1001)
{
  senha<-""
 login<-dados_func[count,3]
 
  i=1
  
  while (i<3) 
  {
    
    n<-abs(round(rnorm(1,4,1),0))
    if(n==0)
    {
      n=n+1
    }
    
    senha<-paste(senha,substr(login,n,n),sep = "")
    
    i<-i+1
    
  }
  i<-1
  while (i<3) 
  {
    
    n<-abs(round(rnorm(1,4,1),0))
    if(n==0)
    {
      n=n+1
    }
    
    senha<-paste(senha,n,sep = "")
    
    i<-i+1
    
  }
  
  senha<-paste(senha,".",sep = "")
  n<-abs(round(rnorm(1,4,1),0))
  senha<-paste(senha,toupper(substr(login,n,n)),sep = "")
    
  dados_func[count,4]=senha
  count=count+1
}

write.csv(dados_func,"C:\\Users\\kurok\\OneDrive\\Documentos\\Band-Tec\\Projeto-Inovação\\Bunny\\dados\\dados-func.csv",row.names=FALSE)
 

