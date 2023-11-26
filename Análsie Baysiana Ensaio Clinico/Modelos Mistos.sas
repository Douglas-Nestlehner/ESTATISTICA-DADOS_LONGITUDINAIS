* ATIVDADE 03;
* AJUSTE DO MODELO LINEAR MISTO;

* Dados;
data PesoNasc; 
  infile "/home/u61737927/Atividades/PesoNascimento_tratados.txt";
  input ID_Mãe Ordem_Nascimento Peso_Nascimento Idade_Materna ID_Filho; 
run; 

title 'Tabela de dados no formato LONGO';
proc print data=PesoNasc(obs=15);
run;


* AJUSTE DE UM MODELO LINEAR NORMAL;
title 'Modelo Linear Considerando dados Independentes';
proc glm data=PesoNasc;
	* class ID_Mãe;
	model Peso_Nascimento = Ordem_Nascimento Idade_Materna / solution ;
run;	
title;


* GERANDO NOVAS VARIÁVEIS CONTENDO EFEITOS QUADRÁTICOS E CÚBICOS;
data PesoNasc2;
   set PesoNasc;
   Ordem2 = Ordem_Nascimento**2;
   Ordem3 = Ordem_Nascimento**3;
   Ordemclass = Ordem_Nascimento;
run; 


/* AJUSTANDO UM MODELO LINEAR MISTO NORMAL
   ESTIMAÇAO VIA REML (para escolha da estrutura de covariância)
*/
proc mixed data=PesoNasc2;
    title 'Modelo com estrutura de covariância não estruturada';
	class Ordemclass ID_Mãe; 
	model Peso_Nascimento = Ordemclass Idade_Materna / solution;
	random intercept OrdemClass / subject=ID_Mãe type=un;
run;

proc mixed data=PesoNasc2;
    title 'Modelo com estrutura de covariância Toeplitz';
	class Ordemclass ID_Mãe; 
	model Peso_Nascimento = Ordemclass Idade_Materna / solution;
	random intercept OrdemClass / subject=ID_Mãe type=TOEP;
run;

proc mixed data=PesoNasc2;
    title 'Modelo com estrutura de covariância simetria composta';
	class Ordemclass ID_Mãe; 
	model Peso_Nascimento = Ordemclass Idade_Materna / solution;
	random intercept OrdemClass / subject=ID_Mãe type=CS;
run;

proc mixed data=PesoNasc2;
    title 'Modelo com estrutura de covariância autoregressiva 1ª ordem';
	class Ordemclass ID_Mãe;
	model Peso_Nascimento = Ordemclass Idade_Materna / solution;
	random intercept OrdemClass / subject=ID_Mãe type=ar(1);
run;

proc mixed data=PesoNasc2;
    title 'Modelo com estrutura de covariância componentes de variâncias';
	class Ordemclass ID_Mãe; 
	model Peso_Nascimento = Ordemclass Idade_Materna / solution;
	random intercept OrdemClass / subject=ID_Mãe type=VC ;
run;



/* AJUSTANDO UM MODELO LINEAR MISTO NORMAL
   ESTIMAÇAO VIA ML (para escolha do preditor linear)
*/

proc mixed data=PesoNasc2 method=ml;
	*class Grupo ID_Mãe;
	model Peso_Nascimento = Ordemclass Idade_Materna / solution;
	random intercept  /  subject=ID_Mãe type=AR(1);
run;


* ANALISE DE DIAGNOSTICOS;
proc mixed data=PesoNasc2 method=ml PLOTS=ALL ASYCORR COVTEST;
	*class Grupo ID;
	model Peso_Nascimento = Ordemclass Idade_Materna / solution ddfm=kenwardroger CORRB;
	random intercept  /  subject=ID_Mãe type=AR(1) G SOLUTION;
run;


*MODELO MARGINAL;
proc mixed data=PesoNasc2 method=ml PLOTS=ALL;
	*class Grupo ID;
	model Peso_Nascimento = Ordemclass Idade_Materna / 
	        solution ddfm=kenwardroger CORRB;
	Repeated  /  subject=ID_Mãe type=AR(1);
run;
