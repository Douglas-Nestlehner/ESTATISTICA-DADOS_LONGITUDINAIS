* Ler os dados; 
data dados; 
  infile "/home/u61737927/Atividades/dados_trabalho.txt";
  input ID Centro Idade Pele Gênero Exposição Y Tratamento Ano; 
run; 


* Criar uma tabela;
title 'Tabela de dados no formato Longo';
proc print data=dados(obs=6);
run;


* Descritiva;
proc freq data=dados order=freq;
   tables Gênero*Ano / 
       plots=freqplot(twoway=stacked orient=horizontal);
run;

proc freq data=dados order=freq;
   tables Tratamento*Ano / 
       plots=freqplot(twoway=stacked orient=horizontal);
run;

proc freq data=dados order=freq;
   tables Pele*Ano / 
       plots=freqplot(twoway=stacked orient=horizontal);
run;

proc freq data=dados order=freq;
   tables Centro*Ano / 
       plots=freqplot(twoway=stacked orient=horizontal);
run;

*;
proc freq data=dados order=freq;
   tables Exposição*Ano / 
       plots=freqplot(twoway=stacked orient=horizontal);
run;

proc freq data=dados order=freq;
   tables Y*Ano / 
       plots=freqplot(twoway=stacked orient=horizontal);
run;

proc univariate data = dados;
	histogram   / ;
run;


*;
* Adicionar uma coluna ID;
data dados2;
	set dados;
	Log_Y = log(Y);;
output;


*modelo;
proc bglimm data=dados seed=10571042 nmc=10000
   outpost = Saida_M1;
   class ID Pele Gênero Ano;
   model Y = Idade Pele Gênero Exposição Tratamento Ano / dist=poisson;
   random int / sub = ID;


* Resultado;
data SMR_PRED;
   array gamma[56] Intercept__County_1-Intercept__County_56;
   array SMR_pred[56];
   set dados_Out;
      do i = 1 to 56;
         set dados(rename=(x=data_x)) point=i;
         SMR_pred[i] = 100 * exp(Intercept + x * data_x + gamma[i]);
      end;
   keep smr_pred:;
   run;
