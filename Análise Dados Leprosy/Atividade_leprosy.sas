* Ler os dados; 
data leprosy; 
  infile "/home/u61737927/Atividades/leprosy-data.txt";
  input droga $ pre pos; 
run; 

* Criar uma tabela;
*title 'Tabela de dados no formato Largo';
*proc print data=leprosy(obs=10);
*run;

* Adicionar uma coluna ID;
data leprosy2;
	set leprosy;
	paciente = _n_;
	output;


*Transformar no formato longo; 
proc transpose data=leprosy2 out = leprosy_longo;
   variables pre pos;
   by paciente droga;
run;

data leprosy_longo;
  set leprosy_longo (rename=(col1=bacilos));
  tempo = _name_;
  drop _name_
run;

* Transformar o tempo em numerico 0 ou 1;
data leprosy_longo;
set leprosy_longo; 
	if tempo = "pre" then tempo = 1;
	else tempo = 0;
run;


*title 'Tabela de dados no formato Longo';
*proc print data=leprosy_longo(obs=10);
*run; 




/* AJUSTANDO UM MLG POISSON VIA GEE PELO PROCEDIMENTO GEE (TESTAR QUAL A MELHOR ESTRUTURA DE CORRELAÇAO)*/
* POPULACIONAL;
* COMO FAZER OS PLOTS DA ANALISE DE DIAGNOSTICO;
proc gee data=leprosy_longo descending;
   class paciente droga tempo;
   model bacilos = tempo tempo*droga / dist=poisson link=log noint;
   repeated subject=paciente / corr=unstr corrw ;	 
 run;



/* AJUSTANDO UM GLMM  VIA PROCEDIMENTO GLIMMIX */
* especificar qual a distribuição dos dados e a fnção de ligação;
* quad de quadraturra gaussiana adptativa;
* empirical fas ajustar os erros padrões para ajustar as incertezas na estrutura da matriz de cov-var; 
proc glimmix data=leprosy_longo order=data  method=quad  
					empirical oddsratio plots=studentpanel;
   class paciente droga tempo;
   model bacilos = tempo tempo*droga / dist=poisson link=log noint solution;
   random intercept / subject=paciente;
run;



