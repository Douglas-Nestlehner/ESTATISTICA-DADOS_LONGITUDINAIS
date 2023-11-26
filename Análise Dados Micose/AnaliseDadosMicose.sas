/* Analise dos dados de micose via GEE
	Dataset: Toenail - Molemberghs & Verbeke (2005)
	Date: 04/2008 - Universiteit Hasselt, Diepenbeek, Belgium
*/


* CONFIGURANDO A BIBLIOTECA;
libname d "/home/u61737927/Atividades";
run;


data work.toenail;
    set d.toenail2;
    timeclass = time;
    run;


proc print data=toenail;
	run;


/* Ajustando um MLG Logistico-Bernoulli (assumindo independencia) */
* Ajuste assumindo a ligação canonica defaut;
proc genmod data=toenail descending;
	title 'Logistic-Bernoulli GLM';
	class treatn;
	model y = treatn time*treatn / dist=binomial noint;
	run;
*observar a tabela de efeitos fixos (Analysis Of Maximum Likelihood Parameter Estimates);


/* AJUSTANDO UM MLG LOGISTICO-BERNOULLI SUPERDISPERSO */
proc genmod data=toenail descending;
	class treatn;
	title 'Logistic-Overdispersed Bernoulli GLM';
	 model y = treatn time*treatn / dist=binomial noint 
	 					scale=pearson aggregate=(treatn time);
	run;



/* AJUSTANDO UM MLG LOGISTICO-BERNOULLI VIA GEE PELO PROCEDIMENTO GENMOD*/
proc genmod data=toenail descending;
	class treatn idnew;
	title 'Logistic-Overdispersed Bernoulli GLM';
	model y = treatn time*treatn / dist=binomial link=logit noint;
	*repeated subject=idnew / corr=ind  corrw ;	 
 	*repeated subject=idnew / corr=exch corrw;	 
 	*repeated subject=idnew / corr=ar(1) corrw;	 
 	repeated subject=idnew / corr=unstr corrw;	 
	run;



/* AJUSTANDO UM MLG LOGISTICO-BERNOULLI VIA GEE PELO PROCEDIMENTO GEE*/
proc gee data=toenail descending;
   class treatn idnew;
   model y = treatn time*treatn / dist=binomial link=logit noint;
   *repeated subject=idnew / corr=ind corrw ;	 
   *repeated subject=idnew / corr=exch corrw;	 
 	*repeated subject=idnew / corr=ar(1) corrw;	 
 	repeated subject=idnew / corr=unstr corrw;	 
 	*repeated subject=idnew / corr=mdep(3) corrw;	 
	run;



*ajuste de um modelo populacional utiliza-se o gee (visto anteriormente);




/* AJUSTANDO UM GLMM  VIA PROCEDIMENTO GLIMMIX */
* especificar qual a distribuição dos dados e a fnção de ligação;
* quad de quadraturra gaussiana adptativa;
* empirical fas ajustar os erros padrões para ajustar as incertezas na estrutura da matriz de cov-var; 
proc glimmix data=toenail order=data  method=quad  
					empirical oddsratio plots=studentpanel;
   class treatn idnew y;
   model y = treatn time*treatn / dist=binary link=logit noint solution;
   random intercept / subject=idnew;
run;
*;



/* AJUSTANDO UM GLMM  VIA PROCEDIMENTO NLMIXED */
* faz o mesmo que o glimmix fez, porem o nlmixed permite ajustar modelos nao planejados (glimmixed direcionado para modelos lineares);
proc nlmixed data=toenail qpoints=10 empirical;
	title 'Logistic-Bernoulli GLMM';
	title2 'Random Effect b1 ~ Normal(0,s^2)';
	parms Beta_0=0  Beta_2=0 Beta_1=0 Beta_3=0 sigma=10;
	if (treatn=0) then eta=Beta_0 + Beta_1*time + b1;
	if (treatn=1) then eta=Beta_2 + Beta_3*time + b1;
	expeta = exp(eta);
	p = expeta /(1 + expeta);
	model y ~ binary(p);
	random b1 ~ normal(0,sigma**2) subject = idnew;
	run;


