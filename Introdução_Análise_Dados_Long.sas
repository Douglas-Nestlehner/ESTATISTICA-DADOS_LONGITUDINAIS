* 	PROJETO: Analise Exploratória Dados Cholesterol
	OBJETIVO: Fazer analise exploratoria dos dados longitudinais de colesterol.
*/

* cometario usar "*" com ponto e virgula no final ;
/* comentario em bloco */

* Ler os dados;
* Achar o caminho do arquvi ir no arquivo --> propriedades; 
data PesoNasc; 
  infile "/home/u61737927/IDL_Colesterol/PesoNascimento_tratados.txt";
  * Definir os nomes das covariaveis;
  input ID_Mãe Ordem_Nascimento Peso_Nascimento Idade_Materna ID_Filho; 
run; 

* Criar uma tabela;
title 'Tabela de dados no formato LONGO';
proc print data=PesoNasc(obs=20000); * Mostra x numero de observações;
run;


* Plotar o grafico de espaguete;	
title "Spaghetti Plot para dados de Peso Nascimento";
proc sgplot data=PesoNasc;
   series x=Ordem_Nascimento y=Peso_Nascimento / group=ID_Mãe 
   								break 
   								transparency=0.5 
   								lineattrs=(pattern=solid);
   xaxis display=(nolabel);
   keylegend / type=linecolor title="Grupo";
run;

* Calcular algumas estatisticas descritivas;
title 'Estatísticas Descritivas';
proc univariate data= PesoNasc;
  var Peso_Nascimento;
  by Ordem_Nascimento;
run;
* Por que não esta calculando no 5 ???

* Reordenando a base para gerar tabela de médias e desvios padrões;
proc sort data=pesonasc out=temp;
  by Ordem_Nascimento;
run;

* Gerar as tabelas contendo as medidas;
title 'Médias de Colesterol por Mes e Grupo';
proc means data=temp;
  var Peso_Nascimento;
  by Ordem_Nascimento;
  output out=tabStats MEAN=Media STD=DPadrao;
run;

* Reordenar por oredem de grupo e descartar as colunas TYPE e FREQ;
proc sort data=tabStats(drop=_TYPE_ _FREQ_) out=tabStats;
  by Ordem_Nascimento;
run;

proc print data=tabStats;
run;

*;
title "Gráfico de Linhas para Médias de Peso Nascimento";
proc sgplot data=tabStats;
   series x=Ordem_Nascimento y=Media / 
   								break 
   								transparency=0.1 
   								lineattrs=(pattern=solid);   							
   xaxis display=(nolabel);
   keylegend / type=linecolor title="Grupo";
run;

title "Gráfico de Linhas para Médias de Peso Nascimento";
proc sgplot data=tabStats;
   series x=Ordem_Nascimento y=DPadrao / 
   								break 
   								transparency=0.1 
   								lineattrs=(pattern=solid);   							
   xaxis display=(nolabel);
   keylegend / type=linecolor title="Grupo";
run;


* Como tranormar para o formato largo para calcular o corr;
title 'Correlação das Medidas';
proc corr data=PesoNasc ;
  var Ordem_Nascimento; 
run;
  
