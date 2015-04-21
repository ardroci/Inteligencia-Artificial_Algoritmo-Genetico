

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "../biblio.h"
#include "../KNN.h"
#include "../NaiveBayes.h"

int main(int argc, char *argv[]){
	int i,j,incorrect=0, correct=0;
	Matriz *Total, *Treino, *Teste;
	time_t start_t, end_t;
	/*Total*/
	Total=malloc(sizeof(Matriz));
	Total= LerDados("../CTG_1500.csv");
	Total->matriz=TrocaLinhas(Total);//Troca de Linhas
	Total=getStatisticalData(Total);//Total --> Medias Variancias Desvio Padrao*/
	Total->matriz=Normalizar2(Total);//Normalizacao Estatistica

	/*Teste*/
	Teste=malloc(sizeof(Matriz));
	Teste->linhas=(3*Total->linhas/10);
	Teste->matriz=DividirMatriz(Teste, Total, 0);//copia linhas da 0 ate 450

	/*Treino*/
	Treino=malloc(sizeof(Matriz));
	Treino->linhas=(7*Total->linhas/10);
	Treino->matriz=DividirMatriz(Treino, Total, Teste->linhas);//copia linhas do Teste->linhas ate ao fim
	Treino=getStatisticalData(Treino);/*Treino --> Medias Variancias Desvio Padrao*/
	//Treino->matriz=removerColunas(Treino);//remove colunas em que a variancia é zero, vai eliminar 5 atributos do nosso dataset
	//Treino=getStatisticalData(Treino);/*Treino --> Medias Variancias Desvio Padrao*/

	printMatrizFile(Treino, "Treino.csv");
	printArrayFile(Treino->frelat,"DadosEstatisticos.csv");

	/*-------------------------------------------Naive Bayes-------------------------------------------*/
	time(&start_t);
	NaiveBayes(Teste,Treino),
	time(&end_t);
	for(i=0;i<classes;i++){
		for(j=0;j<classes;j++){
			if(j!=i)
				incorrect=Teste->matrizConfusao[i][j]+incorrect;
		}
	}
	for(i=0;i<classes;i++){
		for(j=0;j<classes;j++){
			if(j==i)
				correct=Teste->matrizConfusao[i][j]+correct;
		}
	}
	printf("\nNaive Bayes-->Acertou: %.3f%%\n",(float)correct/Teste->linhas*100);
	//Taxa de Positivos (precision), Sensibilidade (Recall) e F-measure.
	printf("Classificacoes correctamente   \t%d\t%.3f\n",correct, (float)correct/Teste->linhas);
	printf("Classificacoes incorrectamente \t%d\t%.3f\n\n",incorrect, (float)incorrect/Teste->linhas);
	printMatrizConfusao(Teste);
	printf("\nTaxa de Positivos (precision), Sensibilidade (Recall) e F-measure");
	printf("\n\t\tClasse 1 Classe 2 Classe 3\n");
	printf("\tPrecision: %.3f  %.3f  %.3f\n",Precision(Teste,0),Precision(Teste,1),Precision(Teste,2));
	printf("\tRecall:    %.3f  %.3f  %.3f\n",Recall(Teste,0),Recall(Teste,1),Recall(Teste,2));
	printf("\tF-measure: %.3f  %.3f  %.3f\n",Fmeasure(Teste,0),Fmeasure(Teste,1),Fmeasure(Teste,2));
	printf("\nExecution time = %.2fs\n", difftime(end_t, start_t));



	/*-------------------------------------------KNN-------------------------------------------*/
	time(&start_t);
	Knn(Treino, Teste);

	time(&end_t);
	correct=0;incorrect=0;
	for(i=0;i<classes;i++){
		for(j=0;j<classes;j++){
			if(j!=i)
				incorrect=Teste->matrizConfusao[i][j]+incorrect;
		}
	}
	for(i=0;i<classes;i++){
			for(j=0;j<classes;j++){
				if(j==i)
					correct=Teste->matrizConfusao[i][j]+correct;
			}
		}
	//Taxa de Positivos (precision), Sensibilidade (Recall) e F-measure.
	printf("\nKNN-->Acertou: %.3f%%\n",(float)correct/Teste->linhas*100);
	printf("Classificacoes correctamente   \t%d\t%.3f\n",correct, (float)correct/Teste->linhas);
	printf("Classificacoes incorrectamente \t%d\t%.3f\n\n",incorrect, (float)incorrect/Teste->linhas);
	printMatrizConfusao(Teste);
	printf("\nTaxa de Positivos (precision), Sensibilidade (Recall) e F-measure");

	printf("\n\t\tClasse 1 Classe 2 Classe 3\n");
	printf("\tPrecision: %.3f  %.3f  %.3f\n",Precision(Teste,0),Precision(Teste,1),Precision(Teste,2));
	printf("\tRecall:    %.3f  %.3f  %.3f\n",Recall(Teste,0),Recall(Teste,1),Recall(Teste,2));
	printf("\tF-measure: %.3f  %.3f  %.3f\n",Fmeasure(Teste,0),Fmeasure(Teste,1),Fmeasure(Teste,2));

	printf("\nExecution time = %.2fs\n", difftime(end_t, start_t));

	return 0;
}
