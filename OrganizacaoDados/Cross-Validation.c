#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "../biblio.h"
#include "../KNN.h"
//#include "../NaiveBayes.h"
//#include "../aux.h"
#include "../NaiveBayes_v3.h"

int main(int argc, char *argv[]){
	int i, k=6,correct=0, incorrect=0;//nr particoes de k
	Matriz *Total, **Dados, *Teste, *Treino;
	time_t start_t, end_t;

	/*Total*/
	Total=malloc(sizeof(Matriz));//conjunto de dados começa por ser armazenado todo na mesma estrutura
	Total= LerDados("CTG_1500.csv");
	Total->matriz=TrocaLinhas(Total);//Troca de Linhas

	/*Treino*/
	int j=0,w,aux;
	Dados=malloc(sizeof(Matriz));
	for(i=0;i<k-1;i++){//ATENCAO AO K-1
		Dados=(Matriz**)realloc(Dados,(i+1)*sizeof(*Dados));//problemas aqui
		Dados[i]=malloc(Total->colunas * sizeof(float));
		Dados[i]->linhas=(Total->linhas/k);
		Dados[i]->matriz=DividirMatriz(Dados[i], Total, (i+1)*(Total->linhas/k));//copia linhas do Teste->linhas ate ao fim
		Dados[i]=getStatisticalData(Dados[i]);//--> Medias Variancias Desvio Padrao
		Dados[i]->matriz=removerColunas(Dados[i]);//remove colunas em que a variancia é zero, vai eliminar 5 atributos do nosso dataset
		Dados[i]=getStatisticalData(Dados[i]);//Treino --> Medias Variancias Desvio Padrao
		Dados[i]->matriz=Normalizar(Dados[i]);//Normalizacao Estatistica
	}

	/*-------------------------------------------Naive Bayes-------------------------------------------*/
	time(&start_t);
	float Bayes=0;aux=0;
	for(j=0;j<k-1;j++){
		for(i=0;i<k-1;i++){
			if(i!=j){
				aux++;
				Bayes+=NaiveBayes(Dados[j],Dados[i]);
			}
		}
	}
	printf("\nNaive Bayes-->Acertou: %.3f%%\n",Bayes/aux);
	time(&end_t);
	for(w=0;w<k;w++){
		for(i=0;i<classes;i++){
				for(j=0;j<classes;j++){
					if(j!=i)
						incorrect=Dados[w]->matrizConfusao[i][j]+incorrect;
				}
		}
	}
	for(w=0;w<k;w++){
		for(i=0;i<classes;i++){
			for(j=0;j<classes;j++){
				if(j==i)
					correct=Dados[w]->matrizConfusao[i][j]+correct;
			}
		}
	}
	//Taxa de Positivos (precision), Sensibilidade (Recall) e F-measure.
	printf("Classificacoes correctamente   \t%d\t%.3f\n",correct, (float)correct/Dados[0]->linhas);
	printf("Classificacoes incorrectamente \t%d\t%.3f\n\n",incorrect, (float)incorrect/Dados[0]->linhas);
	printMatrizConfusao(Dados[0]);//necessário juntar as outras matrizes confusao
	printf("\nTaxa de Positivos (precision), Sensibilidade (Recall) e F-measure");
	printf("\n\t\tClasse 1 Classe 2 Classe 3\n");
	printf("\tPrecision: %.3f  %.3f  %.3f\n",Precision(Dados[0],0),Precision(Dados[0],1),Precision(Dados[0],2));
	printf("\tRecall:    %.3f  %.3f  %.3f\n",Recall(Dados[0],0),Recall(Dados[0],1),Recall(Dados[0],2));
	printf("\tF-measure: %.3f  %.3f  %.3f\n",Fmeasure(Dados[0],0),Fmeasure(Dados[0],1),Fmeasure(Dados[0],2));
	printf("\nExecution time = %.2fs\n", difftime(end_t, start_t));
	printf("DONE NAIVE");
	/*-------------------------------------------KNN-------------------------------------------*/
	time(&start_t);
	float KNN=0;
	aux=0;
	for(j=0;j<k-1;j++){
		for(i=0;i<k-1;i++){
			if(i!=j){
				aux++;
				KNN+=Knn(Dados[j], Dados[i]);
			}
			printf("%d\n",i);
		}
	}
	printf("DONE");
	printf("\nKNN-->Acertou: %.3f%%\n",KNN/aux);
	time(&end_t);
	incorrect=0;
	correct=0;
	for(w=0;w-k;w++){
		for(i=0;i<classes;i++){
				for(j=0;j<classes;j++){
					if(j!=i)
						incorrect=Dados[w]->matrizConfusao[i][j]+incorrect;
				}
			}
	}
	for(w=0;w-k;w++){
		for(i=0;i<classes;i++){
			for(j=0;j<classes;j++){
				if(j==i)
					correct=Dados[w]->matrizConfusao[i][j]+correct;
			}
		}
	}
	//Taxa de Positivos (precision), Sensibilidade (Recall) e F-measure.
	printf("Classificacoes correctamente   \t%d\t%.3f\n",correct, (float)correct/Dados[0]->linhas);
	printf("Classificacoes incorrectamente \t%d\t%.3f\n\n",incorrect, (float)incorrect/Dados[0]->linhas);
	printMatrizConfusao(Dados[0]);
	printf("\nTaxa de Positivos (precision), Sensibilidade (Recall) e F-measure");
	printf("\n\t\tClasse 1 Classe 2 Classe 3\n");
	printf("\tPrecision: %.3f  %.3f  %.3f\n",Precision(Dados[0],0),Precision(Dados[0],1),Precision(Dados[0],2));
	printf("\tRecall:    %.3f  %.3f  %.3f\n",Recall(Dados[0],0),Recall(Dados[0],1),Recall(Dados[0],2));
	printf("\tF-measure: %.3f  %.3f  %.3f\n",Fmeasure(Dados[0],0),Fmeasure(Dados[0],1),Fmeasure(Dados[0],2));
	printf("\nExecution time = %.2fs\n", difftime(end_t, start_t));

	return 0;
}
