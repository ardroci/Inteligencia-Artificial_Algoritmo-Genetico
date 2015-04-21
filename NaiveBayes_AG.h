#ifndef NAIVEBAYES_H_
#define NAIVEBAYES_H_
#define PI 3.14
#endif /* NAIVEBAYES_H_ */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

typedef struct DADOS{
	float *array;
	float media;
	float variancia;
	int linhas;
}Dados;
/*Prototipos*/
int class(float *matriz);
float *ProbPosteriori(Matriz *Treino, float *LikehoodProb);
float *ProbPriori(Matriz *Treino, float value, int col, float *ProbPrior);
Dados *getValues(Matriz *Treino,int col,int classe);
float getMedia(Dados *classe);
float getVariancia(Dados *classe);
float LikehoodProbability(Matriz *M, int j, Dados *classe,int *mascara);

float *initVec(float *vec){
	int i;
	for(i=0;i<classes;i++)
		vec[i]=1;
	return vec;
}
float NaiveBayes(Matriz *Teste,Matriz *Treino, int *mascara ){
	int i,j,erro=0,cl,coluna;
	float *LikehoodProb=malloc(classes*sizeof(float));
	float *ProbPost=malloc(classes*sizeof(float));

	Dados *classe1=NULL,*classe2=NULL,*classe3=NULL;
	initVec(LikehoodProb);
	initMatrizConfusao(Teste);//inicializar Matriz de Confusão
	for(i=0;i<Teste->linhas;i++){
		initVec(LikehoodProb);
		for(j=0;j<Teste->colunas-2;j++){
				classe1=getValues(Treino,j,1);
				classe2=getValues(Treino,j,2);
				classe3=getValues(Treino,j,3);
				//puts("medias");
				classe1->media=getMedia(classe1);
				classe2->media=getMedia(classe2);
				classe3->media=getMedia(classe3);
				//puts("variancias");
				classe1->variancia=getVariancia(classe1);
				classe2->variancia=getVariancia(classe2);
				classe3->variancia=getVariancia(classe3);
		}
		LikehoodProb[0]=LikehoodProbability(Treino, i, classe1, mascara);
		LikehoodProb[1]=LikehoodProbability(Treino, i, classe2, mascara);
		LikehoodProb[2]=LikehoodProbability(Treino, i, classe3, mascara);

		ProbPost=ProbPosteriori(Treino, LikehoodProb);
		cl=maxValue(ProbPost[0],ProbPost[1],ProbPost[2]);//retorna classe com maior probabilidade
		if(cl==0)
			printf("Erro de classificação Naive Bayes\n");
		coluna = (int)Teste->matriz[i][Teste->colunas-1];
		if(coluna-cl<=0.001){
			Teste->matrizConfusao[cl-1][cl-1]=Teste->matrizConfusao[cl-1][cl-1]+1;//identificações corretas
		}
		else{
			erro++;
			Teste->matrizConfusao[coluna-1][cl-1]=Teste->matrizConfusao[coluna-1][cl-1]+1;//identificações
		}
	}
	classe1->array=NULL;
	classe2->array=NULL;
	classe3->array=NULL;
	free(classe1->array);
	free(classe2->array);
	free(classe3->array);
	classe1=NULL;
	classe2=NULL;
	classe3=NULL;
	free(classe1);
	free(classe2);
	free(classe3);

	return (100-(100*erro)/Teste->linhas);
}
float getVariancia(Dados *classe){
	int i;
	float variancia=0;
	for(i=0;i<classe->linhas;i++)
			variancia = variancia+pow(classe->array[i]-classe->media,2);
	return variancia/(classe->linhas-1);
}
float getMedia(Dados *classe){
	int i;
	float sum=0;
	for(i=0;i<classe->linhas;i++)
		sum+=classe->array[i];
	return (sum/classe->linhas);
}
Dados *getValues(Matriz *Treino,int col,int classe){
	int i,w=0;
	Dados *cl=malloc(sizeof(Dados));
	float *dados=NULL;
	dados=malloc(Treino->linhas*sizeof(float));
	for(i=0;i<Treino->linhas;i++){
		if(Treino->matriz[i][Treino->colunas-1]==classe){
			dados[w]=Treino->matriz[i][col];
			w++;
		}
	}
	cl->array=dados;
	cl->linhas=w;
	return cl;
}
float *ProbPosteriori(Matriz *Treino, float *LikehoodProb){
	float *ProbPosteriori=NULL;
	int i;
	ProbPosteriori=malloc(classes*sizeof(float));
	for(i=0;i<classes;i++)
		ProbPosteriori[i]=Treino->frelat[i]*LikehoodProb[i];
	return ProbPosteriori;
}
float LikehoodProbability(Matriz *M, int j, Dados *classe,int *mascara){
	float prob=0;
	int i;
	for(i=0;i<M->colunas-2;i++){
		if(i==1)	prob=1.0;
		if(mascara[i]==1){
			prob=prob*(1/(sqrt(2*PI*classe->variancia)))*(exp(-(pow((M->matriz[j][i]-classe->media),2))/(2*classe->variancia)));
			if(prob<=0.001) prob=0.99;//se a probabilidade calculada for 0 torna-la num membro "nulo" da multiplicação
		}
	}
	return prob;
}
