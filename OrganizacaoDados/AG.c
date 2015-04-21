/*
 * AG.c
 *
 *  Created on: Dec 2, 2014
 *      Author: costa
 */
#ifndef AG_C_
#define AG_C_
#endif /* AG_C_ */
#include<stdio.h>
#include<stdlib.h>
#include <sys/time.h>
#include <time.h>
#include<string.h>
#include "../biblio.h"
#include "../KNN_AG.h"
#include "../NaiveBayes_AG.h"
#define sublinhado "\033[4;2m"
#define none   "\033[0m"
#define bold  "\033[1;1m"
typedef struct AG{
	int linhas;
	int colunas;
	int **matriz;
}Ag;

float rand_lima() {
	float r;
	struct timeval t1;
	do{
		gettimeofday(&t1, NULL);
		srand(t1.tv_usec * t1.tv_sec);
		r = (float)rand()/ (float)RAND_MAX;
	} while (r > 1 || r<0);

    return r;
}
int rand_lim1(int max) {
	int r;
	struct timeval t1;
	do{
		gettimeofday(&t1, NULL);
		srand(t1.tv_usec * t1.tv_sec);
		r = rand() % max;
	} while (r >max || r<0);

    return r;
}
void printArraya(int *array){
	int i;
	for(i=0;i<21;i++)
		printf("%s%d %s",bold,array[i],none);
	printf("\n");
}
void printMatriza(int **matriz, int linhas, int colunas){
	int i,j;
	for(i=0;i<linhas;i++){
		printf("\t%d\t",i+1);
		for(j=0;j<colunas;j++){
			if(j==colunas-1){
				printf("\t%d", matriz[i][j]);
				continue;
			}
			printf("%d", matriz[i][j]);
		}
		printf("\n");
	}
}
void ProgressBar( char *label, int step, int total ){
    const int pwidth = 72;
    int width = pwidth - strlen( label ), pos = ( step * width ) / total ,percent = ( step * 100 ) / total,i;
    printf( "%s [", label );
    for (i = 0; i < pos; i++ )
    	printf( "%s", "=" );
    for (i = pos; i < width; i++ )
       	printf( "%s", " " );
    printf( "] %d%%\r", percent );
}
Ag* avaliaFitnessPop(Matriz *Treino, Matriz *Teste, Ag *Pop, int classificador){
	int i=0;
	//printf("%sA avaliar Fitness da Populacao [0%s", bold, none);
	for(i=0;i<Pop->linhas;i++){
		if(classificador==1 || classificador==0)//KNN classificador por defeito
			Pop->matriz[i][21]=Knn(Treino,Teste,Pop->matriz[i]);
		if(classificador==2)
			Pop->matriz[i][21]=NaiveBayes(Teste,Treino, Pop->matriz[i]);
		ProgressBar(" A avaliar Fitness da Populacao",i+1,Pop->linhas);
	}
	//printf("%s]%s\n", bold, none);
	return Pop;
}
int **ordenaMatrizFitness(int **matriz, int linhas){
	int i,j,trocas=1, colunas=22;
	int *aux=malloc(colunas * sizeof(int));
	while(trocas==1){
		trocas=0;
		for(i=0;i<linhas-1;i++){
			if(matriz[i][21]<matriz[i+1][21]){
				for(j=0;j<colunas;j++){
					aux[j]=matriz[i][j];
					matriz[i][j]=matriz[i+1][j];
					matriz[i+1][j]=aux[j];
				}
				trocas=1;
			}
		}
	}
	free(aux);
	return matriz;
}
Ag *inicializarPopulacao(int nr_individuos){
	Ag *Populacao=NULL;
	Populacao=malloc(sizeof(Ag));
	Populacao->matriz=malloc(1 * sizeof(int *));
	Populacao->colunas=22;
	Populacao->linhas=nr_individuos;
	float rand;
	int i,j;
	for(i=0;i<Populacao->linhas;i++){
		Populacao->matriz = (int **) realloc(Populacao->matriz, (i + 1) * sizeof(*Populacao->matriz));
		Populacao->matriz[i]=malloc(Populacao->colunas * sizeof(int));
		for(j=0;j<Populacao->colunas;j++){
			if(j==21)
				continue;
			rand=rand_lima();
			if(rand>0.5)
				Populacao->matriz[i][j]=1;
			else
				Populacao->matriz[i][j]=0;
		}
	}
	return Populacao;
}
int *mascara_cruzamento(int pontos_corte){
	int j,pnt_corte,*mascara_cruzamento=malloc(22 * sizeof(int *));
	switch(pontos_corte){
	case 1:
		pnt_corte=rand_lim1(21);
		printf("pnt_corte=%d\n",pnt_corte);
		for(j=0;j<21;j++){
			if(j>=pnt_corte){
				mascara_cruzamento[j]=1;
				continue;
			}
			mascara_cruzamento[j]=0;
		}
		break;
	case 2:
		pnt_corte=rand_lim1(20);
		int pnt_corte_2=0;
		do{
			 pnt_corte_2=rand_lim1(21);
		}while(pnt_corte_2<=pnt_corte);
		printf("pnt_corte 1=%d pnt_corte 2=%d\n",pnt_corte,pnt_corte_2);
		for(j=0;j<21;j++){
			if(j>=pnt_corte && j<=pnt_corte_2){
				mascara_cruzamento[j]=1;
				continue;
			}
			mascara_cruzamento[j]=0;
		}
		break;
	default:
		printf("Quantidade de pontos de corte Invalida\n");
	}
	puts("Mascara Cruzamento ");
	printArraya(mascara_cruzamento);
	return mascara_cruzamento;
}
int **cruzamento(int *mascara_cruzamento,int *pai1,int *pai2){
	int **filhos,j;
	filhos=malloc(2 * sizeof(int));
	filhos[0]=malloc(22 * sizeof(int));
	filhos[1]=malloc(22 * sizeof(int));
	for(j=0;j<21;j++){
		if(mascara_cruzamento[j]==1){
			filhos[0][j]=pai2[j];
			filhos[1][j]=pai1[j];
			continue;
		}
		filhos[0][j]=pai1[j];
		filhos[1][j]=pai2[j];
	}
	return filhos;
}
Ag *mutacao(Ag *Pop,int linha){
	int i;
	for(i=0;i<Pop->colunas;i++){
		//printf("col %d\n",i);
		if(Pop->matriz[linha][i]==0)
			Pop->matriz[linha][i]=1;
		else
			Pop->matriz[linha][i]=0;
	}
	return Pop;
}
int selecaoTorneio(int **matriz, int linhas){
	int rand1, rand2;
	rand1=rand_lim1(linhas);//escolhe posicao aleatoria de entre os pais
	do{
		rand2=rand_lim1(linhas);
	}while (rand1==rand2);
	//printf("pai %d - %d pai %d - %d\n",rand1, matriz[rand1][21],rand2,matriz[rand2][21]);
	return (matriz[rand1][21]>matriz[rand2][21]?rand1:rand2);
}
Ag *reproducao(Ag* Pais, int *mascara_cruzamento){
	int i,j=0,*pai1,*pai2,**aux=NULL, rand1,rand2;
	//int k_individuos=rand_lim1(Pais->linhas);/*Quantidade de pais se podem reproduzir*/
	float prob_mutacao;
	Ag *Filhos=NULL;
	Filhos=malloc(sizeof(Ag));
	Filhos->matriz=malloc(1 * sizeof(int *));
	Filhos->colunas=22;
	for(i=0;i<Pais->linhas;i++){
		Filhos->matriz = (int **) realloc(Filhos->matriz, (j + 1) * sizeof(Filhos->matriz));
		Filhos->matriz[j]=malloc(Pais->colunas * sizeof(int));
		//rand1=rand_lim1(k_individuos);//escolhe posicao aleatoria de entre os melhores k_individuos
 		rand1=selecaoTorneio(Pais->matriz,Pais->linhas);//escolhido o 1º Pai
		pai1=Pais->matriz[rand1];//escolhido o 1º Pai*/
 		do{
 	 		rand2=selecaoTorneio(Pais->matriz,Pais->linhas);//escolhido o 2º Pai
 		}while (rand1==rand2);
		pai2=Pais->matriz[rand2];//escolhido o 2º Pai*/
		aux=cruzamento(mascara_cruzamento,pai1,pai2);
		Filhos->matriz[j]=aux[0];
		prob_mutacao=rand_lima();
		if(prob_mutacao<=0.001){
			printf("Ocorreu mutacao Filho 1\nProb mutacao %.4f\n\n",prob_mutacao);
			Filhos=mutacao(Filhos,j);
		}
		Filhos->matriz[j++]=aux[1];
		prob_mutacao=rand_lima();
		if(prob_mutacao<=0.001){
			printf("Ocorreu mutacao Filho 2\nProb mutacao %.4f\n\n",prob_mutacao);
			Filhos=mutacao(Filhos,j-1);
		}
		/*printf("Pai 1 %d\t->",rand1+1);
		printArraya(pai1);
		printf("\nPai 2 %d\t->",rand2+1);
		printArraya(pai2);
		printf("\n\nFilho 1\t->");
		printArraya(aux[0]);
		printf("\nFilho 2\t->");
		printArraya(aux[1]);
		puts("\n");*/
	}
	Filhos->linhas=j;
	return Filhos;
}

int **combina_Matrizes(Ag *Populacao,Ag *Filhos){
	int **aux=malloc(1 * sizeof(int *)),i,j;

	for(i=0;i<Populacao->linhas;i++){
		aux = (int **) realloc(aux, (i + 1) * sizeof(aux));
		aux[i]=malloc(Populacao->colunas * sizeof(int));
		for(j=0;j<Populacao->colunas;j++)
			aux[i][j]=Populacao->matriz[i][j];
	}
	int cont_aux=Filhos->linhas+Populacao->linhas;
	for(;i<cont_aux;i++){
		aux = (int **) realloc(aux, (i + 1) * sizeof(aux));
		aux[i]=malloc(Filhos->colunas * sizeof(int));
		for(j=0;j<Filhos->colunas;j++)
			aux[i][j]=Filhos->matriz[i-Populacao->linhas][j];
	}
	return aux;
}
int main(int argc, char *argv[]){
	int i, classificador=0, k_individuos=0, pts_corte=1;
	for(i=0;i<argc;i++){//help
		if(strcmp(argv[i],"-h")==0 || strcmp(argv[i],"-help")==0){
			printf("%sOpcoes%s\n\t%s-K%s\tUso do KNN como algoritmo de classificacao.\n",bold, none, bold, none);
			printf("\t%s-N%s\tUso do Naive Bayes como algoritmo de classificacao.\n", bold, none);
			printf("\t%s-P%s %sNUM%s\tDefine o tamanho da populacao inicial igual a NUM.\n",bold, none, sublinhado, none);
			printf("\t%s-C%s %sNUM%s\tQuantidade de pontos de corte na recombinacao igual a NUM.\n",bold, none, sublinhado, none);
			printf("%sExemplo:%s\t./AG -C 1 -P 100 -K\tEscolhido o KNN como classificador numa populacao de 100 individuos em que o cruzamento utiliza 1 ponto de corte.\n", bold, none);
			return -1;
		}

	}
	if(argc<3){
		printf("Numero de argumentos necessarios insuficientes\n.");
		printf("%sExemplo:%s\t./AG -C 1 -P 100 -K\tEscolhido o KNN como classificador numa populacao de 100 individuos em que o cruzamento utiliza 1 ponto de corte.\n", bold, none);
		return -1;
	}
	if(argc>=4){
		for(i=0;i<argc;i++){
			if(strcmp(argv[i],"-K")==0){
				classificador=1;
				//printf("Algoritmo de classificacao: KNN\n");
			}
			if(strcmp(argv[i],"-N")==0){
				classificador=2;
				//printf("Algoritmo de classificacao: Naive Bayes\n");
			}
			if(strcmp(argv[i],"-C")==0){
				//printf("Pontos de corte para cruzamento:\t %s\n", argv[i+1]);
				pts_corte=atoi(argv[i+1]);
				if(pts_corte>2){
					printf("Valor escolhido para o numero de pontos de corte e invalido. Escolha 1 ou 2 pontos de corte.\n");
							return -1;
				}
			}
			if(strcmp(argv[i],"-P")==0){
				//printf("Tamanho inicial da populacao:\t %s\n", argv[i+1]);
				k_individuos=atoi(argv[i+1]);
			}
		}
	}
	printf("Tamanho inicial da populacao: %d\n", k_individuos);
	printf("Pontos de corte para cruzamento: %d\n", pts_corte);
	printf("Algoritmo de classificacao: %s\n", (classificador==1 || classificador==0?"KNN":"Naive Bayes"));


	/*Dados Conjunto de Treino e Teste*/
	Matriz *Total, *Treino, *Teste;
	/*Total*/
	Total=malloc(sizeof(Matriz));
	Total= LerDados("../CTG_1500.csv");
	Total->matriz=TrocaLinhas(Total);//Troca de Linhas
	Total=getStatisticalData(Total);//Total --> Medias Variancias Desvio Padrao*/
	Total->matriz=Normalizar2(Total);//Normalizacao Estatistica
	/*Teste*/
	Teste=malloc(sizeof(Matriz));
	Teste->linhas=(1*Total->linhas/10);/*MUDAR DEPOIS ESTAMOS A TRABALHAR APENAS COM 10% PARA TESTE*/
	Teste->matriz=DividirMatriz(Teste, Total, 0);
	/*Treino*/
	Treino=malloc(sizeof(Matriz));
	Treino->linhas=(2*Total->linhas/10);
	Treino->matriz=DividirMatriz(Treino, Total, Teste->linhas);
	Treino=getStatisticalData(Treino);/*Treino --> Medias Variancias Desvio Padrao*/
	/*Fim Conjunto de Treino e Teste*/

	Ag *Filhos=NULL,*Populacao=NULL,*Nova_Populacao=NULL;
	int geracao=0, **aux=NULL,j,cont_aux,linha=0;
	printf("%sPopulacao Inicial%s\n",bold,none);
	Populacao=inicializarPopulacao(k_individuos);		/*Inicializa populacao de forma aleatoria*/
	//printMatriza(Populacao->matriz,Populacao->linhas,Populacao->colunas);
	Populacao=avaliaFitnessPop(Treino, Teste, Populacao,classificador);		/*Avalia Fitness da Populacao*/
	Populacao->matriz=ordenaMatrizFitness(Populacao->matriz,Populacao->linhas);	/*Ordenação da populacao por fitness*/
	printMatriza(Populacao->matriz,Populacao->linhas,Populacao->colunas);
	do{
		printf("%s%dª Geracao%s\n",bold,geracao+1,none);
		printf("%sFilhos%s\n", bold, none);
		Filhos=reproducao(Populacao,mascara_cruzamento(pts_corte));		/*Cruzamento de 1 Ponto*/
		Filhos=avaliaFitnessPop(Treino, Teste, Filhos, classificador);		/*Avalia Fitness dos Filhos*/
		Filhos->matriz=ordenaMatrizFitness(Filhos->matriz,Filhos->linhas);		/*Ordenação da populacao por fitness*/
		printMatriza(Filhos->matriz,Filhos->linhas,Filhos->colunas);

/*puts("Combinacao Matrizes Populacao e Filhos");
		//Escolher melhores casos de entre os Pais e os Filhos. Pais e Filhos competem entre si
		aux=combina_Matrizes(Populacao,Filhos);							//Combinar Populacao de Pais e Filhos
		aux=ordenaMatrizFitness(aux,Populacao->linhas+Filhos->linhas);	//Ordenacao de Pais e Filhos por Fitness
		cont_aux=Filhos->linhas+Populacao->linhas;
		printMatriza(aux,cont_aux,Populacao->colunas);*/

		printf("%sNova Populacao%s\n", bold, none);
		Nova_Populacao=malloc(sizeof(Ag));
		Nova_Populacao->matriz=malloc(1 * sizeof(int *));
		Nova_Populacao->colunas=Populacao->colunas;
		Nova_Populacao->linhas=k_individuos;
		Nova_Populacao->matriz[0]=Populacao->matriz[0];	/*Elitismo. Membro mais adaptado passa para a geracao seguinte*/
		for(i=1;i<k_individuos;i++){
			Nova_Populacao->matriz = (int **) realloc(Nova_Populacao->matriz, (i + 1) * sizeof(Nova_Populacao->matriz));
			Nova_Populacao->matriz[i]=malloc(Nova_Populacao->colunas * sizeof(int));
			//linha=selecaoTorneio(aux,cont_aux);//seleccao por torneio dos novos individuos. Pais e filhos competem entre si.
			for(j=0;j<Nova_Populacao->colunas;j++){
				//Nova_Populacao->matriz[i][j]=aux[i][j];//elitismo total
				//Nova_Populacao->matriz[i][j]=aux[linha][j];//seleccao por torneio dos novos individuos. Pais e filhos competem entre si.
				Nova_Populacao->matriz[i][j]=Filhos->matriz[i-1][j];//AG simples
			}
		}
		Nova_Populacao->matriz=ordenaMatrizFitness(Nova_Populacao->matriz,Nova_Populacao->linhas);
		printMatriza(Nova_Populacao->matriz, Nova_Populacao->linhas,Nova_Populacao->colunas);
		geracao++;
		Populacao->colunas=Nova_Populacao->colunas;
		Populacao->linhas=Nova_Populacao->linhas;
		Populacao->matriz=Nova_Populacao->matriz;
		for(i=0;i<Filhos->linhas;i++){
			Filhos->matriz[i]=NULL;
			free(Filhos->matriz[i]);
		}
		free(Filhos->matriz);
		free(Filhos);
		free(Nova_Populacao);
		//getchar();
	}while(geracao<100);//durante n geracoes
	printf("%sSolucao%s ",bold,none);
	printArraya(Populacao->matriz[0]);
	free(Populacao);

	return 0;
}
