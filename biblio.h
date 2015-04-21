
#ifndef BIBLIO_H_
#define BIBLIO_H_

#endif /* BIBLIO_H_ */
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <time.h>

#define classes 3

typedef struct MATRIZ{
	float **matriz;
	float *medias,*variancias,*desvPadrao, *frelat;
	int matrizConfusao[classes][classes];
	int linhas;
	int colunas;
}Matriz;

/*Prototipos*/
float **DividirMatriz(Matriz *M, Matriz *N, int lines);//no teste lines=0, treino lines=linesTeste
//float ** LerDados(char *ficheiro);
Matriz *LerDados(char *ficheiro);
float media(Matriz *M, int atributo);
float variancia(Matriz *M, int atributo);
float *DesvioPadrao(Matriz *M);
float *medias(Matriz *M);
float *variancias(Matriz *M);
float* frelativas(Matriz *M);
Matriz *getStatisticalData(Matriz *M);

/*Func Matematicas/Estatisticas*/
float maxValue(float n1,float n2,float n3){
	if((float)(n1-n2)>=0.0001 && (float)(n1-n3)>=0.0001)
		return 1.0;
	if((float)(n2-n1)>=0.0001 && (float)(n2-n3)>=0.0001)
		return 2.0;
	if((float)(n3-n1)>=0.0001 && (float)(n3-n2)>=0.0001)
		return 3.0;
	//puts("ERRO");
	return 0;
}
Matriz *getStatisticalData(Matriz *M){
	M->medias=medias(M);
	M->variancias = variancias(M);
	M->desvPadrao=DesvioPadrao(M);
	M->frelat=frelativas(M);
	return M;
}

float *DesvioPadrao(Matriz *M){
	int i;
	M->desvPadrao=NULL;
	M->desvPadrao=malloc(M->colunas * sizeof(float));
	for(i=0;i<M->colunas;i++){
		M->desvPadrao[i]=sqrt(M->variancias[i]);
	}
	return M->desvPadrao;
}
float media(Matriz *M, int atributo){
	int i=0;
	float media=0;
	for(i=0;i<M->linhas;i++)//ATENCAO AO NR DE LINHAS
		media = media+M->matriz[i][atributo];
	return media/M->linhas;
}
float *medias(Matriz *M){
	int i=0;
	float aux=0.0;
	M->medias=NULL;
	M->medias=malloc(M->colunas * sizeof(float));
	for(i=0;i<M->colunas;i++){
		aux=media(M,i);
		M->medias[i]=aux;
	}
	return M->medias;
}
float variancia(Matriz *M, int atributo){
	float variancia=0,aux=0.0;
	int i=0;
	for(i=0;i<M->linhas;i++)//ATENCAO AO NR DE LINHAS{
		variancia = variancia+pow(M->matriz[i][atributo]-M->medias[atributo],2);
	aux=variancia/(M->linhas-1);
	return aux;
}
float *variancias(Matriz *M){
	int i=0;
	M->variancias=NULL;
	M->variancias=malloc(M->colunas * sizeof(float));
	for(i=0;i<M->colunas;i++){
		M->variancias[i]=variancia(M,i);
	}
	return M->variancias;

}
float* frelativas(Matriz *M){
	int i=0,classe=0;
	float suspected=0,pathologic=0,normal=0;
	M->frelat=NULL;
	M->frelat=malloc(classes*sizeof(float));
	for(i=0;i<M->linhas;i++){
		classe=M->matriz[i][M->colunas-1];
		switch (classe){
			case 1:
				normal++;
				break;
			case 2:
				suspected++;
				break;
			case 3:
				pathologic++;
				break;
			//default:
				//printf("ERRO CLASSE - Frequência relativa");
		}
	}
	M->frelat[1]=100*(suspected/M->linhas);
	M->frelat[2]=100*(pathologic/M->linhas);
	M->frelat[0]=100*(normal/M->linhas);
	//printf("\nFreq Relativa\nNormal:%0.2f%%\nSuspected:%0.2f%%\nPathologic:%0.2f%%\n",M->frelat[0],M->frelat[1],M->frelat[2]);
	return M->frelat;
}
/*PreProcessing*/
float **Normalizar(Matriz *M){
	int i,j;
	for(i=0;i<M->linhas;i++){
		for(j=0;j<M->colunas-2;j++){
			if(M->desvPadrao[j]!=0)
				M->matriz[i][j]=(M->matriz[i][j]-M->medias[j])/M->desvPadrao[j];
		}
	}
	return M->matriz;
}
float minimo(Matriz *M,int col){
	float minimo;
	int i;
	minimo=M->matriz[0][col];
	for(i=0;i<M->linhas;i++){
		if(M->matriz[i][col]<minimo)
			minimo=M->matriz[i][col];
	}
	if(isnan(minimo)){
		printf("Erro escolha. Minimo=NaN\n");
		exit(0);
	}
	return minimo;
}
float maximo(Matriz *M,int col){
	float maximo;
	int i;
	maximo=M->matriz[0][col];
	for(i=0;i<M->linhas;i++){
		if(M->matriz[i][col]>maximo)
			maximo=M->matriz[i][col];
	}
	if(isnan(maximo)){
		printf("Erro escolha. Maximo=NaN\n");
		exit(0);
	}
	return maximo;
}
float **Normalizar2(Matriz *M){
	int i,j;
	for(i=0;i<M->linhas;i++){
		for(j=0;j<M->colunas-2;j++){
			M->matriz[i][j]=(M->matriz[i][j]-minimo(M,j))/(maximo(M,j)-minimo(M,j));
		}
	}
	return M->matriz;
}
int countVarianciasZero(Matriz *M){
	int j,c=0;
	for(j=0;j<M->colunas;j++){
		if(M->variancias[j]<=0.00001)
			c++;
	}
	return c;
}
float **removerColunas(Matriz *M){//remove coluna(s) cuja(s) variancia(s) <= 0.00001
	float **matriz=NULL;
	int i,j,z=0,cols=0;
	cols=M->colunas-countVarianciasZero(M);
	matriz=malloc(1+sizeof(float*));
	for(i=0;i<M->linhas;i++){
		matriz=(float**)realloc(matriz,(i+1)*sizeof(*matriz));
		matriz[i]=malloc((cols)*sizeof(float));
		z=0;
		for(j=0;j<M->colunas;j++){
			if(M->variancias[j]>=0.00001){
				matriz[i][z]=M->matriz[i][j];
				z++;
			}
		}

	}
	M->colunas=cols;
	return matriz;
}
float **DividirMatriz(Matriz *M, Matriz *N, int lines){//no teste lines=0, treino lines=linesTeste
	M->matriz=NULL;
	M->matriz=malloc(1+sizeof(float*));
	int i,j;
	for(i=0;i<M->linhas;i++){//ATENCAO AO NR DE LINHAS
		M->matriz=(float**)realloc(M->matriz,(i+1)*sizeof(*M->matriz));
		M->matriz[i]=malloc(M->colunas*sizeof(float));
		for(j=0;j<M->colunas;j++){
			M->matriz[i][j]=N->matriz[i+lines][j];
		}
	}
	M->colunas=N->colunas;
	return M->matriz;
}

/*Matriz Confusão*/
void initMatrizConfusao(Matriz *M){
	int i,j;
	for (i = 0; i < classes; i++)
	  for (j = 0; j < classes; j++)
	    M->matrizConfusao[i][j] = 0;
}
float Precision(Matriz *M, int classe){//identificacoes corretas / soma do nr de identificacoes da classe
	int sum=0, i;
	for(i=0;i<classes;i++)
		sum=sum+M->matrizConfusao[i][classe];
	if(sum==0)
		return 0;
	//printf("Precision: %.3f / %.3f =%.3f\n",(float)M->matrizConfusao[classe][classe], (float)sum, (float)M->matrizConfusao[classe][classe]/sum);
	return((float)M->matrizConfusao[classe][classe]/sum);
}
float Recall(Matriz *M, int classe){//identificacoes corretas / nr total de casos da classe
	int i=0, sum=0;
	for(i=0;i<classes;i++)
		sum+=M->matrizConfusao[classe][i];
	if(sum==0)
		return 0;
	return((float)M->matrizConfusao[classe][classe]/sum);
}
float Fmeasure(Matriz *M, int classe){
	float PrecisionVal = (float)Precision(M, classe);
	float RecallVal =(float) Recall(M, classe);
	if(PrecisionVal+RecallVal==0)
		return 0;
	return((float)(2.0*PrecisionVal*RecallVal)/(PrecisionVal+RecallVal));
}

/*Ficheiros*/
Matriz *LerDados(char *ficheiro){
	Matriz *M=malloc(sizeof(Matriz));;
	float **matriz=NULL;
	FILE *CL = fopen(ficheiro, "r");
	if(CL==NULL)
		printf("Ficheiro nao existe.\n");
	int line=0;
	matriz = malloc(1 * sizeof(float *));
	while(!feof(CL)){
		matriz = (float **) realloc(matriz, (line + 1) * sizeof(*matriz));
		matriz[line]=malloc(23 * sizeof(float));
		fscanf(CL, "%f;%f;%f;%f;%f;%f;%f;%f;%f;%f;%f;%f;%f;%f;%f;%f;%f;%f;%f;%f;%f;%f;%f\n" , &matriz[line][0],&matriz[line][1],&matriz[line][2],&matriz[line][3],&matriz[line][4],&matriz[line][5],&matriz[line][6],&matriz[line][7],&matriz[line][8],&matriz[line][9],&matriz[line][10],&matriz[line][11],&matriz[line][12],&matriz[line][13],&matriz[line][14],&matriz[line][15],&matriz[line][16],&matriz[line][17],&matriz[line][18],&matriz[line][19],&matriz[line][20], &matriz[line][21],&matriz[line][22]);
		line++;
    }
	M->matriz=matriz;
	M->linhas=line;
	M->colunas=23;
	fclose(CL);
	return M;
}
void printMatrizFile(Matriz *M, char* ficheiro){
	FILE *f = fopen(ficheiro, "w+");
	int line=0;
	for(line=0;line<M->linhas;line++)
		fprintf(f, "%f;%f;%f;%f;%f;%f;%f;%f;%f;%f;%f;%f;%f;%f;%f;%f;%f;%f;%f;%f;%f;%f;%f\n" , M->matriz[line][0],M->matriz[line][1],M->matriz[line][2],M->matriz[line][3],M->matriz[line][4],M->matriz[line][5],M->matriz[line][6],M->matriz[line][7],M->matriz[line][8],M->matriz[line][9],M->matriz[line][10],M->matriz[line][11],M->matriz[line][12],M->matriz[line][13],M->matriz[line][14],M->matriz[line][15],M->matriz[line][16],M->matriz[line][17],M->matriz[line][18],M->matriz[line][19],M->matriz[line][20], M->matriz[line][21],M->matriz[line][22]);
	fclose(f);
}
void printArrayFile(float *M, char* ficheiro){
	FILE *f = fopen(ficheiro, "a+");
	int i;
	for(i=0;i<22;i++)
		fprintf(f, "%f;" , M[i]);
	fprintf(f, "\n");
	fclose(f);
}
int rand_lim(Matriz *m) {//Gera random de 0 a lines-1
	int r;
	struct timeval t1;
	do{
		gettimeofday(&t1, NULL);
		srand(t1.tv_usec * t1.tv_sec);
		r = rand() % m->linhas;
	} while (r > m->linhas);

    return r;
}
float **TrocaLinhas(Matriz *M){
	float aux;
	int j, a, b, trocas=0;
	while(trocas<M->linhas/2){
		a = rand_lim(M);
		b = rand_lim(M);
		for(j=0;j<M->colunas;j++){
			aux=M->matriz[a][j];
			M->matriz[a][j]=M->matriz[b][j];
			M->matriz[b][j]=aux;
		}
		trocas++;
	}

	return M->matriz;
}

/*Prints*/
void printMatrizConfusao(Matriz *M){
	int i,j;
	printf("Matriz Confusão\n");
	for(i=0;i<classes;i++){
		printf("\t");
		for(j=0;j<classes;j++){
			printf("%d  ",M->matrizConfusao[i][j]);
		}
		printf("\n");
	}
}
void printMatrizDistancia(float **matriz, int linhas){
	int i;
	for(i=0;i<linhas;i++)
		printf("%f %f \n", matriz[i][0],matriz[i][1]);
}
void printMatriz(Matriz *M, int linhas){
	int i=0,j=0;
	for(i=0;i<linhas;i++){
		for(j=0;j<M->colunas;j++)
			printf("%f  ", M->matriz[i][j]);
		printf("\n");
	}
}
void printArray(float *array, int cols){
	int i=0;
	for(i=0;i<cols;i++)
		printf("%.6f  ",array[i]);
	printf("\n");

}
