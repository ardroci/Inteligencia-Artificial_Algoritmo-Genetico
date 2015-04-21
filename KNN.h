
#ifndef KNN_H_
#define KNN_H_
#endif /* KNN_H_ */

int K=3;

/*Prototipos*/
int classe(float **distEuclidiana);
float **distEuclidiana2(Matriz *Treino, Matriz *Teste, int linha);
int compare(Matriz *Teste, int classe, int linha);
float **ordernarDist(Matriz *Treino, float **distEuclidiana);

float Knn(Matriz *Treino, Matriz *Teste){
	float erro=0.0;
	int i,class=0, coluna=0;
	float **distEuclidiana;
	initMatrizConfusao(Teste);
	for(i=0;i<Teste->linhas;i++){
		distEuclidiana=distEuclidiana2(Treino, Teste, i);
		distEuclidiana=ordernarDist(Treino,distEuclidiana);
		//printMatrizDistancia(distEuclidiana, 7);
		//printf("-------\n");
		class=classe(distEuclidiana);
		while(class==0){//caso exista empate é aumentado o numero de vizinhos a considerar
			K=K+2;
			class=classe(distEuclidiana);
			//printf("Empate nas classes K = %d\n",K);//PROCURAR O PROXIMO VIZINHO
		}
		//printf("%d\n",class);
		K=3;
		if(compare(Teste,class,i)==1)
			Teste->matrizConfusao[class-1][class-1]=Teste->matrizConfusao[class-1][class-1]+1;//identificações corretas
		else{
			erro++;
			coluna = (int)(Teste->matriz[i][Teste->colunas-1]);
			Teste->matrizConfusao[coluna-1][class-1]=Teste->matrizConfusao[coluna-1][class-1]+1;//identificações
		}
	}
	return (100-(100*erro)/Teste->linhas);
}
float **distEuclidiana2(Matriz *Treino, Matriz *Teste, int linha){
	float **distEuclidiana,dist=0;
	distEuclidiana=malloc(sizeof(float*));
	int j, w;
	for(j=0;j<Treino->linhas;j++){
		dist=0;
		distEuclidiana=(float**)realloc(distEuclidiana,(j+1)*sizeof(*distEuclidiana));
		distEuclidiana[j]=malloc(2*sizeof(float));
		for(w=0;w<Treino->colunas-2;w++){//columns - CLASS e NSP
			dist=dist+pow(Teste->matriz[linha][w]-Treino->matriz[j][w],2);//distancia eucladiana
			if(isnan(dist))//verifica se a distancia é um número
				printf("Erro calculo distancia\n");
		}
		dist=sqrt(dist);
		distEuclidiana[j][0]=sqrt(dist);
		distEuclidiana[j][1]=Treino->matriz[j][Treino->colunas-1];
	}
	return distEuclidiana;
}
float **ordernarDist(Matriz *Treino, float **distEuclidiana){
	int i,ordenados=0;
	float aux1, aux2;
	while(ordenados==0){
		ordenados=1;
		for(i=0;i<Treino->linhas-1;i++){//ATENCAO AO NUMERO DE LINHAS
			if(distEuclidiana[i][0]>distEuclidiana[i+1][0]){
				aux1=distEuclidiana[i][0];
				aux2=distEuclidiana[i][1];

				distEuclidiana[i][0]=distEuclidiana[i+1][0];
				distEuclidiana[i][1]=distEuclidiana[i+1][1];

				distEuclidiana[i+1][0]=aux1;
				distEuclidiana[i+1][1]=aux2;
				ordenados=0;
			}
		}

	}
	return distEuclidiana;
}
int classe(float **distEuclidiana){//retorna classe com mais ocorrencias dentro de K
	int aux=0;
	int normal=0, suspected=0,pathologic=0;
	while(aux<K){
		if(distEuclidiana[aux][1]==1)
			normal++;
		if(distEuclidiana[aux][1]==2)
			suspected++;
		if(distEuclidiana[aux][1]==3)
			pathologic++;
		aux++;
	}
	if(normal>suspected && normal>pathologic)
		return 1;
	if(suspected>normal && suspected>pathologic)
		return 2;
	if(pathologic>normal && pathologic>suspected)
		return 3;
	//printf("normal %d, pathologic %d, suspected %d\n", normal, pathologic, suspected);
	return 0;
}
int compare(Matriz *teste, int classe, int linha){
	if(teste->matriz[linha][teste->colunas-1]==classe)
		return 1;
	return 0;
}
