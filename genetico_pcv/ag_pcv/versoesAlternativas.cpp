#include <algorithm>
#include <ctime>
#include <chrono>
#include <cmath>
#include <fstream>
#include <iostream>
#include <iomanip>
#include <random>
#include <sstream>
#include <string>
#include <vector>

using namespace std;

/* ******************************
 *          CONSTANTES          *
 ****************************** */

#define MAXPOPULSIZE 40
#define TAXMUT 0.08
#define BIGMUT 0.08
//#define MAXITERATIONAG 10000
//#define MAXITERATIONLOCAL 500
#define MAXTIMEAG 3600
#define MAXTIMELOCAL 10


/* ******************************
 *      ALGORITMO GENETICO      *
 ****************************** */

typedef struct coord { // coordenadas de uma cidade
    float x;
    float y;

    coord() {}
    coord(float a, float b) : x(a), y(b) {}
} coord;

typedef struct chromosome {
    vector<size_t> order; // ordem das cidades
    double dist; // distancia

    chromosome() {}
    chromosome(vector<size_t> v, double d = 0) : order(v), dist(d) {}

    bool operator<(const chromosome &s) const
    {
        return this->dist < s.dist;
    }
} chromosome;

vector<coord> vertex_list;

vector<chromosome> population;
chromosome best_solution({}, numeric_limits<double>::max());

float distance(const coord &v1, const coord &v2)
{
    return sqrt(pow(v1.x - v2.x, 2) + pow(v1.y - v2.y, 2));
}

void evaluate_solution(chromosome &s)
{
    auto soma = distance(vertex_list[s.order[s.order.size() - 1]], vertex_list[s.order[0]]);
    for (size_t i = 0; i < s.order.size() - 1; i++)
        soma += distance(vertex_list[s.order[i]], vertex_list[s.order[i+1]]);

    s.dist = soma;
}

chromosome tournament(const vector<chromosome> &c)
{
    chromosome best = c[0];
    for (size_t i = 1; i < c.size(); i++) {
        if (best.dist < c[i].dist) best = c[i];
    }
    return best;
}

pair<chromosome, chromosome> select_parents()
// selecao pela roleta - chance de escolha é proporcional à aptidao do individuo - EU NAO SEI COMO COLOCAR
// PESOS NA CHANCE DE ESCOLHA ALEATORIA
// selecao por torneio: escolher aleatoriamente k indivíduos e pegar o melhor deles (k=2,3 ou 4, em geral)
{ // TORNEIO - K = 3, 4 -- com 4 ficou melhor que 3
    vector<chromosome> p1;
    srand(time(NULL));
    p1.push_back(population[rand()%population.size()]);
    p1.push_back(population[rand()%population.size()]);
    p1.push_back(population[rand()%population.size()]);

    vector<chromosome> p2;
    p2.push_back(population[rand()%population.size()]);
    p2.push_back(population[rand()%population.size()]);
    p2.push_back(population[rand()%population.size()]);

    return {tournament(p1), tournament(p2)};
}

vector<size_t>::iterator find(vector<size_t> &v, size_t n)
{
    for (vector<size_t>::iterator it = v.begin(); it != v.end(); it++) {
        if (*it == n) return it;
    }
    return v.end();
}

chromosome crossover_1pt(const pair<chromosome, chromosome> &parents)
{
    size_t index = parents.first.order.size()/2;

    chromosome child1, child2;

    for (size_t i = 0; i < index; i++) {
        child1.order.push_back(parents.first.order[i]);
        child2.order.push_back(parents.second.order[i]);
    }

    vector<size_t> v1(parents.second.order);
    vector<size_t> v2(parents.first.order);

    for (size_t i = 0; i < child1.order.size(); i++) {
        v1.erase(find(v1, child1.order[i]));
        v2.erase(find(v2, child2.order[i]));
    }

    child1.order.insert(child1.order.end(), v1.begin(), v1.end());
    child2.order.insert(child2.order.end(), v2.begin(), v2.end());
    evaluate_solution(child2);
    evaluate_solution(child1);

    if (child1.dist < child2.dist) return child1;

    return child2;
}

chromosome crossover_2pt(const pair<chromosome, chromosome> &parents)
{   //      [a b c d] [a b c d]
    srand(time(NULL));
    size_t index1 = rand()%vertex_list.size();
    size_t index2 = rand()%vertex_list.size();

    if (index1 > index2) swap(index1, index2);

   chromosome child1, child2;

    for (size_t i = index1; i < index2; i++) {
        child1.order.push_back(parents.first.order[i]);
        child2.order.push_back(parents.second.order[i]);
    }

    vector<size_t> v1(parents.second.order);
    vector<size_t> v2(parents.first.order);

    for (size_t i = 0; i < child1.order.size(); i++) {
        v1.erase(find(v1, child1.order[i]));
        v2.erase(find(v2, child2.order[i]));
    }

    child1.order.insert(child1.order.end(), v1.begin(), v1.end());
    child2.order.insert(child2.order.end(), v2.begin(), v2.end());
    evaluate_solution(child2);
    evaluate_solution(child1);

    if (child1.dist < child2.dist) return child1;

    return child2;
}

void mutation(chromosome &child, float tx_mut)
{   
    srand(time(NULL));
    int aux;
    for (size_t i = 0; i < child.order.size(); i++) { 
        aux = rand() % 1000;

        if(aux < tx_mut*1000){
        auto j = rand()%child.order.size();
        auto aux = child.order[i];
        child.order[i] = child.order[j];
        child.order[j] = aux;

        }

    }
    
}

void bigMutation(chromosome &child, float tx_mut) 
{   
    srand(time(NULL));
    unsigned aux;
    aux = rand() % 1000;
    if(aux < tx_mut*1000)
    {   
        unsigned i = rand() % child.order.size();
        unsigned j = rand() % child.order.size();

        if( i < j ){
            for (; i <= j; i++)
            {
                auto aux = child.order[i];
                child.order[i] = child.order[j];
                child.order[j] = aux;
                j--;
            }
            

        }
        else{
            for (; j < i; j++)
            {   
                auto aux = child.order[i];
                child.order[i] = child.order[j];
                child.order[j] = aux;
                i--;
            }

        }

    
    }
}


chromosome opt2_1st_improv(chromosome &s)
{   
    bool melhoria = true;

    auto start = chrono::high_resolution_clock::now();
    ios_base::sync_with_stdio(false);
    auto end = chrono::high_resolution_clock::now();
    double time_taken = chrono::duration_cast<chrono::nanoseconds>(end - start).count();
    time_taken *= 1e-9;

    while(melhoria && time_taken < MAXTIMELOCAL){ // tem q colocar um criterio de parada pra nao ficar infinito aqui
        melhoria = false;
        
        for (size_t i = 0; i < s.order.size() - 2 && melhoria == false; i++) {
            for (size_t j = i + 2; j < s.order.size(); j++) {
                chromosome s2(s.order);
                swap(s2.order[i], s2.order[j]);
                evaluate_solution(s2);

                if (s2.dist < s.dist){
                    s = s2;
                    melhoria = true;
                    break;
                }
            }
        }
        end = chrono::high_resolution_clock::now();
        time_taken = chrono::duration_cast<chrono::nanoseconds>(end - start).count();
        time_taken *= 1e-9;
    }
    return s;
}

void update_population(const chromosome &child)
{   
    if(population.size() < MAXPOPULSIZE){// insere se for melhor que que a pior solução
        if (population.front().dist > child.dist) {
            population.push_back(child);
            push_heap(population.begin(), population.end());
        }
    }
    else{// Recebe o resultado da busca na vizinhança do melhor filho e troca com a pior solucao da pop.
        if (population.front().dist > child.dist) {
            pop_heap(population.begin(), population.end());
            population.pop_back();
            population.push_back(child);
            push_heap(population.begin(), population.end());
        }
    }
        if (child.dist <= best_solution.dist) best_solution = child;
}

void generate_population()
{
    srand(time(NULL));
    population.resize(20);
    vector<size_t> aux;

    for (size_t i = 0; i < vertex_list.size(); i++) aux.push_back(i);

    for (size_t i = 0; i < 20; i++) {
        chromosome s(aux);
//        unsigned seed = chrono::system_clock::now().time_since_epoch().count(); // FIX ME - COLOCAR ALEATORIO
        shuffle(s.order.begin(), s.order.end(), default_random_engine(rand()));
        evaluate_solution(s);
        population[i] = opt2_1st_improv(s);

        if (s.dist < best_solution.dist) best_solution = s;
    }
    make_heap(population.begin(), population.end());
    cout << "POPULACAO INICIAL CRIADA. TAMANHO: " << population.size() << endl;
}

// void reInit_population()
// {
//     vector<size_t> aux;
//     for (size_t i = 0; i < vertex_list.size(); i++) aux.push_back(i);
//     for (size_t i = 0; i < 10; i++) {
//         chromosome s(aux);
//         shuffle(s.order.begin(), s.order.end(), default_random_engine(rand()));
//         evaluate_solution(s);
//         opt2_1st_improv(s);

//         population[i] = s;
        
//         if (s.dist < best_solution.dist) best_solution = s;
//     }
//     make_heap(population.begin(), population.end());
//     cout << "POPULACAO INICIAL RECRIADA. TAMANHO: " << population.size() << endl;
// }

void reInit_population()
{
    while(population.size() > 20){
        pop_heap(population.begin(), population.end());
        population.pop_back();
        
    }
    // vector<size_t> aux; // nesse caso inseria novos individuos junto pra tentar aumentar a variedade
    // for (size_t i = 0; i < vertex_list.size(); i++) aux.push_back(i);
    // for (size_t i = 0; i < 10; i++) {
    //     chromosome s(aux);
    //     shuffle(s.order.begin(), s.order.end(), default_random_engine(rand()));
    //     evaluate_solution(s);
    //     opt2_1st_improv(s);

    //     population.push_back(s);
    //     push_heap(population.begin(), population.end());
        
    //     if (s.dist < best_solution.dist) best_solution = s;
    // }
    cout << "POPULACAO INICIAL RECRIADA. TAMANHO: " << population.size() << endl;
}

void ag()
{
    generate_population();
    for (size_t i = 0; i < population.size(); i++)
    {
     cout << population[i].dist <<endl;   
    }
    size_t iteration = 0;
    int stat = 0;
    double best_prev = numeric_limits<float>::max();
    /////////////////////time///////////////////////
    auto start = chrono::high_resolution_clock::now();
    ios_base::sync_with_stdio(false);
    auto end = chrono::high_resolution_clock::now();
    double time_taken = chrono::duration_cast<chrono::nanoseconds>(end - start).count();
    time_taken *= 1e-9;
    ////////////////////time////////////////////////
    while (time_taken < MAXTIMEAG) { // define um tempo pra rodar
        pair<chromosome, chromosome> parents = select_parents();
        chromosome child = crossover_2pt(parents);
        mutation(child, TAXMUT);
        bigMutation(child, BIGMUT);
        evaluate_solution(child);
        update_population(opt2_1st_improv(child));
        iteration ++;

        if (best_solution.dist < best_prev) {
            cout << fixed << setprecision(2);
            cout << "ITERACAO: " << iteration
                      << "    MELHOR SOLUCAO: " << best_solution.dist << endl;
                      cout << "TAMANHO DA POPULACAO : " << population.size() << endl;
            best_prev = best_solution.dist;
            stat = 0;
        } else {
            stat ++;
        }
        // if(iteration % 100000 == 0)
        // cout << "TAMANHO DA POPULACAO : " << population.size() << endl;
        if(population.size() == MAXPOPULSIZE ){
             reInit_population();
         }// se estagnar repopula mas nao da pra achar um numero bom geral
        /////////////////////time///////////////////////
        end = chrono::high_resolution_clock::now();
        time_taken = chrono::duration_cast<chrono::nanoseconds>(end - start).count();
        time_taken *= 1e-9;
        ////////////////////time////////////////////////

    }
    cout << "Melhor solução: " << best_solution.dist << endl;
    cout << "Total de iteracoes: " << iteration << endl; 
    
    for (size_t i = 0; i < population.size(); i++)
    {
     cout << population[i].dist <<endl;   
    }
}


/* ******************************
 *           READ DATA          *
 ****************************** */

void read_data(const string &file)
{
    ifstream input(file);
    string line;

    while (getline(input, line)) {
        int n;
        coord v;
        istringstream iss(line);

        if ((iss >> n >> v.x >> v.y)) vertex_list.push_back(v);
    }
    input.close();
    cout << "LEITURA OK. QTD CIDADES: " << vertex_list.size() << endl;
}


/* ******************************
 *             MAIN             *
 ****************************** */

int main()
{
    srand(time(NULL));
    //    string s;
    //    cin >> s;
    //    read_data(s);

//    read_data("kroA100.tsp");  // 21 282
//   read_data("tsp225.tsp");   // 3 916
//    read_data("a280.tsp");     // 2 579
    read_data("gr666.tsp");    // 294 358
//    read_data("pr1002.tsp");   // 259 045
//    read_data("pcb1173.tsp");  // 56 892
//    read_data("pla33810.tsp"); // 66 048 945
//    read_data("pla85900.tsp"); // 142 382 641
    auto start = chrono::high_resolution_clock::now();
    ios_base::sync_with_stdio(false);

    ag();

    auto end = chrono::high_resolution_clock::now();
            double time_taken = chrono::duration_cast<chrono::nanoseconds>(end - start).count();
            time_taken *= 1e-9;
            cout << "TIME: " << fixed
                      << time_taken << setprecision(5) << " sec" << endl;
// TESTE DISTANCIA TOTAL
//        vertex_list.push_back(coord(-1, -1));
//        vertex_list.push_back(coord(-2, -1));
//        vertex_list.push_back(coord(-2, -2));
//        vertex_list.push_back(coord(-1, -2));
//        chromosome c;
//        c.order = {0, 1 , 2 , 3};
//        evaluate_solution(c);
//        cout << c.dist << endl;
// //TESTE MUTAÇÃO
//     vector<size_t> order;
//         for (size_t i = 0; i < 10; i++) order.push_back(i);
//     for (size_t i = 0; i < 10; i++)
//     {
        
            
//         int aux;
//         for (size_t i = 0; i < order.size(); i++) { 
//             aux = rand() % 1000;

//             if(aux < 0.1*1000){
//             swap(order[i],order[rand()%order.size()]);

//             }

//         }
//         for (size_t i = 0; i < 10; i++)
//         cout<<order[i]<<endl;
//         cout<<"------"<<endl;
//     }
    return 0;
}
