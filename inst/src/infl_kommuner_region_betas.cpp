//~ * Compile with:
//~ g++ -std=c++11 -o infl_kommuner.exe infl_kommuner.cpp
//~ * ./infl_kommuner.exe
//~ * */
// #include "stdafx.h"
#include "infl_kommuner.h"
#include <math.h>       /* exp */
#include <boost/math/distributions/students_t.hpp>
using namespace std;
//using boost::math::gamma;
default_random_engine generator;

int random_binomial(int n, double p){
  binomial_distribution<int> distribution(n,p);
  return distribution(generator);
}

void cumulative_sum(double **outarray, double **array, int n){
  /// REMEMBER to delete outarray after use
  (*outarray)[0] = (*array)[0];
  for(int i=1; i < n; ++i){
    (*outarray)[i] = (*outarray)[i-1] + (*array)[i];
  }
}

void seir_sim(int &ds, int &de1, int &de2, int &dia, int &di,       // Outputs
              int S, int E, int Ia, int I, float beta, float a,  float gamma, int pop, double delta_t){ // Inputs
  ds = 0; de1 = 0; de2 = 0; dia = 0; di = 0;
  // de1 are the exposed going to asymptomatic, de2 are the exposed going to symptomatic
  int de = 0;
  if(I != 0 || Ia != 0 || E != 0){
    if( E == 0){
      de = 0;
    }
    else{
      de = random_binomial(E, a*delta_t);
      if(de != 0){
        de1 = random_binomial(de, 0.33);
        de2 = de-de1;
      }
    }
    if(I == 0){
      di = 0;
    }
    else{
      di = random_binomial(I, gamma*delta_t);
    }
    if(Ia == 0){
      dia = 0;
    }
    else{
      dia = random_binomial(Ia, gamma*delta_t);
    }
    ds = random_binomial(S, beta*delta_t*I/pop + 0.5*beta*delta_t*Ia/pop);
  }
}


Location::Location(string name_, int Shome){
  name = name_;
  S = Shome;
  E = 0;
  I = 0;
  Ia = 0;
  R = 0;
  N = 0;
}

void Location::add_inlink(Link *link){
  // This function adds a pointer to a newly created link that points to this location
  in_links.push_back(link);
}

void Location::add_outlink(Link  *link){
  // This function adds a pointer to a newly created link that points from this location
  out_links.push_back(link);

}
void Location::add_inlink2(Link *link){
  // This function adds a pointer to a newly created link that points to this location
  in_links2.push_back(link);
}

void Location::add_outlink2(Link  *link){
  // This function adds a pointer to a newly created link that points from this location
  out_links2.push_back(link);

}

void Location::print(){
  cout << "Location " << name << " with S= "<< S << ", E=" << E <<", I=" << I << ", Ia=" << Ia << ", R= " << R << ". " << endl;
  cout << "out_links: "<< out_links.size() <<endl;
  for(int i = 0; i < out_links.size(); ++i){
    cout << "Trying to print from out_link " << i << endl;
    out_links[i]->print();
    cout.flush();
  }
  cout << "in_links: "<< in_links.size()<< endl;
  for(int i = 0; i < in_links.size(); ++i){
    cout << "Trying to print from in_link " << i << endl;
    in_links[i]->print();
    cout.flush();
  }

}

void Location::seir_step_day(int day, int locnum, float beta, float a, float gamma, int &de2){
  int S_tmp = S;
  int E_tmp = E;
  int Ia_tmp = Ia;
  int I_tmp = I;
  int R_tmp = R;
  double pop_tmp = S + E + Ia + I + R;
  int num = in_links.size();
  int *S_probs = new int[num+1]; // Every in_link goes here, and home_pop last
  int *E_probs = new int[num+1];
  int *I_probs = new int[num+1];
  int *Ia_probs = new int[num+1];
  int *R_probs = new int[num+1];
  for(int i = 0; i < in_links.size(); ++i){
    S_tmp += in_links[i]->S;
    E_tmp += in_links[i]->E;
    Ia_tmp += in_links[i]->Ia;
    I_tmp += in_links[i]->I;
    R_tmp += in_links[i]->R;
    pop_tmp += in_links[i]->S + in_links[i]->E + in_links[i]->Ia + in_links[i]->I + in_links[i]->R;

    S_probs[i] = in_links[i]->S;
    I_probs[i] = in_links[i]->I;
    Ia_probs[i] = in_links[i]->Ia;
    E_probs[i] = in_links[i]->E;
    R_probs[i] = in_links[i]->R;
  }

  S_probs[num] = S;
  I_probs[num] = I;
  Ia_probs[num] = Ia;
  E_probs[num] = E;
  R_probs[num] = R;
  int ds; int de1; int dia; int di;


  seir_sim(ds, de1, de2, dia, di, S_tmp, E_tmp, Ia_tmp, I_tmp, beta, a, gamma, pop_tmp, 12.0/24.0);

  double *probs = new double[num+1];
  double *probs_cum = new double[num+1];
  double randomnumber;
  int index = -1;
  for(int i = 0; i < dia; ++i){
    for(int k = 0; k < num+1; ++k) probs[k] =Ia_probs[k]*1.0/ Ia_tmp; // Oneline for-loop
    randomnumber = (rand()%100)/100.0;
    if (randomnumber == 0){
      randomnumber = 0.0001;
    }
    if (randomnumber == 1){
      randomnumber = 0.9999;
    }
    cumulative_sum(&probs_cum, &probs, num+1);
    for(int h = 0; h < num + 1; ++h){
      if(randomnumber < probs_cum[h]){
        index = h;
        break;
      }
    }
    if(index == num){
      // Add to home pop
      Ia -= 1;
      R += 1;
    }
    else{
      in_links[index]->Ia -= 1;
      in_links[index]->R += 1;
    }

    Ia_probs[index] -= 1;
    Ia_tmp -= 1;

  }

  for(int i = 0; i < di; ++i){
    for(int k = 0; k < num+1; ++k) probs[k] =I_probs[k]*1.0/ I_tmp; // Oneline for-loop
    randomnumber = (rand()%100)/100.0;
    if (randomnumber == 0){
      randomnumber = 0.0001;
    }
    if (randomnumber == 1){
      randomnumber = 0.9999;
    }
    cumulative_sum(&probs_cum, &probs, num+1);
    for(int h = 0; h < num + 1; ++h){
      if(randomnumber < probs_cum[h]){
        index = h;
        break;
      }
    }
    if(index == num){
      // Add to home pop
      I -= 1;
      R += 1;
    }
    else {
      in_links[index]->I -= 1;
      in_links[index]->R += 1;
    }
    I_probs[index] -= 1;
    I_tmp -= 1;

  }


  for(int i = 0; i < de1; ++i){
    for(int k = 0; k < num+1; ++k) probs[k] =E_probs[k]*1.0/ E_tmp; // Oneline for-loop
    randomnumber = (rand()%100)/100.0;
    if (randomnumber == 0){
      randomnumber = 0.0001;
    }
    if (randomnumber == 1){
      randomnumber = 0.9999;
    }
    cumulative_sum(&probs_cum, &probs, num+1);
    for(int h = 0; h < num + 1; ++h){
      if(randomnumber < probs_cum[h]){
        index = h;
        break;
      }
    }
    if(index == num){
      // Add to home pop
      E -= 1;
      Ia += 1;
    }
    else{
      in_links[index]->E -= 1;
      in_links[index]->Ia += 1;
    }

    E_probs[index] -= 1;
    E_tmp -= 1;

  }

  for(int i = 0; i < de2; ++i){
    for(int k = 0; k < num+1; ++k) probs[k] =E_probs[k]*1.0/ E_tmp; // Oneline for-loop
    randomnumber = (rand()%100)/100.0;
    if (randomnumber == 0){
      randomnumber = 0.0001;
    }
    if (randomnumber == 1){
      randomnumber = 0.9999;
    }
    cumulative_sum(&probs_cum, &probs, num+1);
    for(int h = 0; h < num + 1; ++h){
      if(randomnumber < probs_cum[h]){
        index = h;
        break;
      }
    }
    if(index == num){
      // Add to home pop
      E -= 1;
      I += 1;
    }
    else{
      in_links[index]->E -= 1;
      in_links[index]->I += 1;
    }
    E_probs[index] -= 1;
    E_tmp -= 1;

  }


  for(int i = 0; i < ds; ++i){
    for(int k = 0; k < num+1; ++k) probs[k] =S_probs[k]*1.0/S_tmp; // Oneline for-loop
    randomnumber = ((rand()%100))/100.0;
    if (randomnumber == 0){
      randomnumber = 0.0001;
    }
    if (randomnumber == 1){
      randomnumber = 0.9999;
    }
    cumulative_sum(&probs_cum, &probs, num+1);
    for(int h = 0; h < num+1; ++h){
      if(randomnumber < probs_cum[h]){
        index = h;
        break;
      }
    }
    if(index == num){
      // Add to home pop
      S -= 1;
      E += 1;
    }
    else{
      in_links[index]->S -= 1;
      in_links[index]->E += 1;
    }
    S_probs[index] -= 1;
    S_tmp -= 1;

  }


  delete[] probs;
  delete[] probs_cum;

  delete[] S_probs;
  delete[] E_probs;
  delete[] I_probs;
  delete[] Ia_probs;
  delete[] R_probs;
}

void Location::seir_step_night(int day, int locnum, float beta, float a, float gamma, int &de2){
  int S_tmp = S;
  int E_tmp = E;
  int Ia_tmp = Ia;
  int I_tmp = I;
  int R_tmp = R;
  double pop_tmp = S + E + Ia + I + R;
  int num = out_links.size();
  int *S_probs = new int[num+1]; // Every in_link goes here, and home_pop last
  int *E_probs = new int[num+1];
  int *I_probs = new int[num+1];
  int *Ia_probs = new int[num+1];
  int *R_probs = new int[num+1];
  for(int i = 0; i < out_links.size(); ++i){
    S_tmp += out_links[i]->S;
    E_tmp += out_links[i]->E;
    Ia_tmp += out_links[i]->Ia;
    I_tmp += out_links[i]->I;
    R_tmp += out_links[i]->R;
    pop_tmp += out_links[i]->S + out_links[i]->E + out_links[i]->Ia + out_links[i]->I + out_links[i]->R;

    S_probs[i] = out_links[i]->S;
    I_probs[i] = out_links[i]->I;
    Ia_probs[i] = out_links[i]->Ia;
    E_probs[i] = out_links[i]->E;
    R_probs[i] = out_links[i]->R;
  }
  S_probs[num] = S;
  I_probs[num] = I;
  Ia_probs[num] = Ia;
  E_probs[num] = E;
  R_probs[num] = R;
  int ds; int de1; int dia; int di;


  seir_sim(ds, de1, de2, dia, di, S_tmp, E_tmp, Ia_tmp, I_tmp, beta, a, gamma, pop_tmp, 12.0/24.0);

  double *probs = new double[num+1];
  double *probs_cum = new double[num+1];
  double randomnumber;
  int index = -1;
  for(int i = 0; i < dia; ++i){
    for(int k = 0; k < num+1; ++k) probs[k] =Ia_probs[k]*1.0/ Ia_tmp; // Oneline for-loop
    randomnumber = (rand()%100)/100.0;
    if (randomnumber == 0){
      randomnumber = 0.0001;
    }
    if (randomnumber == 1){
      randomnumber = 0.9999;
    }
    cumulative_sum(&probs_cum, &probs, num+1);
    for(int h = 0; h < num + 1; ++h){
      if(randomnumber < probs_cum[h]){
        index = h;
        break;
      }
    }
    if(index == num){
      // Add to home pop
      Ia -= 1;
      R += 1;
    }
    else if(index < out_links.size()){
      out_links[index]->Ia -= 1;
      out_links[index]->R += 1;
    }

    Ia_probs[index] -= 1;
    Ia_tmp -= 1;

  }

  for(int i = 0; i < di; ++i){
    for(int k = 0; k < num+1; ++k) probs[k] =I_probs[k]*1.0/ I_tmp; // Oneline for-loop
    randomnumber = (rand()%100)/100.0;
    if (randomnumber == 0){
      randomnumber = 0.0001;
    }
    if (randomnumber == 1){
      randomnumber = 0.9999;
    }
    cumulative_sum(&probs_cum, &probs, num+1);
    for(int h = 0; h < num + 1; ++h){
      if(randomnumber < probs_cum[h]){
        index = h;
        break;
      }
    }
    if(index == num){
      // Add to home pop
      I -= 1;
      R += 1;
    }
    else if(index < out_links.size()){
      out_links[index]->I -= 1;
      out_links[index]->R += 1;
    }


    I_probs[index] -= 1;
    I_tmp -= 1;

  }

  for(int i = 0; i < de1; ++i){
    for(int k = 0; k < num+1; ++k) probs[k] =E_probs[k]*1.0/ E_tmp; // Oneline for-loop
    randomnumber = (rand()%100)/100.0;
    if (randomnumber == 0){
      randomnumber = 0.0001;
    }
    if (randomnumber == 1){
      randomnumber = 0.9999;
    }
    cumulative_sum(&probs_cum, &probs, num+1);
    for(int h = 0; h < num + 1; ++h){
      if(randomnumber < probs_cum[h]){
        index = h;
        break;
      }
    }
    if(index == num){
      // Add to home pop
      E -= 1;
      Ia += 1;
    }
    else if(index < out_links.size()){
      out_links[index]->E -= 1;
      out_links[index]->Ia += 1;
    }

    E_probs[index] -= 1;
    E_tmp -= 1;

  }

  for(int i = 0; i < de2; ++i){
    for(int k = 0; k < num+1; ++k) probs[k] =E_probs[k]*1.0/ E_tmp; // Oneline for-loop
    randomnumber = (rand()%100)/100.0;
    if (randomnumber == 0){
      randomnumber = 0.0001;
    }
    if (randomnumber == 1){
      randomnumber = 0.9999;
    }
    cumulative_sum(&probs_cum, &probs, num+1);
    for(int h = 0; h < num + 1; ++h){
      if(randomnumber < probs_cum[h]){
        index = h;
        break;
      }
    }
    if(index == num){
      // Add to home pop
      E -= 1;
      I += 1;
    }
    else if(index < out_links.size()){
      out_links[index]->E -= 1;
      out_links[index]->I += 1;
    }
    E_probs[index] -= 1;
    E_tmp -= 1;

  }

  for(int i = 0; i < ds; ++i){
    for(int k = 0; k < num+1; ++k) probs[k] =S_probs[k]*1.0/ S_tmp; // Oneline for-loop
    randomnumber = (rand()%100)/100.0;
    if (randomnumber == 0){
      randomnumber = 0.0001;
    }
    if (randomnumber == 1){
      randomnumber = 0.9999;
    }
    cumulative_sum(&probs_cum, &probs, num+1);
    for(int h = 0; h < num + 1; ++h){
      if(randomnumber < probs_cum[h]){
        index = h;
        break;
      }
    }

    if(index == num){
      // Add to home pop
      S -= 1;
      E += 1;
    }
    else if(index < out_links.size()){
      out_links[index]->S -= 1;
      out_links[index]->E += 1;
    }

    S_probs[index] -= 1;
    S_tmp -= 1;

  }

  delete[] probs;
  delete[] probs_cum;

  delete[] S_probs;
  delete[] E_probs;
  delete[] I_probs;
  delete[] Ia_probs;
  delete[] R_probs;
}


Link::Link(Location *from_, Location *to_, int S_, int E_, int I_, int Ia_, int R_){
  from = from_;
  to = to_;
  S  = S_;
  E  = E_;
  I  = I_;
  Ia = Ia_;
  R  = R_;
}

void Link::print(){
  cout << "Link from " << from->name << ", to " << to->name << ", with S= "<< S << ", E=" << E <<", I=" << I << ", Ia=" << Ia << ", R= " << R << endl;
}


Graph::Graph(){
}

void Graph::add_node(string name, int Shome){
  Location newlocation(name, Shome);
  locations.push_back(newlocation);
}

void Graph::add_edge(string name1, string name2, int S, int E, int I, int Ia, int R){
  /// Find locations with names name1 and name2
  int name1_index = -1;
  int name2_index = -1;
  for (int i = 0; i < locations.size(); ++i){
    if(locations[i].name.compare(name1) == 0){
      name1_index = i;
    }
    if(locations[i].name.compare(name2) == 0){
      name2_index = i;
    }
  }
  if( ((name1_index == -1) || (name2_index == -1)) || (name1_index == name2_index)){
    cout << "Error in add edge with input: " << name1 << ", " << name2 << ", " << S << ", " << E << ", " << I << ", " << Ia << ", " << R << endl;
    cout << "name1_index = " << name1_index << ", name2_index = " << name2_index << endl;
    exit(1);
  }
  Link newlink(&(locations[name1_index]), &(locations[name2_index]), S, E, I, Ia, R);
  newlink.from_index = name1_index;
  newlink.to_index = name2_index;
  edges.push_back(newlink);
}


void Graph::add_edge_index(int i1, int i2, int S, int E, int I, int Ia, int R){
  Link newlink(&(locations[i1]), &(locations[i2]), S, E, I, Ia, R);
  edges.push_back(newlink);
}

void Graph::add_edge_index2(int i1, int i2, int S, int E, int I, int Ia, int R){
  Link newlink(&(locations[i1]), &(locations[i2]), S, E, I, Ia, R);
  edges2.push_back(newlink);
}

void Graph::inform_locations_of_edges(){
  for (vector<Link>::iterator it = edges.begin() ; it != edges.end(); ++it){
    (*it).from->add_outlink(&(*it));
    (*it).to->add_inlink(&(*it));
  }
}

void Graph::inform_locations_of_edges2(){
  for (vector<Link>::iterator it = edges2.begin() ; it != edges2.end(); ++it){
    (*it).from->add_outlink2(&(*it));
    (*it).to->add_inlink2(&(*it));
  }
}

void Graph::print(){
  cout << endl << "Printing graph: " << endl;
  cout << "Links: " << endl;
  for(int i = 0; i < edges.size(); ++i){
    edges[i].print();
  }
  cout << "Locations: " << endl;
  for(int i = 0; i < locations.size(); ++i){
    locations[i].print();
    cout.flush();
  }

  cout << endl << endl;;
}

void Graph::copy_graph(Graph G){
  for(vector<Location>::iterator it = G.locations.begin() ; it != G.locations.end(); ++it){
    add_node(it->name, it->S);
  }
  for(vector<Link>::iterator it = G.edges.begin() ; it != G.edges.end(); ++it){
    add_edge_index(it->from_index, it->to_index, it->S, it->E, it->I, it->Ia, it->R);
  }
  inform_locations_of_edges();
}



int main(int nargs, char ** argsv){
  //float beta;  // infection parameter, 0.6
  float betaScale; // 1/infectious period, 1/3
  float betaShape; // 1/infectious period, 1/3
  float gamma; // 1/infectious period, 1/3
  float a;  // 1/latent period, 1/1.9
  int dailySeeding;
  int N = 1;
  int M = 300;
  int n=0;


  const char *outputFile = argsv[1];
  const char *betaFile = argsv[2];
  gamma = 1/atof(argsv[3]);
  a = 1/atof(argsv[4]);
  dailySeeding = atoi(argsv[5]);
  Graph G;
  string tmpstr;
  string tmpstr2;
  char tmpchar[3];
  int ix; int iy; int pop;

  ifstream infile("pop_wo_com.txt");
  if(!infile.is_open()){
    cout << "Error in opening network node file" << endl;
    exit(1);
  }
  char name[256];
  char name2[256];
  while(infile  >> tmpstr >> pop){
    G.add_node(tmpstr, pop);
    n+= 1;
  }
  infile.close();

  float *betas = new float[n];
  ifstream beta_infile(betaFile);
  if(!beta_infile.is_open()){
    cout << "Error in opening start_betas file" << endl;
    exit(1);
  }
  int temp_i=0;
  float tmp_readin_float;
  while(beta_infile >> tmp_readin_float){
    //cout << "xxx\n" << endl;
    betas[temp_i] = tmp_readin_float;
    //cout << betas[temp_i] << "\n" << endl;
    //betas[temp_i] = 0.6;
    temp_i+= 1;
    //cout << temp_i << endl;
  }
  beta_infile.close();

  cout << "Opening start_infected" << endl;
  int *startPoints = new int[n];
  int tmp_readin;
  ifstream start_infile("start_infected.txt");
  if(!start_infile.is_open()){
    cout << "Error in opening infected file" << endl;
    exit(1);
  }
  temp_i=0;
  while(start_infile >> tmp_readin){
    startPoints[temp_i] = tmp_readin;
    temp_i+= 1;
  }
  start_infile.close();

  int S, E, I, Ia, R;
  E = 0;
  I = 0;
  Ia = 0;
  R = 0;
  ifstream edge_infile("di_edge_list.txt");
  if(!edge_infile.is_open()){
    cout << "Error in opening network edge file" << endl;
    exit(1);
  }
  int safecount = 0;
  int n_edges = 0;
  cout << "Starting to add edges, printing every 1000 edge" << endl;
  while(edge_infile >> tmpstr >> tmpstr2 >> S){
    if (S != 0){
      safecount += 1;
      G.add_edge(tmpstr, tmpstr2, S, E, I, Ia, R);

      if(safecount % 1000 == 0){
        cout << safecount << " ";
        cout.flush();
      }
      if(edge_infile.eof()){
        break;
      }
      n_edges += 1;
    }
  }
  cout << "Found " << n_edges << " edges. " << endl;
  cout << endl << endl;
  edge_infile.close();
  G.inform_locations_of_edges();
  // Storage for statistics
  int ***values = new int**[n];
  int **peak_date = new int*[n];
  int **peak_val = new int*[n];
  int **start_date = new int*[n];
  int **I_this = new int*[n];
  int **final_size = new int*[n];
  int *duration = new int[N];
  int ns;
  int ne;
  int ni;
  int nia;
  int nr;
  //~ unsigned int mult_op[5];
  //~ unsigned int Seed2 = 123;
  //~ gsl_rng_env_setup();
  //~ const gsl_rng_type * T2;
  //~ gsl_rng * r2;
  //~ unsigned int Seed2 = time(NULL); // rand();
  //~ T2 = gsl_rng_default;
  //~ r2 = gsl_rng_alloc (T2);
  //~ gsl_rng_set (r2, Seed2);
  /// Values has first index for position, second for time and third for value.
  /// Third index 0=S, 1=E, 2=I, 3=Ia, 4=R;
  for(int i = 0; i < n; ++i){
    values[i] = new int*[M*2];
    peak_date[i] = new int[N];
    peak_val[i] = new int[N];
    start_date[i] = new int[N];
    I_this[i] = new int[M*2];
    final_size[i] = new int[N];
    for(int k = 0; k < M*2; ++k){
      values[i][k] = new int[6];
      values[i][k][0] = 0;
      values[i][k][1] = 0;
      values[i][k][2] = 0;
      values[i][k][3] = 0;
      values[i][k][4] = 0;
      values[i][k][5] = 0; // No 5 is incidence
    }
  }

  unsigned int Seed2 = 123;
  //~ unsigned int Seed2 = time(NULL); // rand();
  int Nk;
  float sum;
  double *pop_sum = new double[n];
  //~ double *p = new double[n];
  int num;
  for (int i = 0; i < n; ++i){
    pop_sum[i] = G.locations[i].S/4214533.0;
    num = G.locations[i].out_links.size();
    for (int j = 0; j<num; ++j){
      pop_sum[i] += G.locations[i].out_links[j]->S/4214533.0;
    }

  }
  //~ for (int i = 0; i < n; ++i){
  //~ p[i] = pop_sum[i];
  //~ }
  int size0 = 4214533;
  int *prob_array = new int[size0];
  int current_index = 0;
  for(int i = 0; i < n; ++i){
    for(int j = 0; j < int(pop_sum[i]*4214533); ++j){

      if(current_index < 4214533){
        prob_array[current_index] = i;
        current_index++;
      }
      else{
        //~ cerr << current_index << endl;
        //~ cout << current_index << endl;
      }
    }
    double tmp = 0;
  }
  delete[] pop_sum;

  cout << "Starting simulation loop" << endl;
  for(int i_sim = 0; i_sim < N; ++i_sim){
    for (int i = 0; i < n; ++i){
      final_size[i][i_sim] = 0;
      for (int k = 0; k < M*2; ++k){
        I_this[i][k] = 0;
      }
    }
    Graph G_current;
    G_current.copy_graph(G);
    cout << "Starting simulation" << i_sim <<  endl;

    // start seeding
    for(int i = 0; i < n; ++i){
      if(G_current.locations[i].S > startPoints[i]){  // Number 40 is Oslo
        G_current.locations[i].I += startPoints[i];
        G_current.locations[i].S -= startPoints[i];
      }
    }

    int dummy = 0;
    int dur_sum = 0;
    for(int i_day = 0; i_day < M; ++i_day){
      /*
      if(G_current.locations[40].S > ceil(dailySeeding*i_day/M/2)){  // Number 40 is Oslo
        G_current.locations[40].I += ceil(dailySeeding*i_day/M/2);
        G_current.locations[40].S -= ceil(dailySeeding*i_day/M/2);
      }
      */
      //if(i_day<M/2){
      for (int i = 0; i < n; ++i){
        if(G_current.locations[i].S > dailySeeding){  // Number 2 is Oslo
          G_current.locations[i].I += dailySeeding;
          G_current.locations[i].S -= dailySeeding;
        }
      }
      //}

      for (int i = 0; i < n; ++i){
        int de2 = 0;

        G_current.locations[i].seir_step_day(i_day, i, betas[i], a, gamma, de2);
        values[i][2*i_day][0] += G_current.locations[i].S;
        values[i][2*i_day][1] += G_current.locations[i].E;
        values[i][2*i_day][2] += G_current.locations[i].I;
        values[i][2*i_day][3] += G_current.locations[i].Ia;
        values[i][2*i_day][4] += G_current.locations[i].R;
        I_this[i][2*i_day] += G_current.locations[i].I;
        int num = G_current.locations[i].out_links.size();
        values[i][2*i_day][5] = de2;
        for(int j = 0; j < num; ++j){
          values[i][2*i_day][0] += G_current.locations[i].out_links[j]->S;
          values[i][2*i_day][1] += G_current.locations[i].out_links[j]->E;
          values[i][2*i_day][2] += G_current.locations[i].out_links[j]->I;
          values[i][2*i_day][3] += G_current.locations[i].out_links[j]->Ia;
          values[i][2*i_day][4] += G_current.locations[i].out_links[j]->R;
          I_this[i][2*i_day] += G_current.locations[i].out_links[j] -> I;
          dur_sum += G_current.locations[i].out_links[j]->E;
          dur_sum += G_current.locations[i].out_links[j]->I;
          dur_sum += G_current.locations[i].out_links[j]->Ia;
        }
      }
      if (dur_sum < 11 & dummy == 0 & i_day > 10){
        duration[i_sim]  = i_day;
        dummy = 1;
      }
      for (int i = 0; i < n; ++i){
        int de2 = 0;

        G_current.locations[i].seir_step_night(i_day, i, betas[i], a, gamma, de2);
        if(i_day == (M-1)){
          final_size[i][i_sim] += G_current.locations[i].R;
        }
        values[i][2*i_day+1][0] += G_current.locations[i].S;
        values[i][2*i_day+1][1] += G_current.locations[i].E;
        values[i][2*i_day+1][2] += G_current.locations[i].I;
        values[i][2*i_day+1][3] += G_current.locations[i].Ia;
        values[i][2*i_day+1][4] += G_current.locations[i].R;
        I_this[i][2*i_day+1] += G_current.locations[i].I;
        values[i][2*i_day+1][5] = de2;
        int num = G_current.locations[i].out_links.size();
        for(int j = 0; j < num; ++j){
          if (i_day == (M-1)){
            final_size[i][i_sim] += G_current.locations[i].out_links[j]->R;
          }
          values[i][2*i_day+1][0] += G_current.locations[i].out_links[j]->S;
          values[i][2*i_day+1][1] += G_current.locations[i].out_links[j]->E;
          values[i][2*i_day+1][2] += G_current.locations[i].out_links[j]->I;
          values[i][2*i_day+1][3] += G_current.locations[i].out_links[j]->Ia;
          values[i][2*i_day+1][4] += G_current.locations[i].out_links[j]->R;
          I_this[i][2*i_day+1] += G_current.locations[i].out_links[j]->I;
        }
      }
    }
    // Here you can find the initial dates and peak dates.
    float baseline;
    int pop = 0;
    int pday = 0;
    int pmax = 0;
    int count;
    int startday = 0;
    for(int i = 0; i < n; i++){
      pday = 0;
      pmax = 0;
      startday = 0;
      count = 0;
      pop = G_current.locations[i].S +  G_current.locations[i].E +  G_current.locations[i].I +  G_current.locations[i].Ia +  G_current.locations[i].R;
      int num = G.locations[i].out_links.size();
      for (int j = 0; j<num; ++j){
        pop += G.locations[i].out_links[j]->S + G.locations[i].out_links[j]->E + G.locations[i].out_links[j]->I + G.locations[i].out_links[j]->Ia + G.locations[i].out_links[j]->R;
      }
      if(pop < 100){
        baseline = 1;
      }
      else{
        baseline = 0.086*pop;
      }
      for(int i_day = 0; i_day < M; ++i_day){
        if(I_this[i][2*i_day] > pmax){
          pday = i_day;
          pmax = I_this[i][2*i_day];
        }
        if(I_this[i][2*i_day] > baseline){
          count += 1;
          if (count > 6){
            startday = i_day;
          }
        }
      }
      peak_date[i][i_sim] = pday;
      peak_val[i][i_sim] = pmax;
      start_date[i][i_sim] = startday;
    }


    cout << endl << "Done simulating" << endl << endl;
  }
  cout << "Done with all simulations" << endl;


  ofstream out_data(outputFile);


  for(int i=0; i < n; ++i){
    for (int k = 0; k<2*M; ++k){
      out_data << i << " " << values[i][k][0]*1.0/N << " " << values[i][k][1]*1.0/N << " " << values[i][k][2]*1.0/N << " " << values[i][k][3]*1.0/N << " " << values[i][k][4]*1.0/N << " " << values[i][k][5]*1.0/N << endl;

    }
  }

  out_data.close();

  /*
  ofstream out_data2("cpp_res_dates.txt");
  for(int i_sim=0; i_sim < N; ++i_sim){
    for (int i = 0; i< n; ++i){
      out_data2 << start_date[i][i_sim] << " " << peak_date[i][i_sim] << " " << peak_val[i][i_sim] << " " <<final_size[i][i_sim] << endl;

    }
  }

  out_data2.close();
   */

  //~ ofstream out_data3("cpp_res_dur.txt");
  //~ for(int i_sim=0; i_sim < N; ++i_sim){
  //~ out_data3 << duration[i_sim] << endl;
  //~ }

  //~ out_data3.close();

  for(int i = 0; i < n; ++i){
    for(int k = 0; k < 2*M; ++k){
      delete[] values[i][k];
    }
    delete[] values[i];
  }
  delete[] values;
  for(int i = 0; i < n; ++i){
    delete[] I_this[i];
    delete[] peak_date[i];
    delete[] peak_val[i];
    delete[] start_date[i];
    delete[] final_size[i];
  }
  delete[] I_this;
  delete[] peak_date;
  delete[] start_date;
  delete[] peak_val;
  delete[] final_size;
  delete[] duration;

}

