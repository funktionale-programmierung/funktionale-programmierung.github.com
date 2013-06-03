#include <iostream>
#include <algorithm>
#include <vector>

using namespace std;

const int BLOCK_SIZE = 1000;

// Eine Liste befreundeter Benutzer
typedef vector<int> AdjList;
typedef vector<AdjList*> GraphLevel2;
typedef vector<GraphLevel2*> Graph;

bool contains(vector<int>& v, int x) {
  return v.end() != find(v.begin(), v.end(), x);
}

AdjList* get_friends(Graph& g, int a) {
  // Indirektion auflösen
  return (*g[a / BLOCK_SIZE])[a % BLOCK_SIZE];
}

Graph* add_friendship(Graph& old, int a, int b) {
  if (contains(*get_friends(old, a), b))
    return &old;
  // kopiere Level 1
  Graph* g = new Graph(old);
  // kopiere Level 2
  GraphLevel2* new_lvl2 = new GraphLevel2(*old[a / BLOCK_SIZE]);
  // kopiere Adjazenzliste
  AdjList* new_adjlst = new AdjList(*(*new_lvl2)[a % BLOCK_SIZE]);
  new_adjlst->push_back(b);
  (*new_lvl2)[a % BLOCK_SIZE] = new_adjlst;
  (*g)[a / BLOCK_SIZE] = new_lvl2;
  return g;
}

int main() {
  Graph g;
  // bildet Versionsnummern auf Zustände des Graphen ab
  vector<Graph*> versions;

  // wir legen 10^6 Benutzer an
  for (int i = BLOCK_SIZE; i > 0; --i) {
    GraphLevel2* block = new GraphLevel2;
    for (int j = BLOCK_SIZE; j > 0; --j)
      block->push_back(new AdjList);
    g.push_back(block);
  }

  // wir legen ein paar verschiedene Versionen an
  versions.push_back(&g);
  versions.push_back(add_friendship(g, 0, 1));
  versions.push_back(add_friendship(*versions[1], 0, 2));
  versions.push_back(add_friendship(*versions[2], 2, 3));

  // Freunde von 0 zum Zeitpunkt 1
  AdjList* friends = get_friends(*versions[1], 0);
  for (size_t i = 0; i < friends->size(); ++i)
    cout << (*friends)[i] << " ";
  cout << endl;

  // Freunde von 0 zum Zeitpunkt 2
  friends = get_friends(*versions[2], 0);
  for (size_t i = 0; i < friends->size(); ++i)
    cout << (*friends)[i] << " ";
  cout << endl;
}
