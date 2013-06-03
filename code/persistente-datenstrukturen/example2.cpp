#include <iostream>
#include <algorithm>
#include <vector>

using namespace std;

typedef vector<int> AdjList;
typedef vector<AdjList*> Graph;

bool contains(vector<int>& v, int x) {
    return v.end() != find(v.begin(), v.end(), x);
}

AdjList* get_friends(Graph& g, int a) {
    return g[a];
}

// legt eine neue Version des Graphen an
Graph* add_friendship(Graph& old, int a, int b) {
    if (contains(*get_friends(old, a), b))
        return &old;
    // alten Graph kopieren (wir benutzen hier den Kopierkonstruktor!)
    Graph* g = new Graph(old);
    // Adjazenzlisten von a und b kopieren
    (*g)[a] = new AdjList(*old[a]);
    (*g)[a]->push_back(b);
    return g;
}

void print_friends(Graph& g, int a) {
    AdjList friends = *get_friends(g, a);
    for (size_t i = 0; i < friends.size(); ++i)
        cout << friends[i] << " ";
    cout << endl;
}

int main() {
    Graph g;
    // bildet Versionsnummern auf Zustände des Graphen ab
    vector<Graph*> versions;

    // wir legen 10^6 Benutzer an
    for (int i = 0; i < 1000000; ++i)
        g.push_back(new AdjList);
    // wir legen ein paar verschiedene Versionen an
    versions.push_back(&g);                                 // Version 0
    versions.push_back(add_friendship(g, 0, 1));            // Version 1
    versions.push_back(add_friendship(*versions[1], 0, 2)); // Version 2
    versions.push_back(add_friendship(*versions[2], 2, 3)); // Version 3

    // Freunde von 0 zum Zeitpunkt 1
    print_friends(*versions[1], 0);
    // Freunde von 0 zum Zeitpunkt 2
    print_friends(*versions[2], 0);
}
