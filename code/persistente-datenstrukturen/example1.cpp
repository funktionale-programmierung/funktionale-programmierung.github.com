#include <iostream>
#include <algorithm>
#include <vector>

using namespace std;

// Typalias: Eine Liste befreundeter Benutzer
typedef vector<int> AdjList;

// bildet Benutzer auf ihre Freundeslisten ab
// z.B: friends_by_user[100] = Liste der Freunde von Benutzer Nummer 100
vector<AdjList*> friends_by_user;

// ein Helfer, der bestimmt ob ein Element in einem
// Array enthalten ist
bool contains(vector<int>& v, int x) {
    return v.end() != find(v.begin(), v.end(), x);
}

void add_friendship(int a, int b) {
    // ist a schon mit b befreundet?
    if (!contains(*friends_by_user[a], b))
        friends_by_user[a]->push_back(b);
}

vector<int>* get_friends(int a) {
    return friends_by_user[a];
}

int main() {
    // wir legen 10^6 Benutzer an
    for (int i = 0; i < 1000000; ++i)
        friends_by_user.push_back(new AdjList);
    // wir fügen einige Freundschaftsbeziehungen hinzu
    add_friendship(0,1);
    add_friendship(0,2);
    add_friendship(1,2);
    // gib die Freunde von 0 aus
    vector<int> friends_of_0 = *get_friends(0);
    for (int i = 0; i < friends_of_0.size(); ++i)
        cout << friends_of_0[i] << endl;
}
