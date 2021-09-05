#include <bits/stdc++.h>
using namespace std;
#define rep(i, n) for (int i = 0; i < (int)(n); i++)
#define ALL(v) (v).begin(), (v).end()
using ll = long long;

constexpr double TIMELIMIT = 2.98;

constexpr int N = 20;
int M;

string s[800];
bool used[800];
int cnt_empty;
vector<int> unused;

struct XorShift {
    unsigned int x, y, z, w, t;

    XorShift(int seed) {
        mt19937 rnd(seed);
        x = rnd();
        y = rnd();
        z = rnd();
        w = rnd();
        t = 1;
    }

    int rand() {
        t = x ^ (x << 11);
        x = y;
        y = z;
        z = w;
        w = (w ^ (w >> 19)) ^ (t ^ (t >> 8));
        return w & 0x7fffffff;
    }
} rnd(rand());

struct Timer {
    chrono::system_clock::time_point start, now;

    Timer() {
        start = chrono::system_clock::now();
    }

    double getTime() {
        now = chrono::system_clock::now();
        return chrono::duration<double>(now - start).count();
    }
};

Timer tmr;
double startclock = -1, nowclock = -1;

struct State {
    string a[N];
    vector<int> empty[N];
    int score;
};

void calc(State& state) {
    state.score = 0;
    for (int idx : unused) {
        bool exist = false;
        rep(x, N) {
            rep(y, N) {
                bool match = true;
                rep(i, s[idx].size()) {
                    if (state.a[(x + i) % N][y] != s[idx][i]) {
                        match = false;
                        break;
                    }
                }
                if (match) {
                    exist = true;
                    break;
                }
            }
            if (exist) break;
        }
        if (exist) state.score++;
    }
}

void init(State& state) {
    fill(state.a, state.a + N, string(20, '.'));
    rep(x, N) {
        state.empty[x].resize(N, 0);
    }
    bool used[M] = {};
    rep(x, N) {
        vector<pair<int, int>> v;
        vector<int> covered[M];
        rep(i, M) {
            int cnt = 0;
            if (used[i]) continue;
            rep(j, M) {
                if (used[j]) continue;
                for (int k = 0; k <= (int)s[i].size() - (int)s[j].size(); k++) {
                    if (s[i].substr(k, s[j].size()) == s[j]) {
                        cnt++;
                        covered[i].emplace_back(j);
                        break;
                    }
                }
            }
            v.emplace_back(cnt, i);
        }
        sort(ALL(v));
        reverse(ALL(v));
        int idx = v.front().second;
        for (int i : covered[idx]) {
            used[i] = true;
        }
        string t = s[idx];
        while (t.size() < N) {
            bool flag = false;
            for (int sz = t.size() - 1; sz > 0; sz--) {
                rep(i, M) {
                    if (used[i]) continue;
                    if ((int)s[i].size() <= sz) continue;
                    if (s[i].substr(s[i].size() - sz, sz) ==
                        t.substr(0, sz)) {  // add front
                        int r = t.size() + s[i].size() - sz - N;
                        if (r > 0 &&
                            s[i].substr(0, r) != t.substr(t.size() - r, r)) {
                            break;
                        }
                        t = s[i].substr(0, s[i].size() - sz) + t;
                        for (int i : covered[i]) {
                            used[i] = true;
                        }
                        flag = true;
                        break;
                    }
                    if (t.substr(t.size() - sz, sz) ==
                        s[i].substr(0, sz)) {  // add back
                        int r = t.size() + s[i].size() - sz - N;
                        if (r > 0 &&
                            s[i].substr(s[i].size() - r, r) != t.substr(0, r)) {
                            break;
                        }
                        t = t + s[i].substr(sz, s[i].size() - sz);
                        for (int i : covered[i]) {
                            used[i] = true;
                        }
                        flag = true;
                        break;
                    }
                }
                if (flag) break;
            }
            if (!flag) break;
        }
        rep(y, t.size()) {
            state.a[x][y] = t[y];
        }
    }
    rep(idx, M) {
        if (used[idx]) continue;
        bool exist = false;
        rep(x, N) {
            rep(y, N) {
                bool match = true;
                rep(i, s[idx].size()) {
                    if (state.a[x][(y + i) % N] != s[idx][i]) {
                        match = false;
                        break;
                    }
                }
                if (match) {
                    exist = true;
                    break;
                }
            }
            if (exist) break;
        }
        if (!exist) {
            unused.emplace_back(idx);
        }
    }
    rep(x, N) {
        rep(y, N) {
            if (state.a[x][y] == '.') {
                state.a[x][y] = 'A';
                state.empty[x][y] = cnt_empty++;
            }
        }
    }
    calc(state);
}

void modify(State& state) {
    int r = rnd.rand() % (cnt_empty > 0 ? 3 : 2);
    if (r == 0) {  // shift
        int x = rnd.rand() % N, shift = rnd.rand() % (N - 1);
        string t = state.a[x];
        vector<int> empty = state.empty[x];
        rep(y, N) {
            state.a[x][(y + shift) % N] = t[y];
            state.empty[x][(y + shift) % N] = empty[y];
        }
    } else if (r == 1) {  // swap
        int i = rnd.rand() % N, j = i;
        while (i == j) {
            j = rnd.rand() % N;
        }
        swap(state.a[i], state.a[j]);
        swap(state.empty[i], state.empty[j]);
    } else {  // change
        int change = rnd.rand() % cnt_empty;
        char c = 'A' + rnd.rand() % 8;
        bool find = false;
        rep(x, N) {
            rep(y, N) {
                if (state.empty[x][y] == change) {
                    state.a[x][y] = c;
                    find = true;
                    break;
                }
            }
            if (find) break;
        }
    }
    calc(state);
}

void solve(State& state) {
    int steps = 0;
    // double starttemp, endtemp;
    while (true) {
        nowclock = tmr.getTime();
        if (nowclock - startclock > TIMELIMIT) break;
        State newstate = state;
        modify(newstate);
        if (newstate.score > state.score) {
            state = newstate;
        }
        /*
        double temp = starttemp + (endtemp - starttemp) *
                                      (nowclock - startclock) / TIMELIMIT;
        double prob = exp((newstate.score - state.score) / temp);
        if (prob > (rnd.rand() % (int)1e9) / 1e9) {
            state = newstate;
        }
        */
        steps++;
    }
    cerr << "score : " << state.score << endl;
    cerr << "steps : " << steps << endl;
}

void output(State& state) {
    rep(i, N) {
        rep(j, N) {
            cout << state.a[i][j];
        }
        cout << endl;
    }
}

void input() {
    int _;
    cin >> _ >> M;
    rep(i, M) {
        cin >> s[i];
    }
    auto f = [](string a, string b) { return a.size() > b.size(); };
    sort(s, s + M, f);
}

signed main() {
    startclock = tmr.getTime();
    input();
    State state;
    init(state);
    solve(state);
    output(state);
    return 0;
}