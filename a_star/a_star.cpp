#include <algorithm>
#include <cstring>
#include <iostream>
#include <unordered_set>
#include <map>
#include <string>
#include <sstream>
#include <vector>


/* *************************************** *
 * ************  HEURISTICAS  ************ *
 * *************************************** */

short final[4][4] = {1, 5, 9, 13,
                     2, 6, 10, 14,
                     3 ,7, 11, 15,
                     4, 8, 12, 0};

typedef int (*heuristic)(const short board[][4]);

int h1(const short board[][4])
{
    int out_of_place = 0;
    for (size_t i = 0; i < 4; i++) {
        for (size_t j = 0; j < 4; j++) {
            if (board[i][j] && board[i][j] != final[i][j]) out_of_place ++;
        }
    }
    return out_of_place;
}

int h2(const short board[][4])
{
    short array[16];
    size_t k = 0;
    for (size_t i = 0; i < 4; i++) {
        for (size_t j = 0; j < 4; j++) {
            array[k] = board[j][i];
            k ++;
        }
    }
    int count = 0;
    for (size_t i = 1; i < 16; i++) {
        if ((array[i - 1] && array[i] != array[i - 1] + 1) || (array[i - 1] == 15 && array[i])) count ++;
    }
    return count;
}

std::map<int, std::pair<int, int>> final_pos = {{1, {0, 0}}, {5, {0, 1}}, {9, {0, 2}}, {13, {0, 3}},
                                                {2, {1, 0}}, {6, {1, 1}}, {10, {1, 2}}, {14, {1, 3}},
                                                {3, {2, 0}}, {7, {2, 1}}, {11, {2, 2}}, {15, {2, 3}},
                                                {4, {3, 0}}, {8, {3, 1}}, {12, {3, 2}}, {0, {3, 3}}};

int distance(size_t n, size_t i, size_t j)
{
    int d1 = 0, d2 = 0;
    size_t i_f = final_pos[n].first;
    size_t j_f = final_pos[n].second;
    if (i_f != i) d1 = std::abs(int(i_f - i));
    if (j_f != j) d2 = std::abs(int(j_f - j));
    return d1 + d2;
}

int h3(const short board[][4])
{
    size_t d = 0;
    for (size_t i = 0; i < 4; i++) {
        for (size_t j = 0; j < 4; j++) {
            if (board[i][j] != final[i][j] && board[i][j]) d += distance(board[i][j], i, j);
        }
    }
    return d;
}

int h4(const short board[][4])
{
    return h3(board);
}

int h5(const short board[][4])
{
    return std::max(h1(board), std::max(h2(board), h3(board)));
}


/* *************************************** *
 * ************   A ESTRELA   ************ *
 * *************************************** */

std::string generate_key(const short b[][4])
{
    std::string s, hex = "0123456789ABCDEF";
    for (size_t i = 0; i < 4; i++) {
        for (size_t j = 0; j < 4; j++)
            s += hex[b[i][j]];
    }
    return s;
}

struct vertex
{
    short board[4][4];
    short g = 0;
    short h = 0;

    bool operator==(const vertex &v) const
    {
        return generate_key(this->board) == generate_key(v.board);
    }

    bool operator<(const vertex &a) const
    {
        return this->g + this->h > a.g + a.h;
    }
};

class vertex_hash {
public:
    size_t operator()(const vertex &v) const
    {
        return std::hash<std::string>()(generate_key(v.board));
    }
};

vertex new_child(const vertex &father, size_t i_zero, size_t j_zero, size_t i, size_t j)
{
    vertex child;
    std::memcpy(child.board, father.board, 16*sizeof(short));
    child.board[i_zero][j_zero] = child.board[i][j];
    child.board[i][j] = 0;
    child.g = father.g + 1;
    return child;
}

void left_right(std::vector<vertex> &children, const vertex &father, size_t i_zero, size_t j_zero)
{
    if (!j_zero) {
        children.push_back(new_child(father, i_zero, j_zero, i_zero, j_zero + 1));
    } else if (j_zero == 3) {
        children.push_back(new_child(father, i_zero, j_zero, i_zero, j_zero - 1));
    } else {
        children.push_back(new_child(father, i_zero, j_zero, i_zero, j_zero + 1));
        children.push_back(new_child(father, i_zero, j_zero, i_zero, j_zero - 1));
    }
}

std::vector<vertex> generate_children(const vertex &father)
{
    size_t i_zero, j_zero;
    for (size_t i = 0; i < 4; i++) {
        for (size_t j = 0; j < 4; j++) {
            if (!father.board[i][j]) {
                i_zero = i;
                j_zero = j;
                break;
            }
        }
    }
    std::vector<vertex> children;
    if (!i_zero) {
        children.push_back(new_child(father, i_zero, j_zero, i_zero + 1, j_zero));
        left_right(children, father, i_zero, j_zero);
    } else if (i_zero == 3) {
        children.push_back(new_child(father, i_zero, j_zero, i_zero - 1, j_zero));
        left_right(children, father, i_zero, j_zero);
    } else {
        children.push_back(new_child(father, i_zero, j_zero, i_zero + 1, j_zero));
        children.push_back(new_child(father, i_zero, j_zero, i_zero - 1, j_zero));
        left_right(children, father, i_zero, j_zero);
    }
    return children;
}

void build_board(const std::string &e, short t[][4])
{
    std::vector<std::string> p;
    std::stringstream ss{e};
    while (ss) {
        std::string w;
        ss >> w;
        if (w.find(" ") != std::string::npos)
            w.pop_back();
        p.push_back(w);
    }
    size_t k = 0;
    for (size_t i = 0; i < 4; i++) {
        for (size_t j = 0; j < 4; j++) {
            std::string s = p[k];
            t[i][j] = std::stoi(s.c_str());
            k ++;
        }
    }
}

int a_star(heuristic h, std::string e)
{
    vertex S;
    build_board(e, S.board);
    S.h = h(S.board);

    std::vector<vertex> A;
    A.push_back(S);

    std::unordered_set<vertex, vertex_hash> A_elem;
    A_elem.insert(S);

    std::unordered_set<std::string> F;

    while (!A.empty()) {
        vertex v;
        while (true) {
            std::pop_heap(A.begin(), A.end());
            std::swap(v, A.back());
            A.pop_back();
            std::unordered_set<vertex, vertex_hash>::iterator it = A_elem.find(v);
            if (it != A_elem.end() && it->g + it->h == v.g + v.h) {
                A_elem.erase(it);
                break;
            }
        }
        std::string v_key = generate_key(v.board);
        F.insert(v_key);

        if (v_key == "159D26AE37BF48C0") return v.g;

        std::vector<vertex> children = generate_children(v);
        for (size_t i = 0; i < children.size(); i++) {
            children[i].h = h(children[i].board);
            if (F.find(generate_key(children[i].board)) != F.end()) {
                continue;
            } else if (A_elem.find(children[i]) != A_elem.end()) {
                std::unordered_set<vertex, vertex_hash>::iterator it = A_elem.find(children[i]);
                if (it->g > children[i].g) {
                    A.push_back(children[i]);
                    std::push_heap(A.begin(), A.end());
                    A_elem.erase(it);
                    A_elem.insert(children[i]);
                }
            } else {
                A.push_back(children[i]);
                std::push_heap(A.begin(), A.end());
                A_elem.insert(children[i]);
            }
        }
    }
    return -1;
}


/* *************************************** *
 * ************      MAIN     ************ *
 * *************************************** */

int main()
{
    std::string e;
    std::getline(std::cin, e);
    std::cout << a_star(h3, e) << std::endl;

    return 0;
}
