#include "Tree.h"
#include "iostream"

Tree emptyT(){
    Tree t = new NodeT;
    return t;
}

Tree nodeT(int elem, Tree left, Tree right){
    Tree n = new NodeT;
    n->elem = elem;
    n->left = left;
    n->right = right;
    return n;
}

bool isEmptyT(Tree t){
    return t->left != NULL && t->right != NULL;
}

int rootT(Tree t){
    return t->elem;
}

Tree left(Tree t){
    return t->left;

}

Tree right(Tree t){
    return t->right;
}
