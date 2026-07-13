#include "../include/io.h"
#include "../include/stdarg.h"
#include "../include/stdio.h"
#include "../include/memory.h"
#include "../include/string.h"

typedef enum {
    NUMBER,
    PLUS,
    TIMES,
    LPAREN,
    RPAREN,
    END
} TokenType;

typedef struct {
  TokenType tt;
  char const* tname;
} TokenDesc;

typedef struct {
    char *input;
    int position;
} Lexer;

TokenType getNextToken(Lexer *lexer, char *token) {
    if (lexer->position >= strlen(lexer->input)) {
        return END;
    }
    if (lexer->input[lexer->position] >= '0' && lexer->input[lexer->position] <= '9') {
        token[0] = lexer->input[lexer->position++];
        while (lexer->position < strlen(lexer->input) && lexer->input[lexer->position] >= '0' && lexer->input[lexer->position] <= '9') {
            token[lexer->position - 1] = lexer->input[lexer->position++];
        }
        token[lexer->position] = '\0';
        return NUMBER;
    } else if (lexer->input[lexer->position] == '+') {
        lexer->position++;
        return PLUS;
    } else if (lexer->input[lexer->position] == '*') {
        lexer->position++;
        return TIMES;
    } else if (lexer->input[lexer->position] == '(') {
        lexer->position++;
        return LPAREN;
    } else if (lexer->input[lexer->position] == ')') {
        lexer->position++;
        return RPAREN;
    }
    return END;
}

typedef struct AST {
    TokenType type;
    struct AST *left;
    struct AST *right;
} AST;

AST *parseExpression(Lexer *lexer) {
    AST *node = malloc(sizeof(AST));
    TokenType token = getNextToken(lexer, NULL);
    if (token == NUMBER) {
        node->type = NUMBER;
        node->left = NULL;
        node->right = NULL;
        return node;
    } else if (token == LPAREN) {
        node->type = PLUS; //Assume all expressions are PLUS.
        node->left = parseExpression(lexer);
        token = getNextToken(lexer, NULL);
        if (token != RPAREN) {
            //error handling.
            free(node);
            return NULL;
        }
        node->right = parseExpression(lexer);
        return node;
    }
    // ... Add other operation's parsing code.
    free(node);
    return NULL;
}

void printInd(int ind)
{
    for (int i = 0; i < ind; i++) {
        printf(" ");
    }
}

void printAst(AST * ast, int ind, TokenDesc * td)
{
    if (ast == 0) { return; }
    printInd(ind);
    printf("%s", td[ast->type]);
    ind++;
    printAst(ast->left, ind + 2, td);
    printAst(ast->right, ind + 2, td);
}

int main() {
    TokenDesc td [] = {
       { NUMBER, "", },
       { PLUS, "", },
       { TIMES, "", },
       { LPAREN, "", },
       { RPAREN, "", },
       { END, "", },
    };
    char input[] = "(1 + 2) * 3";
    Lexer lexer = {input, 0};
    AST *ast = parseExpression(&lexer);
    if (ast != NULL) {
        printf("AST: ");
        printAst(ast, 0, td);
        free(ast);
    } else {
        printf("Parsing error.\n");
    }
    return 0;
}
