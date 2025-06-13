#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include<ctype.h>

#define MAX_SYMBOLS 100
#define MAX_LINE 256

typedef struct {
    char name[50];
    char type[20];
    char scope[50];
} Symbol;

Symbol symbolTable[MAX_SYMBOLS];
int symbolCount = 0;

FILE *input, *output;

bool isDeclared(const char *name, const char *scope) {
    for (int i = 0; i < symbolCount; i++) {
        if (strcmp(symbolTable[i].name, name) == 0 &&
            (strcmp(symbolTable[i].scope, scope) == 0 || strcmp(symbolTable[i].scope, "global") == 0)) {
            return true;
        }
    }
    return false;
}

const char* getType(const char *name, const char *scope) {
    for (int i = 0; i < symbolCount; i++) {
        if (strcmp(symbolTable[i].name, name) == 0 &&
            (strcmp(symbolTable[i].scope, scope) == 0 || strcmp(symbolTable[i].scope, "global") == 0)) {
            return symbolTable[i].type;
        }
    }
    return NULL;
}

void addSymbol(const char *type, const char *name, const char *scope) {
    strcpy(symbolTable[symbolCount].type, type);
    strcpy(symbolTable[symbolCount].name, name);
    strcpy(symbolTable[symbolCount].scope, scope);
    symbolCount++;
}

void printSymbolTable() {
    printf("Symbol Table:- \n");
    fprintf(output, "-------------------------\n");
    fprintf(output, " Name      Type     Scope\n");
    fprintf(output, "-------------------------\n");
    printf(       "-------------------------\n");
    printf(       "Name       Type     Scope\n");
    printf(       "-------------------------\n");

    for (int i = 0; i < symbolCount; i++) {
        fprintf(output, "%-10s %-8s %-10s\n", symbolTable[i].name, symbolTable[i].type, symbolTable[i].scope);
        printf(        "%-10s %-8s %-10s\n", symbolTable[i].name, symbolTable[i].type, symbolTable[i].scope);
    }
    fprintf(output, "-------------------------\n");
    printf(       "-------------------------\n");
}

void analyzeParseTree() {
    char line[MAX_LINE];
    char currentScope[50] = "global";
    char lastType[20] = "";

    while (fgets(line, sizeof(line), input)) {
        // Remove trailing newline
        line[strcspn(line, "\r\n")] = 0;

        char *trimmed = line;
        while (*trimmed == ' ' || *trimmed == '+' || *trimmed == '-' || *trimmed == '|') trimmed++;

        if (strstr(trimmed, "Function Name:") == trimmed) {
            sscanf(trimmed, "Function Name: %s", currentScope);
            addSymbol("function", currentScope, "global");
        } else if (strstr(trimmed, "Type:") == trimmed) {
            sscanf(trimmed, "Type: %s", lastType);
        } else if (strstr(trimmed, "Identifier:") == trimmed) {
            char identifier[50];
            sscanf(trimmed, "Identifier: %s", identifier);
            if (!isDeclared(identifier, currentScope)) {
                addSymbol(lastType, identifier, currentScope);
            }
        } else if (strstr(trimmed, "Expression:") == trimmed) {
            char expr[200];
            strcpy(expr, trimmed + strlen("Expression: "));

            // Check for assignment expressions like x = y + z
            char lhs[50], rhs[150];
            if (strstr(expr, "=")) {
                sscanf(expr, "%s = %[^\n]", lhs, rhs);
                if (!isDeclared(lhs, currentScope)) {
                    printf("Warning: Undeclared identifier '%s' in scope '%s'\n", lhs, currentScope);
                    fprintf(output, "Warning: Undeclared identifier '%s' in scope '%s'\n", lhs, currentScope);
                }
                // Check identifiers in RHS
                char *token = strtok(rhs, " +-*/<>()");
                while (token) {
                    if (!isdigit(token[0]) && !isDeclared(token, currentScope)) {
                        printf("Warning: Undeclared identifier '%s' used in expression in scope '%s'\n", token, currentScope);
                        fprintf(output, "Warning: Undeclared identifier '%s' used in expression in scope '%s'\n", token, currentScope);
                    }
                    token = strtok(NULL, " +-*/<>()");
                }

                // Type checking (simplified): types of lhs and first rhs identifier must match
                const char *lhsType = getType(lhs, currentScope);
                char firstId[50];
                sscanf(rhs, "%s", firstId);
                const char *rhsType = getType(firstId, currentScope);
                if (lhsType && rhsType && strcmp(lhsType, rhsType) != 0) {
                    printf("Type Error: Cannot assign %s to %s\n", rhsType, lhsType);
                    fprintf(output, "Type Error: Cannot assign %s to %s\n", rhsType, lhsType);
                }
            }
        }
    }
}

int main() {
    input = fopen("parse_tree.txt", "r");
    if (!input) {
        perror("Could not open parse_tree.txt");
        return 1;
    }

    output = fopen("symbol_table.txt", "w");
    if (!output) {
        perror("Could not open symbol_table.txt");
        fclose(input);
        return 1;
    }

    analyzeParseTree();
    printSymbolTable();

    fclose(input);
    fclose(output);
    return 0;
}
