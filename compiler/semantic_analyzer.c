#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_SYMBOLS 1000
#define MAX_PARAMS 20
#define MAX_TOKEN_LEN 100
#define MAX_SCOPE_DEPTH 50

typedef enum { TYPE_INT, TYPE_FLOAT, TYPE_CHAR, TYPE_VOID, TYPE_UNKNOWN } DataType;

typedef struct {
    char name[MAX_TOKEN_LEN];
    DataType type;
    bool isFunction;
    int paramCount;
    DataType paramTypes[MAX_PARAMS];
    int lineNumber;
    int scopeLevel;
} Symbol;

FILE *fp, *symbolOut, *annotOut;
bool semanticError = false;
int currentLine = 1;
int currentScopeLevel = 0;

Symbol symbolTable[MAX_SYMBOLS];
int symbolCount = 0;
int scopeStack[MAX_SCOPE_DEPTH];
int scopeStackTop = -1;

char type[50], token[100];
DataType currentFunctionReturnType = TYPE_UNKNOWN;

bool isDatatype(const char *token) {
    return strcmp(token, "int") == 0 || strcmp(token, "float") == 0 ||
           strcmp(token, "char") == 0 || strcmp(token, "void") == 0 ||
           strcmp(token, "double") == 0 || strcmp(token, "long") == 0 ||
           strcmp(token, "short") == 0;
}

bool isIdentifier(const char *type) {
    return strcmp(type, "IDENTIFIER") == 0;
}

DataType stringToDataType(const char *str) {
    if (strcmp(str, "int") == 0) return TYPE_INT;
    if (strcmp(str, "float") == 0) return TYPE_FLOAT;
    if (strcmp(str, "char") == 0) return TYPE_CHAR;
    if (strcmp(str, "void") == 0) return TYPE_VOID;
    return TYPE_UNKNOWN;
}

const char *dataTypeToString(DataType dt) {
    switch(dt) {
        case TYPE_INT: return "int";
        case TYPE_FLOAT: return "float";
        case TYPE_CHAR: return "char";
        case TYPE_VOID: return "void";
        default: return "unknown";
    }
}

void semanticReportError(const char *msg) {
    fprintf(stderr, "Semantic Error (Line %d): %s\n", currentLine, msg);
    fprintf(annotOut, "Semantic Error (Line %d): %s\n", currentLine, msg);
    semanticError = true;
}

void enterScope() {
    if (currentScopeLevel < MAX_SCOPE_DEPTH - 1) {
        scopeStack[++scopeStackTop] = currentScopeLevel;
        currentScopeLevel++;
    } else {
        semanticReportError("Maximum scope depth exceeded");
    }
}

void exitScope() {
    if (scopeStackTop >= 0) {
        for (int i = symbolCount - 1; i >= 0; i--) {
            if (symbolTable[i].scopeLevel > scopeStack[scopeStackTop]) {
                symbolCount--;
            } else {
                break;
            }
        }
        currentScopeLevel = scopeStack[scopeStackTop--];
    }
}

int findSymbol(const char *name) {
    for (int i = symbolCount - 1; i >= 0; i--) {
        if (strcmp(symbolTable[i].name, name) == 0 && symbolTable[i].scopeLevel <= currentScopeLevel) {
            return i;
        }
    }
    return -1;
}

int addSymbol(const char *name, DataType type, bool isFunction) {
    if (findSymbol(name) != -1) {
        char err[200];
        snprintf(err, sizeof(err), "Redeclaration of '%s' in the same scope", name);
        semanticReportError(err);
        return -1;
    }
    if (symbolCount >= MAX_SYMBOLS) {
        semanticReportError("Symbol table overflow");
        return -1;
    }
    strcpy(symbolTable[symbolCount].name, name);
    symbolTable[symbolCount].type = type;
    symbolTable[symbolCount].isFunction = isFunction;
    symbolTable[symbolCount].paramCount = 0;
    symbolTable[symbolCount].lineNumber = currentLine;
    symbolTable[symbolCount].scopeLevel = currentScopeLevel;
    return symbolCount++;
}

void addFunctionParams(int symIndex, DataType *params, int count) {
    if (symIndex < 0 || symIndex >= symbolCount) return;
    symbolTable[symIndex].paramCount = count;
    for (int i = 0; i < count; i++) {
        symbolTable[symIndex].paramTypes[i] = params[i];
    }
}

bool checkTypeCompatibility(DataType left, DataType right) {
    if (left == TYPE_UNKNOWN || right == TYPE_UNKNOWN) return false;
    if (left == right) return true;
    if ((left == TYPE_INT && right == TYPE_FLOAT) || (left == TYPE_FLOAT && right == TYPE_INT)) return true;
    if ((left == TYPE_INT && right == TYPE_CHAR) || (left == TYPE_CHAR && right == TYPE_INT)) return true;
    return false;
}

bool isDeclared(const char *name) {
    return findSymbol(name) != -1;
}

bool checkParamTypes(int symIndex, DataType *callParams, int callCount) {
    if (symIndex < 0 || symIndex >= symbolCount) return false;
    Symbol *sym = &symbolTable[symIndex];
    if (sym->paramCount != callCount) {
        char err[200];
        snprintf(err, sizeof(err), "Function '%s' expected %d parameters, got %d", 
                 sym->name, sym->paramCount, callCount);
        semanticReportError(err);
        return false;
    }
    for (int i = 0; i < callCount; i++) {
        if (!checkTypeCompatibility(sym->paramTypes[i], callParams[i])) {
            char err[200];
            snprintf(err, sizeof(err), "Incompatible parameter type for parameter %d in call to '%s'", 
                     i + 1, sym->name);
            semanticReportError(err);
            return false;
        }
    }
    return true;
}

bool nextToken() {
    while (fscanf(fp, "%s %s", type, token) != EOF) {
        if (strcmp(type, "PREPROCESSOR") == 0 || strcmp(type, "HEADER_FILE") == 0) {
            continue;
        }
        if (strcmp(token, "\n") == 0) {
            currentLine++;
            continue;
        }
        return true;
    }
    return false;
}

DataType parseExpression();

DataType parseFactor() {
    if (strcmp(token, "(") == 0) {
        if (!nextToken()) {
            semanticReportError("Unexpected end in expression");
            return TYPE_UNKNOWN;
        }
        DataType type = parseExpression();
        if (strcmp(token, ")") != 0) {
            semanticReportError("Expected closing parenthesis");
            return TYPE_UNKNOWN;
        }
        nextToken();
        return type;
    } else if (strcmp(type, "NUMBER") == 0) {
        DataType result = (strchr(token, '.') != NULL) ? TYPE_FLOAT : TYPE_INT;
        nextToken();
        return result;
    } else if (strcmp(type, "IDENTIFIER") == 0) {
        int symIdx = findSymbol(token);
        if (symIdx == -1) {
            char err[200];
            snprintf(err, sizeof(err), "Undeclared identifier '%s' used", token);
            semanticReportError(err);
            nextToken();
            return TYPE_UNKNOWN;
        }
        DataType type = symbolTable[symIdx].type;
        if (symbolTable[symIdx].isFunction) {
            if (!nextToken() || strcmp(token, "(") != 0) {
                semanticReportError("Expected '(' for function call");
                return TYPE_UNKNOWN;
            }
            DataType callParams[MAX_PARAMS];
            int callCount = 0;
            while (nextToken() && strcmp(token, ")") != 0) {
                if (callCount >= MAX_PARAMS) {
                    semanticReportError("Too many arguments in function call");
                    return TYPE_UNKNOWN;
                }
                callParams[callCount++] = parseExpression();
                if (strcmp(token, ",") == 0) continue;
                if (strcmp(token, ")") != 0) {
                    semanticReportError("Expected ',' or ')' in function call");
                    return TYPE_UNKNOWN;
                }
            }
            checkParamTypes(symIdx, callParams, callCount);
            nextToken();
            return symbolTable[symIdx].type;
        }
        nextToken();
        return type;
    } else if (strcmp(type, "CHAR_CONSTANT") == 0) {
        nextToken();
        return TYPE_CHAR;
    } else if (strcmp(type, "STRING_LITERAL") == 0) {
        nextToken();
        return TYPE_CHAR; // String literals treated as char pointers
    } else {
        semanticReportError("Invalid factor in expression");
        nextToken();
        return TYPE_UNKNOWN;
    }
}

DataType parseTerm() {
    DataType leftType = parseFactor();
    while (strcmp(token, "*") == 0 || strcmp(token, "/") == 0 || strcmp(token, "%") == 0) {
        char op[3];
        strcpy(op, token);
        if (!nextToken()) {
            semanticReportError("Expected operand after operator");
            return TYPE_UNKNOWN;
        }
        DataType rightType = parseFactor();
        if (!checkTypeCompatibility(leftType, rightType)) {
            char err[200];
            snprintf(err, sizeof(err), "Incompatible types for operator '%s': %s and %s", 
                     op, dataTypeToString(leftType), dataTypeToString(rightType));
            semanticReportError(err);
        }
        leftType = (leftType == TYPE_FLOAT || rightType == TYPE_FLOAT) ? TYPE_FLOAT : TYPE_INT;
    }
    return leftType;
}

DataType parseExpression() {
    DataType leftType = parseTerm();
    while (strcmp(token, "+") == 0 || strcmp(token, "-") == 0) {
        char op[3];
        strcpy(op, token);
        if (!nextToken()) {
            semanticReportError("Expected operand after operator");
            return TYPE_UNKNOWN;
        }
        DataType rightType = parseTerm();
        if (!checkTypeCompatibility(leftType, rightType)) {
            char err[200];
            snprintf(err, sizeof(err), "Incompatible types for operator '%s': %s and %s", 
                     op, dataTypeToString(leftType), dataTypeToString(rightType));
            semanticReportError(err);
        }
        leftType = (leftType == TYPE_FLOAT || rightType == TYPE_FLOAT) ? TYPE_FLOAT : TYPE_INT;
    }
    return leftType;
}

void parseDeclaration() {
    if (strcmp(type, "KEYWORD") != 0 || !isDatatype(token)) return;
    DataType varType = stringToDataType(token);
    fprintf(annotOut, "Variable Declaration:\n");
    fprintf(annotOut, "  Type: %s\n", token);

    if (!nextToken() || !isIdentifier(type)) {
        semanticReportError("Expected identifier after type");
        return;
    }

    char varName[MAX_TOKEN_LEN];
    strcpy(varName, token);
    fprintf(annotOut, "  Name: %s\n", varName);

    int symIdx = addSymbol(varName, varType, false);
    if (symIdx == -1) {
        while (nextToken() && strcmp(token, ";") != 0);
        return;
    }

    if (!nextToken()) {
        semanticReportError("Expected ';' or '=' after identifier");
        return;
    }

    if (strcmp(token, ";") == 0) {
        fprintf(annotOut, "  Initialization: None\n");
        return;
    } else if (strcmp(token, "=") == 0) {
        fprintf(annotOut, "  Initialization:\n");
        if (!nextToken()) {
            semanticReportError("Expected expression after '='");
            return;
        }
        DataType exprType = parseExpression();
        if (!checkTypeCompatibility(varType, exprType)) {
            char err[200];
            snprintf(err, sizeof(err), "Incompatible types in assignment to '%s': expected %s, got %s", 
                     varName, dataTypeToString(varType), dataTypeToString(exprType));
            semanticReportError(err);
        }
        fprintf(annotOut, "    Expression Type: %s\n", dataTypeToString(exprType));
        if (strcmp(token, ";") != 0) {
            semanticReportError("Expected ';' after expression");
        }
    } else if (strcmp(token, ",") == 0) {
        while (strcmp(token, ",") == 0) {
            if (!nextToken() || !isIdentifier(type)) {
                semanticReportError("Expected identifier after ','");
                return;
            }
            strcpy(varName, token);
            fprintf(annotOut, "  Name: %s\n", varName);
            symIdx = addSymbol(varName, varType, false);
            if (symIdx == -1) {
                while (nextToken() && strcmp(token, ";") != 0);
                return;
            }
            if (!nextToken()) {
                semanticReportError("Expected '=', ',' or ';' after identifier");
                return;
            }
            if (strcmp(token, "=") == 0) {
                fprintf(annotOut, "  Initialization:\n");
                if (!nextToken()) {
                    semanticReportError("Expected expression after '='");
                    return;
                }
                DataType exprType = parseExpression();
                if (!checkTypeCompatibility(varType, exprType)) {
                    char err[200];
                    snprintf(err, sizeof(err), "Incompatible types in assignment to '%s': expected %s, got %s", 
                             varName, dataTypeToString(varType), dataTypeToString(exprType));
                    semanticReportError(err);
                }
                fprintf(annotOut, "    Expression Type: %s\n", dataTypeToString(exprType));
            }
        }
        if (strcmp(token, ";") != 0) {
            semanticReportError("Expected ';' after declaration");
        }
    } else {
        semanticReportError("Expected '=', ',' or ';' after identifier");
    }
}

void parseParameters(DataType *params, int *paramCount) {
    *paramCount = 0;
    enterScope();
    while (true) {
        long pos = ftell(fp);
        if (!nextToken()) {
            semanticReportError("Unexpected end in parameters");
            return;
        }
        if (strcmp(token, ")") == 0) {
            fseek(fp, pos, SEEK_SET);
            return;
        }
        if (strcmp(type, "KEYWORD") == 0 && isDatatype(token)) {
            params[*paramCount] = stringToDataType(token);
            fprintf(annotOut, "    Parameter %d:\n", *paramCount + 1);
            fprintf(annotOut, "      Type: %s\n", token);
            if (!nextToken() || !isIdentifier(type)) {
                semanticReportError("Expected identifier after parameter type");
                return;
            }
            fprintf(annotOut, "      Name: %s\n", token);
            addSymbol(token, params[*paramCount], false);
            (*paramCount)++;
            long pos2 = ftell(fp);
            if (!nextToken()) return;
            if (strcmp(token, ",") == 0) {
                continue;
            } else if (strcmp(token, ")") == 0) {
                fseek(fp, pos2, SEEK_SET);
                return;
            } else {
                semanticReportError("Expected ',' or ')' after parameter");
                return;
            }
        } else {
            semanticReportError("Expected type in parameters");
            return;
        }
    }
}

void parseFunctionBody(DataType funcReturnType) {
    int depth = 1;
    enterScope();
    while (depth > 0 && nextToken()) {
        if (strcmp(token, "{") == 0) {
            depth++;
            continue;
        } else if (strcmp(token, "}") == 0) {
            depth--;
            if (depth == 0) {
                exitScope();
                break;
            }
            continue;
        }
        if (strcmp(type, "KEYWORD") == 0) {
            if (isDatatype(token)) {
                parseDeclaration();
            } else if (strcmp(token, "return") == 0) {
                fprintf(annotOut, "    Return Statement:\n");
                if (!nextToken()) {
                    semanticReportError("Unexpected end after 'return'");
                    return;
                }
                DataType exprType = TYPE_VOID;
                if (strcmp(token, ";") != 0) {
                    exprType = parseExpression();
                }
                if (funcReturnType == TYPE_VOID && exprType != TYPE_VOID) {
                    semanticReportError("Void function cannot return a value");
                } else if (funcReturnType != TYPE_VOID && exprType == TYPE_VOID) {
                    semanticReportError("Non-void function must return a value");
                } else if (funcReturnType != TYPE_VOID && !checkTypeCompatibility(funcReturnType, exprType)) {
                    char err[200];
                    snprintf(err, sizeof(err), "Incompatible return type: expected %s, got %s",
                             dataTypeToString(funcReturnType), dataTypeToString(exprType));
                    semanticReportError(err);
                }
                fprintf(annotOut, "      Expression Type: %s\n", dataTypeToString(exprType));
                if (strcmp(token, ";") != 0) {
                    semanticReportError("Expected ';' after return expression");
                }
            } else if (strcmp(token, "if") == 0 || strcmp(token, "for") == 0 || strcmp(token, "while") == 0) {
                while (nextToken() && strcmp(token, ";") != 0 && strcmp(token, "{") != 0);
                if (strcmp(token, "{") == 0) {
                    enterScope();
                    depth++;
                }
            }
        } else if (isIdentifier(type)) {
            int symIdx = findSymbol(token);
            if (symIdx == -1) {
                char err[200];
                snprintf(err, sizeof(err), "Undeclared identifier '%s' used", token);
                semanticReportError(err);
                while (nextToken() && strcmp(token, ";") != 0);
            } else if (symbolTable[symIdx].isFunction) {
                if (!nextToken() || strcmp(token, "(") != 0) {
                    semanticReportError("Expected '(' for function call");
                    continue;
                }
                DataType callParams[MAX_PARAMS];
                int callCount = 0;
                while (nextToken() && strcmp(token, ")") != 0) {
                    if (callCount >= MAX_PARAMS) {
                        semanticReportError("Too many arguments in function call");
                        break;
                    }
                    callParams[callCount++] = parseExpression();
                    if (strcmp(token, ",") == 0) continue;
                    if (strcmp(token, ")") != 0) {
                        semanticReportError("Expected ',' or ')' in function call");
                        break;
                    }
                }
                checkParamTypes(symIdx, callParams, callCount);
                if (nextToken() && strcmp(token, ";") != 0) {
                    semanticReportError("Expected ';' after function call");
                }
                fprintf(annotOut, "    Function Call: %s (Return Type: %s)\n", 
                        symbolTable[symIdx].name, dataTypeToString(symbolTable[symIdx].type));
            } else {
                fprintf(annotOut, "    Identifier Usage: %s (Type: %s)\n", 
                        token, dataTypeToString(symbolTable[symIdx].type));
                DataType leftType = symbolTable[symIdx].type;
                if (!nextToken()) continue;
                if (strcmp(token, "=") == 0) {
                    if (!nextToken()) {
                        semanticReportError("Expected expression after '='");
                        continue;
                    }
                    DataType exprType = parseExpression();
                    if (!checkTypeCompatibility(leftType, exprType)) {
                        char err[200];
                        snprintf(err, sizeof(err), "Incompatible types in assignment to '%s': expected %s, got %s", 
                                 symbolTable[symIdx].name, dataTypeToString(leftType), dataTypeToString(exprType));
                        semanticReportError(err);
                    }
                    fprintf(annotOut, "    Assignment Expression Type: %s\n", dataTypeToString(exprType));
                    if (strcmp(token, ";") != 0) {
                        semanticReportError("Expected ';' after assignment");
                    }
                }
            }
        }
    }
}

void parseFunction() {
    DataType retType = stringToDataType(token);
    fprintf(annotOut, "Function Declaration:\n");
    fprintf(annotOut, "  Return Type: %s\n", dataTypeToString(retType));

    if (!nextToken() || !isIdentifier(type)) {
        semanticReportError("Expected function name");
        return;
    }

    char funcName[MAX_TOKEN_LEN];
    strcpy(funcName, token);
    fprintf(annotOut, "  Name: %s\n", funcName);

    int symIdx = addSymbol(funcName, retType, true);
    if (symIdx == -1) return;

    if (!nextToken() || strcmp(token, "(") != 0) {
        semanticReportError("Expected '(' after function name");
        return;
    }
    fprintf(annotOut, "  Parameters:\n");

    DataType paramTypes[MAX_PARAMS];
    int paramCount = 0;
    parseParameters(paramTypes, &paramCount);
    if (symIdx != -1) {
        addFunctionParams(symIdx, paramTypes, paramCount);
    }

    if (!nextToken() || strcmp(token, ")") != 0) {
        semanticReportError("Expected ')' after parameters");
        return;
    }
    if (!nextToken() || strcmp(token, "{") != 0) {
        semanticReportError("Expected '{' to start function body");
        return;
    }

    currentFunctionReturnType = retType;
    fprintf(annotOut, "  Body:\n");
    parseFunctionBody(retType);
    fprintf(annotOut, "End Function: %s\n\n", funcName);
}

void parseTranslationUnit() {
    enterScope();
    while (nextToken()) {
        if (strcmp(type, "KEYWORD") == 0 && isDatatype(token)) {
            long pos = ftell(fp);
            char nextType[50], nextToken[100];
            if (!fscanf(fp, "%s %s", nextType, nextToken)) break;
            if (isIdentifier(nextType)) {
                long pos2 = ftell(fp);
                char tType[50], tToken[100];
                if (fscanf(fp, "%s %s", tType, tToken) && strcmp(tToken, "(") == 0) {
                    fseek(fp, pos, SEEK_SET);
                    parseFunction();
                } else {
                    fseek(fp, pos, SEEK_SET);
                    parseDeclaration();
                }
            } else {
                fseek(fp, pos, SEEK_SET);
                parseDeclaration();
            }
        }
    }
    exitScope();
}

int main() {
    fp = fopen("tokens.txt", "r");
    if (!fp) {
        perror("Could not open tokens.txt");
        return 1;
    }

    symbolOut = fopen("symbol_table.txt", "w");
    if (!symbolOut) {
        perror("Could not open symbol_table.txt");
        fclose(fp);
        return 1;
    }

    annotOut = fopen("annotated_parse_tree.txt", "w");
    if (!annotOut) {
        perror("Could not open annotated_parse_tree.txt");
        fclose(fp);
        fclose(symbolOut);
        return 1;
    }

    parseTranslationUnit();

    fprintf(symbolOut, "SYMBOL TABLE\n");
    fprintf(symbolOut, "%-20s %-10s %-10s %-15s %-10s\n", 
            "Name", "Type", "Function", "Parameters", "Line");
    fprintf(symbolOut, "------------------------------------------------------------\n");
    
    for (int i = 0; i < symbolCount; i++) {
        fprintf(symbolOut, "%-20s %-10s %-10s ", 
                symbolTable[i].name,
                dataTypeToString(symbolTable[i].type),
                symbolTable[i].isFunction ? "Yes" : "No");
        
        if (symbolTable[i].isFunction) {
            fprintf(symbolOut, "%-15d", symbolTable[i].paramCount);
            for (int j = 0; j < symbolTable[i].paramCount; j++) {
                if (j > 0) fprintf(symbolOut, ",");
                fprintf(symbolOut, "%s", dataTypeToString(symbolTable[i].paramTypes[j]));
            }
        } else {
            fprintf(symbolOut, "%-15s", "N/A");
        }
        
        fprintf(symbolOut, "%-10d\n", symbolTable[i].lineNumber);
    }

    if (semanticError) {
        printf("Semantic analysis completed with errors.\n");
        fprintf(symbolOut, "\nSemantic analysis completed with errors.\n");
        fprintf(annotOut, "\nSemantic analysis completed with errors.\n");
    } else {
        printf("Semantic analysis completed successfully.\n");
        fprintf(symbolOut, "\nSemantic analysis completed successfully.\n");
        fprintf(annotOut, "\nSemantic analysis completed successfully.\n");
    }

    fclose(fp);
    fclose(symbolOut);
    fclose(annotOut);
    return 0;
}
