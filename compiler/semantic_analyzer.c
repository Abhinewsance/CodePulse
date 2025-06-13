#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_SYMBOLS 1000
#define MAX_PARAMS 20
#define MAX_TOKEN_LEN 100

typedef enum { TYPE_INT, TYPE_FLOAT, TYPE_CHAR, TYPE_VOID, TYPE_UNKNOWN } DataType;

typedef struct {
    char name[MAX_TOKEN_LEN];
    DataType type;
    bool isFunction;
    int paramCount;
    DataType paramTypes[MAX_PARAMS];
} Symbol;

FILE *fp;
bool semanticError = false;

Symbol symbolTable[MAX_SYMBOLS];
int symbolCount = 0;

char type[50], token[100];

// Utility functions to convert string tokens to DataType enum
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

// Error reporting
void semanticReportError(const char *msg) {
    fprintf(stderr, "Semantic Error: %s\n", msg);
    semanticError = true;
}

// Lookup symbol by name
int findSymbol(const char *name) {
    for (int i = 0; i < symbolCount; i++) {
        if (strcmp(symbolTable[i].name, name) == 0) return i;
    }
    return -1;
}

// Add symbol to table
int addSymbol(const char *name, DataType type, bool isFunction) {
    if (findSymbol(name) != -1) {
        char err[200];
        snprintf(err, sizeof(err), "Duplicate declaration of '%s'", name);
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
    return symbolCount++;
}

// Add function parameters to symbol table entry
void addFunctionParams(int symIndex, DataType *params, int count) {
    if (symIndex < 0 || symIndex >= symbolCount) return;
    symbolTable[symIndex].paramCount = count;
    for (int i = 0; i < count; i++) {
        symbolTable[symIndex].paramTypes[i] = params[i];
    }
}

// Check if identifier is declared
bool isDeclared(const char *name) {
    return findSymbol(name) != -1;
}

// Check parameter types match
bool checkParamTypes(int symIndex, DataType *callParams, int callCount) {
    if (symIndex < 0 || symIndex >= symbolCount) return false;
    Symbol *sym = &symbolTable[symIndex];
    if (sym->paramCount != callCount) return false;
    for (int i = 0; i < callCount; i++) {
        if (sym->paramTypes[i] != callParams[i]) return false;
    }
    return true;
}

// Parsing forward declarations

void parseTranslationUnit();
void parseFunction();
void parseParameters(DataType *params, int *paramCount);
void parseFunctionBody(DataType funcReturnType);
void parseDeclaration();
void parseStatement();
void parseExpression();  // Simplified: just read tokens until ; or )

// Global variable to keep track of current function return type
DataType currentFunctionReturnType = TYPE_UNKNOWN;

// Helpers
bool isDatatype(const char *token) {
    return strcmp(token, "int") == 0 || strcmp(token, "float") == 0 ||
           strcmp(token, "char") == 0 || strcmp(token, "void") == 0;
}

bool isIdentifier(const char *type) {
    return strcmp(type, "IDENTIFIER") == 0;
}

bool isNumber(const char *type) {
    return strcmp(type, "NUMBER") == 0;
}

// Consume next token pair, return false if EOF
bool nextToken() {
    if (fscanf(fp, "%s %s", type, token) == EOF) return false;
    return true;
}

// Parse Translation Unit: multiple functions or declarations
void parseTranslationUnit() {
    while (nextToken()) {
        if (strcmp(type, "KEYWORD") == 0 && isDatatype(token)) {
            // Peek next token
            long pos = ftell(fp);
            char nextType[50], nextToken[100];
            if (fscanf(fp, "%s %s", nextType, nextToken) == EOF) break;

            if (isIdentifier(nextType)) {
                long pos2 = ftell(fp);
                char tType[50], tToken[100];
                if (fscanf(fp, "%s %s", tType, tToken) == EOF) break;

                if (strcmp(tToken, "(") == 0) {
                    // Function
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
        } else {
            // Skip unknown or unhandled tokens
            // For example PREPROCESSOR or KEYWORD void outside function
        }
    }
}

// Parse function definition
void parseFunction() {
    // Expect return type
    if (strcmp(type, "KEYWORD") != 0 || !isDatatype(token)) {
        semanticReportError("Expected function return type");
        return;
    }
    DataType retType = stringToDataType(token);

    if (!nextToken()) {
        semanticReportError("Unexpected EOF after return type");
        return;
    }
    if (!isIdentifier(type)) {
        semanticReportError("Expected function name identifier");
        return;
    }

    char funcName[MAX_TOKEN_LEN];
    strcpy(funcName, token);

    if (!nextToken() || strcmp(token, "(") != 0) {
        semanticReportError("Expected '(' after function name");
        return;
    }

    // Add function symbol
    int symIndex = addSymbol(funcName, retType, true);

    // Parse parameters
    DataType paramTypes[MAX_PARAMS];
    int paramCount = 0;
    parseParameters(paramTypes, &paramCount);
    if (symIndex != -1) addFunctionParams(symIndex, paramTypes, paramCount);

    // After parameters, expect ')'
    if (!nextToken() || strcmp(token, ")") != 0) {
        semanticReportError("Expected ')' after function parameters");
        return;
    }

    // Expect '{'
    if (!nextToken() || strcmp(token, "{") != 0) {
        semanticReportError("Expected '{' after function header");
        return;
    }

    currentFunctionReturnType = retType;
    parseFunctionBody(retType);
}

// Parse parameters list
void parseParameters(DataType *params, int *paramCount) {
    *paramCount = 0;
    while (true) {
        long pos = ftell(fp);
        if (!nextToken()) {
            semanticReportError("Unexpected EOF in function parameters");
            return;
        }
        if (strcmp(token, ")") == 0) {
            fseek(fp, pos, SEEK_SET);
            return;
        }
        if (strcmp(type, "KEYWORD") == 0 && isDatatype(token)) {
            params[*paramCount] = stringToDataType(token);

            if (!nextToken() || !isIdentifier(type)) {
                semanticReportError("Expected identifier in parameters");
                return;
            }
            // We could add param variable symbol here for local checking if needed

            (*paramCount)++;

            long pos2 = ftell(fp);
            if (!nextToken()) {
                semanticReportError("Unexpected EOF in parameters");
                return;
            }
            if (strcmp(token, ",") == 0) {
                continue;
            } else if (strcmp(token, ")") == 0) {
                fseek(fp, pos2, SEEK_SET);
                return;
            } else {
                semanticReportError("Expected ',' or ')' in parameters");
                return;
            }
        } else {
            semanticReportError("Expected data type in parameters");
            return;
        }
    }
}

// Parse function body - for semantic checks, we only check declarations and return statements for now
void parseFunctionBody(DataType funcReturnType) {
    while (true) {
        if (!nextToken()) {
            semanticReportError("Unexpected EOF in function body");
            return;
        }
        if (strcmp(token, "}") == 0) {
            // End of function
            return;
        }
        if (strcmp(type, "KEYWORD") == 0) {
            if (isDatatype(token)) {
                parseDeclaration();
            } else if (strcmp(token, "return") == 0) {
                // parse return expression and check type
                char exprTypeStr[100] = "";
                DataType exprType = TYPE_UNKNOWN;
                char expr[256] = "";

                // read tokens until ;
                while (true) {
                    long pos = ftell(fp);
                    if (!nextToken()) {
                        semanticReportError("Unexpected EOF in return statement");
                        return;
                    }
                    if (strcmp(token, ";") == 0) break;
                    strcat(expr, token);
                    strcat(expr, " ");
                }

                // For simplicity, let's just check if return expr is empty (void return)
                if (strlen(expr) == 0 && funcReturnType != TYPE_VOID) {
                    semanticReportError("Non-void function must return a value");
                }
                // We can extend this to type check expression if needed
            } else {
                // For simplicity skip other statements
                // Ideally parse statements, check usage of identifiers, etc.
            }
        } else if (isIdentifier(type)) {
            // Could be function call or assignment
            // Check if function call and check args (not implemented here)
            // Check if identifier declared
            if (!isDeclared(token)) {
                char err[200];
                snprintf(err, sizeof(err), "Undeclared identifier '%s' used", token);
                semanticReportError(err);
                // skip rest of statement until ';'
                while (nextToken() && strcmp(token, ";") != 0);
            } else {
                // Skip for now
                // could parse assignments or function calls
                while (nextToken() && strcmp(token, ";") != 0);
            }
        } else {
            // skip unexpected tokens
        }
    }
}

// Parse variable declaration
void parseDeclaration() {
    if (strcmp(type, "KEYWORD") != 0 || !isDatatype(token)) {
        semanticReportError("Expected data type for declaration");
        return;
    }
    DataType varType = stringToDataType(token);

    if (!nextToken()) {
        semanticReportError("Unexpected EOF after type in declaration");
        return;
    }
    if (!isIdentifier(type)) {
        semanticReportError("Expected identifier after type");
        return;
    }
    char varName[MAX_TOKEN_LEN];
    strcpy(varName, token);

    // Check duplicate declaration
    if (findSymbol(varName) != -1) {
        char err[200];
        snprintf(err, sizeof(err), "Duplicate declaration of variable '%s'", varName);
        semanticReportError(err);
        // skip rest
        while (nextToken() && strcmp(token, ";") != 0);
        return;
    }
    // Add variable symbol
    addSymbol(varName, varType, false);

    if (!nextToken()) {
        semanticReportError("Unexpected EOF after identifier");
        return;
    }

    if (strcmp(token, ";") == 0) {
        // simple declaration without initializer
        return;
    } else if (strcmp(token, "=") == 0) {
        // skip initializer expression until ';'
        while (nextToken() && strcmp(token, ";") != 0);
    } else {
        semanticReportError("Expected ';' or '=' after identifier in declaration");
        return;
    }
}

int main() {
    fp = fopen("tokens.txt", "r");
    if (!fp) {
        perror("Could not open tokens.txt");
        return 1;
    }

    parseTranslationUnit();

    if (semanticError) {
        printf("Semantic analysis completed with errors.\n");
    } else {
        printf("Semantic analysis completed successfully.\n");
    }

    fclose(fp);
    return 0;
}
