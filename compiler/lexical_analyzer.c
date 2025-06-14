#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

// All C keywords
char *keywords[] = {
    "auto","break","case","char","const","continue","default","do","double",
    "else","enum","extern","float","for","goto","if","int","long","register",
    "return","short","signed","sizeof","static","struct","switch","typedef",
    "union","unsigned","void","volatile","while", NULL
};

// Check if string is a keyword
int isKeyword(char *word) {
    for (int i = 0; keywords[i] != NULL; i++) {
        if (strcmp(keywords[i], word) == 0) return 1;
    }
    return 0;
}

// Check if character is an operator
int isOperator(char c) {
    return strchr("+-*/%=<>!&|^", c) != NULL;
}

const char *validOperators[] = {
    "+", "-", "*", "/", "%", "=", "<", ">", "!", "&", "|", "^",
    "++", "--", "==", "!=", "<=", ">=", "&&", "||", "+=", "-=", "*=", "/=", "%=",
    NULL
};

// Check if character is a separator
int isSeparator(char c) {
    return strchr("(){}[]:;\"'.,|", c) != NULL;
}

// Print token with type
void printToken(char *type, char *token) {
    printf("<%s> %s\n", type, token);
    FILE *fp = fopen("tokens.txt", "a");
    fprintf(fp, "%s %s\n", type, token);
    fclose(fp);
}

// Main tokenizer
void tokenize(char *code) {
    int i = 0, j = 0;
    char token[256];

    while (code[i] != '\0') {
        // Skip whitespace
        if (isspace(code[i])) {
            i++;
            continue;
        }

        // Comments
        if (code[i] == '/' && code[i+1] == '/') {
            while (code[i] != '\n' && code[i] != '\0') i++;
            continue;
        }
        if (code[i] == '/' && code[i+1] == '*') {
            i += 2;
            while (!(code[i] == '*' && code[i+1] == '/') && code[i] != '\0') i++;
            if (code[i] != '\0') i += 2;
            continue;
        }

        // Preprocessor directives
        if (code[i] == '#') {
            j = 0;
            while (code[i] != '\n' && code[i] != '\0') {
                token[j++] = code[i++];
            }
            token[j] = '\0';

            if (strncmp(token, "#include", 8) == 0) {
                printToken("PREPROCESSOR", "#include");

                char *start = token + 8;
                while (isspace(*start)) start++;

                char headerToken[128];
                int k = 0;

                if ((*start == '<' || *start == '"')) {
                    char delim = (*start == '<') ? '>' : '"';
                    headerToken[k++] = *start++;

                    while (*start != '\0' && *start != delim && k < 126) {
                        headerToken[k++] = *start++;
                    }

                    if (*start == delim) {
                        headerToken[k++] = *start++;
                    }

                    headerToken[k] = '\0';
                    printToken("HEADER_FILE", headerToken);
                } else {
                    printToken("HEADER_FILE", start);
                }
            } else {
                fprintf(stderr, "Error: Invalid preprocessor directive: %s\n", token);
                exit(1);
            }
            continue;
        }

        // String literal
        if (code[i] == '"') {
            j = 0;
            token[j++] = code[i++];

            while (code[i] != '\0') {
                if (code[i] == '\\' && code[i + 1] != '\0') {
                    token[j++] = code[i++];
                    token[j++] = code[i++];
                } else if (code[i] == '"') {
                    token[j++] = code[i++];
                    break;
                } else {
                    token[j++] = code[i++];
                }
            }

            token[j] = '\0';

            if (token[j - 1] != '"') {
                fprintf(stderr, "Error: Unterminated string literal\n");
                exit(1);
            }

            printToken("STRING_LITERAL", token);
            continue;
        }

        // Character constant
        if (code[i] == '\'') {
            j = 0;
            token[j++] = code[i++];

            if (code[i] == '\\' && code[i + 1] != '\0') {
                token[j++] = code[i++];
                token[j++] = code[i++];
            } else if (code[i] != '\0' && code[i] != '\'') {
                token[j++] = code[i++];
            }

            if (code[i] == '\'') {
                token[j++] = code[i++];
            } else {
                fprintf(stderr, "Error: Unterminated char constant\n");
                exit(1);
            }

            token[j] = '\0';
            printToken("CHAR_CONSTANT", token);
            continue;
        }

        // Numbers (check for invalid identifiers like 1abc)
        if (isdigit(code[i])) {
            j = 0;
            int dotCount = 0;

            while (isdigit(code[i]) || code[i] == '.') {
                if (code[i] == '.') {
                    dotCount++;
                    if (dotCount > 1) {
                        fprintf(stderr, "Error: Invalid number (multiple decimal points): ");
                        while (isdigit(code[i]) || code[i] == '.') putchar(code[i++]);
                        putchar('\n');
                        exit(1);
                    }
                }
                token[j++] = code[i++];
            }

            if (isalpha(code[i]) || code[i] == '_') {
                while (isalnum(code[i]) || code[i] == '_') {
                    token[j++] = code[i++];
                }
                token[j] = '\0';
                fprintf(stderr, "Error: Invalid identifier starting with digit: %s\n", token);
                exit(1);
            }

            token[j] = '\0';
            printToken("NUMBER", token);
            continue;
        }

        // Identifiers / Keywords
        if (isalpha(code[i]) || code[i] == '_') {
            j = 0;
            while (isalnum(code[i]) || code[i] == '_') {
                token[j++] = code[i++];
            }
            token[j] = '\0';

            if (isKeyword(token)) {
                printToken("KEYWORD", token);
            } else {
                printToken("IDENTIFIER", token);
            }
            continue;
        }

        // Operators
        if (isOperator(code[i])) {
            j = 0;
            token[j++] = code[i++];
            if (isOperator(code[i])) token[j++] = code[i++];
            token[j] = '\0';

            int valid = 0;
            for (int k = 0; validOperators[k] != NULL; k++) {
                if (strcmp(validOperators[k], token) == 0) {
                    valid = 1;
                    break;
                }
            }

            if (valid) {
                printToken("OPERATOR", token);
            } else {
                fprintf(stderr, "Error: Invalid operator '%s'\n", token);
                exit(1);
            }
            continue;
        }

        // Separators
        if (isSeparator(code[i])) {
            token[0] = code[i++];
            token[1] = '\0';
            printToken("SEPARATOR", token);
            continue;
        }

        // Unknown character
        token[0] = code[i++];
        token[1] = '\0';
        printToken("UNKNOWN", token);
    }
}

int main() {
    FILE *clear = fopen("tokens.txt", "w"); // Clear file
    if (clear) fclose(clear);

    FILE *fp = fopen("input.c", "r");
    if (!fp) {
        printf("Cannot open input file.\n");
        return 1;
    }

    char code[10000];
    fread(code, sizeof(char), 10000, fp);
    fclose(fp);

    printf("----- Tokens Identified -----\n");
    tokenize(code);
    return 0;
}
