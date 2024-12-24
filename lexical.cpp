#include <iostream>
#include <vector>
#include <string>
#include <map>
#include <stdexcept>
#include <fstream>
#include <sstream>
#include <algorithm>
#include <unordered_map>
#include <unordered_set>

using namespace std;

enum TokenType {
    T_INT, T_FLOAT, T_DOUBLE, T_STRING, T_BOOL, T_CHAR, T_VOID, // Added data types
    T_ID, T_NUM, T_IF, T_ELSE, T_RETURN,T_INCREMENT, T_DECREMENT,
    T_WHILE, T_FOR, T_BREAK, T_CONTINUE, T_SWITCH, T_CASE, T_DEFAULT, T_PRINT, // Added key words
    T_ASSIGN, T_PLUS, T_MINUS, T_MUL, T_DIV, 
    T_LPAREN, T_RPAREN, T_LBRACE, T_RBRACE,  
    T_SEMICOLON, T_COLON, T_GT, T_LT, T_AND, T_OR, T_EQ, T_NE, T_NOT,
    T_FUNCTION, T_COMMA, T_FUNCTION_CALL, // token types for function handling
    T_EOF
};

struct Token {
    TokenType type;
    string value;
    int lineNumber;
};

// human friendly error handling
class CompilerError : public runtime_error {
public:
    CompilerError(const string& message, int lineNumber) 
        : runtime_error(formatError(message, lineNumber)) {}

private:
    static string formatError(const string& message, int lineNumber) {
        return "Error at line " + to_string(lineNumber) + ": " + message;
    }
};

class Lexer {
private:
    string src;
    size_t pos;
    int lineNumber;
    // track last declared type
    TokenType lastDeclaredType;
    TokenType booltype; 
public:
    Lexer(const string &src) : src(src), pos(0), lineNumber(1) {}

    vector<Token> tokenize() {
        vector<Token> tokens;
        while (pos < src.size()) {
            char current = src[pos];

            if (current == '\n') {
                lineNumber++;
                pos++;
                continue;
            }
            if (isspace(current)) {
                pos++;
                continue;
            }
             // Handle single-line comment starting with #
            if (current == '#') {
                skipSingleLineComment();
                continue;
            }

            // Handle multi-line comment starting with %
            if (current == '%') {
                skipMultiLineComment();
                continue;
            }
            // handle identifiers and keywords
            if (isalpha(current)) {
                string word = consumeWord();
                TokenType type = T_ID;

                if (word == "fn") {
                    type = T_FUNCTION;
                    lastDeclaredType = T_FUNCTION;
                }
                else if (word == "call") {
                    type = T_FUNCTION_CALL;
                }
                else if (word == "int") {
                    type = T_INT;
                    lastDeclaredType = T_INT;  // Update lastDeclaredType
                }
                else if (word == "float") {
                    type = T_FLOAT;
                    lastDeclaredType = T_FLOAT;  // Update lastDeclaredType
                }
                else if (word == "double") {
                    type = T_DOUBLE;
                    lastDeclaredType = T_DOUBLE;
                }
                else if (word == "string") type = T_STRING;
                else if (word == "bool") {
                    type = T_BOOL;
                    lastDeclaredType = T_BOOL;
                }
                else if (word == "void") type = T_VOID;
                else if (word == "char") type = T_CHAR;
                else if (word == "if") type = T_IF;
                else if (word == "else") type = T_ELSE;
                else if (word == "return") type = T_RETURN;
                else if (word == "while") type = T_WHILE;
                else if (word == "for") type = T_FOR;
                else if (word == "break") type = T_BREAK;
                else if (word == "continue") type = T_CONTINUE;
                else if (word == "switch") type = T_SWITCH;
                else if (word == "case") type = T_CASE;
                else if (word == "default") type = T_DEFAULT;
                else if(word == "print") type = T_PRINT;
                else if (word == "true" || word == "false") {
                    type = T_BOOL;
                } 

                //cout<<"Type: "<<type<<"word: "<<word<<endl;
                tokens.push_back(Token{type, word, lineNumber});
                continue;
            }
            // Handle numbers
            if (isdigit(current) || current == '.') {
                string number = consumeNumber();
                TokenType type;

                bool isFloatLiteral = !number.empty() && number.back() == 'f';
                if (isFloatLiteral) {
                    number.pop_back();
                }
                // Assign type based on lastDeclaredType and number format
                if (lastDeclaredType == T_INT) {
                    if (number.find('.') != string::npos) {
                        cout << "Warning: Decimal number assigned to int at line " << lineNumber << endl;
                    }
                    type = T_INT;
                }
                else if (lastDeclaredType == T_FLOAT || isFloatLiteral) {
                    type = T_FLOAT;
                }
                else if (lastDeclaredType == T_DOUBLE) {
                    type = T_DOUBLE;
                }
                else {
                    type = number.find('.') != string::npos ? T_DOUBLE : T_INT;
                }
                tokens.push_back(Token{type, number, lineNumber});
                continue;
            }
            if (current == '=') {
                if (pos + 1 < src.size() && src[pos + 1] == '=') {
                    tokens.push_back(Token{T_EQ, "==", lineNumber});
                    pos += 2;
                    continue;
                } else {
                    tokens.push_back(Token{T_ASSIGN, "=", lineNumber});
                    pos++;
                    continue;
                }
            }
            // Handle semicolon and reset lastDeclaredType
            if (current == ';') {
                tokens.push_back(Token{T_SEMICOLON, ";", lineNumber});
                lastDeclaredType = T_ID;
                pos++;
                continue;
            }
            if(current == ','){
                tokens.push_back(Token{T_COMMA, ",", lineNumber});
                pos++;
                continue;
            }
            if (current == '"') {
                // Handle string literals
                string strValue = consumeString();
                //cout << "Word: " << strValue << " Type: String" << endl;
                tokens.push_back(Token{T_STRING, "\"" + strValue + "\"", lineNumber});
                continue;
            }
            if (current == '\'') {
                string charValue = consumeChar();
                //cout << "Word: " << charValue << " Type: Char" << endl;
                tokens.push_back(Token{T_CHAR, charValue, lineNumber});
                continue;
            }

             // Handle ++ and --
            if (current == '+') {
                if (pos + 1 < src.size() && src[pos + 1] == '+') {
                    tokens.push_back(Token{T_INCREMENT, "++", lineNumber});
                    pos += 2;
                    continue;
                }
                tokens.push_back(Token{T_PLUS, "+", lineNumber});
                pos++;
                continue;
            }

            if (current == '-') {
                if (pos + 1 < src.size() && src[pos + 1] == '-') {
                    tokens.push_back(Token{T_DECREMENT, "--", lineNumber});
                    pos += 2;
                    continue;
                }
                tokens.push_back(Token{T_MINUS, "-", lineNumber});
                pos++;
                continue;
            }

            // Handle &&, || and ! operators
            if (current == '&') {
                if (pos + 1 < src.size() && src[pos + 1] == '&') {
                    tokens.push_back(Token{T_AND, "&&", lineNumber});
                    pos += 2;
                    continue;
                }
            }
            if (current == '|') {
                if (pos + 1 < src.size() && src[pos + 1] == '|') {
                    tokens.push_back(Token{T_OR, "||", lineNumber});
                    pos += 2;
                    continue;
                }
            }
            if (current == '!') {
                if (pos + 1 < src.size() && src[pos + 1] == '=') {
                    tokens.push_back(Token{T_NE, "!=", lineNumber});
                    pos += 2;
                    continue;
                } else {
                    tokens.push_back(Token{T_NOT, "!", lineNumber});
                    pos++;
                    continue;
                }
            }

            switch (current) {
                case '=': tokens.push_back(Token{T_ASSIGN, "=", lineNumber}); break;
                case '+': tokens.push_back(Token{T_PLUS, "+", lineNumber}); break;
                case '-': tokens.push_back(Token{T_MINUS, "-", lineNumber}); break;
                case '*': tokens.push_back(Token{T_MUL, "*", lineNumber}); break;
                case '/': tokens.push_back(Token{T_DIV, "/", lineNumber}); break;
                case '(': tokens.push_back(Token{T_LPAREN, "(", lineNumber}); break;
                case ')': tokens.push_back(Token{T_RPAREN, ")", lineNumber}); break;
                case '{': tokens.push_back(Token{T_LBRACE, "{", lineNumber}); break;
                case '}': tokens.push_back(Token{T_RBRACE, "}", lineNumber}); break;
                case ';': tokens.push_back(Token{T_SEMICOLON, ";", lineNumber}); break;
                case '>': tokens.push_back(Token{T_GT, ">", lineNumber}); break;
                case '<': tokens.push_back(Token{T_LT, "<", lineNumber}); break;
                case ':': tokens.push_back(Token{T_COLON, ":", lineNumber}); break;
                default:
                    cout << "Unexpected character: " << current << " at line " << lineNumber << endl;
                    exit(1);
            }
            pos++;
        }
        tokens.push_back(Token{T_EOF, "", lineNumber});
        return tokens;
    }

     // Skip single-line comment
    void skipSingleLineComment() {
        while (pos < src.size() && src[pos] != '\n') {
            pos++;
        }
    }

    // Skip multi-line comment
    void skipMultiLineComment() {
        pos++;
        while (pos < src.size() && !(src[pos] == '%' && (src[pos + 1] == ' ' || src[pos + 1 != ' ']))) {
            pos++;
        }
        pos += 2;
    }

    string consumeNumber() {
        size_t start = pos;
        bool hasDot = false;

        // Handle leading decimal point
        if (src[pos] == '.') {
            hasDot = true;
            pos++;
        }

        // Consume digits and decimal point
        while (pos < src.size() && (isdigit(src[pos]) || src[pos] == '.' || src[pos] == 'f')) {
            if (src[pos] == '.') {
                if (hasDot) {
                    throw CompilerError("Multiple decimal points in number", lineNumber);
                }
                hasDot = true;
            }
            pos++;
        }

        string number = src.substr(start, pos - start);

        // Validate the number format
        if (number == "." || number == "f") {
            throw CompilerError("Invalid number format", lineNumber);
        }

        return number;
    }


    string consumeWord() {
        size_t start = pos;
        while (pos < src.size() && isalnum(src[pos])) pos++;
        return src.substr(start, pos - start);
    }

    string consumeString() {
        pos++; // Skip the opening double quote
        size_t start = pos;
        
        while (pos < src.size()) {
            if (src[pos] == '\\' && pos + 1 < src.size()) {
                // Handle escaped characters
                pos += 2;
                continue;
            }
            if (src[pos] == '"') {
                string result = src.substr(start, pos - start);
                pos++; // Skip the closing double quote
                return result;
            }
            pos++;
        }
        
        throw CompilerError("Unterminated string literal", lineNumber);
    }

    string consumeChar() {
        pos++;

        if (pos >= src.size() || src[pos] == '\n') {
            throw CompilerError("Unterminated character literal", lineNumber);
        }

        char c = src[pos++];

        if (pos >= src.size() || src[pos] != '\'') {
            throw CompilerError("Invalid character literal", lineNumber);
        }

        pos++;
        return string(1, c);
    }

    // handle boolean values
    string consumeBool() {
        string word = consumeWord();
        string lowerWord = word;
        transform(lowerWord.begin(), lowerWord.end(), lowerWord.begin(), ::tolower);
        if (lowerWord == "true" || lowerWord == "1") {
            return "true";
        }
        else if (lowerWord == "false" || lowerWord == "0") {
            return "false";
        }        
        throw CompilerError("Invalid boolean value: '" + word + "'. Expected true, false, 1, or 0", lineNumber);
    }
};

class SymbolTable {
private:
    vector<map<string, string>> scopes;
    unordered_map<string, vector<string>> functionParameters;
    unordered_map<string, string> functionReturnTypes;
public:
    // Enter a new scope
    void enterScope() {
        scopes.push_back(map<string, string>());
    }
    // Exit the current scope
    void exitScope() {
        if (!scopes.empty()) {
            scopes.pop_back();
        }
    }
    void declareVariable(const string &name, const string &type) {
        // If we have no scopes, create the global scope
        if (scopes.empty()) {
            scopes.push_back(map<string, string>());
        }
        
        // Check only in the current scope
        if (scopes.back().find(name) != scopes.back().end()) {
            throw runtime_error("Semantic error: Variable '" + name + "' is already declared in current scope.");
        }
        scopes.back()[name] = type;
    }

    string getVariableType(const string &name) {
        // Search from innermost to outermost scope
        for (auto it = scopes.rbegin(); it != scopes.rend(); ++it) {
            auto var = it->find(name);
            if (var != it->end()) {
                return var->second;
            }
        }
        throw runtime_error("Semantic error: Variable '" + name + "' is not declared.");
    }

    bool isDeclared(const string &name) const {
        // Search from innermost to outermost scope
        for (auto it = scopes.rbegin(); it != scopes.rend(); ++it) {
            if (it->find(name) != it->end()) {
                return true;
            }
        }
        return false;
    }
    void declareFunction(const string &name, const string &returnType, const vector<string> &parameters) {
        if (functionParameters.count(name)) {
            throw CompilerError("Function '" + name + "' is already defined", -1);
        }
        functionParameters[name] = parameters;
        functionReturnTypes[name] = returnType;
    }

    bool isFunctionDefined(const string &name) const {
        return functionParameters.count(name) > 0;
    }

    vector<string> getFunctionParameterTypes(const string &name) const {
        if (!isFunctionDefined(name)) {
            throw CompilerError("Function '" + name + "' is not defined", -1);
        }
        return functionParameters.at(name);
    }

    string getFunctionReturnType(const string &name) const {
        if (!isFunctionDefined(name)) {
            throw CompilerError("Function '" + name + "' is not defined", -1);
        }
        return functionReturnTypes.at(name);
    }

private:
    map<string, string> symbolTable;
};

// Intermediate code generator
class IntermediateCodeGnerator {
public:
    vector<string> instructions;
    int tempCount = 0;
    int labelCounter = 0;

    string newTemp() {
        return "t" + to_string(tempCount++);
    }

    const vector<string>& getInstructions() const {
        return instructions;
    }

    void addInstruction(const string &instr) {
        instructions.push_back(instr);
    }

    string getUniqueLabel(const string& base) {
        return base + "_" + to_string(labelCounter++);
    }

    void printInstructions() {
        for (const auto &instr : instructions) {
            cout << instr << endl;
        }
    }
};

// parser
class Parser {
private:
    vector<Token> tokens;
    size_t pos;
    SymbolTable &symTable;
    IntermediateCodeGnerator &icg;

    string getTokenTypeName(TokenType type) {
        switch (type) {
            case T_INT: return "int";
            case T_FLOAT: return "float";
            case T_DOUBLE: return "double";
            case T_STRING: return "string";
            case T_BOOL: return "bool";
            case T_CHAR: return "char";
            case T_VOID: return "void";
            case T_ID: return "identifier";
            case T_NUM: return "number";
            case T_IF: return "if";
            case T_ELSE: return "else";
            case T_RETURN: return "return";
            case T_WHILE: return "while";
            case T_BREAK: return "break";
            case T_CONTINUE: return "continue";
            case T_SWITCH: return "switch";
            case T_FOR: return "for";
            case T_CASE: return "case";
            case T_DEFAULT: return "default";
            case T_COLON: return ":";
            case T_ASSIGN: return "=";
            case T_PLUS: return "+";
            case T_MINUS: return "-";
            case T_MUL: return "*";
            case T_DIV: return "/";
            case T_LPAREN: return "(";
            case T_RPAREN: return ")";
            case T_LBRACE: return "{";
            case T_RBRACE: return "}";
            case T_SEMICOLON: return ";";
            case T_GT: return ">";
            case T_LT: return "<";
            case T_FUNCTION: return "fn";
            case T_FUNCTION_CALL: return "call";
            case T_INCREMENT: return "++";
            case T_DECREMENT: return "--";
            case T_PRINT: return "print";
            case T_AND: return "&&";
            case T_OR: return "||";
            case T_EQ: return "==";
            case T_NE: return "!=";
            case T_NOT: return "!";
            case T_EOF: return "end of file";
            default: return "unknown token";
        }
    }

    void expect(TokenType type) {
        if (tokens[pos].type != type) {
            string expected = getTokenTypeName(type);
            string found = getTokenTypeName(tokens[pos].type);
            throw CompilerError(
                "Expected '" + expected + "' but found '" + found + "'",
                tokens[pos].lineNumber
            );
        }
        pos++;
    }

    string expectAndReturnValue(TokenType type) {
        string value = tokens[pos].value;
        expect(type);
        return value;
    }

    void parseStatement() {
        try {
            switch (tokens[pos].type) {
                case T_FUNCTION:
                    parseFunctionDeclaration();
                    break;
                case T_FUNCTION_CALL:
                    parseFunctionCall();
                    break;
                case T_INT:
                    parseDeclaration();
                    break;
                case T_FLOAT:
                    parseDeclaration();
                    break;
                case T_DOUBLE:
                    parseDeclaration();
                    break;
                case T_STRING:
                    parseDeclaration();
                    break;
                case T_BOOL:
                    parseDeclaration();
                    break;
                case T_CHAR:
                    parseDeclaration();
                    break;
                case T_ID:
                     {
                        string varName = tokens[pos].value;
                        pos++; 
                        
                        // Check next token for increment/decrement
                        if (tokens[pos].type == T_INCREMENT || tokens[pos].type == T_DECREMENT)
                        {
                            parseIncrementStatement();
                        } 
                        else
                        {
                            // Move back to handle the ID token again
                            pos--;
                            parseAssignment();
                        }
                        break;
                    }
                case T_IF:
                    parseIfStatement();
                    break;
                case T_RETURN:
                    parseReturnStatement();
                    break;
                case T_WHILE:
                    parseWhileStatement();
                    break;
                case T_FOR:
                    parseForLoop();
                    break;
                case T_SWITCH:
                    parseSwitchStatement();
                    break;
                case T_LBRACE:
                    parseBlock();
                    break;
                case T_PRINT:
                    parsePrintStatement();
                    break;
                default:
                    throw CompilerError(
                        "Unexpected token '" + tokens[pos].value + "'",
                        tokens[pos].lineNumber
                    );
            }
        } catch (const CompilerError& e) {
            throw;
        } catch (const exception& e) {
            throw CompilerError(e.what(), tokens[pos].lineNumber);
        }
    }
    // handle function call
void parseFunctionCall(bool standalone = true) {
    string returnTemp = icg.newTemp();
    expect(T_FUNCTION_CALL);
    if (tokens[pos].type != T_ID) {
        throw CompilerError("Expected a function name after 'call'", tokens[pos].lineNumber);
    }
    string functionName = tokens[pos].value;
    pos++;
    
    if (!symTable.isFunctionDefined(functionName)) {
        throw CompilerError("Function '" + functionName + "' is not declared", tokens[pos].lineNumber);
    }
    
    expect(T_LPAREN);
    // Parse arguments
    vector<string> arguments;
    vector<string> argTemps;  // Store temporary variables for arguments
    
    while (tokens[pos].type != T_RPAREN) {
        string argument = parseExpression();
        arguments.push_back(argument);
        
        // Create and store temp for each argument
        string argTemp = icg.newTemp();
        argTemps.push_back(argTemp);
        
        if (tokens[pos].type == T_COMMA) {
            pos++;
        } else if (tokens[pos].type != T_RPAREN) {
            throw CompilerError("Expected ',' or ')' in function call", tokens[pos].lineNumber);
        }
    }
    expect(T_RPAREN);
    
    // Only expect semicolon for standalone function calls
    if (standalone) {
        expect(T_SEMICOLON);
    }
    
    // Type checking for arguments
    vector<string> paramTypes = symTable.getFunctionParameterTypes(functionName);
    if (arguments.size() != paramTypes.size()) {
        throw CompilerError("Function '" + functionName + "' expects " + 
                          to_string(paramTypes.size()) + " argument(s), but got " + 
                          to_string(arguments.size()) + ".", tokens[pos].lineNumber);
    }
    
    // Check argument types and generate temp assignments
    for (size_t i = 0; i < arguments.size(); i++) {
        string argumentType = getExpressionType(arguments[i]);
        string paramType = paramTypes[i];
        if (argumentType != paramType) {
            throw CompilerError("Function '" + functionName + "' expects parameter of type '" + 
                              paramType + "' but got '" + argumentType + "' for argument " + 
                              to_string(i + 1) + ".", tokens[pos].lineNumber);
        }
        // Generate temp assignment for each argument
        icg.addInstruction(argTemps[i] + " = " + arguments[i]);
    }
    
    // Generate PARAM instructions using temps
    for (const string& argTemp : argTemps) {
        icg.addInstruction("PARAM " + argTemp);
    }
    
    // Generate function call with proper return value handling
    if (!standalone) {
        icg.addInstruction(returnTemp + " = CALL " + functionName);
    } else {
        icg.addInstruction("CALL " + functionName);
    }
}
    string getExpressionType(const string& expr) {
        // If it's a variable, get its type from the symbol table
        if (symTable.isDeclared(expr)) {
            return symTable.getVariableType(expr);
        }
        if (isdigit(expr[0])) {
            if (expr.find('.') != string::npos) {
                return "double";
            } else {
                return "int";
            }
        }
        if (expr == "true" || expr == "false") {
            return "bool";
        }
        if (expr[0] == '"' && expr[expr.size() - 1] == '"') {
            return "string";
        }
        return "";
    }
    // handle print statement
    void parsePrintStatement() {
        expect(T_PRINT);
        expect(T_LPAREN);

        vector<pair<string, string>> printArgs; // Stores pairs of (value, type)
        bool isFirstArg = true;

        while (tokens[pos].type != T_RPAREN) {
            if (!isFirstArg) {
                expect(T_COMMA);
            }

            // Handle string literals
            if (tokens[pos].type == T_STRING) {
                string strLiteral = tokens[pos].value;
                printArgs.push_back({strLiteral, "string_literal"});
                pos++;

            // Handle variables
            } else if (tokens[pos].type == T_ID) {
                string varName = tokens[pos].value;
                
                if (!symTable.isDeclared(varName)) {
                    throw CompilerError("Undefined variable '" + varName + "' in print statement", 
                        tokens[pos].lineNumber);
                }
                
                string varType = symTable.getVariableType(varName);
                if (varType != "string" && varType != "int" && varType != "float" && 
                    varType != "double" && varType != "char") {
                    throw CompilerError("Cannot print variable of type '" + varType + "'", 
                        tokens[pos].lineNumber);
                }
                
                printArgs.push_back({varName, varType});
                pos++;

            // Handle numeric literals
            } else if (tokens[pos].type == T_INT) {
                printArgs.push_back({tokens[pos].value, "int"});
                pos++;
            } else if (tokens[pos].type == T_FLOAT) {
                printArgs.push_back({tokens[pos].value, "float"});
                pos++;
            } else if (tokens[pos].type == T_DOUBLE) {
                printArgs.push_back({tokens[pos].value, "double"});
                pos++;
            } else if (tokens[pos].type == T_CHAR) {
                printArgs.push_back({tokens[pos].value, "char"});
                pos++;
            } else {
                throw CompilerError("Invalid argument in print statement", tokens[pos].lineNumber);
            }

            // Handle string concatenation if next token is '+'
            while (tokens[pos].type == T_PLUS) {
                pos++; // Consume '+'
                
                if (tokens[pos].type == T_STRING) {
                    string& lastValue = printArgs.back().first;
                    lastValue += " + " + tokens[pos].value;
                    pos++;
                } else if (tokens[pos].type == T_ID) {
                    string varName = tokens[pos].value;
                    
                    if (!symTable.isDeclared(varName)) {
                        throw CompilerError("Undefined variable '" + varName + "' in print statement", 
                            tokens[pos].lineNumber);
                    }
                    
                    string varType = symTable.getVariableType(varName);
                    if (varType != "string") {
                        throw CompilerError("Can only concatenate strings in print statement", 
                            tokens[pos].lineNumber);
                    }
                    
                    string& lastValue = printArgs.back().first;
                    lastValue += " + " + varName;
                    pos++;
                } else {
                    throw CompilerError("Expected string or variable after '+'", tokens[pos].lineNumber);
                }
            }

            isFirstArg = false;
        }

        expect(T_RPAREN);
        expect(T_SEMICOLON);

        // Generate intermediate code for print statement
        string printInstruction = "print ";
        for (size_t i = 0; i < printArgs.size(); i++) {
            if (i > 0) {
                printInstruction += ", ";
            }
            printInstruction += printArgs[i].first;
        }
        icg.addInstruction(printInstruction);
    }
    // parse function declarations
    void parseFunctionDeclaration() {
        expect(T_FUNCTION);
        // Validate and consume the return type
        if (tokens[pos].type != T_INT && tokens[pos].type != T_FLOAT && tokens[pos].type != T_DOUBLE &&
            tokens[pos].type != T_STRING && tokens[pos].type != T_BOOL && tokens[pos].type != T_VOID && tokens[pos].type != T_CHAR) {
            throw CompilerError("Expected a valid return type after 'fn'", tokens[pos].lineNumber);
        }
        string returnType = tokens[pos].value;
        pos++;
        // Validate and consume the function name
        if (tokens[pos].type != T_ID) {
            throw CompilerError("Expected a valid function name after return type", tokens[pos].lineNumber);
        }
        string functionName = tokens[pos].value;
        pos++;
        // Create a new scope for the function
        symTable.enterScope();
        expect(T_LPAREN);
        vector<pair<string, string>> parameters;
        while (tokens[pos].type != T_RPAREN) {
            if (tokens[pos].type != T_INT && tokens[pos].type != T_FLOAT && tokens[pos].type != T_DOUBLE &&
                tokens[pos].type != T_STRING && tokens[pos].type != T_BOOL && tokens[pos].type != T_CHAR &&
                tokens[pos].type != T_VOID) {
                throw CompilerError("Invalid parameter type in function declaration", tokens[pos].lineNumber);
            }
            string paramType = tokens[pos].value;
            pos++;
            if (tokens[pos].type != T_ID) {
                throw CompilerError("Invalid parameter name in function declaration", tokens[pos].lineNumber);
            }
            string paramName = tokens[pos].value;
            pos++;
            symTable.declareVariable(paramName, paramType);
            parameters.push_back({paramType, paramName});
            if (tokens[pos].type == T_COMMA) {
                pos++;
            } else if (tokens[pos].type != T_RPAREN) {
                throw CompilerError("Expected ',' or ')' in parameter list", tokens[pos].lineNumber);
            }
        }
        expect(T_RPAREN);
        vector<string> paramTypes;
        for (const auto &param : parameters) {
            paramTypes.push_back(param.first);
        }
        symTable.declareFunction(functionName, returnType, paramTypes);

        // Validate function body
        expect(T_LBRACE);
        string funcStartLabel = icg.getUniqueLabel("func_" + functionName);
        icg.addInstruction(funcStartLabel + ":");
        bool hasReturnStatement = false;
        string lastReturnType = "";
        while (tokens[pos].type != T_RBRACE) {
            if (tokens[pos].type == T_RETURN) {
                hasReturnStatement = true;
                string variable = checkReturnStatement();
                // Store the current position
                size_t currentPos = pos;
                // the return statement and get its type
                string typeForReturnVariable = symTable.getVariableType(variable);
                checkReturnTypeMatch(returnType, typeForReturnVariable, tokens[pos].lineNumber);
                continue;
            }
            parseStatement();
        }
        expect(T_RBRACE);
        // Exit the function scope
        symTable.exitScope();
    }
    void checkReturnTypeMatch(const string& functionReturnType, const string& actualReturnType, int lineNumber) {
        // First handle void function cases
        if (functionReturnType == "void") {
            if (actualReturnType != "") {
                throw CompilerError("Void function cannot return a value", lineNumber);
            }
            return;
        }
        
        // Handle non-void functions with no return value
        if (actualReturnType == "") {
            throw CompilerError("Non-void function must return a value", lineNumber);
        }

        // Check for type compatibility
        if (functionReturnType != actualReturnType) {
            // Special case for numeric type conversions if you want to allow them
            bool isNumericConversion = (
                (functionReturnType == "float" && actualReturnType == "int") ||
                (functionReturnType == "double" && (actualReturnType == "int" || actualReturnType == "float"))
            );

            if (!isNumericConversion) {
                throw CompilerError(
                    "Return type mismatch: function expects '" + functionReturnType + 
                    "' but got '" + actualReturnType + "'", 
                    lineNumber
                );
            }
              // For string and char types, require exact matches
            if (functionReturnType == "string" || functionReturnType == "char") {
                throw CompilerError(
                    "Type mismatch: function of type '" + functionReturnType + 
                    "' must return value of exactly the same type, got '" + actualReturnType + "'", 
                    lineNumber
                );
            }
        }
    }
    string checkReturnStatement() {
        try {
            expect(T_RETURN);
            size_t exprPos = pos;
            string expr = parseExpression();
            TokenType exprType = tokens[exprPos].type;
            string value = tokens[exprPos].value;
            icg.addInstruction("return " + expr);
            expect(T_SEMICOLON);
            return value;
        } catch (const CompilerError& e) {
            throw;
        } catch (const exception& e) {
            throw CompilerError(e.what(), tokens[pos].lineNumber);
        }

    }
    // method to handle switch statements
    void parseSwitchStatement() {
        try {
            expect(T_SWITCH);
            expect(T_LPAREN);
            string switchVar = expectAndReturnValue(T_ID);
            expect(T_RPAREN);
            expect(T_LBRACE);

            //string endLabel = icg.newTemp();
            //string nextCaseLabel = icg.newTemp();
            string nextCaseLabel = icg.getUniqueLabel("case");
            //string defaultLabel = icg.getUniqueLabel("default");
            string endLabel = icg.getUniqueLabel("end_switch");
            //L1
            while (tokens[pos].type != T_RBRACE && tokens[pos].type != T_EOF) {
                if (tokens[pos].type == T_CASE) {
                    pos++; // consume 'case'
                    string caseValue = tokens[pos].value;
                    pos++; // consume case value
                    expect(T_COLON);

                    icg.addInstruction(nextCaseLabel + ":");
                    nextCaseLabel = icg.newTemp();
                    
                    string condition = switchVar + " == " + caseValue;
                    icg.addInstruction("if !" + condition + " goto " + nextCaseLabel);

                    while (tokens[pos].type != T_CASE && 
                        tokens[pos].type != T_DEFAULT && 
                        tokens[pos].type != T_RBRACE) {
                        if (tokens[pos].type == T_BREAK) {
                            pos++; // consume 'break'
                            expect(T_SEMICOLON);
                            icg.addInstruction("goto " + endLabel);
                            break;
                        }
                        parseStatement();
                    }
                }
                else if (tokens[pos].type == T_DEFAULT) {
                    pos++; // consume 'default'
                    expect(T_COLON);
                    
                    icg.addInstruction(nextCaseLabel + ":");

                    while (tokens[pos].type != T_RBRACE) {
                        if (tokens[pos].type == T_BREAK) {
                            pos++; // consume 'break'
                            expect(T_SEMICOLON);
                            break;
                        }
                        parseStatement();
                    }
                }
                else {
                    throw CompilerError(
                        "Expected 'case' or 'default' in switch statement",
                        tokens[pos].lineNumber
                    );
                }
            }

            icg.addInstruction(endLabel + ":");
            expect(T_RBRACE);
        } catch (const CompilerError& e) {
            throw;
        }
    }

    void parseForLoop() {
        try {
            expect(T_FOR);
            expect(T_LPAREN);

            // Parse initialization
            parseStatement();

            // Parse condition
            string condition = parseExpression();
            expect(T_SEMICOLON);

            // Parse increment expression
            string incrementCode;
            if (tokens[pos].type == T_ID || tokens[pos].type == T_INCREMENT || tokens[pos].type == T_DECREMENT) {
                string varName = tokens[pos].value;
                pos++; // Consume the identifier or operator

                // Handle ++, --, and compound increments
                if (tokens[pos].type == T_INCREMENT) {
                    incrementCode = varName + " = " + varName + " + 1";
                    pos++; // Consume '++'
                } else if (tokens[pos].type == T_DECREMENT) {
                    incrementCode = varName + " = " + varName + " - 1";
                    pos++; // Consume '--'
                } else if (tokens[pos].type == T_ASSIGN) {
                    pos++; // Consume '='
                    string expression = parseExpression();
                    incrementCode = varName + " = " + expression;
                }
            }

            expect(T_RPAREN);

            // Generate unique labels for the loop
            string loopStart = icg.getUniqueLabel("For_loop_start");
            string loopEnd = icg.getUniqueLabel("For_loop_end");

            // Generate intermediate code
            icg.addInstruction(loopStart + ":");
            icg.addInstruction("if " + condition + " == 0 goto " + loopEnd);

            // Parse loop body
            parseStatement();

            // Add increment at the end of the loop body, if any
            if (!incrementCode.empty()) {
                icg.addInstruction(incrementCode);
            }

            icg.addInstruction("goto " + loopStart);

            // End label for the loop
            icg.addInstruction(loopEnd + ":");
        } catch (const CompilerError& e) {
            throw;
        } catch (const exception& e) {
            throw CompilerError(e.what(), tokens[pos].lineNumber);
        }
    }

    // method to parse increment/decrement statements
    void parseIncrementStatement() {
        string varName = tokens[pos - 1].value;
        
        if (tokens[pos].type == T_INCREMENT) {
            pos++; // Consume the '++'
            if (!symTable.isDeclared(varName)) {
                throw CompilerError(
                    "Variable '" + varName + "' is not declared",
                    tokens[pos-1].lineNumber
                );
            }
            icg.addInstruction(varName + " = " + varName + " + 1");
            expect(T_SEMICOLON);
        } 
        else if (tokens[pos].type == T_DECREMENT) {
            pos++;
            if (!symTable.isDeclared(varName)) {
                throw CompilerError(
                    "Variable '" + varName + "' is not declared",
                    tokens[pos-1].lineNumber
                );
            }
            icg.addInstruction(varName + " = " + varName + " - 1");
            expect(T_SEMICOLON);
        }
        else {
            // If it's not an increment/decrement, it must be an assignment
            pos = pos - 1;
            parseAssignment();
        }
    }

    // Parser: Handle variable declarations and assignments
    void parseDeclaration() {
        if (tokens[pos].type == T_INT || tokens[pos].type == T_FLOAT || tokens[pos].type == T_DOUBLE || 
            tokens[pos].type == T_STRING || tokens[pos].type == T_BOOL || tokens[pos].type == T_CHAR) {
            string type = getTokenTypeName(tokens[pos].type);
            pos++; 
            // Expect identifier after type
            if (tokens[pos].type != T_ID) {
                throw CompilerError("Expected identifier after data type", tokens[pos].lineNumber);
            }
            string varName = tokens[pos].value;
            pos++;
            symTable.declareVariable(varName, type);
            if (tokens[pos].type == T_ASSIGN) {
                pos++; 
                string expr = parseExpression(); // Parse the right-hand side
                icg.addInstruction(varName + " = " + expr); // Generate intermediate code
            }
            // Ensure semicolon is present
            if (tokens[pos].type != T_SEMICOLON) {
                throw CompilerError("Expected semicolon after declaration", tokens[pos].lineNumber);
            }
            pos++; 
        }
        else
        {
            throw CompilerError("Invalid variable declaration", tokens[pos].lineNumber);
        }
    }


    void parseAssignment() {
    try {
        string varName = expectAndReturnValue(T_ID);
        
        if (!symTable.isDeclared(varName)) {
            throw CompilerError(
                "Variable '" + varName + "' is not declared",
                tokens[pos-1].lineNumber
            );
        }
        string varType = symTable.getVariableType(varName);        
        expect(T_ASSIGN);
        size_t exprPos = pos;
        string temp = icg.newTemp();
        if (tokens[pos].type == T_FUNCTION_CALL) {
            // Get the function name
            pos++;
            string functionName = tokens[pos].value;
            if (tokens[pos].type != T_ID) {
                throw CompilerError("Expected function name after 'call'", tokens[pos].lineNumber);
            }
            pos--;
            // Check if function exists and get its return type
            if (!symTable.isFunctionDefined(functionName)) {
                throw CompilerError("Function '" + functionName + "' is not declared", tokens[pos].lineNumber);
            }
            // Get the function's return type
            string returnType = symTable.getFunctionReturnType(functionName);
            cout << "Function return type: " << returnType << ", Variable type: " << varType << endl;
            // Check if the return type matches the variable type
            if (returnType != varType) {
                throw CompilerError("Type mismatch: Cannot assign return type '" + returnType + 
                                     "' to variable type '" + varType + "'", tokens[exprPos].lineNumber);
            }
            // Generate intermediate code for the function call
            parseFunctionCall(false);
        }
        else {
            string expr = parseExpression();
            TokenType exprType = tokens[exprPos].type;
            bool isNumeric = isNumericExpression(expr) || exprType == T_INT || exprType == T_FLOAT || 
                             exprType == T_DOUBLE || exprType == T_NUM;

            // Handle the numeric assignment logic based on the variable type (as before)
            if (varType == "double") {
                if (isNumeric) {
                    icg.addInstruction(varName + " = " + expr);
                    expect(T_SEMICOLON);
                    return;
                }
                throw CompilerError(
                    "Type mismatch: Cannot assign non-numeric value to double variable '" + varName + "'",
                    tokens[exprPos].lineNumber
                );
            }
            else if (varType == "int") {
                if (isNumeric) {
                    icg.addInstruction(varName + " = " + expr);
                    expect(T_SEMICOLON);
                    return;
                }
                throw CompilerError(
                    "Type mismatch: Cannot assign non-numeric value to int variable '" + varName + "'",
                    tokens[exprPos].lineNumber
                );
            }
            else if (varType == "float") {
                if (exprType != T_FLOAT && exprType != T_INT && exprType != T_NUM && exprType != T_DOUBLE) {
                    throw CompilerError(
                        "Type mismatch: Cannot assign non-float value to float variable '" + varName + "'",
                        tokens[exprPos].lineNumber
                    );
                }
            }
            else if (varType == "string") {
                if (expr[0] != '"' && exprType != T_STRING) {
                    expr = "\"" + expr + "\"";  // Convert to string literal
                }
            }
            else if (varType == "char") {
                if (expr[0] != '\'' && exprType != T_CHAR) {
                    expr = "\'" + expr + "\'";  // Convert to char literal
                }
            }

            icg.addInstruction(varName + " = " + expr);
        }

        expect(T_SEMICOLON);
    } catch (const CompilerError& e) {
        throw;
    }
}


    bool isNumericExpression(const string& expr) {        
        if (expr[0] == 't' && isdigit(expr[1]))
        {
            return true;
        }
        bool hasDecimalPoint = false;
        for (char c : expr) {
            if (c == '.') {
                if (hasDecimalPoint) return false;
                hasDecimalPoint = true;
            } else if (!isdigit(c) && c != '+' && c != '-' && c != '*' && 
                    c != '/' && c != ' ' && c != '(' && c != ')') {
                return false;
            }
        }
        return true;
    }


    void parseWhileStatement() {
        try {
            expect(T_WHILE);
            expect(T_LPAREN);
            // Parse condition
            string condition = parseExpression();
            expect(T_RPAREN);
            // Generate unique labels for loop
            string loopStart = icg.getUniqueLabel("While_loop_start");
            string loopEnd = icg.getUniqueLabel("While_loop_end");
            // Generate intermediate code
            icg.addInstruction(loopStart + ":");
            icg.addInstruction("if " + condition + " == 0 goto " + loopEnd);
            // Parse loop body
            parseStatement();
            // Jump back to the start of the loop
            icg.addInstruction("goto " + loopStart);
            // End label for the loop
            icg.addInstruction(loopEnd + ":");
        } catch (const CompilerError& e) {
            throw;
        } catch (const exception& e) {
            throw CompilerError(e.what(), tokens[pos].lineNumber);
        }
    }


    void parseIfStatement() {
        try {
            expect(T_IF);
            expect(T_LPAREN);
            string cond = parseExpression();
            expect(T_RPAREN);
            string ifTrueLabel = icg.getUniqueLabel("If_true");
            string ifFalseLabel = icg.getUniqueLabel("If_false");
            string endLabel = icg.getUniqueLabel("If_end");
            icg.addInstruction("if " + cond + " goto " + ifTrueLabel);
            icg.addInstruction("goto " + ifFalseLabel);
            icg.addInstruction(ifTrueLabel + ":");
            parseStatement();

            if (tokens[pos].type == T_ELSE) {
                icg.addInstruction("goto " + endLabel);
                icg.addInstruction(ifFalseLabel + ":");
                expect(T_ELSE);
                parseStatement();
                icg.addInstruction(endLabel + ":");
            } else {
                icg.addInstruction(ifFalseLabel + ":");
            }
        } catch (const CompilerError& e) {
            throw;
        } catch (const exception& e) {
            throw CompilerError(e.what(), tokens[pos].lineNumber);
        }
    }


    void parseReturnStatement() {
        try {
            expect(T_RETURN);
            string expr = parseExpression();
            icg.addInstruction("return " + expr);
            expect(T_SEMICOLON);
        } catch (const CompilerError& e) {
            throw;
        } catch (const exception& e) {
            throw CompilerError(e.what(), tokens[pos].lineNumber);
        }
    }

    void parseBlock() {
        try {
            expect(T_LBRACE);
            while (tokens[pos].type != T_RBRACE && tokens[pos].type != T_EOF) {
                parseStatement();
            }
            expect(T_RBRACE);
        } catch (const CompilerError& e) {
            throw;
        } catch (const exception& e) {
            throw CompilerError(e.what(), tokens[pos].lineNumber);
        }
    }

    string parseExpression() {
        try {
            string term = parseTerm();
            // Handle addition and subtraction
            while (tokens[pos].type == T_PLUS || tokens[pos].type == T_MINUS) {
                TokenType op = tokens[pos++].type;
                string nextTerm = parseTerm();
                string temp = icg.newTemp();
                icg.addInstruction(temp + " = " + term + (op == T_PLUS ? " + " : " - ") + nextTerm);
                term = temp;
            }
            // Handle > and <
            if (tokens[pos].type == T_GT) {
                pos++;
                string nextExpr = parseExpression();
                string temp = icg.newTemp();
                icg.addInstruction(temp + " = " + term + " > " + nextExpr);
                term = temp;
            }
            else if(tokens[pos].type == T_LT){
                pos++;
                string nextExpr = parseExpression();
                string temp = icg.newTemp();
                icg.addInstruction(temp + " = " + term + " < " + nextExpr);
                term = temp;
            }
             // Handle == and != operators
           else if (tokens[pos].type == T_EQ || tokens[pos].type == T_NE) {
                TokenType equalityOp = tokens[pos++].type;
                string nextExpr = parseExpression();
                string temp = icg.newTemp();
                string opSymbol = (equalityOp == T_EQ) ? " == " : " != ";
                icg.addInstruction(temp + " = " + term + opSymbol + nextExpr);
                term = temp;
            }
            // Handle && and || operators
           while (tokens[pos].type == T_AND || tokens[pos].type == T_OR) {
                TokenType logicalOp = tokens[pos++].type;
                string nextExpr = parseExpression();
                string temp = icg.newTemp();
                if (logicalOp == T_AND) {
                    string falseLabel = icg.getUniqueLabel("false_case");
                    string endLabel = icg.getUniqueLabel("end_and");
                    icg.addInstruction("if " + term + " == 0 goto " + falseLabel);
                    icg.addInstruction("if " + nextExpr + " == 0 goto " + falseLabel);
                    icg.addInstruction(temp + " = 1");
                    icg.addInstruction("goto " + endLabel);
                    icg.addInstruction(falseLabel+":");
                    icg.addInstruction(temp + " = 0");
                    icg.addInstruction(endLabel+":");
                }
                else {
                    icg.addInstruction("if " + term + " != 0 jump to setTrue");
                    icg.addInstruction("if " + nextExpr + " != 0 jump to setTrue");
                    icg.addInstruction(temp + " = 0");
                    icg.addInstruction("jump to end");
                    icg.addInstruction("setTrue: " + temp + " = 1");
                    icg.addInstruction("end:");
                }
                term = temp;
            }
            return term;
        } catch (const CompilerError& e) {
            throw;
        } catch (const exception& e) {
            throw CompilerError(e.what(), tokens[pos].lineNumber);
        }
    }

    string parseTerm() {
        try {
            string factor = parseFactor();
            while (tokens[pos].type == T_MUL || tokens[pos].type == T_DIV) {
                TokenType op = tokens[pos++].type;
                string nextFactor = parseFactor();
                
                // Handle numeric literal conversion
                if (nextFactor.find('.') != string::npos) {
                    if (nextFactor.back() == 'f') {
                        nextFactor = nextFactor.substr(0, nextFactor.length() - 1);
                    }
                }
                
                string temp = icg.newTemp();
                icg.addInstruction(temp + " = " + factor + (op == T_MUL ? " * " : " / ") + nextFactor);
                factor = temp;
            }
            return factor;
        } catch (const CompilerError& e) {
            throw;
        }
    }

    string parseFactor() {
        if (pos >= tokens.size()) {
            throw CompilerError(
                "Unexpected end of input while parsing expression",
                tokens[pos-1].lineNumber
            );
        }
        // Check for function call
        if (tokens[pos].type == T_FUNCTION_CALL) {
            pos++;
            string functionName = tokens[pos].value;
            // Check if the function is declared
            if (!symTable.isFunctionDefined(functionName)) {
                throw CompilerError("Function '" + functionName + "' is not declared", tokens[pos].lineNumber);
            }
            // Expect the function arguments
            pos++;
            expect(T_LPAREN);
            vector<string> arguments;
            while (tokens[pos].type != T_RPAREN) {
                string argument = parseExpression();
                arguments.push_back(argument);
                if (tokens[pos].type == T_COMMA) {
                    pos++;
                } else if (tokens[pos].type != T_RPAREN) {
                    throw CompilerError("Expected ',' or ')' in function call", tokens[pos].lineNumber);
                }
            }
            expect(T_RPAREN);
            // Check if the number of arguments matches the function's parameters
            vector<string> paramTypes = symTable.getFunctionParameterTypes(functionName);
            if (arguments.size() != paramTypes.size()) {
                throw CompilerError("Function '" + functionName + "' expects " + 
                                    to_string(paramTypes.size()) + " argument(s), but got " + 
                                    to_string(arguments.size()), tokens[pos].lineNumber);
            }
            // Generate intermediate code for the call
            for (const string &arg : arguments) {
                icg.addInstruction("param " + arg);
            }
            icg.addInstruction("call " + functionName);
            return functionName;
        }
        // Check for variable
        if (tokens[pos].type == T_ID) {
            string varName = tokens[pos].value;
            pos++;  
            // Handle ++ and --
            if (pos < tokens.size()) {
                if (tokens[pos].type == T_INCREMENT) {
                    pos++;
                    string temp = icg.newTemp();
                    icg.addInstruction(temp + " = " + varName);
                    icg.addInstruction(varName + " = " + varName + " + 1");
                    return temp;
                }
                else if (tokens[pos].type == T_DECREMENT) {
                    pos++;
                    string temp = icg.newTemp();
                    icg.addInstruction(temp + " = " + varName);
                    icg.addInstruction(varName + " = " + varName + " - 1");
                    return temp;
                }
            }
            
            if (!symTable.isDeclared(varName)) {
                throw CompilerError(
                    "Variable '" + varName + "' is not declared",
                    tokens[pos-1].lineNumber
                );
            }
            return varName;
        }

        // number handling to include all numeric types
        if (tokens[pos].type == T_INT || tokens[pos].type == T_FLOAT || 
            tokens[pos].type == T_DOUBLE || tokens[pos].type == T_NUM || tokens[pos].type == T_BOOL) {
            return tokens[pos++].value;
        } 
        else if (tokens[pos].type == T_STRING) {
            return tokens[pos++].value;
        }
        else if (tokens[pos].type == T_CHAR) {
            return tokens[pos++].value;
        }
        else if (tokens[pos].type == T_ID) {
            string varName = tokens[pos++].value;
            if (!symTable.isDeclared(varName)) {
                throw CompilerError(
                    "Variable '" + varName + "' is not declared",
                    tokens[pos-1].lineNumber
                );
            }
            return varName;
        } 
        else if (tokens[pos].type == T_LPAREN) {
            expect(T_LPAREN);
            string expr = parseExpression();
            expect(T_RPAREN);
            return expr;
        }
        // Handle unary operators
        else if (tokens[pos].type == T_MINUS || tokens[pos].type == T_NOT) {
            TokenType unaryOp = tokens[pos++].type;
            string factor = parseFactor();
            string temp = icg.newTemp();
            string opSymbol = (unaryOp == T_MINUS) ? "-" : "!";
            icg.addInstruction(temp + " = " + opSymbol + factor);
            return temp;
        } 
        else {
            throw CompilerError(
                "Expected number, identifier, string literal, or '(' but found '" + 
                tokens[pos].value + "'",
                tokens[pos].lineNumber
            );
        }
    }

public:
    Parser(const vector<Token>& tokens, SymbolTable& symTable, IntermediateCodeGnerator& icg)
        : tokens(tokens), pos(0), symTable(symTable), icg(icg) {}

    void parseProgram() {
        try {
            while (tokens[pos].type != T_EOF) {
                parseStatement();
            }
        } catch (const CompilerError& e) {
            throw;
        } catch (const exception& e) {
            throw CompilerError(e.what(), tokens[pos].lineNumber);
        }
    }
};

class AssemblyCodeGenerator {
private:
    vector<string> intermediateCode;
    vector<string> assemblyCode;
    unordered_set<string> declaredVars;
    int labelCounter;
    SymbolTable& symTable;
    unordered_map<string, string> stringLiterals;  // Track string literals
    unordered_map<string, string> floatLiterals;   // Track float literals

    // Helper function to generate unique labels
    string generateLabel(const string& base) {
        return base + to_string(labelCounter++);
    }

    bool isFloatingPointType(const string& type) {
        return type == "float" || type == "double";
    }

void generateDataSection() {
        assemblyCode.push_back("section .data");
        assemblyCode.push_back("TRUE equ 1");
        assemblyCode.push_back("FALSE equ 0");
        
        // First pass: Handle string literals
        for (const auto& line : intermediateCode) {
            cout<<line<<endl;
            if (line.find('"') != string::npos) {
                size_t start = line.find('"');
                size_t end = line.find('"', start + 1);
                if (end != string::npos) {
                    string varName = line.substr(0, line.find('=')); 
                    varName = trim(varName);
                    string strValue = line.substr(start, end - start + 1);
                    // Generate unique label for string literal
                    string strLabel = generateLabel("str_" + varName);
                    stringLiterals[varName] = strLabel;
                    assemblyCode.push_back(strLabel + " db " + strValue + ", 0");
                }
            }
        }

        // Second pass: Handle numeric literals and variable declarations
        for (const auto& line : intermediateCode) {
            stringstream ss(line);
            string varName, equals, value;
            ss >> varName;
            
            if (varName.find('=') != string::npos) {
                varName = varName.substr(0, varName.find('='));
                varName = trim(varName);
                
                // Get the value after '='
                size_t equalsPos = line.find('=');
                value = trim(line.substr(equalsPos + 1));

                if (symTable.isDeclared(varName) && !isDeclared(varName)) {
                    string type = symTable.getVariableType(varName);
                    declareVariableWithValue(varName, type, value);
                }
            }
        }

        // Third pass: Handle temporary variables in .bss section
        assemblyCode.push_back("\nsection .bss");
        for (const auto& line : intermediateCode) {
            stringstream ss(line);
            string token;
            ss >> token;
            
            if (token.find("t") == 0 && !isDeclared(token)) {
                assemblyCode.push_back(token + " resd 1");
                declaredVars.insert(token);
            }
        }
    }
    string trim(const string& str) {
        size_t first = str.find_first_not_of(" \t");
        size_t last = str.find_last_not_of(" \t");
        return (first != string::npos) ? str.substr(first, last - first + 1) : "";
    }

    bool isDeclared(const string& name) {
        return declaredVars.find(name) != declaredVars.end();
    }

    void declareVariableWithValue(const string& name, const string& type, const string& value) {
        if (type == "int") {
            assemblyCode.push_back(name + " dd " + value);
        } 
        else if (type == "float" || type == "double") {
            // Handle floating point numbers
            string floatValue = value;
            if (floatValue.back() == 'f') {
                floatValue = floatValue.substr(0, floatValue.length() - 1);
            }
            assemblyCode.push_back(name + (type == "float" ? " dd " : " dq ") + floatValue);
        }
        else if (type == "char") {
            // Handle character literal 'A'
            if (value[0] == '\'') {
                assemblyCode.push_back(name + " db " + to_string(static_cast<int>(value[1])));
            } else {
                assemblyCode.push_back(name + " db " + value);
            }
        }
        else if (type == "bool") {
            // Handle boolean values
            string boolValue = (value == "true" || value == "1") ? "TRUE" : "FALSE";
            assemblyCode.push_back(name + " db " + boolValue);
        }
        else if (type == "string") {
            // Use the stored string literal label
            if (stringLiterals.find(name) == stringLiterals.end()) {
                assemblyCode.push_back(name + " times 256 db 0");
            }
        }
        declaredVars.insert(name);
    }

     // Helper method to declare variables in assembly
    void declareVariable(const string& name, const string& type) {
        if (type == "int") {
            assemblyCode.push_back(name + " dd 0");
        } else if (type == "float") {
            assemblyCode.push_back(name + " dd 0.0");
        } else if (type == "double") {
            assemblyCode.push_back(name + " dq 0.0");
        } else if (type == "char") {
            assemblyCode.push_back(name + " db 0");
        } else if (type == "bool") {
            assemblyCode.push_back(name + " db FALSE");
        } else if (type == "string") {
            assemblyCode.push_back(name + " times 256 db 0");
        }
        declaredVars.insert(name);
    }

    void generateTextSection() {
        assemblyCode.push_back("\nsection .text");
        assemblyCode.push_back("global _start");
        assemblyCode.push_back("_start:");
        
        // Add helper procedures for string operations
        generateStringHelpers();
    }

    void generateStringHelpers() {
        // Add string copy procedure
        assemblyCode.push_back("\n; String copy helper procedure");
        assemblyCode.push_back("string_copy:");
        assemblyCode.push_back("    push ecx");
        assemblyCode.push_back("    push esi");
        assemblyCode.push_back("    push edi");
        assemblyCode.push_back("    rep movsb");
        assemblyCode.push_back("    pop edi");
        assemblyCode.push_back("    pop esi");
        assemblyCode.push_back("    pop ecx");
        assemblyCode.push_back("    ret");
    }

    void handleCondition(const string& condition, const string& gotoLabel) {
        size_t opPos = condition.find('>');
        if (opPos == string::npos) return;

        string lhs = condition.substr(0, opPos);
        string rhs = condition.substr(opPos + 1);
        
        string tempResult = generateLabel("_temp_cmp");
        assemblyCode.insert(assemblyCode.begin() + 1, tempResult + " db 0");
        
        if (symTable.isDeclared(lhs) && isFloatingPointType(symTable.getVariableType(lhs))) {
            assemblyCode.push_back("; Floating point comparison");
            assemblyCode.push_back("fld dword [" + rhs + "]");
            assemblyCode.push_back("fld dword [" + lhs + "]");
            assemblyCode.push_back("fcompp");
            assemblyCode.push_back("fstsw ax");
            assemblyCode.push_back("sahf");
            assemblyCode.push_back("mov byte [" + tempResult + "], 0");
            assemblyCode.push_back("ja " + gotoLabel);
        } else {
            assemblyCode.push_back("; Integer comparison");
            assemblyCode.push_back("mov eax, [" + lhs + "]");
            if (rhs.find_first_not_of("0123456789") == string::npos) {
                assemblyCode.push_back("cmp eax, " + rhs);
            } else {
                assemblyCode.push_back("cmp eax, [" + rhs + "]");
            }
            assemblyCode.push_back("mov byte [" + tempResult + "], 0");
            assemblyCode.push_back("jg " + gotoLabel);
        }
    }

    void handleAssignment(const string& lhs, string rhs) {
        rhs.erase(0, rhs.find_first_not_of(" \t"));
        rhs.erase(rhs.find_last_not_of(" \t") + 1);
        
        string lhsType = symTable.isDeclared(lhs) ? symTable.getVariableType(lhs) : "int";
        
        assemblyCode.push_back("; Assignment: " + lhs + " = " + rhs);
        
        if (lhsType == "bool") {
            handleBooleanAssignment(lhs, rhs);
        } else if (lhsType == "string") {
            handleStringAssignment(lhs, rhs);
        } else if (lhsType == "float" || lhsType == "double") {
            handleFloatAssignment(lhs, rhs);
        } else if (lhsType == "char") {
            handleCharAssignment(lhs, rhs);
        } else {
            handleIntegerAssignment(lhs, rhs);
        }
    }

    void handleBooleanAssignment(const string& lhs, const string& rhs) {
        if (rhs == "true" || rhs == "TRUE") {
            assemblyCode.push_back("mov byte [" + lhs + "], TRUE");
        } else if (rhs == "false" || rhs == "FALSE") {
            assemblyCode.push_back("mov byte [" + lhs + "], FALSE");
        } else {
            assemblyCode.push_back("mov al, [" + rhs + "]");
            assemblyCode.push_back("mov [" + lhs + "], al");
        }
    }

    void handleStringAssignment(const string& lhs, const string& rhs) {
        if (rhs[0] == '"') {
            // String literal
            string strLabel = generateLabel("_str");
            string cleanStr = rhs.substr(1, rhs.length() - 2);  // Remove quotes
            stringLiterals[strLabel] = cleanStr;
            
            assemblyCode.insert(assemblyCode.begin() + 1, 
                strLabel + " db " + rhs + ", 0");
            
            assemblyCode.push_back("mov esi, " + strLabel);
            assemblyCode.push_back("mov edi, " + lhs);
            assemblyCode.push_back("mov ecx, " + to_string(cleanStr.length() + 1));
            assemblyCode.push_back("call string_copy");
        } else {
            assemblyCode.push_back("mov esi, " + rhs);
            assemblyCode.push_back("mov edi, " + lhs);
            assemblyCode.push_back("mov ecx, 256");
            assemblyCode.push_back("call string_copy");
        }
    }

    void handleFloatAssignment(const string& lhs, const string& rhs) {
        if (rhs.find('.') != string::npos) {
            string floatLabel = generateLabel("_float");
            floatLiterals[floatLabel] = rhs;
            
            assemblyCode.insert(assemblyCode.begin() + 1, 
                floatLabel + " dd " + rhs);
            
            assemblyCode.push_back("fld dword [" + floatLabel + "]");
            assemblyCode.push_back("fstp dword [" + lhs + "]");
        } else {
            assemblyCode.push_back("fld dword [" + rhs + "]");
            assemblyCode.push_back("fstp dword [" + lhs + "]");
        }
    }

    void handleCharAssignment(const string& lhs, const string& rhs) {
        if (rhs[0] == '\'') {
            assemblyCode.push_back("mov byte [" + lhs + "], " + 
                to_string(static_cast<int>(rhs[1])));
        } else {
            assemblyCode.push_back("mov al, [" + rhs + "]");
            assemblyCode.push_back("mov [" + lhs + "], al");
        }
    }

    void handleIntegerAssignment(const string& lhs, const string& rhs) {
        if (isdigit(rhs[0])) {
            assemblyCode.push_back("mov dword [" + lhs + "], " + rhs);
        } else {
            assemblyCode.push_back("mov eax, [" + rhs + "]");
            assemblyCode.push_back("mov [" + lhs + "], eax");
        }
    }

    void handleReturn(const string& var) {
        assemblyCode.push_back("; Return statement");
        if (var == "true" || var == "TRUE") {
            assemblyCode.push_back("mov eax, TRUE");
        } else if (var == "false" || var == "FALSE") {
            assemblyCode.push_back("mov eax, FALSE");
        } else if (!symTable.isDeclared(var)) {
            assemblyCode.push_back("mov eax, " + var);
        } else {
            string varType = symTable.getVariableType(var);
            if (varType == "bool" || varType == "char") {
                assemblyCode.push_back("movzx eax, byte [" + var + "]");
            } else if (varType == "float" || varType == "double") {
                assemblyCode.push_back("fld dword [" + var + "]");
                assemblyCode.push_back("fstp dword [esp-4]");
                assemblyCode.push_back("mov eax, [esp-4]");
            } else {
                assemblyCode.push_back("mov eax, [" + var + "]");
            }
        }
    }

public:
    AssemblyCodeGenerator(const vector<string>& ic, SymbolTable& st) 
        : intermediateCode(ic), symTable(st), labelCounter(0) {}

    void generateAssembly() {
        generateDataSection();
        generateTextSection();

        for (size_t i = 0; i < intermediateCode.size(); ++i) {
            const string& line = intermediateCode[i];
            stringstream ss(line);
            string token;
            ss >> token;

            assemblyCode.push_back("\n; Processing: " + line);

            if (token == "return") {
                string var;
                ss >> var;
                handleReturn(var);
            } else if (token == "if") {
                string condition, dummy, gotoLabel;
                ss >> condition >> dummy >> gotoLabel;
                handleCondition(condition, gotoLabel);
            } else if (token == "goto") {
                string label;
                ss >> label;
                assemblyCode.push_back("jmp " + label);
            } else if (line.back() == ':') {
                // Label definition
                assemblyCode.push_back("\n" + line);
            } else if (token.find('=') != string::npos) {
                // Assignment
                size_t eqPos = line.find('=');
                string lhs = line.substr(0, eqPos);
                string rhs = line.substr(eqPos + 1);
                lhs.erase(lhs.find_last_not_of(" \t") + 1);
                handleAssignment(lhs, rhs);
            }
        }

        // Program exit
        assemblyCode.push_back("\n; Program exit");
        assemblyCode.push_back("mov ebx, eax  ; Save return value");
        assemblyCode.push_back("mov eax, 1    ; sys_exit syscall");
        assemblyCode.push_back("int 0x80      ; Call kernel");
    }

    const vector<string>& getAssembly() const {
        return assemblyCode;
    }

    void printAssembly() {
        for (const string& line : assemblyCode) {
            cout << line << endl;
        }
    }
};
int main(int argc, char* argv[]) {
    if (argc < 2) {
        cerr << "Usage: " << argv[0] << " <source_file>" << endl;
        return 1;
    }

    try {
        string filename = argv[1];
        ifstream file(filename);
        if (!file.is_open()) {
            throw runtime_error("Unable to open file '" + filename + "'");
        }

        stringstream buffer;
        buffer << file.rdbuf();
        string src = buffer.str();
        file.close();

        Lexer lexer(src);
        vector<Token> tokens = lexer.tokenize();
        SymbolTable symTable;
        IntermediateCodeGnerator icg;
        Parser parser(tokens, symTable, icg);

        parser.parseProgram();
        cout << "\nIntermediate Code Generated:" << endl;
        icg.printInstructions();

        // Assembly code
        AssemblyCodeGenerator acg(icg.getInstructions(), symTable);
        acg.generateAssembly();
        cout << "\nGenerated Assembly Code:\n";
        acg.printAssembly();
        
    } catch (const CompilerError& e) {
        cerr << "\033[1;31m" << e.what() << "\033[0m" << endl;
        return 1;
    } catch (const exception& e) {
        cerr << "\033[1;31mError: " << e.what() << "\033[0m" << endl;
        return 1;
    }

    return 0;
}