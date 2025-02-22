#include<iostream>
#include<format>
#include<string>
#include<map>
#include<vector>
#include<cstring>
#include<fstream>
#include<sstream>
using namespace std;
typedef unsigned int u32;

#define DEBUG_MODE 1

enum Opcode_Name
{
    OP_RETURN,
    OP_CONSTANT,
    OP_NEG,
    OP_ADD, OP_SUB, OP_MUL, OP_DIV,
    OP_NOT,
    OP_LESS, OP_GREATER, OP_EQUAL,
    OP_TRUE, OP_FALSE, OP_NIL,
    OP_PRINT,
    OP_POP,
    OP_DEFINE_GLOBAL,
    OP_GET_GLOBAL,
    OP_SET_GLOBAL,
    OP_GET_LOCAL,
    OP_SET_LOCAL,
    OP_GOTO_IF_FALSE,
    OP_GOTO_IF_TRUE,
    OP_GOTO,
    OP_CALL,
    OP_STORE_TO_RET,
    OP_PUSH_FROME_RET,
    OP_END,
};

enum Token_Type
{
    TOKEN_LEFT_PAREN, TOKEN_RIGHT_PAREN, TOKEN_LEFT_BRACE, TOKEN_RIGHT_BRACE,
    TOKEN_PLUS, TOKEN_MINUS, TOKEN_STAR, TOKEN_SLASH,
    TOKEN_SEMICOLON, TOKEN_COMMA, TOKEN_DOT,
    TOKEN_EQUAL, TOKEN_EQUAL_EQUAL, TOKEN_BANG_EQUAL, TOKEN_LESS, TOKEN_LESS_EQUAL, TOKEN_GREATER, TOKEN_GREATER_EQUAL,
    TOKEN_BANG, TOKEN_VERT_BAR_VERT_BAR, TOKEN_AMPERSAN_AMPERSAN,

    TOKEN_TRUE, TOKEN_FALSE, TOKEN_NIL,
    TOKEN_RETURN, TOKEN_PRINT,
    TOKEN_FOR, TOKEN_WHILE, TOKEN_FUNC, TOKEN_VAR, TOKEN_IF, TOKEN_ELSE,
    TOKEN_NUMBER, TOKEN_STRING, TOKEN_IDENTIFIER,

    TOKEN_EOF, TOKEN_ERROR,
};

map<u32, string> opcode_name_to_string =
{
    {OP_RETURN, "RETURN"},
    {OP_CONSTANT, "CONSTANT"},
    {OP_NEG, "NEG"},
    {OP_ADD, "ADD"},
    {OP_SUB, "SUB"},
    {OP_MUL, "MUL"},
    {OP_DIV, "DIV"},
    {OP_NOT, "NOT"},
    {OP_LESS, "LESS"},
    {OP_GREATER, "GREATER"},
    {OP_EQUAL, "EQUAL"},
    {OP_TRUE, "TRUE"},
    {OP_FALSE, "FALSE"},
    {OP_NIL, "NIL"},
    {OP_PRINT, "PRINT"},
    {OP_POP, "POP"},
    {OP_DEFINE_GLOBAL, "DEFINE_GLOBAL"},
    {OP_GET_GLOBAL, "GET_GLOBAL"},
    {OP_SET_GLOBAL, "SET_GLOBAL"},
    {OP_GET_LOCAL, "GET_LOCAL"},
    {OP_SET_LOCAL, "SET_LOCAL"},
    {OP_GOTO_IF_FALSE, "GOTO_IF_FALSE"},
    {OP_GOTO_IF_TRUE, "GOTO_IF_TRUE"},
    {OP_GOTO, "GOTO"},
    {OP_CALL, "CALL"},
    {OP_END, "END"},
    {OP_STORE_TO_RET, "OP_STORE_TO_RET"},
    {OP_PUSH_FROME_RET, "OP_PUSH_FROME_RET"},
};

map<u32, string> token_type_to_string = 
{
    {TOKEN_LEFT_PAREN, "LEFT_PAREN"},
    {TOKEN_RIGHT_PAREN, "LEFT_PAREN"},
    {TOKEN_LEFT_BRACE, "LEFT_BRACE"},
    {TOKEN_RIGHT_BRACE, "RIGHT_BRACE"},
    {TOKEN_PLUS, "PLUS"},
    {TOKEN_MINUS, "MINUS"},
    {TOKEN_STAR, "STAR"},
    {TOKEN_SLASH, "SLASH"},
    {TOKEN_SEMICOLON, "SEMICOLON"},
    {TOKEN_COMMA, "COMMA"},
    {TOKEN_DOT, "DOT"},
    {TOKEN_EQUAL, "EQUAL"},
    {TOKEN_EQUAL_EQUAL, "EQUAL_EQUAL"},
    {TOKEN_BANG_EQUAL, "BANG_EQUAL"},
    {TOKEN_LESS, "LESS"},
    {TOKEN_LESS_EQUAL, "LESS_EQUAL"},
    {TOKEN_GREATER, "GREATER"},
    {TOKEN_GREATER_EQUAL, "GREATER_EQUAL"},
    {TOKEN_BANG, "BANG"},
    {TOKEN_VERT_BAR_VERT_BAR, "VERT_BAR_VERT_BAR"},
    {TOKEN_AMPERSAN_AMPERSAN, "AMPERSAN_AMPERSAN"},
    {TOKEN_FOR, "FOR"},
    {TOKEN_WHILE, "WHILE"},
    {TOKEN_FUNC, "FUNC"},
    {TOKEN_VAR, "VAR"},
    {TOKEN_IF, "IF"},
    {TOKEN_ELSE, "ELSE"},
    {TOKEN_PRINT, "PRINT"},
    {TOKEN_TRUE, "TRUE"},
    {TOKEN_FALSE, "FALSE"},
    {TOKEN_NIL, "NIL"},
    {TOKEN_IDENTIFIER, "IDENTIFIER"},
    {TOKEN_NUMBER, "NUMBER"},
    {TOKEN_STRING, "STRING"},
    {TOKEN_EOF, "EOF"},
};

map<string, Token_Type> keywords =
{
    {"true", TOKEN_TRUE},
    {"false", TOKEN_FALSE},
    {"nil", TOKEN_NIL},
    {"for", TOKEN_FOR},
    {"while", TOKEN_WHILE},
    {"func", TOKEN_FUNC},
    {"var", TOKEN_VAR},
    {"if", TOKEN_IF},
    {"else", TOKEN_ELSE},
    {"print", TOKEN_PRINT},
    {"return", TOKEN_RETURN},
};

struct Opcode
{
    Opcode_Name name;
    int index;

    Opcode(Opcode_Name name, int index)
    {
        this->name = name;
        this->index = index;
    }

    Opcode(Opcode_Name name)
    {
        this->name = name;
        this->index = -1;
    }

    string to_string()
    {
        if (this->index == -1) {return format("{}", opcode_name_to_string[this->name]);}
        else {return format("{}({})", opcode_name_to_string[this->name], this->index);}
    }
};

enum Type {VAL_NUMBER, VAL_BOOL, VAL_NIL, VAL_ERROR};

struct Value
{
    Type type;
    union
    {
        double n;
        bool b;
    }as;

    Value() {this->type = VAL_ERROR;}

    Value(Type type) {this->type = type;}

    Value(Type type, auto val)
    {
        this->type = type;
        switch (this->type)
        {
            case VAL_NUMBER: this->as.n = val; break;
            case VAL_BOOL: this->as.b = val; break;
            default: break;
        }
    }
    
    string to_string()
    {
        switch (type)
        {
            case VAL_NUMBER: return format("NUMBER({})", this->as.n);
            case VAL_BOOL: return format("BOOL({})", this->as.b);
            case VAL_NIL: return format("NIL");
            default: break;
        }
        return format("UNDEFINED");
    }
};

struct Chunk
{
    vector<Opcode> codes;
    vector<Value> values;
    vector<u32> lines;
    map<string, int> globals;

    int get_codes_len() {return this->codes.size();}
    int get_values_len() {return this->values.size();}

    void add_opcode(Opcode opcode, u32 line)
    {
        this->codes.push_back(opcode);
        this->lines.push_back(line);
    }

    int add_value_to_values(Value value)
    {
        // adds value(immediate) to value array and returns its index
        this->values.push_back(value);
        return this->values.size()-1;
    }

    int add_global_to_globals(string name)
    {
        int index = add_value_to_values(Value(VAL_NIL));
        globals.insert({name, index});
        return index;
    }

    int add_constant(Value value, u32 line)
    {
        int index = this->add_value_to_values(value);
        this->add_opcode(Opcode(OP_CONSTANT, index), line);
        return index;
    }

    void print()
    {
        cout << "====Opcodes====\n";
        for (Opcode& opcode : this->codes) {cout << opcode.to_string() << "\n";}
        cout << "===Immediates===\n";
        for (Value& value : this->values) {cout << value.to_string() << "\n";}
    }
};



struct Token
{
    Token_Type type;
    string lexeme;
    Value literal;
    int line;

    Token()
    {
        type = TOKEN_ERROR;
    }

    Token(Token_Type type, string lexeme, Value literal, int line)
    {
        this->type = type;
        this->lexeme = lexeme;
        this->line = line;
        this->literal = literal;
    }
    string to_string() {return format("{} {} {} {}", token_type_to_string[type], lexeme, literal.to_string(), line);}
};

struct Tokenizer
{
    string source;
    int current;
    int start;
    int line;
    vector<Token> tokens; 

    Tokenizer(string source)
    {
        this->source = source;
        this->current = 0;
        this->start = 0;
        this->line = 0;
    }

    void scan_tokens()
    {
        while (!is_at_end())
        {
            start = current;
            scan_token();
        }

        push_token(TOKEN_EOF);
    }

    void print_tokens()
    {
        for (Token& token : tokens)
        {cout << token.to_string() << "\n";}
    }

    void scan_token()
    {
        char c = advance();
        switch (c)
        {
            case '(': push_token(TOKEN_LEFT_PAREN); break;
            case ')': push_token(TOKEN_RIGHT_PAREN); break;
            case '{': push_token(TOKEN_LEFT_BRACE); break;
            case '}': push_token(TOKEN_RIGHT_BRACE); break;
            case '+': push_token(TOKEN_PLUS); break;
            case '-': push_token(TOKEN_MINUS); break;
            case '*': push_token(TOKEN_STAR); break;
            case ';': push_token(TOKEN_SEMICOLON); break;
            case '.': push_token(TOKEN_DOT); break;
            case ',': push_token(TOKEN_COMMA); break;
            case ' ':
            case '\t':
            case '\r': break;
            case '\n': ++line; break;
            case '/':
                if (match('/'))
                {while (peek() != '\n' && !is_at_end()) {advance();}}
                else  {push_token(TOKEN_SLASH);} break;
            case '=': match('=') ? push_token(TOKEN_EQUAL_EQUAL):push_token(TOKEN_EQUAL);break;
            case '!': match('=') ? push_token(TOKEN_BANG_EQUAL):push_token(TOKEN_BANG);break;
            case '<': match('=') ? push_token(TOKEN_LESS_EQUAL):push_token(TOKEN_LESS);break;
            case '>': match('=') ? push_token(TOKEN_GREATER_EQUAL):push_token(TOKEN_GREATER);break;
            case '|': match('|') ? push_token(TOKEN_VERT_BAR_VERT_BAR):error("missing '|' after '|'");break;
            case '&': match('&') ? push_token(TOKEN_AMPERSAN_AMPERSAN):error("missing '&' after '&'");break;
            default:
                if (is_digit(c)) {number();}
                else if (is_alpha(c)) {identifier();}
                else {error(format("Unrecognizable token: '{}'", c));}
                break;
        }
    }

    char advance() {return source[current++];}
    bool is_at_end() {return current >= source.length();}
    char peek() 
    {
        if (this->is_at_end()) {return '\0';}
        return source[current];
    }
    char peek_next()
    {
        if (current+1 >= source.length()) {return '\0';}
        return source[current+1];
    }
    bool match(char expected)
    {
        if (this->peek() == expected) {advance(); return true;}
        return false;
    }
    void eat(char expected)
    {
        if (this->peek() == expected) {advance(); return;}
        error(format("Expected \'{}\', got \'{}\'", expected, this->peek()));
    }
    void error(string msg)
    {
        fprintf(stderr, "Lexer error at line[%d] -> [%s]\n", line, msg.c_str());
        exit(1);
    }
    void push_token(Token_Type type) {this->push_token(type, Value());}
    void push_token(Token_Type type, Value literal)
    {
        string lexeme = source.substr(start, current-start);
        Token t(type, lexeme, literal, line);
        tokens.push_back(t);
    }
    bool is_alpha(char c) {return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';}
    bool is_digit(char c) {return c >= '0' && c<= '9';}
    bool is_alphanumeric(char c) {return is_alpha(c) || is_digit(c);}

    void number()
    {
        while (is_digit(peek())) {advance();}
        if (peek() == '.' && is_digit(peek_next())) 
        {
            advance();
            while (isdigit(peek())) {advance();}
        }
        string lexeme = source.substr(start, current-start);
        Value literal(VAL_NUMBER, stod(lexeme));
        push_token(TOKEN_NUMBER, literal);
    }

    void identifier()
    {
        while (is_alphanumeric(peek())) {advance();}
        string lexeme = source.substr(start, current-start);
        if (keywords.find(lexeme) == keywords.end()) {push_token(TOKEN_IDENTIFIER); return;}
        push_token(keywords[lexeme]);
    }
};

enum Precedence 
{
    PREC_NONE,
    PREC_ASSIGNMENT,  // =
    PREC_OR,          // or
    PREC_AND,         // and
    PREC_EQUALITY,    // == !=
    PREC_COMPARISON,  // < > <= >=
    PREC_TERM,        // + -
    PREC_FACTOR,      // * /
    PREC_UNARY,       // ! -
    PREC_CALL,        // . ()
    PREC_PRIMARY
};

struct Parse_Rule
{
    const char* prefix;
    const char* infix;
    Precedence precedence;
};

struct Local
{
    Token name;
    int depth;

    Local() {}

    Local(Token name, int depth)
    {
        this->name = name;
        this->depth = depth;
    }
};

struct Compiler
{
    int previous;
    int current;
    vector<Token> tokens;
    Chunk* current_chunk;

    vector<Local> locals; // makeshift stack which tells you where each local variable is stored in runtime, during compiletime.
    int scope_depth;

    Parse_Rule rules[36];

    Compiler(vector<Token> tokens)
    {
        this->current = 0;
        this->tokens = tokens;
        this->current_chunk = NULL;
        this->scope_depth = 0;
        rules[TOKEN_LEFT_PAREN]        = {"grouping", NULL,   PREC_NONE};
        rules[TOKEN_RIGHT_PAREN]       = {NULL,     NULL,   PREC_NONE};
        rules[TOKEN_LEFT_BRACE]        = {NULL,     NULL,   PREC_NONE};
        rules[TOKEN_RIGHT_BRACE]       = {NULL,     NULL,   PREC_NONE};
        rules[TOKEN_COMMA]             = {NULL,     NULL,   PREC_NONE};
        rules[TOKEN_DOT]               = {NULL,     NULL,   PREC_NONE};
        rules[TOKEN_MINUS]             = {"unary",    "binary", PREC_TERM};
        rules[TOKEN_PLUS]              = {NULL,     "binary", PREC_TERM};
        rules[TOKEN_SEMICOLON]         = {NULL,     NULL,   PREC_NONE};
        rules[TOKEN_SLASH]             = {NULL,     "binary", PREC_FACTOR};
        rules[TOKEN_STAR]              = {NULL,     "binary", PREC_FACTOR};
        rules[TOKEN_BANG]              = {"unary",     NULL,   PREC_NONE};
        rules[TOKEN_BANG_EQUAL]        = {NULL,     "binary",   PREC_EQUALITY};
        rules[TOKEN_EQUAL]             = {NULL,     NULL,   PREC_NONE};
        rules[TOKEN_EQUAL_EQUAL]       = {NULL,     "binary",   PREC_EQUALITY};
        rules[TOKEN_GREATER]           = {NULL,     "binary",   PREC_COMPARISON};
        rules[TOKEN_GREATER_EQUAL]     = {NULL,     "binary",   PREC_COMPARISON};
        rules[TOKEN_LESS]              = {NULL,     "binary",   PREC_COMPARISON};
        rules[TOKEN_LESS_EQUAL]        = {NULL,     "binary",   PREC_COMPARISON};
        rules[TOKEN_IDENTIFIER]        = {"variable",     NULL,   PREC_NONE};
        rules[TOKEN_STRING]            = {"string",     NULL,  PREC_NONE};
        rules[TOKEN_NUMBER]            = {"number",   NULL,   PREC_NONE};
        rules[TOKEN_AMPERSAN_AMPERSAN] = {NULL,     "and",   PREC_AND};
        rules[TOKEN_ELSE]              = {NULL,     NULL,   PREC_NONE};
        rules[TOKEN_FALSE]             = {"literal",     NULL,   PREC_NONE};
        rules[TOKEN_FOR]               = {NULL,     NULL,   PREC_NONE};
        rules[TOKEN_FUNC]              = {NULL,     NULL,   PREC_NONE};
        rules[TOKEN_IF]                = {NULL,     NULL,   PREC_NONE};
        rules[TOKEN_NIL]               = {"literal",     NULL,   PREC_NONE};
        rules[TOKEN_VERT_BAR_VERT_BAR] = {NULL,     "or",   PREC_OR};
        rules[TOKEN_PRINT]             = {NULL,     NULL,   PREC_NONE};
        rules[TOKEN_RETURN]            = {NULL,     NULL,   PREC_NONE};
        rules[TOKEN_TRUE]              = {"literal",     NULL,   PREC_NONE};
        rules[TOKEN_VAR]               = {NULL,     NULL,   PREC_NONE};
        rules[TOKEN_WHILE]             = {NULL,     NULL,   PREC_NONE};
        rules[TOKEN_EOF]               = {NULL,     NULL,   PREC_NONE};
    }

    void compile(Chunk* chunk)
    {
        current_chunk = chunk;
        while (!match(TOKEN_EOF))
        {
            declaration();
        }
        end_compiler();
    }

    Token advance() {previous = current; return tokens[current++];}
    Token peek() {return tokens[current];}
    Token peek_previous() {return tokens[previous];}
    bool is_at_end() {return tokens[current].type == TOKEN_EOF;}
    bool match(Token_Type expected)
    {
        if (peek().type == expected) {advance(); return true;}
        return false;
    }
    void eat(Token_Type expected)
    {
        if (peek().type == expected) {advance(); return;}
        error(format("Expected token of type {}. Got {}.", token_type_to_string[expected], token_type_to_string[peek().type]), peek().line);
    }
    void error(string msg, int line)
    {
        fprintf(stderr, "Compiler error at line[%d] -> [%s]\n", line, msg.c_str());
        exit(1);
    }

    void emit_opcode(Opcode opcode) {current_chunk->add_opcode(opcode, peek_previous().line);}
    void emit_opcode(Opcode_Name opcode_name) {Opcode o(opcode_name); emit_opcode(o);}
    void emit_opcode(Opcode_Name opcode_name, int index) {Opcode o(opcode_name, index); emit_opcode(o);}
    int emit_constant(Value val) {return current_chunk->add_constant(val, peek_previous().line);}
    void emit_opcodes(Opcode opcode1, Opcode opcode2) {emit_opcode(opcode1);emit_opcode(opcode2);}
    void emit_opcodes(Opcode_Name opcode_name1, Opcode_Name opcode_name2) {emit_opcode(opcode_name1);emit_opcode(opcode_name2);}
    void emit_end() {emit_opcode(OP_END);}
    void end_compiler(){emit_end();}

    Parse_Rule* get_rule(Token_Type type) {return &rules[type];}

    void parse_precedence(Precedence precedence)
    {
        advance();
        const char* prefix_rule = get_rule(peek_previous().type)->prefix;
        if (prefix_rule == NULL) {error("Expected expression.", peek().line); return;}
        bool can_assign = precedence <= PREC_ASSIGNMENT;
        resolve_rule(prefix_rule, can_assign);

        while (precedence <= get_rule(peek().type)->precedence)
        {
            advance();
            const char* infix_rule = get_rule(peek_previous().type)->infix;
            resolve_rule(infix_rule, can_assign);
        }

        if (can_assign && match(TOKEN_EQUAL)) {error("Invalid assignment target.", peek_previous().line);}
    }

    void resolve_rule(const char* str_rule, bool can_assign)
    {
        if (!strcmp(str_rule, "grouping")){grouping(); return;}
        if (!strcmp(str_rule, "binary"))  {binary(); return;}
        if (!strcmp(str_rule, "unary"))   {unary(); return;}
        if (!strcmp(str_rule, "number"))  {number(); return;}
        if (!strcmp(str_rule, "literal")) {literal(); return;}
        if (!strcmp(str_rule, "variable")) {variable(can_assign); return;}
        if (!strcmp(str_rule, "and")) {and_(); return;}
        if (!strcmp(str_rule, "or")) {or_(); return;}
    }

    void expression() {parse_precedence(PREC_ASSIGNMENT);}

    void and_()
    {
        int end_jump = emit_goto(OP_GOTO_IF_FALSE);
        emit_opcode(OP_POP);
        parse_precedence(PREC_AND);
        patch_goto(end_jump);
    }

    void or_()
    {
        int end_jump = emit_goto(OP_GOTO_IF_TRUE);
        emit_opcode(OP_POP);
        parse_precedence(PREC_OR);
        patch_goto(end_jump);
    }

    void binary()
    {
        Token_Type operator_type = peek_previous().type;
        Parse_Rule* rule = get_rule(operator_type);
        parse_precedence((Precedence)(rule->precedence+1));

        switch (operator_type)
        {
            case TOKEN_PLUS: emit_opcode(OP_ADD); break;
            case TOKEN_MINUS: emit_opcode(OP_SUB); break;
            case TOKEN_STAR: emit_opcode(OP_MUL); break;
            case TOKEN_SLASH: emit_opcode(OP_DIV); break;
            case TOKEN_EQUAL_EQUAL: emit_opcode(OP_EQUAL); break;
            case TOKEN_BANG_EQUAL: emit_opcodes(OP_EQUAL, OP_NOT); break;
            case TOKEN_LESS: emit_opcode(OP_LESS); break;
            case TOKEN_GREATER: emit_opcode(OP_GREATER); break;
            case TOKEN_LESS_EQUAL: emit_opcodes(OP_GREATER, OP_NOT); break;
            case TOKEN_GREATER_EQUAL: emit_opcodes(OP_LESS, OP_NOT); break;
            default:break;
        }
    }

    void unary()
    {
        Token_Type operator_type = peek_previous().type;
        parse_precedence(PREC_UNARY);
        switch (operator_type)
        {
            case TOKEN_MINUS: emit_opcode(OP_NEG);break;
            case TOKEN_BANG: emit_opcode(OP_NOT);break;
            default: break;
        }
    }

    void grouping()
    {
        expression();
        eat(TOKEN_RIGHT_PAREN);
    }

    void number()
    {
        Value val = peek_previous().literal;
        emit_constant(val);
    }

    void literal()
    {
        switch (peek_previous().type)
        {
            case TOKEN_TRUE: emit_opcode(OP_TRUE);break;
            case TOKEN_FALSE: emit_opcode(OP_FALSE);break;
            case TOKEN_NIL: emit_opcode(OP_NIL);break;
            default:break;
        }
    }

    void declaration()
    {
        if (match(TOKEN_VAR)) {variable_declaration();}
        else if (match(TOKEN_FUNC)) {function_declaration();}
        else {statement();}
    }

    void function_declaration()
    {
        if (scope_depth > 0) {error("Only global functions are allowed.", peek_previous().line);}
        int goto_end = emit_goto(OP_GOTO);
        int global_index = parse_variable_name();
        begin_scope();
        eat(TOKEN_LEFT_PAREN);
        int arity = 0;
        while (match(TOKEN_IDENTIFIER))
        {
            add_local(peek_previous());
            arity++;
            if (!match(TOKEN_COMMA)) {break;}
        }
        eat(TOKEN_RIGHT_PAREN);
        int function_body_instruction_index = current_chunk->codes.size();
        current_chunk->values[global_index] = Value(VAL_NUMBER, function_body_instruction_index);
        statement();
        for (int i = 0; i < arity; i++) {locals.pop_back();}
        scope_depth--;
        patch_goto(goto_end);
    }

    void variable_declaration()
    {
        int global_index = parse_variable_name();
        if (match(TOKEN_EQUAL)) {expression();}
        else {emit_opcode(OP_NIL);}
        eat(TOKEN_SEMICOLON);
        define_variable(global_index);
    }

    int parse_variable_name()
    {
        eat(TOKEN_IDENTIFIER);
        declare_local_variable();
        if (scope_depth > 0) {return 0;}
        return identifier_constant(peek_previous());
    }

    int identifier_constant(Token token)
    {
        if (current_chunk->globals.find(token.lexeme) == current_chunk->globals.end())
        {return current_chunk->add_global_to_globals(token.lexeme);}
        return current_chunk->globals[token.lexeme];
    }

    void define_variable(int index)
    {
        if (scope_depth > 0)
        {
            mark_local_initialized();
            return;
        }
        emit_opcode(Opcode(OP_DEFINE_GLOBAL, index));
    }

    void variable(bool can_assign)
    {
        named_variable(peek_previous(), can_assign);
    }

    void named_variable(Token token, bool can_assign)
    {
        int index = resolve_local(token);
        Opcode_Name get_op;
        Opcode_Name set_op;
        if (index != -1)
        {
            get_op = OP_GET_LOCAL;
            set_op = OP_SET_LOCAL;
        }
        else
        {
            index = identifier_constant(token);
            get_op = OP_GET_GLOBAL;
            set_op = OP_SET_GLOBAL;
        }

        if (can_assign && match(TOKEN_EQUAL))
        {
            expression();
            emit_opcode(Opcode(set_op, index));
        }
        else if (match(TOKEN_LEFT_PAREN)) {call_function(get_op, index);}
        else {emit_opcode(Opcode(get_op, index));}
    }

    void call_function(Opcode_Name get_op, int index)
    {
        emit_constant(Value(VAL_NUMBER, 0)); // return val
        int addr_index = emit_constant(Value(VAL_NUMBER, 0)); // return address
        int arity = 0;
        while (true) // arguements
        {
            expression();
            arity++;
            if (!match(TOKEN_COMMA)) {break;}
        }
        eat(TOKEN_RIGHT_PAREN);
        emit_opcode(Opcode(get_op, index)); // in runtime, the instruction index to the start of function will be on top of stack.
        emit_opcode(OP_CALL, arity);
        patch_constant(addr_index, Value(VAL_NUMBER, current_chunk->codes.size()));
        emit_opcode(OP_POP); // return address
    }

    void patch_constant(int index, Value val) {current_chunk->values[index] = val;}

    void declare_local_variable()
    {
        if (scope_depth == 0) {return;}
        Token name = peek_previous();

        for (int i = locals.size()-1; i >= 0; i--)
        {
            Local& local = locals[i];
            if (local.depth != -1 && local.depth < scope_depth) {break;}
            if (name.lexeme == local.name.lexeme) {error("Already a local variable with the same name exits in this scope.", name.line);}
        }
        add_local(name);
    }

    void mark_local_initialized() {locals[locals.size()-1].depth = scope_depth;}

    void add_local(Token name)
    {
        Local local(name, scope_depth);
        locals.push_back(local);
    }

    int resolve_local(Token name)
    {
        for (int i = locals.size()-1; i >= 0; i--)
        {
            Local& local = locals[i];
            if (name.lexeme == local.name.lexeme) 
            {
                if (local.depth == -1) {error("Can't read local variable in its own initialization.", name.line);}
                return i;
            }
        }
        return -1;
    }

    void statement()
    {
        if (match(TOKEN_PRINT)) {print_statement();}
        else if (match(TOKEN_LEFT_BRACE))
        {
            begin_scope();
            block_statement();
            end_scope();
        }
        else if (match(TOKEN_IF)) {if_statement();}
        else if (match(TOKEN_WHILE)) {while_statement();}
        else if (match(TOKEN_FOR)) {for_statement();}
        else if (match(TOKEN_RETURN)) {return_statement();}
        else {expression_statement();}
    }

    void begin_scope()
    {
        scope_depth++;
    }

    void end_scope()
    {
        scope_depth--;
        while (locals.size() > 0 && locals[locals.size()-1].depth > scope_depth)
        {
            emit_opcode(OP_POP);
            locals.pop_back();
        }
    }

    void block_statement()
    {
        while (peek().type != TOKEN_RIGHT_BRACE && peek().type != TOKEN_EOF) {declaration();}
        eat(TOKEN_RIGHT_BRACE);
    }

    void print_statement()
    {
        expression();
        eat(TOKEN_SEMICOLON);
        emit_opcode(OP_PRINT);
    }

    void expression_statement()
    {
        expression();
        eat(TOKEN_SEMICOLON);
        emit_opcode(OP_POP);
    }

    int emit_goto(Opcode_Name opcode_name)
    {
        emit_opcode(opcode_name);
        return current_chunk->codes.size()-1;
    }

    int emit_goto(Opcode_Name opcode_name, int index)
    {
        emit_opcode(opcode_name, index);
        return current_chunk->codes.size()-1;
    }

    void patch_goto(int index)
    {
        Opcode_Name name = current_chunk->codes[index].name; 
        if (name != OP_GOTO && name != OP_GOTO_IF_TRUE && name != OP_GOTO_IF_FALSE)
        {error("Tried to patch an opcode which is not a GOTO.", peek().line);}
        current_chunk->codes[index].index = current_chunk->codes.size();
    }

    void if_statement()
    {
        eat(TOKEN_LEFT_PAREN);
        expression();
        eat(TOKEN_RIGHT_PAREN);
        int goto_else = emit_goto(OP_GOTO_IF_FALSE);
        emit_opcode(OP_POP);
        statement();
        int goto_end = emit_goto(OP_GOTO);
        patch_goto(goto_else);
        emit_opcode(OP_POP);
        if (match(TOKEN_ELSE)) {statement();}
        patch_goto(goto_end);
    }

    void while_statement()
    {
        int loop_start = current_chunk->codes.size();
        eat(TOKEN_LEFT_PAREN);
        expression();
        eat(TOKEN_RIGHT_PAREN);
        int goto_exit = emit_goto(OP_GOTO_IF_FALSE);
        emit_opcode(OP_POP);
        statement();
        emit_goto(OP_GOTO, loop_start);
        patch_goto(goto_exit);
        emit_opcode(OP_POP);
    }

    void for_statement()
    {
        begin_scope();
        eat(TOKEN_LEFT_PAREN);
        if (match(TOKEN_SEMICOLON)) {}
        else if (match(TOKEN_VAR)) {variable_declaration();}
        else {expression_statement();}

        int loop_start = current_chunk->codes.size();
        int goto_exit = -1;
        if (!match(TOKEN_SEMICOLON))
        {
            expression();
            eat(TOKEN_SEMICOLON);
            goto_exit = emit_goto(OP_GOTO_IF_FALSE);
            emit_opcode(OP_POP);
        }
        
        if (!match(TOKEN_RIGHT_PAREN))
        {
            int goto_body = emit_goto(OP_GOTO);
            int increment_start = current_chunk->codes.size();
            expression();
            emit_opcode(OP_POP);
            eat(TOKEN_RIGHT_PAREN);

            emit_goto(OP_GOTO, loop_start);
            loop_start = increment_start;
            patch_goto(goto_body);    
        }

        statement();
        emit_goto(OP_GOTO, loop_start);
        if (goto_exit != -1)
        {
            patch_goto(goto_exit);
            emit_opcode(OP_POP);
        }

        end_scope();
    }

    void return_statement()
    {
        expression();
        eat(TOKEN_SEMICOLON);
        emit_opcode(OP_RETURN);
    }
};

struct Virtual_Machine
{
    Chunk chunk;
    vector<Value> stack;
    int instruction_index = -1;
    int frame_index = 0;
    vector<int> frame_index_history;
    Value ret_register;

    int get_stack_index() {return this->stack.size();}

    void push(Value v) {this->stack.push_back(v);}

    Value peek(int n)
    {
        if (stack.size()-n-1 < 0) {error(0, "Tried to reach stack index -1.");}
        return stack[stack.size()-n-1];
    }

    Value pop()
    {
        Value v = this->stack[this->stack.size()-1];
        this->stack.pop_back();
        return v;
    }

    void error(int line, string msg)
    {
        fprintf(stderr, "Runtime error at line[%d] -> [%s]\n", line, msg.c_str());
        exit(1);
    }
    
    void run()
    {
        instruction_index = 0;
        for (;;)
        {
            Opcode& code = chunk.codes[instruction_index];
            int line = chunk.lines[instruction_index];
            instruction_index++;

            if (DEBUG_MODE) {cout << code.to_string() << ' ' << line << "\n";}

            switch (code.name)
            {
                case OP_END: return;
                case OP_RETURN:
                    {
                        Value v = pop();
                        stack[frame_index-2] = v;
                        instruction_index = stack[frame_index-1].as.n;
                        while (stack.size()-1 >= frame_index) {pop();}
                        frame_index = frame_index_history[frame_index_history.size()-1];
                        frame_index_history.pop_back();
                        break;
                    }
                case OP_CONSTANT: {Value v = this->chunk.values[code.index];push(v);break;}
                case OP_TRUE: push(Value(VAL_BOOL, true));
                case OP_FALSE: push(Value(VAL_BOOL, false));
                case OP_NIL: push(Value(VAL_NIL));

                case OP_NEG:
                    {
                        Value v = pop();
                        if (v.type != VAL_NUMBER) {error(line, "NEG expects NUMBER.");}
                        v.as.n = -v.as.n;
                        push(v);break;
                    }
                case OP_NOT:
                    {
                        Value v = pop();
                        if (v.type != VAL_BOOL) {error(line, "NOT expects BOOL.");}
                        v.as.b = !v.as.b;
                        push(v);break;
                    }
                case OP_ADD:
                    {
                        Value right = pop();
                        Value left = pop();
                        if (!(right.type == VAL_NUMBER && left.type == VAL_NUMBER)) {error(line, "ADD expects NUMBER*NUMBER.");}
                        Value v(VAL_NUMBER, left.as.n+right.as.n);
                        push(v);break;
                    }
                case OP_SUB:
                    {
                        Value right = pop();
                        Value left = pop();
                        if (!(right.type == VAL_NUMBER && left.type == VAL_NUMBER)) {error(line, "SUB expects NUMBER*NUMBER.");}
                        Value v(VAL_NUMBER, left.as.n-right.as.n);
                        push(v);break;
                    }
                case OP_MUL:
                    {
                        Value right = pop();
                        Value left = pop();
                        if (!(right.type == VAL_NUMBER && left.type == VAL_NUMBER)) {error(line, "MUL expects NUMBER*NUMBER.");}
                        Value v(VAL_NUMBER, left.as.n*right.as.n);
                        push(v);break;
                    }
                case OP_DIV:
                    {
                        Value right = pop();
                        Value left = pop();
                        if (!(right.type == VAL_NUMBER && left.type == VAL_NUMBER)) {error(line, "DIV expects NUMBER*NUMBER.");}
                        if (right.as.n == 0) {this->error(line, "Cannont divide by 0.");}
                        Value v(VAL_NUMBER, left.as.n/right.as.n);
                        push(v);break;
                    }
                case OP_EQUAL:
                    {
                        Value right = this->pop();
                        Value left = this->pop();
                        Value v(VAL_BOOL);
                        if (right.type == VAL_NUMBER && left.type == VAL_NUMBER) {v.as.b = (right.as.n == left.as.n);}
                        else if (right.type == VAL_BOOL && left.type == VAL_BOOL) {v.as.b = (right.as.b == left.as.b);}
                        else if (right.type == VAL_NIL && left.type == VAL_NIL) {v.as.b = true;}
                        else {this->error(line, "EQUAL expects 'a*'a ('a=NUMBER|BOOL|NIL).");}                        
                        push(v);break;
                    }
                case OP_LESS:
                    {
                        Value right = this->pop();
                        Value left = this->pop();
                        if (!(right.type == VAL_NUMBER && left.type == VAL_NUMBER)) {error(line, "LESS expects NUMBER*NUMBER");}
                        Value v(VAL_BOOL, left.as.n < right.as.n);
                        push(v);break;
                    }
                case OP_GREATER:
                    {
                        Value right = this->pop();
                        Value left = this->pop();
                        if (!(right.type == VAL_NUMBER && left.type == VAL_NUMBER)) {error(line, "LESS expects NUMBER*NUMBER");}
                        Value v(VAL_BOOL, left.as.n > right.as.n);
                        push(v);break;
                    }
                case OP_PRINT: cout << pop().to_string() << "\n"; break;
                case OP_POP: pop(); break;
                case OP_DEFINE_GLOBAL: chunk.values[code.index] = peek(0); pop(); break;
                case OP_GET_GLOBAL:
                    {
                        Value v = chunk.values[code.index];
                        push(v);break;
                    }
                case OP_SET_GLOBAL: chunk.values[code.index] = peek(0); break;
                case OP_GET_LOCAL: push(stack[code.index+frame_index]);break;
                case OP_SET_LOCAL: stack[code.index+frame_index] = peek(0); break;
                case OP_GOTO_IF_FALSE: if (!peek(0).as.b) {instruction_index = code.index;} break;
                case OP_GOTO_IF_TRUE: if (peek(0).as.b) {instruction_index = code.index;} break;
                case OP_GOTO: instruction_index = code.index; break;
                case OP_CALL:
                    {
                        Value v = pop();
                        instruction_index = v.as.n;
                        frame_index_history.push_back(frame_index);
                        frame_index = stack.size()-code.index;
                        break;
                    }
                case OP_STORE_TO_RET: ret_register = peek(0); break;
                case OP_PUSH_FROME_RET: push(ret_register); ret_register = Value(); break;
                default: break;
            }
            
            if (DEBUG_MODE)
            {
                for (Value& v : this->stack)
                {
                    cout << format("[{}]", v.to_string());
                }
                cout << format(" fp:{}", frame_index);
                cout << "\n";
            }
        }
    }
};

int main(int argc, char* argv[])
{
    if (argc < 2 || argc > 2) {fprintf(stderr, "Usage: ./[path_to_executable] [path_to_source_file]\n");return 1;}
    ifstream in_file(argv[1]);
    if (in_file.fail()) {fprintf(stderr, "the file %s doesn't exist.", argv[1]); return 1;}
    stringstream buffer;
    buffer << in_file.rdbuf();
    Tokenizer tokenizer(buffer.str());
    tokenizer.scan_tokens();
    //tokenizer.print_tokens();
    Compiler parser(tokenizer.tokens);
    Virtual_Machine vm;
    parser.compile(&(vm.chunk));
    vm.run();
    return 0;
}