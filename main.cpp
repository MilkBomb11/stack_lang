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
};

enum Token_Type
{
    TOKEN_LEFT_PAREN, TOKEN_RIGHT_PAREN, TOKEN_LEFT_BRACE, TOKEN_RIGHT_BRACE,
    TOKEN_PLUS, TOKEN_MINUS, TOKEN_STAR, TOKEN_SLASH,
    TOKEN_SEMICOLON,
    TOKEN_EQUAL, TOKEN_EQUAL_EQUAL, TOKEN_BANG_EQUAL, TOKEN_LESS, TOKEN_LESS_EQUAL, TOKEN_GREATER, TOKEN_GREATER_EQUAL,
    TOKEN_BANG, TOKEN_VERT_BAR_VERT_BAR, TOKEN_AMPERSAN_AMPERSAN,

    TOKEN_TRUE, TOKEN_FALSE, TOKEN_NIL,
    TOKEN_FOR, TOKEN_WHILE, TOKEN_FUNC, TOKEN_VAR, TOKEN_IF,
    TOKEN_NUMBER, TOKEN_IDENTIFIER,

    TOKEN_EOF,
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
    {TOKEN_TRUE, "TRUE"},
    {TOKEN_FALSE, "FALSE"},
    {TOKEN_NIL, "NIL"},
    {TOKEN_IDENTIFIER, "IDENTIFIER"},
    {TOKEN_NUMBER, "NUMBER"},
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

enum Type {VAL_NUMBER, VAL_BOOL, VAL_ERROR};

struct Value
{
    Type type;
    union
    {
        double n;
        bool b;
    }as;

    Value()
    {
        this->type = VAL_ERROR;
    }

    Value(Type type, auto val)
    {
        this->type = type;
        switch (this->type)
        {
            case Type::VAL_NUMBER: this->as.n = val; break;
            case Type::VAL_BOOL: this->as.b = val; break;
            default: break;
        }
    }
    
    string to_string()
    {
        switch (type)
        {
            case Type::VAL_NUMBER: return format("NUMBER({})", this->as.n);
            case Type::VAL_BOOL: return format("BOOL({})", this->as.b);    
            default: break;
        }
        return format("NILVAL");
    }
};

struct Chunk
{
    vector<Opcode> codes;
    vector<Value> values;
    vector<u32> lines;

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

    void add_constant(Value value, u32 line)
    {
        int index = this->add_value_to_values(value);
        this->add_opcode(Opcode(OP_CONSTANT, index), line);
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
            case ' ':
            case '\t':
            case '\r': break;
            case '\n': ++line; break;
            case '/':
                if (match('/'))
                {while (peek() != '\n') {advance();}}
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
        Value literal(Type::VAL_NUMBER, stod(lexeme));
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

struct Virtual_Machine
{
    Chunk chunk;
    vector<Value> stack;
    int instruction_index = -1;

    int get_stack_index() {return this->stack.size();}

    void push(Value v) {this->stack.push_back(v);}

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
                case OP_RETURN:
                    cout << this->pop().to_string() << "\n";
                    return;
                case OP_CONSTANT:
                    {Value v = this->chunk.values[code.index];
                    this->push(v);
                    break;}
                case OP_NEG:
                    {Value v = this->pop();
                    if (v.type != VAL_NUMBER) {this->error(line, "NEG expects NUMBER.");}
                    v.as.n = -v.as.n;
                    this->push(v);
                    break;}
                case OP_ADD:
                    {Value right = this->pop();
                    Value left = this->pop();
                    if (!(right.type == VAL_NUMBER && left.type == VAL_NUMBER)) {this->error(line, "ADD expects NUMBER*NUMBER.");}
                    Value v(VAL_NUMBER, left.as.n+right.as.n);
                    this->push(v);
                    break;}
                case OP_SUB:
                    {Value right = this->pop();
                    Value left = this->pop();
                    if (!(right.type == VAL_NUMBER && left.type == VAL_NUMBER)) {this->error(line, "SUB expects NUMBER*NUMBER.");}
                    Value v(VAL_NUMBER, left.as.n-right.as.n);
                    this->push(v);
                    break;}
                case OP_MUL:
                    {Value right = this->pop();
                    Value left = this->pop();
                    if (!(right.type == VAL_NUMBER && left.type == VAL_NUMBER)) {this->error(line, "MUL expects NUMBER*NUMBER.");}
                    Value v(VAL_NUMBER, left.as.n*right.as.n);
                    this->push(v);
                    break;}
                case OP_DIV:
                    {Value right = this->pop();
                    Value left = this->pop();
                    if (!(right.type == VAL_NUMBER && left.type == VAL_NUMBER)) {this->error(line, "DIV expects NUMBER*NUMBER.");}
                    if (right.as.n == 0) {this->error(line, "Cannont divide by 0.");}
                    Value v(VAL_NUMBER, left.as.n/right.as.n);
                    this->push(v);
                    break;}
                default:
                    break;
            }
            
            if (DEBUG_MODE)
            {
                for (Value& v : this->stack)
                {
                    cout << format("[{}]", v.to_string());
                }
                cout << "\n";
            }
        }
    }
};

int main(int argc, char* argv[])
{
    // Chunk c;
    // c.add_constant(Value(NUMBER, 32), 0);
    // c.add_constant(Value(NUMBER, 16), 0);
    // c.add_opcode(Opcode(ADD), 0);
    // c.add_constant(Value(NUMBER, 15), 0);
    // c.add_opcode(Opcode(DIV), 0);
    // c.add_opcode(Opcode(NEG), 0);
    // c.add_opcode(Opcode(RETURN), 0);
    
    // Virtual_Machine vm;
    // vm.chunk = c;
    // vm.run();
    if (argc < 2 || argc > 2) {fprintf(stderr, "Usage: ./[path_to_executable] [path_to_source_file]\n");return 1;}
    ifstream in_file(argv[1]);
    if (in_file.fail()) {fprintf(stderr, "the file %s doesn't exist.", argv[1]); return 1;}
    stringstream buffer;
    buffer << in_file.rdbuf();
    Tokenizer tokenizer(buffer.str());
    tokenizer.scan_tokens();
    tokenizer.print_tokens();
    return 0;
}