#include<iostream>
#include<format>
#include<string>
#include<map>
#include<vector>
#include<cstring>
using namespace std;
typedef unsigned int u32;

#define DEBUG_MODE 1

enum Opcode_Name
{
    RETURN,
    CONSTANT,
    NEG,
    ADD, SUB, MUL, DIV,
};

enum Token_Type
{
    LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE,
    PLUS, MINUS, STAR, SLASH,
    SEMICOLON,
    EQUAL, EQUAL_EQUAL, BANG_EQUAL, LESS, LESS_EQUAL, GREATER, GREATER_EQUAL,
    BANG, VERT_BAR_VERT_BAR, AMPERSAN_AMPERSAN,

    TRUE, FALSE, NIL,
    FOR, WHILE, FUNC, VAR, IF,
    NUMBER, IDENTIFIER,

    EOF_,
};

map<u32, string> opcode_name_to_string =
{
    {Opcode_Name::RETURN, "RETURN"},
    {Opcode_Name::CONSTANT, "CONSTANT"},
    {Opcode_Name::NEG, "NEG"},
    {Opcode_Name::ADD, "ADD"},
    {Opcode_Name::SUB, "SUB"},
    {Opcode_Name::MUL, "MUL"},
    {Opcode_Name::DIV, "DIV"},
};

map<u32, string> token_type_to_string = 
{
    {LEFT_PAREN, "LEFT_PAREN"},
    {RIGHT_PAREN, "LEFT_PAREN"},
    {LEFT_BRACE, "LEFT_BRACE"},
    {RIGHT_BRACE, "RIGHT_BRACE"},
    {PLUS, "PLUS"},
    {MINUS, "MINUS"},
    {STAR, "STAR"},
    {SLASH, "SLASH"},
    {EQUAL, "EQUAL"},
    {EQUAL_EQUAL, "EQUAL_EQUAL"},
    {BANG_EQUAL, "BANG_EQUAL"},
    {LESS, "LESS"},
    {LESS_EQUAL, "LESS_EQUAL"},
    {GREATER, "GREATER"},
    {GREATER_EQUAL, "GREATER_EQUAL"},
    {BANG, "BANG"},
    {VERT_BAR_VERT_BAR, "VERT_BAR_VERT_BAR"},
    {AMPERSAN_AMPERSAN, "AMPERSAN_AMPERSAN"},
    {FOR, "FOR"},
    {WHILE, "WHILE"},
    {FUNC, "FUNC"},
    {VAR, "VAR"},
    {IF, "IF"},
    {TRUE, "TRUE"},
    {FALSE, "FALSE"},
    {NIL, "NIL"},
    {IDENTIFIER, "IDENTIFIER"},
    {NUMBER, "NUMBER"},
    {EOF_, "EOF"},
};

map<string, Token_Type> keywords =
{
    {"true", TRUE},
    {"false", FALSE},
    {"nil", NIL},
    {"for", FOR},
    {"while", WHILE},
    {"func", FUNC},
    {"var", VAR},
    {"if", IF},
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

enum Type {NUMBER, BOOL, ERROR};

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
        this->type = ERROR;
    }

    Value(Type type, auto val)
    {
        this->type = type;
        switch (this->type)
        {
            case Type::NUMBER: this->as.n = val; break;
            case Type::BOOL: this->as.b = val; break;
            default: break;
        }
    }
    
    string to_string()
    {
        switch (type)
        {
            case Type::NUMBER: return format("NUMBER({})", this->as.n);
            case Type::BOOL: return format("BOOL({})", this->as.b);      
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
        this->add_opcode(Opcode(Opcode_Name::CONSTANT, index), line);
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

        push_token(EOF_);
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
            case '(': push_token(LEFT_PAREN); break;
            case ')': push_token(RIGHT_PAREN); break;
            case '{': push_token(LEFT_BRACE); break;
            case '}': push_token(RIGHT_BRACE); break;
            case '+': push_token(PLUS); break;
            case '-': push_token(MINUS); break;
            case '*': push_token(STAR); break;
            case ';': push_token(SEMICOLON); break;
            case ' ':
            case '\t':
            case '\r': break;
            case '\n': ++line; break;
            case '/':
                if (match('/'))
                {while (peek() != '\n') {advance();}}
                else  {push_token(SLASH);} break;
            case '=': match('=') ? push_token(EQUAL_EQUAL):push_token(EQUAL);break;
            case '!': match('=') ? push_token(BANG_EQUAL):push_token(BANG);break;
            case '<': match('=') ? push_token(LESS_EQUAL):push_token(LESS);break;
            case '>': match('=') ? push_token(GREATER_EQUAL):push_token(GREATER);break;
            case '|': match('|') ? push_token(VERT_BAR_VERT_BAR):error("missing '|' after '|'");break;
            case '&': match('&') ? push_token(AMPERSAN_AMPERSAN):error("missing '&' after '&'");break;
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
        Value literal(Type::NUMBER, stod(lexeme));
        push_token(Token_Type::NUMBER, literal);
    }

    void identifier()
    {
        while (is_alphanumeric(peek())) {advance();}
        string lexeme = source.substr(start, current-start);
        if (keywords.find(lexeme) == keywords.end()) {push_token(IDENTIFIER); return;}
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
                case RETURN:
                    cout << this->pop().to_string() << "\n";
                    return;
                case CONSTANT:
                    {Value v = this->chunk.values[code.index];
                    this->push(v);
                    break;}
                case NEG:
                    {Value v = this->pop();
                    if (v.type != NUMBER) {this->error(line, "NEG expects NUMBER.");}
                    v.as.n = -v.as.n;
                    this->push(v);
                    break;}
                case ADD:
                    {Value right = this->pop();
                    Value left = this->pop();
                    if (!(right.type == NUMBER && left.type == NUMBER)) {this->error(line, "ADD expects NUMBER*NUMBER.");}
                    Value v(NUMBER, left.as.n+right.as.n);
                    this->push(v);
                    break;}
                case SUB:
                    {Value right = this->pop();
                    Value left = this->pop();
                    if (!(right.type == NUMBER && left.type == NUMBER)) {this->error(line, "SUB expects NUMBER*NUMBER.");}
                    Value v(NUMBER, left.as.n-right.as.n);
                    this->push(v);
                    break;}
                case MUL:
                    {Value right = this->pop();
                    Value left = this->pop();
                    if (!(right.type == NUMBER && left.type == NUMBER)) {this->error(line, "MUL expects NUMBER*NUMBER.");}
                    Value v(NUMBER, left.as.n*right.as.n);
                    this->push(v);
                    break;}
                case DIV:
                    {Value right = this->pop();
                    Value left = this->pop();
                    if (!(right.type == NUMBER && left.type == NUMBER)) {this->error(line, "DIV expects NUMBER*NUMBER.");}
                    if (right.as.n == 0) {this->error(line, "Cannont divide by 0.");}
                    Value v(NUMBER, left.as.n/right.as.n);
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

int main(int arc, char* argv[])
{
    Chunk c;
    c.add_constant(Value(NUMBER, 32), 0);
    c.add_constant(Value(NUMBER, 16), 0);
    c.add_opcode(Opcode(ADD), 0);
    c.add_constant(Value(NUMBER, 15), 0);
    c.add_opcode(Opcode(DIV), 0);
    c.add_opcode(Opcode(NEG), 0);
    c.add_opcode(Opcode(RETURN), 0);
    
    Virtual_Machine vm;
    vm.chunk = c;
    vm.run();
    return 0;
}