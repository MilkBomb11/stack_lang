#include<iostream>
#include<format>
#include<string>
#include<map>
#include<vector>
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
        return format("ERROR VALUE");
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