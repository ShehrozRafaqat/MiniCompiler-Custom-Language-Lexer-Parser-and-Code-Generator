#include <iostream>
#include <vector>
#include <string>

using namespace std;

struct Instruction {
    string op;
    string arg1;
    string arg2;
    string result;
};

class CodeGenerator {
private:
    vector<Instruction> instructions; // Intermediate Code
    vector<string> assemblyCode;      // Generated Assembly Code

public:
    CodeGenerator(const vector<Instruction> &instr) : instructions(instr) {}

    void generateAssembly() {
        for (const auto &instr : instructions) {
            if (instr.op == "=") {
                handleAssignment(instr);
            } else if (instr.op == "+") {
                handleBinaryOperation("ADD", instr);
            } else if (instr.op == "-") {
                handleBinaryOperation("SUB", instr);
            } else if (instr.op == "log") {
                handleLog(instr);
            } else if (instr.op == "*") {
                handleBinaryOperation("IMUL", instr);
            } else if (instr.op == "/") {
                handleDivision(instr);
            } else if (instr.op == "print") {
                handlePrint(instr);
            } else if (instr.op == "ifFalse") {
                handleConditionalJump(instr);
            } else if (instr.op == "goto") {
                assemblyCode.push_back("JMP " + instr.arg1);
            } else if (instr.op == "label") {
                assemblyCode.push_back(instr.arg1 + ":");
            } else if (instr.op == "return") {
                handleReturn(instr);
            }
        }
    }

    void printAssembly() {
        cout << endl;
        cout << "Generated Assembly Code:" << endl;
        for (const auto &line : assemblyCode) {
            cout << line << endl;
        }
    }

private:
    void handleAssignment(const Instruction &instr) {
        assemblyCode.push_back("MOV " + instr.result + ", " + instr.arg1);
    }

    void handleBinaryOperation(const string &operation, const Instruction &instr) {
        assemblyCode.push_back("MOV AX, " + instr.arg1);
        assemblyCode.push_back(operation + " AX, " + instr.arg2);
        assemblyCode.push_back("MOV " + instr.result + ", AX");
    }

    void handleDivision(const Instruction &instr) {
        assemblyCode.push_back("MOV AX, " + instr.arg1);
        assemblyCode.push_back("CWD"); // Convert to 32-bit for division
        assemblyCode.push_back("IDIV " + instr.arg2);
        assemblyCode.push_back("MOV " + instr.result + ", AX");
    }

    void handlePrint(const Instruction &instr) {
        assemblyCode.push_back("MOV EAX, " + instr.arg1);
        assemblyCode.push_back("CALL PRINT"); // Assume PRINT is a procedure
    }

    void handleConditionalJump(const Instruction &instr) {
        assemblyCode.push_back("CMP " + instr.arg1 + ", 0");
        assemblyCode.push_back("JE " + instr.arg2);
    }

    void handleReturn(const Instruction &instr) {
        assemblyCode.push_back("MOV EAX, " + instr.arg1);
        assemblyCode.push_back("RET");
    }

    void handleLog(const Instruction &instr) {
        string prefix;
        if (instr.arg1 == "INFO")
            prefix = "[INFO]: ";
        else if (instr.arg1 == "WARN")
            prefix = "[WARN]: ";
        else if (instr.arg1 == "ERROR")
            prefix = "[ERROR]: ";

        assemblyCode.push_back("MOV EAX, " + instr.arg2); // Message
        assemblyCode.push_back("PRINT '" + prefix + "'"); // Prefix
        assemblyCode.push_back("CALL PRINT");             // Print message
    }
};

int main() {
    vector<Instruction> instructions = {
        {"=", "5", "", "a"},
        {"+", "a", "3", "b"},
        {"-", "b", "2", "c"},
        {"log", "INFO", "c", ""},
        {"print", "c", "", ""},
        {"return", "c", "", ""}
    };

    CodeGenerator codeGen(instructions);
    codeGen.generateAssembly();
    codeGen.printAssembly();

    return 0;
}