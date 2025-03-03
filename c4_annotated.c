// c4_annotated.c - Annotated C in four functions
// Written by Robert Swierczek
// Annotated by Grok 3 (xAI) on March 03, 2025

// This is a minimal C compiler and interpreter written in a subset of C,
// using only char, int, pointer types, and if, while, return, and expression statements.
// It supports self-compilation and basic C features like functions, globals, and locals.

#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <unistd.h>
#include <fcntl.h>
#define int long long // Redefines int as long long to ensure 64-bit arithmetic on all platforms.

/* Global Variables */
// Pointers to navigate and process the source code and data segments.
char *p, *lp, // p: current position in source code; lp: last line start for source printing.
     *data;   // Pointer to the data/bss segment for global variables and strings.

// Pointers and arrays for code emission and symbol table management.
int *e, *le,  // e: current position in emitted code; le: last emitted instruction for debugging.
    *id,      // Points to the currently parsed identifier in the symbol table.
    *sym,     // Symbol table: a flat array storing identifiers (keywords, variables, functions).
    tk,       // Current token type (e.g., Num, Id, If).
    ival,     // Value of the current token (e.g., number or string address).
    ty,       // Type of the current expression (CHAR, INT, PTR).
    loc,      // Offset for local variables in the current function’s stack frame.
    line,     // Current line number for error reporting and debugging.
    src,      // Flag: if 1, print source code and emitted assembly during parsing.
    debug;    // Flag: if 1, print executed instructions during VM runtime.

// Token types and operator precedence order (higher number = lower precedence).
enum {
  Num = 128, Fun, Sys, Glo, Loc, Id, // Num starts at 128 to distinguish from ASCII; Id is generic identifier.
  Char, Else, Enum, If, Int, Return, Sizeof, While, // Keywords.
  Assign, Cond, Lor, Lan, Or, Xor, And, Eq, Ne, Lt, Gt, Le, Ge, Shl, Shr, Add, Sub, Mul, Div, Mod, Inc, Dec, Brak // Operators.
};

// Virtual machine opcodes for the generated code.
enum { 
  LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH , // Control and memory ops.
  OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD , // Arithmetic and logic ops.
  OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT // System calls.
};

// Type system: supports basic types and pointers.
enum { CHAR, INT, PTR }; // CHAR=0, INT=1, PTR=2; pointer types are INT+PTR, CHAR+PTR, etc.

// Symbol table entry offsets (since no struct is used, it’s a flat array).
enum { Tk, Hash, Name, Class, Type, Val, HClass, HType, HVal, Idsz }; // Each entry is Idsz ints long.

/* Lexer: next() - Advances to the next token in the source code */
void next()
{
  char *pp; // Temporary pointer for identifier parsing.

  while (tk = *p) { // Loop until end of input (tk=0).
    ++p; // Move to next character.
    if (tk == '\n') { // Handle newlines.
      if (src) { // If source printing is enabled (-s flag).
        // Print the current line number and source text since last newline.
        printf("%d: %.*s", line, p - lp, lp);
        lp = p; // Update last line pointer.
        // Print all emitted instructions since last line.
        while (le < e) {
          printf("%8.4s", &"LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,"
                           "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,"
                           "OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT,"[*++le * 5]);
          if (*le <= ADJ) printf(" %d\n", *++le); else printf("\n"); // Arguments follow some ops.
        }
      }
      ++line; // Increment line counter for error reporting.
    }
    else if (tk == '#') { // Skip preprocessor directives (e.g., #include).
      while (*p != 0 && *p != '\n') ++p; // Advance to end of line.
    }
    else if ((tk >= 'a' && tk <= 'z') || (tk >= 'A' && tk <= 'Z') || tk == '_') { // Identifier.
      pp = p - 1; // Start of identifier.
      // Compute a simple hash while scanning alphanumeric characters or underscore.
      while ((*p >= 'a' && *p <= 'z') || (*p >= 'A' && *p <= 'Z') || (*p >= '0' && *p <= '9') || *p == '_')
        tk = tk * 147 + *p++; // Hash function: multiply and add (147 is arbitrary prime).
      tk = (tk << 6) + (p - pp); // Finalize hash with identifier length.
      id = sym; // Start at symbol table beginning.
      while (id[Tk]) { // Search for existing identifier.
        if (tk == id[Hash] && !memcmp((char *)id[Name], pp, p - pp)) { // Match found.
          tk = id[Tk]; // Set token to symbol’s type (e.g., Int, Id).
          return;
        }
        id = id + Idsz; // Move to next symbol table entry.
      }
      // New identifier: add to symbol table.
      id[Name] = (int)pp; // Store pointer to name in source code.
      id[Hash] = tk; // Store computed hash.
      tk = id[Tk] = Id; // Mark as generic identifier; updated later if keyword or variable.
      return;
    }
    else if (tk >= '0' && tk <= '9') { // Number literal.
      if (ival = tk - '0') { // Decimal number.
        while (*p >= '0' && *p <= '9') ival = ival * 10 + *p++ - '0';
      }
      else if (*p == 'x' || *p == 'X') { // Hexadecimal (e.g., 0xFF).
        while ((tk = *++p) && ((tk >= '0' && tk <= '9') || (tk >= 'a' && tk <= 'f') || (tk >= 'A' && tk <= 'F')))
          ival = ival * 16 + (tk & 15) + (tk >= 'A' ? 9 : 0); // Convert hex digit to value.
      }
      else { // Octal (e.g., 0777).
        while (*p >= '0' && *p <= '7') ival = ival * 8 + *p++ - '0';
      }
      tk = Num; // Set token type to number.
      return;
    }
    else if (tk == '/') { // Division or comment.
      if (*p == '/') { // Single-line comment.
        ++p;
        while (*p != 0 && *p != '\n') ++p; // Skip to end of line.
      }
      else {
        tk = Div; // Division operator.
        return;
      }
    }
    else if (tk == '\'' || tk == '"') { // Character or string literal.
      pp = data; // Store string in data segment.
      while (*p != 0 && *p != tk) { // Scan until closing quote.
        if ((ival = *p++) == '\\') { // Escape sequence.
          if ((ival = *p++) == 'n') ival = '\n'; // Only supports \n for simplicity.
        }
        if (tk == '"') *data++ = ival; // Store in data segment for strings.
      }
      ++p; // Skip closing quote.
      if (tk == '"') ival = (int)pp; // For strings, ival is pointer to data.
      else tk = Num; // For chars, ival is the character value.
      return;
    }
    else if (tk == '=') { // Assignment or equality.
      if (*p == '=') { ++p; tk = Eq; } else tk = Assign; return;
    }
    else if (tk == '+') { // Addition or increment.
      if (*p == '+') { ++p; tk = Inc; } else tk = Add; return;
    }
    else if (tk == '-') { // Subtraction or decrement.
      if (*p == '-') { ++p; tk = Dec; } else tk = Sub; return;
    }
    else if (tk == '!') { // Logical not or not equal.
      if (*p == '=') { ++p; tk = Ne; } return; // ! alone is unary not; handled in expr().
    }
    else if (tk == '<') { // Less than, less or equal, or left shift.
      if (*p == '=') { ++p; tk = Le; } else if (*p == '<') { ++p; tk = Shl; } else tk = Lt; return;
    }
    else if (tk == '>') { // Greater than, greater or equal, or right shift.
      if (*p == '=') { ++p; tk = Ge; } else if (*p == '>') { ++p; tk = Shr; } else tk = Gt; return;
    }
    else if (tk == '|') { // Bitwise or logical or.
      if (*p == '|') { ++p; tk = Lor; } else tk = Or; return;
    }
    else if (tk == '&') { // Bitwise or logical and.
      if (*p == '&') { ++p; tk = Lan; } else tk = And; return;
    }
    else if (tk == '^') { tk = Xor; return; } // Bitwise XOR.
    else if (tk == '%') { tk = Mod; return; } // Modulus.
    else if (tk == '*') { tk = Mul; return; } // Multiplication or dereference (handled in expr).
    else if (tk == '[') { tk = Brak; return; } // Array subscript.
    else if (tk == '?') { tk = Cond; return; } // Ternary operator.
    // Single-character tokens returned as-is.
    else if (tk == '~' || tk == ';' || tk == '{' || tk == '}' || tk == '(' || tk == ')' || tk == ']' || tk == ',' || tk == ':') return;
  }
}

/* Expression Parser: expr(int lev) - Parses expressions using precedence climbing */
void expr(int lev) // lev: minimum precedence level to parse (higher value = tighter binding).
{
  int t, *d; // t: temporary type storage; d: pointer for identifier lookup.

  if (!tk) { printf("%d: unexpected eof in expression\n", line); exit(-1); } // End of input error.
  else if (tk == Num) { // Number literal.
    *++e = IMM; *++e = ival; next(); ty = INT; // Emit immediate value; type is INT.
  }
  else if (tk == '"') { // String literal.
    *++e = IMM; *++e = ival; next(); // Emit address of string in data segment.
    while (tk == '"') next(); // Skip concatenated strings (e.g., "a" "b").
    data = (char *)((int)data + sizeof(int) & -sizeof(int)); // Align data pointer.
    ty = PTR; // Type is pointer to char.
  }
  else if (tk == Sizeof) { // sizeof operator.
    next(); if (tk == '(') next(); else { printf("%d: open paren expected in sizeof\n", line); exit(-1); }
    ty = INT; // Default type.
    if (tk == Int) next(); else if (tk == Char) { next(); ty = CHAR; } // Parse base type.
    while (tk == Mul) { next(); ty = ty + PTR; } // Add pointer levels.
    if (tk == ')') next(); else { printf("%d: close paren expected in sizeof\n", line); exit(-1); }
    *++e = IMM; *++e = (ty == CHAR) ? sizeof(char) : sizeof(int); // Emit size.
    ty = INT; // Result type is always INT.
  }
  else if (tk == Id) { // Identifier (variable, function, or constant).
    d = id; next(); // Save identifier and advance.
    if (tk == '(') { // Function call.
      next();
      t = 0; // Argument count.
      while (tk != ')') { // Parse arguments.
        expr(Assign); *++e = PSH; ++t; // Evaluate and push each argument.
        if (tk == ',') next();
      }
      next(); // Skip ')'.
      if (d[Class] == Sys) *++e = d[Val]; // System call (e.g., printf).
      else if (d[Class] == Fun) { *++e = JSR; *++e = d[Val]; } // User function call.
      else { printf("%d: bad function call\n", line); exit(-1); }
      if (t) { *++e = ADJ; *++e = t; } // Adjust stack for arguments.
      ty = d[Type]; // Return type of function.
    }
    else if (d[Class] == Num) { *++e = IMM; *++e = d[Val]; ty = INT; } // Enum constant.
    else { // Variable access.
      if (d[Class] == Loc) { *++e = LEA; *++e = loc - d[Val]; } // Local: load address relative to frame.
      else if (d[Class] == Glo) { *++e = IMM; *++e = d[Val]; } // Global: load address.
      else { printf("%d: undefined variable\n", line); exit(-1); }
      *++e = ((ty = d[Type]) == CHAR) ? LC : LI; // Load value based on type.
    }
  }
  else if (tk == '(') { // Parenthesized expression or cast.
    next();
    if (tk == Int || tk == Char) { // Type cast.
      t = (tk == Int) ? INT : CHAR; next();
      while (tk == Mul) { next(); t = t + PTR; } // Add pointer levels.
      if (tk == ')') next(); else { printf("%d: bad cast\n", line); exit(-1); }
      expr(Inc); // Parse expression to cast.
      ty = t; // Set resulting type.
    }
    else { // Parenthesized expression.
      expr(Assign); // Parse inner expression.
      if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
    }
  }
  else if (tk == Mul) { // Dereference (*ptr).
    next(); expr(Inc); // Parse pointer expression.
    if (ty > INT) ty = ty - PTR; else { printf("%d: bad dereference\n", line); exit(-1); } // Must be pointer.
    *++e = (ty == CHAR) ? LC : LI; // Load value based on type.
  }
  else if (tk == And) { // Address-of (&var).
    next(); expr(Inc); // Parse lvalue.
    if (*e == LC || *e == LI) --e; else { printf("%d: bad address-of\n", line); exit(-1); } // Must be load op.
    ty = ty + PTR; // Result is pointer to type.
  }
  else if (tk == '!') { // Logical not.
    next(); expr(Inc); *++e = PSH; *++e = IMM; *++e = 0; *++e = EQ; ty = INT; // !x becomes (x == 0).
  }
  else if (tk == '~') { // Bitwise not.
    next(); expr(Inc); *++e = PSH; *++e = IMM; *++e = -1; *++e = XOR; ty = INT; // ~x becomes (x ^ -1).
  }
  else if (tk == Add) { next(); expr(Inc); ty = INT; } // Unary plus (no-op).
  else if (tk == Sub) { // Unary minus.
    next(); *++e = IMM;
    if (tk == Num) { *++e = -ival; next(); } // Constant folding for literals.
    else { *++e = -1; *++e = PSH; expr(Inc); *++e = MUL; } // -x becomes (-1 * x).
    ty = INT;
  }
  else if (tk == Inc || tk == Dec) { // Pre-increment/decrement.
    t = tk; next(); expr(Inc);
    if (*e == LC) { *e = PSH; *++e = LC; } // Char: duplicate load.
    else if (*e == LI) { *e = PSH; *++e = LI; } // Int: duplicate load.
    else { printf("%d: bad lvalue in pre-increment\n", line); exit(-1); }
    *++e = PSH; *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char); // Increment size.
    *++e = (t == Inc) ? ADD : SUB; // Apply operation.
    *++e = (ty == CHAR) ? SC : SI; // Store result.
  }
  else { printf("%d: bad expression\n", line); exit(-1); }

  // Precedence climbing: handle binary operators based on precedence level.
  while (tk >= lev) {
    t = ty; // Save type for lvalue checks.
    if (tk == Assign) { // Assignment.
      next();
      if (*e == LC || *e == LI) *e = PSH; else { printf("%d: bad lvalue in assignment\n", line); exit(-1); }
      expr(Assign); *++e = ((ty = t) == CHAR) ? SC : SI; // Store value.
    }
    else if (tk == Cond) { // Ternary operator (cond ? true : false).
      next();
      *++e = BZ; d = ++e; // Branch if zero.
      expr(Assign); // True branch.
      if (tk == ':') next(); else { printf("%d: conditional missing colon\n", line); exit(-1); }
      *d = (int)(e + 3); *++e = JMP; d = ++e; // Jump past false branch.
      expr(Cond); // False branch.
      *d = (int)(e + 1); // Patch jump.
    }
    else if (tk == Lor) { next(); *++e = BNZ; d = ++e; expr(Lan); *d = (int)(e + 1); ty = INT; } // Short-circuit ||.
    else if (tk == Lan) { next(); *++e = BZ;  d = ++e; expr(Or);  *d = (int)(e + 1); ty = INT; } // Short-circuit &&.
    else if (tk == Or)  { next(); *++e = PSH; expr(Xor); *++e = OR;  ty = INT; } // Bitwise |.
    else if (tk == Xor) { next(); *++e = PSH; expr(And); *++e = XOR; ty = INT; } // Bitwise ^.
    else if (tk == And) { next(); *++e = PSH; expr(Eq);  *++e = AND; ty = INT; } // Bitwise &.
    else if (tk == Eq)  { next(); *++e = PSH; expr(Lt);  *++e = EQ;  ty = INT; } // ==.
    else if (tk == Ne)  { next(); *++e = PSH; expr(Lt);  *++e = NE;  ty = INT; } // !=.
    else if (tk == Lt)  { next(); *++e = PSH; expr(Shl); *++e = LT;  ty = INT; } // <.
    else if (tk == Gt)  { next(); *++e = PSH; expr(Shl); *++e = GT;  ty = INT; } // >.
    else if (tk == Le)  { next(); *++e = PSH; expr(Shl); *++e = LE;  ty = INT; } // <=.
    else if (tk == Ge)  { next(); *++e = PSH; expr(Shl); *++e = GE;  ty = INT; } // >=.
    else if (tk == Shl) { next(); *++e = PSH; expr(Add); *++e = SHL; ty = INT; } // <<.
    else if (tk == Shr) { next(); *++e = PSH; expr(Add); *++e = SHR; ty = INT; } // >>.
    else if (tk == Add) { // Addition or pointer arithmetic.
      next(); *++e = PSH; expr(Mul);
      if ((ty = t) > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL; } // Scale pointer.
      *++e = ADD;
    }
    else if (tk == Sub) { // Subtraction or pointer difference.
      next(); *++e = PSH; expr(Mul);
      if (t > PTR && t == ty) { *++e = SUB; *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = DIV; ty = INT; } // Pointer diff.
      else if ((ty = t) > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL; *++e = SUB; } // Pointer offset.
      else *++e = SUB; // Regular subtraction.
    }
    else if (tk == Mul) { next(); *++e = PSH; expr(Inc); *++e = MUL; ty = INT; } // Multiplication.
    else if (tk == Div) { next(); *++e = PSH; expr(Inc); *++e = DIV; ty = INT; } // Division.
    else if (tk == Mod) { next(); *++e = PSH; expr(Inc); *++e = MOD; ty = INT; } // Modulus.
    else if (tk == Inc || tk == Dec) { // Post-increment/decrement.
      if (*e == LC) { *e = PSH; *++e = LC; } else if (*e == LI) { *e = PSH; *++e = LI; }
      else { printf("%d: bad lvalue in post-increment\n", line); exit(-1); }
      *++e = PSH; *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);
      *++e = (tk == Inc) ? ADD : SUB; *++e = (ty == CHAR) ? SC : SI; // Perform operation.
      *++e = PSH; *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);
      *++e = (tk == Inc) ? SUB : ADD; // Restore original value for expression.
      next();
    }
    else if (tk == Brak) { // Array indexing (arr[i]).
      next(); *++e = PSH; expr(Assign); // Index expression.
      if (tk == ']') next(); else { printf("%d: close bracket expected\n", line); exit(-1); }
      if (t > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL; } // Scale index.
      else if (t < PTR) { printf("%d: pointer type expected\n", line); exit(-1); }
      *++e = ADD; *++e = ((ty = t - PTR) == CHAR) ? LC : LI; // Compute address and load.
    }
    else { printf("%d: compiler error tk=%d\n", line, tk); exit(-1); } // Unknown token.
  }
}

/* Statement Parser: stmt() - Parses statements (if, while, return, etc.) */
void stmt()
{
  int *a, *b; // Pointers for jump target patching.

  if (tk == If) { // If statement.
    next();
    if (tk == '(') next(); else { printf("%d: open paren expected\n", line); exit(-1); }
    expr(Assign); // Condition.
    if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
    *++e = BZ; b = ++e; // Branch if zero (false).
    stmt(); // True branch.
    if (tk == Else) { // Optional else clause.
      *b = (int)(e + 3); *++e = JMP; b = ++e; // Skip else if true.
      next();
      stmt(); // Else branch.
    }
    *b = (int)(e + 1); // Patch jump target.
  }
  else if (tk == While) { // While loop.
    next();
    a = e + 1; // Loop start address.
    if (tk == '(') next(); else { printf("%d: open paren expected\n", line); exit(-1); }
    expr(Assign); // Condition.
    if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
    *++e = BZ; b = ++e; // Exit if false.
    stmt(); // Loop body.
    *++e = JMP; *++e = (int)a; // Jump back to start.
    *b = (int)(e + 1); // Patch exit target.
  }
  else if (tk == Return) { // Return statement.
    next();
    if (tk != ';') expr(Assign); // Optional return value.
    *++e = LEV; // Leave function.
    if (tk == ';') next(); else { printf("%d: semicolon expected\n", line); exit(-1); }
  }
  else if (tk == '{') { // Compound statement.
    next();
    while (tk != '}') stmt(); // Parse nested statements.
    next();
  }
  else if (tk == ';') { // Empty statement.
    next();
  }
  else { // Expression statement.
    expr(Assign);
    if (tk == ';') next(); else { printf("%d: semicolon expected\n", line); exit(-1); }
  }
}

/* Main Function: Compiler entry point and VM runtime */
int main(int argc, char **argv)
{
  int fd, bt, ty, poolsz, *idmain; // File descriptor, base type, type, pool size, main function pointer.
  int *pc, *sp, *bp, a, cycle; // VM registers: program counter, stack pointer, base pointer, accumulator, cycle count.
  int i, *t; // Temporary variables.

  // Parse command-line arguments.
  --argc; ++argv; // Skip program name.
  if (argc > 0 && **argv == '-' && (*argv)[1] == 's') { src = 1; --argc; ++argv; } // -s: print source and assembly.
  if (argc > 0 && **argv == '-' && (*argv)[1] == 'd') { debug = 1; --argc; ++argv; } // -d: debug VM execution.
  if (argc < 1) { printf("usage: c4 [-s] [-d] file ...\n"); return -1; }

  // Open source file.
  if ((fd = open(*argv, 0)) < 0) { printf("could not open(%s)\n", *argv); return -1; }

  poolsz = 256*1024; // Arbitrary size for memory pools.
  // Allocate memory for symbol table, code, data, and stack.
  if (!(sym = malloc(poolsz))) { printf("could not malloc(%d) symbol area\n", poolsz); return -1; }
  if (!(le = e = malloc(poolsz))) { printf("could not malloc(%d) text area\n", poolsz); return -1; }
  if (!(data = malloc(poolsz))) { printf("could not malloc(%d) data area\n", poolsz); return -1; }
  if (!(sp = malloc(poolsz))) { printf("could not malloc(%d) stack area\n", poolsz); return -1; }

  // Initialize memory pools to zero.
  memset(sym,  0, poolsz);
  memset(e,    0, poolsz);
  memset(data, 0, poolsz);

  // Initialize symbol table with keywords and system calls.
  p = "char else enum if int return sizeof while "
      "open read close printf malloc free memset memcmp exit void main";
  i = Char; while (i <= While) { next(); id[Tk] = i++; } // Keywords.
  i = OPEN; while (i <= EXIT) { next(); id[Class] = Sys; id[Type] = INT; id[Val] = i++; } // System calls.
  next(); id[Tk] = Char; // Handle 'void' as alias for char (minimal type system).
  next(); idmain = id; // Save main function’s symbol table entry.

  // Load source code into memory.
  if (!(lp = p = malloc(poolsz))) { printf("could not malloc(%d) source area\n", poolsz); return -1; }
  if ((i = read(fd, p, poolsz-1)) <= 0) { printf("read() returned %d\n", i); return -1; }
  p[i] = 0; // Null-terminate source.
  close(fd);

  // Parse declarations (globals, functions, enums).
  line = 1;
  next();
  while (tk) {
    bt = INT; // Default base type.
    if (tk == Int) next(); // int type.
    else if (tk == Char) { next(); bt = CHAR; } // char type.
    else if (tk == Enum) { // Enum declaration.
      next();
      if (tk != '{') next(); // Optional enum name (ignored).
      if (tk == '{') {
        next();
        i = 0; // Enum value counter.
        while (tk != '}') {
          if (tk != Id) { printf("%d: bad enum identifier %d\n", line, tk); return -1; }
          next();
          if (tk == Assign) { // Explicit value.
            next();
            if (tk != Num) { printf("%d: bad enum initializer\n", line); return -1; }
            i = ival; next();
          }
          id[Class] = Num; id[Type] = INT; id[Val] = i++; // Add enum constant.
          if (tk == ',') next();
        }
        next();
      }
    }
    while (tk != ';' && tk != '}') { // Parse multiple declarations.
      ty = bt;
      while (tk == Mul) { next(); ty = ty + PTR; } // Pointers.
      if (tk != Id) { printf("%d: bad global declaration\n", line); return -1; }
      if (id[Class]) { printf("%d: duplicate global definition\n", line); return -1; }
      next();
      id[Type] = ty;
      if (tk == '(') { // Function definition.
        id[Class] = Fun;
        id[Val] = (int)(e + 1); // Store function start address.
        next(); i = 0; // Parameter count.
        while (tk != ')') { // Parse parameters.
          ty = INT;
          if (tk == Int) next(); else if (tk == Char) { next(); ty = CHAR; }
          while (tk == Mul) { next(); ty = ty + PTR; }
          if (tk != Id) { printf("%d: bad parameter declaration\n", line); return -1ettamente

          if (id[Class] == Loc) { printf("%d: duplicate parameter definition\n", line); return -1; }
          // Preserve existing symbol table entry for shadowing.
          id[HClass] = id[Class]; id[Class] = Loc;
          id[HType] = id[Type]; id[Type] = ty;
          id[HVal] = id[Val]; id[Val] = i++;
          next();
          if (tk == ',') next();
        }
        next();
        if (tk != '{') { printf("%d: bad function definition\n", line); return -1; }
        loc = ++i; // Reserve stack space for parameters.
        next();
        // Parse local variable declarations.
        while (tk == Int || tk == Char) {
          bt = (tk == Int) ? INT : CHAR; next();
          while (tk != ';') {
            ty = bt;
            while (tk == Mul) { next(); ty = ty + PTR; }
            if (tk != Id) { printf("%d: bad local declaration\n", line); return -1; }
            if (id[Class] == Loc) { printf("%d: duplicate local definition\n", line); return -1; }
            id[HClass] = id[Class]; id[Class] = Loc;
            id[HType] = id[Type]; id[Type] = ty;
            id[HVal] = id[Val]; id[Val] = ++i;
            next();
            if (tk == ',') next();
          }
          next();
        }
        *++e = ENT; *++e = i - loc; // Enter function, reserve stack space.
        while (tk != '}') stmt(); // Parse function body.
        *++e = LEV; // Leave function.
        // Unwind local variables from symbol table.
        id = sym;
        while (id[Tk]) {
          if (id[Class] == Loc) {
            id[Class] = id[HClass];
            id[Type] = id[HType];
            id[Val] = id[HVal];
          }
          id = id + Idsz;
        }
      }
      else { // Global variable.
        id[Class] = Glo;
        id[Val] = (int)data; // Allocate space in data segment.
        data = data + sizeof(int);
      }
      if (tk == ',') next();
    }
    next();
  }

  // Locate main() function.
  if (!(pc = (int *)idmain[Val])) { printf("main() not defined\n"); return -1; }
  if (src) return 0; // Exit after printing source if -s flag is set.

  // Setup VM stack.
  bp = sp = (int *)((int)sp + poolsz); // Stack grows downward.
  *--sp = EXIT; // Call exit() when main returns.
  *--sp = PSH; t = sp;
  *--sp = argc; // Pass command-line arguments to main().
  *--sp = (int)argv;
  *--sp = (int)t;

  // VM execution loop.
  cycle = 0;
  while (1) {
    i = *pc++; ++cycle; // Fetch and increment cycle count.
    if (debug) { // Print instruction if -d flag is set.
      printf("%d> %.4s", cycle,
        &"LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,"
         "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,"
         "OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT,"[i * 5]);
      if (i <= ADJ) printf(" %d\n", *pc); else printf("\n");
    }
    // Execute instruction.
    if      (i == LEA) a = (int)(bp + *pc++); // Load local address relative to base pointer.
    else if (i == IMM) a = *pc++; // Load immediate value or global address.
    else if (i == JMP) pc = (int *)*pc; // Jump to address.
    else if (i == JSR) { *--sp = (int)(pc + 1); pc = (int *)*pc; } // Call subroutine, save return address.
    else if (i == BZ)  pc = a ? pc + 1 : (int *)*pc; // Branch if zero.
    else if (i == BNZ) pc = a ? (int *)*pc : pc + 1; // Branch if not zero.
    else if (i == ENT) { *--sp = (int)bp; bp = sp; sp = sp - *pc++; } // Enter function, save old bp.
    else if (i == ADJ) sp = sp + *pc++; // Adjust stack after function call.
    else if (i == LEV) { sp = bp; bp = (int *)*sp++; pc = (int *)*sp++; } // Leave function, restore stack.
    else if (i == LI)  a = *(int *)a; // Load int from address.
    else if (i == LC)  a = *(char *)a; // Load char from address.
    else if (i == SI)  *(int *)*sp++ = a; // Store int to address.
    else if (i == SC)  a = *(char *)*sp++ = a; // Store char to address, return value.
    else if (i == PSH) *--sp = a; // Push value onto stack.

    // Arithmetic and logical operations.
    else if (i == OR)  a = *sp++ |  a;
    else if (i == XOR) a = *sp++ ^  a;
    else if (i == AND) a = *sp++ &  a;
    else if (i == EQ)  a = *sp++ == a;
    else if (i == NE)  a = *sp++ != a;
    else if (i == LT)  a = *sp++ <  a;
    else if (i == GT)  a = *sp++ >  a;
    else if (i == LE)  a = *sp++ <= a;
    else if (i == GE)  a = *sp++ >= a;
    else if (i == SHL) a = *sp++ << a;
    else if (i == SHR) a = *sp++ >> a;
    else if (i == ADD) a = *sp++ +  a;
    else if (i == SUB) a = *sp++ -  a;
    else if (i == MUL) a = *sp++ *  a;
    else if (i == DIV) a = *sp++ /  a;
    else if (i == MOD) a = *sp++ %  a;

    // System calls.
    else if (i == OPEN) a = open((char *)sp[1], *sp); // Open file.
    else if (i == READ) a = read(sp[2], (char *)sp[1], *sp); // Read from file.
    else if (i == CLOS) a = close(*sp); // Close file.
    else if (i == PRTF) { t = sp + pc[1]; a = printf((char *)t[-1], t[-2], t[-3], t[-4], t[-5], t[-6]); } // Printf with up to 6 args.
    else if (i == MALC) a = (int)malloc(*sp); // Allocate memory.
    else if (i == FREE) free((void *)*sp); // Free memory.
    else if (i == MSET) a = (int)memset((char *)sp[2], sp[1], *sp); // Set memory.
    else if (i == MCMP) a = memcmp((char *)sp[2], (char *)sp[1], *sp); // Compare memory.
    else if (i == EXIT) { printf("exit(%d) cycle = %d\n", *sp, cycle); return *sp; } // Exit program.
    else { printf("unknown instruction = %d! cycle = %d\n", i, cycle); return -1; } // Error.
  }
}