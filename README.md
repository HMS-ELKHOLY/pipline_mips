# Pipelined MIPS Processor with SystemVerilog

A complete implementation of a pipelined MIPS processor with integrated Floating Point Unit (FPU) written in SystemVerilog. This project demonstrates advanced computer architecture concepts including instruction pipelining, hazard detection and resolution, and IEEE 754 floating-point arithmetic.

## 📋 Table of Contents

- [Overview](#overview)
- [Team Members](#team-members)
- [Architecture](#architecture)
- [Features](#features)
- [Module Documentation](#module-documentation)
- [File Structure](#file-structure)
- [Supported Instructions](#supported-instructions)
- [Testing](#testing)
- [Usage](#usage)
- [Build Instructions](#build-instructions)

## 🎯 Overview

This project implements a pipelined MIPS processor that supports both integer and floating-point operations. The processor features a classic 5-stage pipeline (Fetch, Decode, Execute, Memory, Writeback) with hazard detection and forwarding mechanisms. The integrated FPU supports single-precision IEEE 754 floating-point arithmetic operations.

**Course:** Computer Architecture  
**Instructor:** Dr. Mohamed Mahmoud Ibrahim

## 👥 Team Members

- **Abdullah Khaled Kamal El Sayed Ali Siam**
- **Abdullah Mohammed Abu Almajd Ali Mohamed Albasony**
- **Abdulrahman Rajab Hashim Ismail**
- **Assem Hossam Mahmoud**
- **Eman Mahmoud Rashwan Rashed**
- **Hussein Mostafa Said Elkholy**
- **Mahmoud Hassan Mohamed Mahmoud**
- **Osama Mohamed Abdel Tawab Ramadan**
- **Saleh Mahmoud Saleh Abdullah Mohammed**
- **Zaghlola Atef Abdel Mawla Abdel Wahab**

## 🏗️ Architecture

The processor implements a 5-stage pipeline with the following stages:

1. **Fetch (F):** Instruction fetch from memory
2. **Decode (D):** Instruction decode and register file read
3. **Execute (E):** ALU operations and FPU operations
4. **Memory (M):** Data memory access
5. **Writeback (W):** Register file write

### Key Components

- **Controller:** Manages all control signals and instruction decoding
- **Datapath:** Implements the pipeline stages and data flow
- **ALU:** Handles integer arithmetic and logic operations
- **FPU:** Dedicated floating-point unit for IEEE 754 operations
- **Hazard Unit:** Detects and resolves pipeline hazards
- **Forwarding Unit:** Implements data forwarding to reduce stalls

## ✨ Features

### Integer Operations
- ✅ Basic arithmetic (ADD, SUB, ADDI, ADDIU)
- ✅ Logical operations (AND, OR, XOR, ANDI, ORI)
- ✅ Shift operations (SLL, SRL, SRA, SLLV)
- ✅ Comparison operations (SLT, SLTI)
- ✅ Load/Store operations with multiple data sizes
- ✅ Branch instructions (BEQ, BNE, BLEZ, BGTZ)
- ✅ Jump instructions (J, JAL, JR, JALR)

### Memory Operations
- ✅ Load Word (LW)
- ✅ Load Halfword (LH, LHU)
- ✅ Load Byte (LB, LBU)
- ✅ Store Word (SW)
- ✅ Store Halfword (SH)
- ✅ Store Byte (SB)

### Floating-Point Operations
- ✅ Single-precision addition (ADD.S)
- ✅ Single-precision subtraction (SUB.S)
- ✅ Single-precision multiplication (MUL.S)
- ✅ Single-precision division (DIV.S)
- ✅ Floating-point negation (NEG.S)
- ✅ Floating-point absolute value (ABS.S)
- ✅ Load/Store floating-point (LWC1, SWC1)

### Advanced Features
- ✅ Pipeline hazard detection and resolution
- ✅ Data forwarding mechanisms
- ✅ Branch prediction
- ✅ Load-use hazard handling
- ✅ IEEE 754 compliance for FPU operations

## 📁 File Structure

```
pipline_mips/
├── README.md                              # This file
├── doc.pdf                               # Detailed technical documentation
├── src/
│   └── FPUintegration/
│       ├── mipspipline.sv                # Top-level MIPS processor module
│       ├── fpu.sv                        # Floating Point Unit
│       ├── adder_floating_point.sv       # FPU addition/subtraction
│       ├── multi.sv                      # FPU multiplication
│       ├── FPU_division.sv              # FPU division
│       └── normalization_machine.sv      # FPU normalization
├── testbenches/
│   ├── testbench__lb.sv                 # Load byte testbench
│   ├── testbench__lh.sv                 # Load halfword testbench
│   ├── testbench__lhu.sv                # Load halfword unsigned testbench
│   ├── testbench__lbu.sv                # Load byte unsigned testbench
│   ├── testbench__sb.sv                 # Store byte testbench
│   └── testbench__sh.sv                 # Store halfword testbench
└── memfiles/
    ├── memfile.dat                       # Main memory initialization
    ├── memfile__bgtz__blez.dat          # Branch instruction tests
    ├── memfile__lhu.dat                 # Load halfword unsigned tests
    ├── memfile__ori.dat                 # OR immediate tests
    ├── memfile__sh.dat                  # Store halfword tests
    ├── memfile__jalr.dat                # Jump and link register tests
    ├── memfile__jr.dat                  # Jump register tests
    ├── memfile__lh.dat                  # Load halfword tests
    └── memfile__shift__andi.dat         # Shift and AND immediate tests
```

## 📚 Module Documentation

### Core Modules

#### `mips` (mipspipline.sv)
**Top-level processor module**
- **Inputs:** `clk`, `reset`, `instrF`, `readdataM`
- **Outputs:** `pcF`, `memwriteM`, `aluoutM`, `final_writedata`
- **Description:** Integrates the controller and datapath, manages the complete processor operation

#### `controller` (mipspipline.sv)
**Control unit for instruction decoding**
- **Function:** Decodes instructions and generates control signals for all pipeline stages
- **Features:** Handles both integer and floating-point instruction control

#### `fpu` (fpu.sv)
**Floating Point Unit**
- **Inputs:** `clk`, `funct[3:0]`, `a[31:0]`, `b[31:0]`
- **Outputs:** `o[31:0]`
- **Description:** Implements IEEE 754 single-precision floating-point operations

### FPU Sub-modules

#### `adder_floating_point` (adder_floating_point.sv)
**FPU Addition/Subtraction Unit**
- **Description:** Handles floating-point addition and subtraction with proper IEEE 754 compliance

#### `multi` (multi.sv)
**FPU Multiplication Unit**
- **Inputs:** `clk`, `a[31:0]`, `b[31:0]`
- **Outputs:** `op[31:0]`, `finish`
- **Description:** Implements floating-point multiplication

#### `FPU_division` (FPU_division.sv)
**FPU Division Unit**
- **Description:** Handles floating-point division operations

#### `normalization_machine` (normalization_machine.sv)
**FPU Normalization Unit**
- **Inputs:** `fraction`, `exponent`
- **Outputs:** `n_fraction`, `n_exponenet`, `overflow`, `underflow`, `done`
- **Description:** Normalizes floating-point results according to IEEE 754 standard

## 🛠️ Supported Instructions

### Integer Instructions
| Instruction | Format | Description |
|-------------|--------|-------------|
| `ADD` | R-type | Add registers |
| `ADDI` | I-type | Add immediate |
| `ADDIU` | I-type | Add immediate unsigned |
| `SUB` | R-type | Subtract registers |
| `AND` | R-type | Bitwise AND |
| `ANDI` | I-type | Bitwise AND immediate |
| `OR` | R-type | Bitwise OR |
| `ORI` | I-type | Bitwise OR immediate |
| `SLL` | R-type | Shift left logical |
| `SLLV` | R-type | Shift left logical variable |
| `SRL` | R-type | Shift right logical |
| `SRA` | R-type | Shift right arithmetic |
| `BEQ` | I-type | Branch if equal |
| `BNE` | I-type | Branch if not equal |
| `BLEZ` | I-type | Branch if less than or equal zero |
| `BGTZ` | I-type | Branch if greater than zero |
| `J` | J-type | Jump |
| `JAL` | J-type | Jump and link |
| `JR` | R-type | Jump register |
| `JALR` | R-type | Jump and link register |

### Memory Instructions
| Instruction | Description |
|-------------|-------------|
| `LW` | Load word |
| `LH` | Load halfword (signed) |
| `LHU` | Load halfword unsigned |
| `LB` | Load byte (signed) |
| `LBU` | Load byte unsigned |
| `SW` | Store word |
| `SH` | Store halfword |
| `SB` | Store byte |

### Floating-Point Instructions
| Instruction | Description |
|-------------|-------------|
| `ADD.S` | Single-precision addition |
| `SUB.S` | Single-precision subtraction |
| `MUL.S` | Single-precision multiplication |
| `DIV.S` | Single-precision division |
| `NEG.S` | Single-precision negation |
| `ABS.S` | Single-precision absolute value |
| `LWC1` | Load word to FPU |
| `SWC1` | Store word from FPU |

## 🧪 Testing

The project includes comprehensive testbenches for various instruction types:

- **Load/Store Testing:** Separate testbenches for each memory operation type
- **Branch Testing:** Validation of branch instruction behavior
- **Arithmetic Testing:** Integer and floating-point operation verification
- **Pipeline Testing:** Hazard detection and forwarding validation

### Running Tests

Each testbench can be simulated using your preferred SystemVerilog simulator:

```bash
# Example with ModelSim/QuestaSim
vsim -do "run -all" testbench__lb

# Example with Vivado
xvlog testbench__lb.sv
xelab testbench__lb
xsim testbench__lb -R
```

## 🚀 Usage

1. **Initialize Memory:** Load your program into the appropriate memory file (.dat format)
2. **Configure Testbench:** Set up the testbench with proper reset and clock signals
3. **Run Simulation:** Execute the simulation using your SystemVerilog simulator
4. **Monitor Results:** Check output signals and memory contents for verification

## 🔨 Build Instructions

### Prerequisites
- SystemVerilog-compatible simulator (ModelSim, Vivado, Verilator, etc.)
- Basic understanding of MIPS assembly language

### Compilation Steps

1. **Clone the repository:**
   ```bash
   git clone https://github.com/HMS-ELKHOLY/pipline_mips.git
   cd pipline_mips
   ```

2. **Compile the design:**
   ```bash
   # For ModelSim/QuestaSim
   vlog src/FPUintegration/*.sv
   
   # For Vivado
   xvlog src/FPUintegration/*.sv
   ```

3. **Run simulation:**
   ```bash
   # Load testbench and run
   vsim top_module
   run -all
   ```

### Memory File Format
Memory files use hexadecimal format with one 32-bit instruction per line:
```
20020005   # addi $2, $0, 5
20030007   # addi $3, $0, 7
00430820   # add $1, $2, $3
...
```

## 📖 Documentation

For detailed technical documentation, refer to `doc.pdf` which contains:
- Comprehensive instruction implementation details
- Pipeline architecture diagrams
- Control signal specifications
- FPU operation details
- Hazard detection and resolution mechanisms

## 🤝 Contributing

This project was developed as part of a computer architecture course. For educational purposes and improvements, please follow standard contribution guidelines.

## 📄 License

This project is developed for educational purposes. Please refer to your institution's academic policies regarding code sharing and collaboration.

---

*For more detailed technical information, please refer to the documentation in `doc.pdf`.*

