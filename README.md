# entwurf_vhdl_prozessor
## General Information
1. Vergebene Dateinamen in Textfile eintragen
2. Alles Kommentieren!
3. Alle Subtyps in Subtype-Package-File schreiben
4. Dateinamen wie folgt: Typ_Name_Kürzel.vhdl	(**Typ**: pack, packbod, arch, en; **Name**: cpu,...; **Kürzel**: wp, mf, up, zl)

## CPU Spec
1. External Interface
	1. Generic Parameters
    	1. The data width is generic with a minimum of 12 bit.	(data_width : positive)
    	2. The address width is generic with a minimum of 12 bit which implies a total of 2^(address_width : positive) addressable memory cells.
  2. Ports
    1. A clock input (processor is triggered on the positive clock edge).	(clk:bit)
    2. A reset input (asynchronous, active low).	(res:bit)
    3. A data bus from memory or I/O-devices to processor, size according to 1.1.1. (dbus_m_io : positive)
    4. A data bus from processor to memory, size according to 1.1.1. 		(dbus_mem : positive)
    5. An address bus from processor to memory, size according to 1.1.2.	(abus_mem : positive)
    6. A 1-Bit control signal to switch between data from memory (if ‘0’) and I/O-devices (if ‘1’).	(c_m_io:bit)
    7. A 1-Bit control signal to indicate write-access to the memory (active high).		(c_wa:bit)
    8. A 1-Bit control signal to indicate access to the I/O-devices (active high).		(c_io:bit)
    9. A 1-Bit control signal to distinguish between read- (if ‘0’) and write-access (if ‘1’) to the I/O-devices. 	(d_io _ bit)
    10. A 1-Bit control signal to indicate if the addressed I/O-device is ready to send/receive data to/from the processor (active high). 	(i_sr:bit)

2. Structural Description
  1. Internal Registers
    1. Code and Data are accessed in the same address space (“Von Neumann” Architecture)
    2. Four data registers number 
      1. The size of these registers is based on the general data width (according to 1.1.1).
      2. The initial value of these registers is 0x0.
      3. The registers contain operands and results from operations; there is no restriction regarding the use of registers.
      4. The content of all registers not currently used as destination of an instruction won’t change their value.
    3. Program Counter (PC)
      1. The size of the PC is based on the general address width (according to 1.1.2).
      2. The initial value of the PC is 0x0.
      3. After an instruction has been fetched from the code memory, PC is incremented by one and thus contains the address of the next instruction to be fetched.
      4. After the PC reaches its maximum value (2^(address width) – 1), the next value is 0x0.
  2. System Reset
    1. A low level on the reset input of the processor causes the system to go in a defined state (asynchronous reset, active low).
    2. Data registers and PC return to their initial states (according to points 2.1.2.2 and 2.1.3.2).
    3. The internal flags return to their initial state (according to points 3.2.4.2, 3.2.5.7, 3.2.6.3 and 3.2.7.4).

3. Instructions
  1. General Issues
    1. All bits of an instruction following the OpCode (size 6 bit) and the necessary parameters to the end of the instruction are ‘don’t care’.
    2. The instructions are distributed as follows (see point 3.3 for details):
      1. Miscellaneous Instructions (2): NOP, STOP.
      2. Arithmetic Instructions (4): ADD, ADDC, SUB, SUBC.
      3. Logical Instructions (7): NOT, AND, OR, XOR, REA, REO, REX.
      4. Shift / Rotate Instructions (7): SLL, SRL, SRA, ROL, ROLC, ROR, RORC.
      5. Memory Access Instructions (5): LDC, LDD, LDR, STD, STR.
      6. I/O Instructions (2): IN, OUT.
      7. Jump Instructions (9): JMP, JZ, JC, JN, JO, JNZ, JNC, JNN, JNO.
  2. Internal Flags
    1. In order to store particular results of some operations, there are 4 internal Flags: Zero, Carry, Negative and Overflow.
    2. These flags are bits that can contain the values ‘0’ (the flag is cleared) or ‘1’ (the flag is set).
    3. The flags are only affected when a data word is written into a register (that means by the following instructions: ADD, ADDC, SUB, SUBC, NOT, AND, OR, XOR, REA, REO, REX, SLL, SRL, SRA, ROL, ROLC, ROR, RORC, LDC, LDD, LDR, IN).
    4. Zero Flag
      1. When executing one of the instructions listed in 3.2.3, the result is evaluated; if the value equals 0x0, the zero flag is set, otherwise it is cleared.
      2. The initial value of the zero flag is ‘0’.
    5. Carry Flag
      1. The carry flag is cleared by the following instructions: NOT, AND, OR, XOR, REA, REO, REX, ROL, ROR.
      2. When executing the ADD or ADDC instruction, the carry output of an unsigned addition of the two source operands is assigned to the carry flag.
      3. When executing the SUB or SUBC instruction, the carry output of an unsigned addition of the first source operand and the two’s complement of the second source operand is assigned to the carry flag.
      4. When executing the SLL or ROLC instruction, the most significant bit of the source operand is assigned to the carry flag.
      5. When executing the SRL, SRA or RORC instruction, the least significant bit of the source operand is assigned to the carry flag.
      6. LDC, LDD, LDR and IN don’t affect the carry flag.
      7. The initial value of the carry flag is ‘0’.
    6. Negative Flag
      1. When executing the ADD, ADDC, SUB or SUBC instruction, the source operands are sign-extended by one bit; the additional bit of the result is assigned to the negative flag.
      2. When executing one of the remaining instructions listed in 3.2.3, the most significant bit of the result is assigned to the negative flag.
      3. The initial value of the negative flag is ‘0’.
    7. Overflow Flag
      1. When executing the ADD, ADDC, SUB or SUBC instruction, the source operands are sign-extended by one bit; if the additional bit of the result differs from the (original) most significant bit of the result, the overflow flag is set, otherwise it is cleared.
      2. When executing the SLL instruction, the overflow flag is set if the most significant bit of the source operand differs from the most significant bit of the result, otherwise it is cleared.
      3. When executing one of the remaining instructions listed in 3.2.3, the overflow flag is cleared.
      4. The initial value of the overflow flag is ‘0’.
  3. Detailed Instruction Set
    1. The following table presents the detailed instruction set of the processor, the syntax and the function that must be realized for each instruction: Look up table in Email
    2. The instruction word is composed of the 6 bit OpCode and following 6 bitsreserved for destination and source register specification (as shown in the table above; if only one register has to be specified, it has to be in the first position). In case of a data width larger than 12 the bits following the 12 bit instruction word are ‘don’t care’.
    3. The processor is able to work with unsigned and signed numbers. The carry flag is always computed interpreting the operands as unsigned, while the negative and overflow flags are always computed interpreting the operands as signed.
    4. For REA, REO and REX operations the result is stored in the least significant bit while all other bits of the destination register are cleared.
    5. When an unknown OpCode is decoded, it is treated as a NOP instruction and the user is informed of the illegal OpCode by an assertion.
    6. When a STOP instruction is decoded the processor stops fetching new instructions and reaches an explicit deadlock state. In this state there are no changes to any registers (data registers, flags, PC) and no communication to memory or I/O devices.
  4. I/O-Instructions
    The IN instruction causes the input-device to send the content of its data register to the processor if the device is ready; if the device is not ready, the processor is blocked until the input-device is ready again. The OUT instruction causes the output-device to receive a data word from the processor and store it in its data register if the device is ready; if the device is not ready, the processor is blocked until the output-device is ready again.
  5. I/O-Devices
    1. General Structure All I/O-devices contain two registers, one register for storing a data word (size according to 1.1.1), and a one-bit registers storing the information if the data register is empty or not.
    2. The external interface of the I/O-devices is composed of:
    	1. A Clock input (I/O-devices are triggered on the positive clock edge).
			2. A Reset input (asynchronous, active low).
			3. A control signal to indicate if the I/O-device is ready to send/receive data to/from the processor (active high).
			4. A control signal to indicate access to the I/O-device (active high).
			5. A control signal to distinguish between read- (if ‘0’) and write-access (if ‘1’) to the I/O-device.
			6. A data bus from the processor (only output device, size according to 1.1.1).
			7. A data bus to the outside (only output device; size according to 1.1.1).
			8. A data ready signal to the outside (only output device).
			9. A data request signal from the outside (only output device).
			10. A data bus to the processor (only input device, size according to 1.1.1).
			11. A data bus from the outside (only input device; size according to 1.1.1).
			12. A data ready signal from the outside (only input device).
			13. A data request signal to the outside (only input device).
    3. Input device: The device includes a register for storing a data word from the outside bus to be sent to the processor. If the internal register contains data, the DevRdy signal of this device has to be set (since the device is now ready to send this data to the processor); otherwise it has to be cleared. If the AccEn input is set, the AccType input is set to ‘0’ and the internal register contains data, this data has to be made visible at the port DataOut; in all other cases DataOut has to be set to 0x0. If the internal register is empty, the DataReq signal of this device has to be set; otherwise it has to be cleared. If the DataRdy input is set and the internal register is empty, the data at the port DataIn has to be stored in the internal register; in all other content of the internal register must not change.
    4. Output device: The device includes a register for storing a data word from the processor to be sent to the outside bus. If the internal register is empty, the DevRdy signal of this device has to be set (since the device is now ready to receive data from the processor); otherwise it has to be cleared. If the AccEn input is set, the AccType input is set to ‘1’ and the internal register is empty, the data at the port DataIn has to be stored in the internal register; in all other cases the content of the internal register must not change. If the DataReq input is set and the internal register contains data, this data has to be made visible at the port DataOut and DataRdy has to be set; in all other cases DataOut has to be set to 0x0 and DataRdy has to be cleared.
