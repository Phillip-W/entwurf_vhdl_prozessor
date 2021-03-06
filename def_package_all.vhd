LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;
USE std.textio.ALL;

PACKAGE def_package_all IS
	-- ===============================================================================================================================================
	--Files
	-- ===============================================================================================================================================

	FILE IOOutputFile : Text IS OUT "IOOutput.txt";
	FILE IOInputFile : Text IS IN "IOInput.txt";
	FILE DumpFile : Text IS OUT "Dump.txt";

	-- ===============================================================================================================================================
	--Hier alle Konstanten definieren
	-- ===============================================================================================================================================

	CONSTANT data_width : POSITIVE := 12; --1.1.1
	CONSTANT address_width : POSITIVE := 12; --1.1.2
	CONSTANT opcode_width : POSITIVE := 6; --3.1.1

	CONSTANT reg_addr_width : POSITIVE := 2; --2.1.2
	CONSTANT addr_width : POSITIVE := 12; -- nicht Aendern
	-- ===============================================================================================================================================
	--Hier alle Datentypen, Subtyps definieren
	-- ===============================================================================================================================================

	SUBTYPE addr_type IS -- unsers Adressen zum ansprechen des Speichers, z.B. durch den PC
	NATURAL RANGE 0 TO 2 ** address_width - 1;
	SUBTYPE opcode_type IS -- fuer unsere OPCode Deklarationen
	NATURAL RANGE 0 TO 2 ** opcode_width - 1;
	SUBTYPE reg_addr_type IS -- zum Ansprechen unserer Register
	NATURAL RANGE 0 TO 2 ** reg_addr_width - 1;
	SUBTYPE data_type IS -- Typ, wie unsere Anweisungen, etc. im Speicher abgelegt werden
	NATURAL RANGE 0 TO 2 ** data_width - 1;
	TYPE mem_type IS -- unser "Speicher" (Array)
	ARRAY(addr_type) OF data_type;
	TYPE reg_type IS -- 2.1.2.1 unsere "Register" (Array)
	ARRAY(reg_addr_type) OF data_type;

	-- ===============================================================================================================================================
	--Hier alle OPCodes definieren (vollstaendig)
	-- ===============================================================================================================================================

	CONSTANT code_nop : opcode_type := 0; --3.3.1.1
	CONSTANT code_stop : opcode_type := 1; --3.3.1.2
	CONSTANT code_add : opcode_type := 2; --3.3.1.3
	CONSTANT code_addc : opcode_type := 3; --3.3.1.4
	CONSTANT code_sub : opcode_type := 4; --3.3.1.5
	CONSTANT code_subc : opcode_type := 5; --3.3.1.6
	CONSTANT code_not : opcode_type := 6; --3.3.1.7
	CONSTANT code_and : opcode_type := 7; --3.3.1.8
	CONSTANT code_or : opcode_type := 8; --3.3.1.9
	CONSTANT code_xor : opcode_type := 9; --3.3.1.10
	CONSTANT code_rea : opcode_type := 10; --3.3.1.11
	CONSTANT code_reo : opcode_type := 11; --3.3.1.12
	CONSTANT code_rex : opcode_type := 12; --3.3.1.13
	CONSTANT code_sll : opcode_type := 13; --3.3.1.14
	CONSTANT code_srl : opcode_type := 14; --3.3.1.15
	CONSTANT code_sra : opcode_type := 15; --3.3.1.16
	CONSTANT code_rol : opcode_type := 16; --3.3.1.17
	CONSTANT code_rolc : opcode_type := 17; --3.3.1.18
	CONSTANT code_ror : opcode_type := 18; --3.3.1.19
	CONSTANT code_rorc : opcode_type := 19; --3.3.1.20
	CONSTANT code_ldc : opcode_type := 32; --3.3.1.21
	CONSTANT code_ldd : opcode_type := 33; --3.3.1.22
	CONSTANT code_ldr : opcode_type := 34; --3.3.1.23
	CONSTANT code_std : opcode_type := 35; --3.3.1.24
	CONSTANT code_str : opcode_type := 36; --3.3.1.25
	CONSTANT code_in : opcode_type := 37; --3.3.1.26
	CONSTANT code_out : opcode_type := 38; --3.3.1.27
	CONSTANT code_jmp : opcode_type := 48; --3.3.1.28
	CONSTANT code_jz : opcode_type := 49; --3.3.1.29
	CONSTANT code_jc : opcode_type := 50; --3.3.1.30
	CONSTANT code_jn : opcode_type := 51; --3.3.1.31
	CONSTANT code_jo : opcode_type := 52; --3.3.1.32
	CONSTANT code_jnz : opcode_type := 53; --3.3.1.33
	CONSTANT code_jnc : opcode_type := 54; --3.3.1.34
	CONSTANT code_jnn : opcode_type := 55; --3.3.1.35
	CONSTANT code_jno : opcode_type := 56; --3.3.1.36

	FUNCTION INC (CONSTANT PC : addr_type) RETURN addr_type;  -- PC-"increaser" (2.1.3.4; 2.1.3.3) 
	-- ===============================================================================================================================================
	-- die Funktionen / Proceduren fuer unsere OPCodes
	-- ===============================================================================================================================================

	FUNCTION "NOT" (CONSTANT A : data_type) RETURN data_type;

	FUNCTION "AND" (CONSTANT A, B : data_type) RETURN data_type;

	FUNCTION "OR" (CONSTANT A, B : data_type) RETURN data_type;

	FUNCTION "XOR" (CONSTANT A, B : data_type) RETURN data_type;
 
  PROCEDURE rea (VARIABLE X : INOUT data_type; CONSTANT Y : IN data_type);

	PROCEDURE reo (VARIABLE X : INOUT data_type; CONSTANT Y : IN data_type);

  PROCEDURE rex (VARIABLE X : INOUT data_type; CONSTANT Y : IN data_type);

  FUNCTION CheckOverflowFlag(CONSTANT Reg : data_type) RETURN BOOLEAN;

  FUNCTION CheckZeroFlag (CONSTANT Reg : data_type) RETURN BOOLEAN;

  PROCEDURE ADD (CONSTANT O1, O2 : IN data_type; R : INOUT data_type; C, Z, O, N : INOUT BOOLEAN);

  PROCEDURE ADDC (CONSTANT O1, O2 : IN data_type; R : INOUT data_type; C, Z, O, N : INOUT BOOLEAN);

  PROCEDURE SUB (CONSTANT O1, O2 : IN data_type; R : INOUT data_type; C, Z, O, N : INOUT BOOLEAN);

  PROCEDURE SUBC (CONSTANT O1, O2 : IN data_type; R : INOUT data_type; C, Z, O, N : INOUT BOOLEAN);

  procedure XSLL(constant O1 : in data_type; R: out data_type; C: out boolean; O: out Boolean);
  
  procedure XSRL(constant O1: in data_type; R: out data_type; C: out boolean);

  Procedure XSRA(constant O1: in data_type; R: out data_type; C: out boolean);
 
  procedure ROLC(constant O1: in data_type;  R: out data_type; CI : inout boolean);
 
  procedure RORC(constant O1: in data_type;  R: out data_type; CI : inout boolean);
  
  PROCEDURE ReadIn (Reg : OUT data_type);
  PROCEDURE WriteOut(CONSTANT Reg : IN data_type);
 
-- JUMP Funktionen
  FUNCTION jmp (CONSTANT position : IN data_type) RETURN data_type;
  FUNCTION jz(CONSTANT position, pc_old : IN data_type; zero_flag : IN BOOLEAN) RETURN data_type;
  FUNCTION jc(CONSTANT position, pc_old : IN data_type; carry_flag : IN BOOLEAN) RETURN data_type;
  FUNCTION jn(CONSTANT position, pc_old : IN data_type; negative_flag : IN BOOLEAN) RETURN data_type;
  FUNCTION jo(CONSTANT position, pc_old : IN data_type; overflow_flag : IN BOOLEAN) RETURN data_type;
  FUNCTION jnz(CONSTANT position, pc_old : IN data_type; zero_flag : IN BOOLEAN) RETURN data_type;
  FUNCTION jnc(CONSTANT position, pc_old : IN data_type; carry_flag : IN BOOLEAN) RETURN data_type;
  FUNCTION jnn(CONSTANT position, pc_old : IN data_type; negative_flag : IN BOOLEAN) RETURN data_type;
  FUNCTION jno(CONSTANT position, pc_old : IN data_type; overflow_flag : IN BOOLEAN) RETURN data_type;
 
-- ===============================================================================================================================================
-- die Proceduren / Funktionen fÃÂ¼r unser IO
-- ===============================================================================================================================================
 

  FUNCTION PrintOpcode(code : opcode_type) RETURN STRING; 

  FUNCTION PrintBoolean(booleanvalue : BOOLEAN) RETURN CHARACTER; 
 
  PROCEDURE print_tail (VARIABLE f : OUT text);
 
  PROCEDURE write_PC_CMD (VARIABLE l : INOUT line; CONSTANT PC : IN data_type; CONSTANT OP : IN opcode_type; CONSTANT x, y, z : IN reg_addr_type);
 
  PROCEDURE print_header (VARIABLE f : OUT text);
 
  PROCEDURE write_param (VARIABLE l : INOUT line; CONSTANT param : IN data_type);
 
  PROCEDURE write_NoParam (VARIABLE l : INOUT line);
 
  PROCEDURE write_regs (VARIABLE l : INOUT line; CONSTANT r0, r1, r2, r3 : IN data_type );
 
  PROCEDURE write_flags (VARIABLE l : INOUT line; CONSTANT Zero, Carry, Negative, Overflow : IN BOOLEAN );
 
  PROCEDURE readIOinput (VARIABLE f : IN Text; l : INOUT line; x : OUT data_type);
 
  PROCEDURE writeIOoutput (VARIABLE f : OUT Text; l : INOUT line; CONSTANT x : IN data_type);
 
  PROCEDURE print_dump (CONSTANT memory : IN mem_type; VARIABLE dump_file : OUT text);

--===============================================================================================================================================
-- Assembler
--===============================================================================================================================================
 
  PROCEDURE InputDecode (VARIABLE l : IN line; Par: inout Boolean; OP: inout data_type; variable Mem : inout mem_type; variable ad: inout addr_type);
  Function to_int (a: in Character) return Integer;
-- PROCEDURE read_register (VARIABLE l : IN line; CONSTANT i_in : IN INTEGER; VARIABLE i_out: OUT INTEGER; VARIABLE reg: OUT String);
-- PROCEDURE read_param (VARIABLE l : IN line; CONSTANT i_in : IN INTEGER; VARIABLE param: OUT String);

END def_package_all;

