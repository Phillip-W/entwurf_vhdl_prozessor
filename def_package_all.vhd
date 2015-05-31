LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;
USE std.textio.ALL;

PACKAGE def_package_all IS

	-- ===============================================================================================================================================
	--Hier alle Konstanten definieren
	-- ===============================================================================================================================================

	CONSTANT data_width : POSITIVE := 12; --1.1.1
	CONSTANT address_width : POSITIVE := 12; --1.1.2
	CONSTANT opcode_width : POSITIVE := 6; --3.1.1

	CONSTANT reg_addr_width : POSITIVE := 2; --2.1.2
	CONSTANT addr_width : POSITIVE := 12; -- nicht ändern
	-- ===============================================================================================================================================
	--Hier alle Datentypen, Subtyps definieren
	-- ===============================================================================================================================================

	SUBTYPE addr_type IS -- unsers Adressen zum ansprechen des Speichers, z.B. durch den PC
	NATURAL RANGE 0 TO 2 ** address_width - 1;
	SUBTYPE opcode_type IS -- für unsere OPCode Deklarationen
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
	--Hier alle OPCodes definieren (vollständig)
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

	FUNCTION INC (CONSTANT PC : addr_type) -- PC-"increaser" (2.1.3.4; 2.1.3.3) 
		RETURN addr_type;
	-- ===============================================================================================================================================
	-- die Funktionen / Proceduren für unsere OPCodes
	-- ===============================================================================================================================================

	FUNCTION "NOT" (CONSTANT A : data_type) RETURN data_type;

	FUNCTION "AND" (CONSTANT A, B : data_type) RETURN data_type;

	FUNCTION "OR" (CONSTANT A, B : data_type) RETURN data_type;

	FUNCTION "XOR" (CONSTANT A, B : data_type) RETURN data_type;
	
	PROCEDURE rea (CONSTANT Y : IN data_type; VARIABLE X: INOUT data_type);

	PROCEDURE reo (CONSTANT Y : IN data_type; VARIABLE X: INOUT data_type);

	PROCEDURE rex (CONSTANT Y : IN data_type; VARIABLE X: INOUT data_type);

	FUNCTION CheckZeroFlag (CONSTANT Reg : data_type) RETURN BOOLEAN;

	PROCEDURE ADD (CONSTANT O1, O2 : IN data_type; R : INOUT data_type; C, Z, O : OUT BOOLEAN);

	PROCEDURE ADDC (CONSTANT O1, O2 : IN data_type; R : INOUT data_type; C : INOUT BOOLEAN; Z, O : OUT BOOLEAN);

	PROCEDURE SUB (CONSTANT O1, O2 : IN data_type; R : INOUT data_type; Z, N : OUT BOOLEAN);

	PROCEDURE SUBC (CONSTANT O1, O2 : IN data_type; R : INOUT data_type; C : INOUT BOOLEAN; Z, O, N : OUT BOOLEAN);

	-- Procedure XSLL(constant A: in data_type; variable R: out data_type; variable Z,CO,N,O: out boolean);
					
	-- Procedure XSRA(constant A: in data_type; variable R: out data_type; variable Z,CO,N,O: out boolean);
				  
	-- procedure ROLC(constant A: in data_type; variable R: out data_type; variable Z: out boolean; variable C: inout boolean;
		       -- variable N,O: out boolean) ;
		
	-- procedure RORC(constant A: in data_type; variable R: out data_type; variable Z: out boolean; variable C: inout boolean;
		       -- variable N,O: out boolean);
	
	-- JUMP Funktionen
	FUNCTION jmp (CONSTANT position: INOUT data_type) RETURN data_type;
	FUNCTION jz(CONSTANT position, pc_old: INOUT data_type; zero_flag: IN BOOLEAN) RETURN data_type;
	FUNCTION jc(CONSTANT position, pc_old: INOUT data_type; carry_flag: IN BOOLEAN) RETURN data_type;
	FUNCTION jn(CONSTANT position, pc_old: INOUT data_type; negative_flag: IN BOOLEAN) RETURN data_type;
	FUNCTION jo(CONSTANT position, pc_old: INOUT data_type; overflow_flag: IN BOOLEAN) RETURN data_type;
	FUNCTION jnz(CONSTANT position, pc_old: INOUT data_type; zero_flag: IN BOOLEAN) RETURN data_type;
	FUNCTION jnc(CONSTANT position, pc_old: INOUT data_type; carry_flag: IN BOOLEAN) RETURN data_type;
	FUNCTION jnn(CONSTANT position, pc_old: INOUT data_type; negative_flag: IN BOOLEAN) RETURN data_type;
	FUNCTION jno(CONSTANT position, pc_old: INOUT data_type; overflow_flag: IN BOOLEAN) RETURN data_type;
	
	-- ===============================================================================================================================================
	-- die Proceduren / Funktionen für unser IO
	-- ===============================================================================================================================================
	

	FUNCTION PrintOpcode(code : opcode_type) return String;	

	FUNCTION PrintBoolean(booleanvalue : boolean) return Character;	
	
	PROCEDURE print_tail (VARIABLE f : OUT text);
	
	PROCEDURE write_PC_CMD (VARIABLE l : INOUT line; CONSTANT PC : IN data_type; CONSTANT OP : IN opcode_type; CONSTANT x, y, z : IN reg_addr_type);
	
	PROCEDURE print_header (VARIABLE f : OUT text);
	
	PROCEDURE write_param (VARIABLE l : INOUT line; CONSTANT param : IN data_type);
	
	PROCEDURE write_NoParam (VARIABLE l : INOUT line);
	
	PROCEDURE write_regs (VARIABLE l : INOUT line; CONSTANT r0, r1, r2, r3 : IN data_type );
	
	PROCEDURE write_flags (VARIABLE l : INOUT line; CONSTANT Zero, Carry, Negative, Overflow : IN BOOLEAN );
	
	-- unsere transformationsfunktionen (hex_image, cmd_image, ...) fehlen noch.

END def_package_all;
