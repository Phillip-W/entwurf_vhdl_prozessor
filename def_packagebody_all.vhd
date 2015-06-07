LIBRARY IEEE;
USE IEEE.numeric_std.ALL;
USE IEEE.STD_LOGIC_1164.ALL;
USE std.textio.ALL;

PACKAGE BODY def_package_all IS
	-- ===============================================================================================================
	-- Funktionen fÃÂ¼r die KernfunktionalitÃÂ¤t unserer CPU
	-- ===============================================================================================================

	FUNCTION INC (CONSTANT PC : addr_type) RETURN addr_type IS  -- PC-"increaser" (2.1.3.4; 2.1.3.3) 
	BEGIN
		RETURN (PC + 1)MOD 2 ** addr_width; -- ÃÂberlauf unseres PC vermeiden
	END INC;

	-- ===============================================================================================================
	-- Funktionen fÃÂ¼r unsere OPCodes
	-- ===============================================================================================================
	FUNCTION "NOT" (CONSTANT A : data_type) RETURN data_type IS  -- IO fehlt noch 
		BEGIN
			RETURN - A - 1 + 2 ** data_width;
	END "NOT";

	FUNCTION "AND" (CONSTANT A, B : data_type) RETURN data_type IS  -- IO fehlt noch 
		VARIABLE r : data_type := 0;
	BEGIN
		FOR i IN 0 TO data_width LOOP
			IF ((A/2 ** i) MOD 2) + ((B/2 ** i)MOD 2) > 1 THEN
				r := r + 2 ** i;
			END IF;
		END LOOP; RETURN r;
	END "AND";

	FUNCTION "OR" (CONSTANT A, B : data_type) RETURN data_type IS  -- IO fehlt noch 
		BEGIN
			RETURN to_integer(to_unsigned(a, data_width) AND to_unsigned(b, data_width)); -- hab lange nach einer eleganteren Methode gesucht, mir wollte aber keine einfallen

	END "OR";

	FUNCTION "XOR" (CONSTANT A, B : data_type) RETURN data_type IS  -- IO fehlt noch 
		BEGIN
			RETURN to_integer(to_unsigned(a, data_width) XOR to_unsigned(b, data_width)); -- hab lange nach einer eleganteren Methode gesucht, mir wollte aber keine einfallen

	END "XOR";

	PROCEDURE rea (CONSTANT Y : IN data_type; VARIABLE X : INOUT data_type ) IS
VARIABLE Z, A : std_logic_vector(data_width - 1 DOWNTO 0);
BEGIN
	Z := std_logic_vector(to_unsigned(Y, data_width));
	A := std_logic_vector(to_unsigned(X, data_width));
	FOR I IN 0 TO DATA_WIDTH - 2 LOOP Z(i + 1) := Z(i) AND Z(i + 1);
	END LOOP;
	A(0) := Z(data_width - 1);
	X := to_integer(unsigned(A));
END rea;

PROCEDURE reo (CONSTANT Y : IN data_type; VARIABLE X : INOUT data_type ) IS
VARIABLE Z, A : std_logic_vector(data_width - 1 DOWNTO 0);
BEGIN
	Z := std_logic_vector(to_unsigned(Y, data_width));
	A := std_logic_vector(to_unsigned(X, data_width));
	FOR I IN 0 TO DATA_WIDTH - 2 LOOP Z(i + 1) := Z(i) OR Z(i + 1);
	END LOOP;
	A(0) := Z(data_width - 1);
	X := to_integer(unsigned(A));
END reo;

PROCEDURE rex (CONSTANT Y : IN data_type; VARIABLE X : INOUT data_type ) IS
VARIABLE Z, A : std_logic_vector(data_width - 1 DOWNTO 0);
BEGIN
	Z := std_logic_vector(to_unsigned(Y, data_width));
	A := std_logic_vector(to_unsigned(X, data_width));
	FOR I IN 0 TO DATA_WIDTH - 2 LOOP Z(i + 1) := Z(i) XOR Z(i + 1);
	END LOOP;
	A(0) := Z(data_width - 1);
	X := to_integer(unsigned(A));
END rex;

FUNCTION CheckZeroFlag (CONSTANT Reg : data_type) RETURN BOOLEAN IS
BEGIN
	IF (Reg = 0) THEN RETURN true;
	ELSE RETURN false;
	END IF;
END CheckZeroFlag;

PROCEDURE ADD (CONSTANT O1, O2 : IN data_type; R : INOUT data_type; C, Z, O : OUT BOOLEAN) IS
VARIABLE ZR : NATURAL;
BEGIN
	ZR := O1 + O2;
	IF (ZR >= (2 ** data_width)) THEN
		O := true;
		ZR := ZR - 2 ** data_width;
		C := true;
	END IF;
	R := ZR;
 
	Z := CheckZeroFlag(R);
END ADD;

PROCEDURE ADDC (CONSTANT O1, O2 : IN data_type; R : INOUT data_type; C : INOUT BOOLEAN; Z, O : OUT BOOLEAN) IS
VARIABLE ZR : NATURAL;
BEGIN
	ZR := 0;
	IF C THEN
		ZR := 2 ** data_width;
	END IF;
	ZR := O1 + O2 + ZR;
	IF (ZR >= (2 ** data_width)) THEN
		O := true;
		ZR := ZR - 2 ** data_width;
		C := true;
	ELSE
		C := false;
	END IF;
	R := ZR;
	Z := CheckZeroFlag(R);
END ADDC;
 
PROCEDURE SUB (CONSTANT O1, O2 : IN data_type; R : INOUT data_type; Z, N : OUT BOOLEAN) IS
VARIABLE ZR : INTEGER;
BEGIN
	ZR := O1 - O2;
	IF (ZR < 0) THEN
		N := true;
		ZR := ZR * ( - 1);
	END IF;
	R := ZR;
	Z := CheckZeroFlag(R);
END SUB;
 
PROCEDURE SUBC (CONSTANT O1, O2 : IN data_type; R : INOUT data_type; C : INOUT BOOLEAN; Z, O, N : OUT BOOLEAN) IS
VARIABLE ZR : INTEGER;
BEGIN
	ZR := 0;
	IF (C) THEN
		ZR := 2 ** data_width;
	END IF;
	ZR := O1 - O2 - ZR;
	IF (ZR < 0) THEN
		N := true;
		IF (ZR <= ( - 1) * (2 ** data_width)) THEN
			ZR := ZR + 2 ** data_width;
			C := true;
		ELSE
			C := false;
		END IF;
	ELSE
		C := false;
	END IF;
	R := ZR;
	Z := CheckZeroFlag(R);
END SUBC;
 
 
PROCEDURE ReadIn (Reg : OUT data_type) IS
VARIABLE il : line;
BEGIN
	readIOinput(IOInputFile, il, Reg);
END ReadIn;
 
PROCEDURE WriteOut (CONSTANT Reg : IN data_type) IS
VARIABLE ol : line;
BEGIN
	writeIOoutput (IOOutputFile, ol, Reg);
END WriteOut;
 
-- Jump procedures
FUNCTION jmp(CONSTANT position : IN data_type) RETURN data_type IS
BEGIN
	RETURN position;
END jmp;
 
FUNCTION jz(CONSTANT position, pc_old : IN data_type; zero_flag : IN BOOLEAN) RETURN data_type IS
BEGIN
	IF zero_flag = true THEN RETURN position;
	ELSE RETURN pc_old;
	END IF;
END jz;
 
FUNCTION jc(CONSTANT position, pc_old : IN data_type; carry_flag : IN BOOLEAN) RETURN data_type IS
BEGIN
	IF carry_flag = true THEN RETURN position;
	ELSE RETURN pc_old;
	END IF;
END jc;
 
FUNCTION jn(CONSTANT position, pc_old : IN data_type; negative_flag : IN BOOLEAN) RETURN data_type IS
BEGIN
	IF negative_flag = true THEN RETURN position;
	ELSE RETURN pc_old;
	END IF;
END jn;
 
FUNCTION jo(CONSTANT position, pc_old : IN data_type; overflow_flag : IN BOOLEAN) RETURN data_type IS
BEGIN
	IF overflow_flag = true THEN RETURN position;
	ELSE RETURN pc_old;
	END IF;
END jo;
 
FUNCTION jnz(CONSTANT position, pc_old : IN data_type; zero_flag : IN BOOLEAN) RETURN data_type IS
BEGIN
	IF zero_flag = false THEN RETURN position;
	ELSE RETURN pc_old;
	END IF;
END jnz;
 
FUNCTION jnc(CONSTANT position, pc_old : IN data_type; carry_flag : IN BOOLEAN) RETURN data_type IS
BEGIN
	IF carry_flag = false THEN RETURN position;
	ELSE RETURN pc_old;
	END IF;
END jnc;
 
FUNCTION jnn(CONSTANT position, pc_old : IN data_type; negative_flag : IN BOOLEAN) RETURN data_type IS
BEGIN
	IF negative_flag = false THEN RETURN position;
	ELSE RETURN pc_old;
	END IF;
END jnn;
 
FUNCTION jno(CONSTANT position, pc_old : IN data_type; overflow_flag : IN BOOLEAN) RETURN data_type IS
BEGIN
	IF overflow_flag = false THEN RETURN position;
	ELSE RETURN pc_old;
	END IF;
END jno;
 
-- ===============================================================================================================
-- Proceduren / Funktionen fÃÂ¼r unser IO
-- ===============================================================================================================
 
FUNCTION PrintOpcode(code : opcode_type) RETURN STRING IS
BEGIN
	CASE code IS
		WHEN 0 => RETURN"NOP";
		WHEN 1 => RETURN"STOP";
		WHEN 2 => RETURN"ADD";
		WHEN 3 => RETURN"ADDC";
		WHEN 4 => RETURN"SUB";
		WHEN 5 => RETURN"SUBC";
		WHEN 6 => RETURN"NOT";
		WHEN 7 => RETURN"AND";
		WHEN 8 => RETURN"OR";
		WHEN 9 => RETURN"XOR";
		WHEN 10 => RETURN"REA";
		WHEN 11 => RETURN"REO";
		WHEN 12 => RETURN"REX";
		WHEN 13 => RETURN"SLL";
		WHEN 14 => RETURN"SRL";
		WHEN 15 => RETURN"SRA";
		WHEN 16 => RETURN"ROL";
		WHEN 17 => RETURN"ROLC";
		WHEN 18 => RETURN"ROR";
		WHEN 19 => RETURN"RORC";
		WHEN 32 => RETURN"LDC";
		WHEN 33 => RETURN"LDD";
		WHEN 34 => RETURN"LDR";
		WHEN 35 => RETURN"STD";
		WHEN 36 => RETURN"STR";
		WHEN 37 => RETURN"IN";
		WHEN 38 => RETURN"OUT";
		WHEN 48 => RETURN"JMP";
		WHEN 49 => RETURN"JZ";
		WHEN 50 => RETURN"JC";
		WHEN 51 => RETURN"JN";
		WHEN 52 => RETURN"JO";
		WHEN 53 => RETURN"JNZ";
		WHEN 54 => RETURN"JNC";
		WHEN 55 => RETURN"JNN";
		WHEN 56 => RETURN"JNO";
		WHEN OTHERS => 
	END CASE;
END PrintOpcode;
FUNCTION PrintBoolean(booleanvalue : BOOLEAN) RETURN CHARACTER IS
BEGIN
	IF (booleanvalue = true) THEN RETURN 'T';
	ELSE RETURN 'F';
	END IF;
END PrintBoolean;
 
PROCEDURE print_tail (VARIABLE f : OUT text) IS
VARIABLE l : line;
BEGIN
	write(l, STRING'("------------------------------------------------------"));
	writeline(f, l);
END print_tail;
 
PROCEDURE write_PC_CMD (VARIABLE l : INOUT line; CONSTANT PC : IN data_type; CONSTANT OP : IN opcode_type; CONSTANT x, y, z : IN reg_addr_type) IS
BEGIN
	write(l, INTEGER'image(PC), left, 3);
	write(l, STRING'(" | "));
	write(l, PrintOpcode(OP), left, 4); -- INTEGER'image ist heir nicht ganz das Richtige, weil hier danach der Name des OPCodes und nicht die Nummer stehen soll
	write(l, STRING'(" | "));
	write(l, X, left, 1);
	write(l, y, left, 1);
	write(l, z, left, 1);
	write(l, STRING'(" | "));
END write_PC_CMD;
 
PROCEDURE print_header (VARIABLE f : OUT text) IS
VARIABLE l : line;
BEGIN
	write(l, STRING'("PC"), left, 3);
	write(l, STRING'(" | "));
	write(l, STRING'("Cmd"), left, 4);
	write(l, STRING'(" | "));
	write(l, STRING'("XYZ"), left, 3);
	write(l, STRING'(" | "));
	write(l, STRING'("P"), left, 3);
	write(l, STRING'(" | "));
	write(l, STRING'("R0"), left, 3);
	write(l, STRING'(" | "));
	write(l, STRING'("R1"), left, 3);
	write(l, STRING'(" | "));
	write(l, STRING'("R2"), left, 3);
	write(l, STRING'(" | "));
	write(l, STRING'("R3"), left, 3);
	write(l, STRING'(" | "));
	write(l, STRING'("ZCNO"), left, 4);
	writeline(f, l);
END print_header;
 
PROCEDURE write_param (VARIABLE l : INOUT line; CONSTANT param : IN data_type) IS
BEGIN
	write(l, INTEGER'image(param), left, 3);
	write(l, STRING'(" | "));
END write_param;
 
PROCEDURE write_NoParam (VARIABLE l : INOUT line) IS
BEGIN
	write(l, STRING'("---"));
	write(l, STRING'(" | "));
END write_NoParam;
 
PROCEDURE write_regs (VARIABLE l : INOUT line; CONSTANT r0, r1, r2, r3 : IN data_type ) IS
BEGIN
	write(l, INTEGER'IMAGE(r0), left, 3);
	write(l, STRING'(" | "));
	write(l, INTEGER'IMAGE (r1), left, 3);
	write(l, STRING'(" | "));
	write(l, INTEGER'IMAGE (r2), left, 3);
	write(l, STRING'(" | "));
	write(l, INTEGER'IMAGE (r3), left, 3);
	write(l, STRING'(" | "));
END write_regs;
 
PROCEDURE write_flags (VARIABLE l : INOUT line; CONSTANT Zero, Carry, Negative, Overflow : IN BOOLEAN ) IS
BEGIN
	write(l, PrintBoolean(Zero), left, 1); -- geÃÂ¤ndert;-)Funktion gibt character und nich string ZurÃÂ¼ck... Wenn das ein Problem ist schnell und einfach abÃÂ¤ndern
	write(l, PrintBoolean(Carry), left, 1);
	write(l, PrintBoolean(Negative), left, 1);
	write(l, PrintBoolean(Overflow), left, 1);
END write_flags;
 
PROCEDURE readIOinput (VARIABLE f : IN Text; l : INOUT line; x : OUT data_type) IS
VARIABLE success : BOOLEAN;
BEGIN
	readline(f, l);
	read(l, x, success);
END readIOinput;
 
PROCEDURE writeIOoutput (VARIABLE f : OUT Text; l : INOUT line;CONSTANT x : IN data_type) IS
BEGIN
	write(l, INTEGER'image(x));
	writeline(f, l);
END writeIOoutput;
 
PROCEDURE print_dump (CONSTANT memory : IN mem_type; VARIABLE dump_file : OUT text) IS
VARIABLE dump_line : line;
VARIABLE success : BOOLEAN;
VARIABLE temp : data_type;
VARIABLE counter : addr_type := 0;
BEGIN
	-- write header
	write(dump_line, STRING'("Memory Inhalt:"));
	writeline(dump_file, dump_line);
 
	writing : LOOP
	EXIT WHEN counter >= 2 ** address_width - 1; -- Exit Loop if end of Memory is reached;
	-- write memory line per line
	write(dump_line, STRING'("Adresse: "));
	write(dump_line, INTEGER'IMAGE(counter), right, 4);
	write(dump_line, STRING'(" | Inahlt: "));
	write(dump_line, INTEGER'IMAGE(memory(counter)), right, 4);
	writeline(dump_file, dump_line);
	-- increase Counter
	counter := counter + 1;
END LOOP writing;
END PROCEDURE;

--===============================================================================================================================================
-- Assembler
--===============================================================================================================================================
 
PROCEDURE InputDecode (VARIABLE l : IN line; Par: inout Boolean; OP: inout data_type) IS
	VARIABLE llength : natural := l'length;
	VARIABLE i,j : NATURAL := 1;
	VARIABLE RegCounter: Natural :=0; 	-- gibt an wie viele Register gelesen werden müssen
	
 
BEGIN
	whitespace: WHILE l(i)/=' ' LOOP
		i := i+1;
		exit whitespace when
			i=llength+1;
	END LOOP whitespace;
	
	i := i - 1; -- sonst liest er bis zum Leerzeiche nund findet natürlich keine Übereinstimmung
	IF l(1 TO i) = "NOP" THEN -- OP-CODE Erkennung (NOP)
		OP := code_nop * (2 ** reg_addr_width) ** 3;
		RegCounter:= 0; Par:= FALSE;
	ELSIF l(1 TO i) = "STOP" THEN -- OP-Code erkennung (STOP)
		OP := code_stop * (2 ** reg_addr_width) ** 3;	
		RegCounter:= 0; Par:= FALSE;
	ELSIF l(1 TO i) = "ADD" THEN -- OP-Code erkennung (ADD)
		OP := code_add * (2 ** reg_addr_width) ** 3;
		RegCounter:= 3;
	ELSIF l(1 TO i) = "ADDC" THEN
		OP := code_addc * (2 ** reg_addr_width) ** 3;
		RegCounter:= 3;
	ELSIF l(1 TO i) = "SUB" THEN
		OP := code_sub * (2 ** reg_addr_width) ** 3;
	ELSIF l(1 TO i) = "SUBC" THEN
		OP := code_subc * (2 ** reg_addr_width) ** 3;
	ELSIF l(1 TO i) = "NOT" THEN
		OP := code_not * (2 ** reg_addr_width) ** 3;
	ELSIF l(1 TO i) = "AND" THEN
		OP := code_and * (2 ** reg_addr_width) ** 3;
	ELSIF l(1 TO i) = "OR" THEN
		OP := code_or * (2 ** reg_addr_width) ** 3;
	ELSIF l(1 TO i) = "XOR" THEN
		OP := code_xor * (2 ** reg_addr_width) ** 3;
	ELSIF l(1 TO i) = "REA" THEN
		OP := code_rea * (2 ** reg_addr_width) ** 3;
	ELSIF l(1 TO i) = "REO" THEN
		OP := code_reo * (2 ** reg_addr_width) ** 3;
	ELSIF l(1 TO i) = "REX" THEN
		OP := code_rex * (2 ** reg_addr_width) ** 3;
	ELSIF l(1 TO i) = "SLL" THEN
		OP := code_sll * (2 ** reg_addr_width) ** 3;
	ELSIF l(1 TO i) = "SRL" THEN
		OP := code_srl * (2 ** reg_addr_width) ** 3;
	ELSIF l(1 TO i) = "SRA" THEN
		OP := code_sra * (2 ** reg_addr_width) ** 3;
	ELSIF l(1 TO i) = "ROL" THEN
		OP := code_rol * (2 ** reg_addr_width) ** 3;
	ELSIF l(1 TO i) = "ROLC" THEN
		OP := code_rolc * (2 ** reg_addr_width) ** 3;
	ELSIF l(1 TO i) = "ROR" THEN
		OP := code_ror * (2 ** reg_addr_width) ** 3;
	ELSIF l(1 TO i) = "RORC" THEN
		OP := code_rorc * (2 ** reg_addr_width) ** 3;
	ELSE
		ASSERT FALSE
		REPORT "ungültig"
			SEVERITY error;
	END IF; 
	
	readReg: WHILE RegCounter/=0 LOOP
		RegCounter:= RegCounter-1;
		OP:= OP+(to_int(l(i+2*j))*(2 ** reg_addr_width) ** RegCounter);
		j:=j+1;
	END LOOP readReg;
	
END PROCEDURE;
Function to_int (a: in Character) return Integer IS
	variable b: integer;
Begin
	if a='0' then b:=0;
	elsif a='1' then b:=1;
	elsif a='2' then b:=2;
	elsif a='3' then b:=3;
	else ASSERT FALSE
		REPORT "ungültiges Register"
			SEVERITY error;
	end if;
	return b;
END to_int;

END def_package_all;
