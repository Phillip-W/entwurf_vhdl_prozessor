LIBRARY IEEE;
USE IEEE.numeric_std.ALL;
use IEEE.STD_LOGIC_1164.all;

PACKAGE BODY def_package_all IS
	-- ===============================================================================================================
	-- Funktionen für die Kernfunktionalität unserer CPU
	-- ===============================================================================================================

	FUNCTION INC (CONSTANT PC : addr_type) -- PC-"increaser" (2.1.3.4; 2.1.3.3) 
	RETURN addr_type IS
	BEGIN
		RETURN (PC + 1)MOD 2 ** addr_width; -- Überlauf unseres PC vermeiden
	END INC;

	-- ===============================================================================================================
	-- Funktionen für unsere OPCodes
	-- ===============================================================================================================
	FUNCTION "NOT" (CONSTANT A : data_type) -- IO fehlt noch 
	RETURN data_type IS
		BEGIN
			RETURN - A - 1 + 2 ** data_width;
	END "NOT";

	FUNCTION "AND" (CONSTANT A, B : data_type) -- IO fehlt noch 
	RETURN data_type IS
		VARIABLE r : data_type := 0;
	BEGIN
		FOR i IN 0 TO data_width LOOP
			IF ((A/2 ** i) MOD 2) + ((B/2 ** i)MOD 2) > 1 THEN
				r := r + 2 ** i;
			END IF;
		END LOOP; RETURN r;
	END "AND";

	FUNCTION "OR" (CONSTANT A, B : data_type) -- IO fehlt noch 
	RETURN data_type IS
		BEGIN
			RETURN to_integer(to_unsigned(a, data_width) AND to_unsigned(b, data_width)); -- hab lange nach einer eleganteren Methode gesucht, mir wollte aber keine einfallen

	END "OR";
 
	FUNCTION "XOR" (CONSTANT A, B : data_type) -- IO fehlt noch 
	RETURN data_type IS
		BEGIN
			RETURN to_integer(to_unsigned(a, data_width) XOR to_unsigned(b, data_width)); -- hab lange nach einer eleganteren Methode gesucht, mir wollte aber keine einfallen

	END "XOR";

  PROCEDURE rea (CONSTANT Y : IN data_type; VARIABLE X: INOUT data_type ) IS
		VARIABLE Z, A: std_logic_vector(data_width-1 downto 0);
		BEGIN
			Z:= std_logic_vector(to_unsigned(Y, data_width));
			A:= std_logic_vector(to_unsigned(X, data_width));
			FOR I IN 0 TO DATA_WIDTH-2 LOOP Z(i+1):=Z(i) AND Z(i+1); end loop;
			A(0):= Z(data_width-1);
			X:= to_integer(unsigned(A));
	END rea;

  PROCEDURE reo (CONSTANT Y : IN data_type; VARIABLE X: INOUT data_type ) IS
		VARIABLE Z, A: std_logic_vector(data_width-1 downto 0);
		BEGIN
			Z:= std_logic_vector(to_unsigned(Y, data_width));
			A:= std_logic_vector(to_unsigned(X, data_width));
			FOR I IN 0 TO DATA_WIDTH-2 LOOP Z(i+1):=Z(i) OR Z(i+1); end loop;
			A(0):= Z(data_width-1);
			X:= to_integer(unsigned(A));
	END reo;

  PROCEDURE rex (CONSTANT Y : IN data_type; VARIABLE X: INOUT data_type ) IS
		VARIABLE Z, A: std_logic_vector(data_width-1 downto 0);
		BEGIN
			Z:= std_logic_vector(to_unsigned(Y, data_width));
			A:= std_logic_vector(to_unsigned(X, data_width));
			FOR I IN 0 TO DATA_WIDTH-2 LOOP Z(i+1):=Z(i) XOR Z(i+1); end loop;
			A(0):= Z(data_width-1);
			X:= to_integer(unsigned(A));
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
	
	-- Jump procedures
	FUNCTION jmp(CONSTANT position: IN data_type)
	RETURN data_type IS
	BEGIN
		return position;
	END jmp;
	
	FUNCTION jz(CONSTANT position, pc_old: IN data_type; zero_flag: IN BOOLEAN)
	RETURN data_type IS
	BEGIN
		IF zero_flag = true THEN
			return position;
		ELSE
			return pc_old;
		END IF;
	END jz;
	
	FUNCTION jc(CONSTANT position, pc_old: IN data_type; carry_flag: IN BOOLEAN)
	RETURN data_type IS
	BEGIN
		IF carry_flag = true THEN
			return position;
		ELSE
			return pc_old;
		END IF;
	END jc;
	
	FUNCTION jn(CONSTANT position, pc_old: IN data_type; negative_flag: IN BOOLEAN)
	RETURN data_type IS
	BEGIN
		IF negative_flag = true THEN
			return position;
		ELSE
			return pc_old;
		END IF;
	END jn;
	
	FUNCTION jo(CONSTANT position, pc_old: IN data_type; overflow_flag: IN BOOLEAN)
	RETURN data_type IS
	BEGIN
		IF overflow_flag = true THEN
			return position;
		ELSE
			return pc_old;
		END IF;
	END jo;
	
	FUNCTION jnz(CONSTANT position, pc_old: IN data_type; zero_flag: IN BOOLEAN)
	RETURN data_type IS
	BEGIN
		IF zero_flag = false THEN
			return position;
		ELSE
			return pc_old;
		END IF;
	END jnz;
	
	FUNCTION jnc(CONSTANT position, pc_old: IN data_type; carry_flag: IN BOOLEAN)
	RETURN data_type IS
	BEGIN
		IF carry_flag = false THEN
			return position;
		ELSE
			return pc_old;
		END IF;
	END jnc;
	
	FUNCTION jnn(CONSTANT position, pc_old: IN data_type; negative_flag: IN BOOLEAN)
	RETURN data_type IS
	BEGIN
		IF negative_flag = false THEN
			return position;
		ELSE
			return pc_old;
		END IF;
	END jnn;
	
	FUNCTION jno(CONSTANT position, pc_old: IN data_type; overflow_flag: IN BOOLEAN)
	RETURN data_type IS
	BEGIN
		IF overflow_flag = false THEN
			return position;
		ELSE
			return pc_old;
		END IF;
	END jno;
	
	-- ===============================================================================================================
	-- Proceduren / Funktionen für unser IO
	-- ===============================================================================================================
	
	FUNCTION PrintOpcode(code : opcode_type) return String is
	BEGIN
		Case code is
			when 0 => return"NOP";
			when 1 => return"STOP";
			when 2 => return"ADD";
			when 3 => return"ADDC";
			when 4 => return"SUB";
			when 5 => return"SUBC";
			when 6 => return"NOT";
			when 7 => return"AND";
			when 8 => return"OR";
			when 9 => return"XOR";
			when 10 => return"REA";
			when 11 => return"REO";
			when 12 => return"REX";
			when 13 => return"SLL";
			when 14 => return"SRL";
			when 15 => return"SRA";
			when 16 => return"ROL";
			when 17 => return"ROLC";
			when 18 => return"ROR";
			when 19 => return"RORC";
			when 32 => return"LDC";
			when 33 => return"LDD";
			when 34 => return"LDR";
			when 35 => return"STD";
			when 36 => return"STR";
			when 37 => return"IN";
			when 38 => return"OUT";
			when 48 => return"JMP";
			when 49 => return"JZ"; 
			when 50 => return"JC";
			when 51 => return"JN";
			when 52 => return"JO";
			when 53 => return"JNZ";
			when 54 => return"JNC";
			when 55 => return"JNN";
			when 56 => return"JNO";
			when others =>
	end case;
	end PrintOpcode;


	FUNCTION PrintBoolean(booleanvalue : boolean) return Character is
	BEGIN
		if (booleanvalue=true) then
			return 'T';
		else
			return 'F';
		end if;
	end PrintBoolean;
	
	PROCEDURE print_tail (VARIABLE f : OUT text) IS
	VARIABLE l : line;
	BEGIN
		write(l, STRING'("----------------------------------------------------------------"));
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
		write(l, INTEGER'image(r0), left, 3); 
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
		write(l, PrintBoolean(Zero), left, 3); -- geändert ;-)Funktion gibt character und nich string Zurück... Wenn das ein Problem ist schnell und einfach abändern
		write(l, STRING'(" | ")); 
		write(l, PrintBoolean(Carry), left, 3); 
		write(l, STRING'(" | ")); 
		write(l, PrintBoolean(Negative), left, 3); 
		write(l, STRING'(" | ")); 
		write(l, PrintBoolean(Overflow), left, 3); 
		write(l, STRING'(" | ")); 
	END write_flags;
	
	PROCEDURE print_dump (CONSTANT memory: IN mem_type; VARIABLE dump_file: OUT text) IS
    VARIABLE dump_line: line;
    VARIABLE success: boolean;
    VARIABLE temp: data_type;
    VARIABLE counter: addr_type := 0;
  BEGIN
    -- write header
    write(dump_line, String'("Memory Inhalt:"));
    writeline(dump_file, dump_line);
		
		writing: LOOP
      EXIT WHEN counter >= 2 ** address_width - 1; -- Exit Loop if end of Memory is reached;
      -- write memory line per line
		  write(dump_line, String'("Adresse: "));
		  write(dump_line, INTEGER'IMAGE(counter), right, 4);
		  write(dump_line, String'(" |  Inahlt: "));
		  IF memory(counter) /= 0 THEN  -- IF register is Empty return String not "0"
		    write(dump_line, INTEGER'IMAGE(memory(counter)), right, 4);
		  ELSE
		    write(dump_line, String'("empty"));
		  END IF;
		  writeline(dump_file, dump_line);
		  -- increase Counter
		  counter := counter + 1;
		END LOOP writing;
  END PROCEDURE;
  
END def_package_all;
