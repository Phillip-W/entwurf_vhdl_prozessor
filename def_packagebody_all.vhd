LIBRARY IEEE;
USE std.standard.ALL;
USE IEEE.numeric_std.ALL;

PACKAGE BODY def_package_all IS
	-- ===============================================================================================================
	-- Funktionen für die Kernfunktionalität unserer CPU
	-- ===============================================================================================================

	FUNCTION INC (CONSTANT PC : addr_type) -- PC-"increaser" (2.1.3.4; 2.1.3.3) RETURN addr_type IS
	BEGIN
		RETURN (PC + 1)MOD 2 ** addr_width; -- Überlauf unseres PC vermeiden
	END INC;

	-- ===============================================================================================================
	-- Funktionen für unsere OPCodes
	-- ===============================================================================================================
	FUNCTION "NOT" (CONSTANT A : data_type) -- IO fehlt noch RETURN data_type IS
		BEGIN
			RETURN - A - 1 + 2 ** data_width;
	END "NOT";

	FUNCTION "AND" (CONSTANT A, B : data_type) -- IO fehlt noch RETURN data_type IS
		VARIABLE r : data_type := 0;
	BEGIN
		FOR i IN 0 TO data_width LOOP
			IF ((A/2 ** i) MOD 2) + ((B/2 ** i)MOD 2) > 1 THEN
				r := r + 2 ** i;
			END IF;
		END LOOP; RETURN r;
	END "AND";

	FUNCTION "OR" (CONSTANT A, B : data_type) -- IO fehlt noch RETURN data_type IS
		BEGIN
			RETURN to_integer(to_unsigned(a, data_width) AND to_unsigned(b, data_width)); -- hab lange nach einer eleganteren Methode gesucht, mir wollte aber keine einfallen

	END "OR";
 
	FUNCTION "XOR" (CONSTANT A, B : data_type) -- IO fehlt noch RETURN data_type IS
		BEGIN
			RETURN to_integer(to_unsigned(a, data_width) XOR to_unsigned(b, data_width)); -- hab lange nach einer eleganteren Methode gesucht, mir wollte aber keine einfallen

	END "XOR";

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
	
	-- ===============================================================================================================
	-- Proceduren / Funktionen für unser IO
	-- ===============================================================================================================
	
	PROCEDURE print_tail (VARIABLE f : OUT text) IS
	VARIABLE l : line;
	BEGIN
		write(l, STRING'("----------------------------------------------------------------"));
		writeline(f, l);
	END print_tail;
	
	PROCEDURE write_PC_CMD (VARIABLE l : INOUT line; CONSTANT PC : IN data_type; CONSTANT OP : IN opcode_type; CONSTANT x, y, z : IN reg_addr_type) IS
	BEGIN
		write(l, INTEGER'image(PC), left, 3); -- hex_image function muss noch geschrieben werden
		write(l, STRING'(" | "));
		write(l, INTEGER'image(OP), left, 4); -- cmd_image function muss noch geschrieben werden
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
		write(l, INTEGER'image(param), left, 3); -- dafür brauchen wir auch noch die transformation integers zu string
		write(l, STRING'(" | "));
	END write_param;
	
	PROCEDURE write_NoParam (VARIABLE l : INOUT line) IS
	BEGIN
		write(l, STRING'("---"));
		write(l, STRING'(" | "));
	END write_NoParam;
	
	PROCEDURE write_regs (VARIABLE l : INOUT line; CONSTANT r0, r1, r2, r3 : IN data_type ) IS
	BEGIN
		write(l, INTEGER'image(r0), left, 3); -- dafür brauchen wir auch noch die transformation integers zu string (param_image verwenden)
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
		write(l, BOOLEAN'image(Zero), left, 3); -- dafür brauchen wir auch noch die transformation von boolean zu Characters/string (T, F)
		write(l, STRING'(" | ")); 
		write(l, BOOLEAN'IMAGE (Carry), left, 3); 
		write(l, STRING'(" | ")); 
		write(l, BOOLEAN'IMAGE (Negative), left, 3); 
		write(l, STRING'(" | ")); 
		write(l, BOOLEAN'IMAGE (Overflow), left, 3); 
		write(l, STRING'(" | ")); 
	END write_flags;
	 
END def_package_all;
