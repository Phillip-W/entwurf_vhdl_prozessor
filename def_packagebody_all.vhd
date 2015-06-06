LIBRARY IEEE;
USE IEEE.numeric_std.ALL;
use std_logic_arith.all;
use std_logic_unsigned.all;
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
	
	--- rotation and shift
	
	procedure XSLL (constant A: in data_type;
			variable R: out data_type;
			variable Z,C,N,O: out boolean) is
		variable A_s : integer;
		constant a_length : integer := A'length - 1;
		variable D: std_logic_vector(A_length downto 1) := conv_std_logic_vector(to_unsigned(A,A_Length),A_Length);
		variable T: std_logic_vector(A_length downto 1) := (others=>"0");
		begin
		    T(A_length downto 1) = D(A_length-1 downto 0);
           
		    R:= conv_integer(T);
			
                if D(A_length) = 1 then 
                   C = TRUE;
                else
                   C = FALSE;
	        end if;
			
		if R mod 2**data_width = 0 then
	           Z = TRUE;
		else
		   Z = FALSE;
	        end if;
			
		if R < 0 then
	           N = TRUE;
		else
	           N = FALSE;
		end if;
			
		if (R < -2**(data_width-1)) or (R >= 2**(data_width-1)) then
	           O := TRUE;
		else
		   O := FALSE;
		end if;
	end XSLL;
	
	procedure XSRL(constant A: in data_type;
		       variable R: out data_type;
		       variable Z,C,N,O: out boolean) is
		constant A_Length : Integer := A'Length -1;
		variable A_copy : std_logic_vector(A_Length downto 0) := conv_std_logic_vector(to_unsigned(A,A_Length),A_Length);
		variable ergebnis : std_logic_vector (A_Length downto 0) := (others=>'0');
		begin
                        ergebnis(A_Length-1 downto 0) := A_copy(A_Length downto 1);
			R=conv_integer(ergebnis);
			
			if A_copy(0) = 1 then
			   C = TRUE;
			else
			   C = FALSE;
			end if;
			
			if R mod 2**data_width = 0 then
			   Z = TRUE;
			else
			   Z = FALSE;
			end if;
			
			if R < 0 then
			   N = TRUE;
			else
			   N = FALSE;
			end if;
			
			if (R < -2**(data_width-1)) or (R >= 2**(data_width-1)) then
			   O := TRUE;
			else
			   O := FALSE;
			end if;
        end XSRL;
        
	procedure XSRA(constant A: in data_type;
			variable R: out data_type;
			variable Z,C,N,O: out boolean) is  --
		constant A_Length : Integer := A'Length-1;
		variable A_copy : std_logic_vector(A_Length downto 0) := conv_std_logic_vector(to_unsigned(A,A_Length),A_Length);
		variable ergebnis: std_logic_vector(A_Length downto 0);
		begin
                     ergebnis(A_Length-1 downto 0):=A_copy(A_Length downto 1);
                     ergebnis(A_Length) := (others => A_copy(A_Length);
                     R :=conv_integer(ergebnis);
			
		        if A_copy(0) = 1 then
	                     C = TRUE;
		        else
		             C = FALSE;
		        end if;
			
			if R mod 2**data_width = 0 then
			   Z = TRUE;
			else
			   Z = FALSE;
			end if;
			
			if R < 0 then
			   N = TRUE;
			else
			   N = FALSE;
			end if;
			
			if (R < -2**(data_width-1)) or (R >= 2**(data_width-1)) then
			   O := TRUE;
			else
			   O := FALSE;
			end if;
	end XSRA;
	
	procedure ROLC(constant A: in data_type;
	               variable R: out data_type;
		       variable Z: out boolean;
		       variable C: inout boolean;
		       variable N,O: out boolean) is --
		       constant A_Length : Integer := A'Length-1;
		       variable A_s，CI, CD: Integer ;
		       variable A_copy : std_logic_vector(A_Length downto 0) := conv_std_logic_vector(to_unsigned(A,A_Length),A_Length);
		       variable T: std_logic_vector(A_Length downto 0) := A_copy;
		       begin
				T(A_Length downto 1) := A_copy(A_Length-1 downto 0);
		                T(0) := A_copy(A_Length);
						
				if A >= 2**(data_width-1) then
				        A_s = A - 2**(data_width);
				else 
					A_s:=A;
				end if;
						
				if C = FALSE then
					R := conv_integer(T);
					C := FALSE;
				else
					CI = '1';
					variable A_copy_s std_logic_vector(A_Length downto 0) := conv_std_logic_vector(to_unsigned(A_s,A_Length),A_Length);
					variable A_copy_s_1 : std_logic_vector(A_Length+1 downto 0) := CI & A_copy_s;
					variable T_s: std_logic_vector(A_Length+1 downto 0) := A_copy_s_1;

                                        T_s(A_Length+1 downto 1) := A_copy_s_1(A_Length downto 0);
                                        T_s(0) := CI;
				        CD = T_s(A_Length+1);
                                        if CD = 1 then
                                                C = TRUE;
                                        else
                                                C = FALSE;
                                        end if;

                                         T_s(A_Length downto 0) := (others => '0');
                                         R := conv_integer(T_s);

                                end if;
						
				if R mod 2**data_width = 0 then
			                Z = TRUE;
			        else
			                Z = FALSE;
			        end if;
			
			        if R < 0 then
			               N = TRUE;
			        else
			               N = FALSE;
			        end if;
			
			        if (R < -2**(data_width-1)) or (R >= 2**(data_width-1)) then
			               O := TRUE;
			        else
			               O := FALSE;
			        end if;
        end ROLC;
        
        procedure RORC(constant A: in data_type;
	               variable R: out data_type;
		       variable Z: out boolean;
		       variable C: inout boolean;
		       variable N,O: out boolean) is --
		       constant A_Length : Integer := A'Length-1;
		       variable A_s，CI, CD: Integer ;
		       variable A_copy : std_logic_vector(A_Length downto 0) := conv_std_logic_vector(to_unsigned(A,A_Length),A_Length);
		       variable T: std_logic_vector(A_Length downto 0) := A_copy;
		       begin
		                T(A_Length-1 downto 0) := A_copy(A_Length down to 1);
		                T(A_Length) := A_copy(0);
				if A >= 2**(data_width-1) then
				        A_s = A - 2**(data_width);
				else 
				        A_s:=A;
				end if;  
				   
				if C = FALSE then
					R := conv_integer(T);
					C := FALSE;
				else
					CI = '1';
					variable A_copy_s std_logic_vector(A_Length downto 0) := conv_std_logic_vector(to_unsigned(A_s,A_Length),A_Length);
					variable A_copy_s_1 : std_logic_vector(A_Length+1 downto 0) := CI & A_copy_s;
					variable T_s: std_logic_vector(A_Length+1 downto 0) := A_copy_s_1;
                                        T_s(A_Length downto 0) := A_copy_s_1(A_Length downto 1);
                                        T_s(A_Length) := A_copy_s_1(0);
					CD = T_s(A_Length+1);
                                        if CD = 1 then
                                            C = TRUE;
                                        else
                                            C = FALSE;
                                        end if;
                                        T_s(A_Length downto 0) := (others => '0');
                                        R := conv_integer(T_s);
                                end if;
				   
				if R mod 2**data_width = 0 then
			               Z = TRUE;
			        else
			               Z = FALSE;
			        end if;
			
			        if R < 0 then
			               N = TRUE;
			        else
			               N = FALSE;
			        end if;
			
			        if (R < -2**(data_width-1)) or (R >= 2**(data_width-1)) then
			               O := TRUE;
			        else
			               O := FALSE;
			        end if;
        end RORC;
	
	-----------------------------------------------------------------------------------------------------
	
		
	Procedure ReadIn (Reg: out data_type) is 
	variable il: line;
	begin 
	readIOinput(IOInputFile, il, Reg);
	end ReadIn;
	
	Procedure WriteOut (constant Reg: in data_type) is
	variable ol: line; 
	begin
	writeIOoutput (IOOutputFile, ol, Reg);
	end WriteOut;
	
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
		write(l, PrintBoolean(Zero), left, 1); -- geändert ;-)Funktion gibt character und nich string Zurück... Wenn das ein Problem ist schnell und einfach abändern
		write(l, PrintBoolean(Carry), left, 1); 
		write(l, PrintBoolean(Negative), left, 1); 
		write(l, PrintBoolean(Overflow), left, 1); 
	END write_flags;
	
	Procedure readIOinput (Variable f: in Text; l : inout line;  x: out data_type) is
	variable success : boolean;
	begin 
		readline(f, l);
		read(l, x, success);
	end readIOinput;
	
	Procedure writeIOoutput (Variable f: OUT Text; l : inout line;constant x: in data_type) is
	BEGIN
		write(l, integer'image(x));
		writeline(f, l);
	END writeIOoutput;
	 
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
			write(dump_line, INTEGER'IMAGE(memory(counter)), right, 4);
			writeline(dump_file, dump_line);
			-- increase Counter
			counter := counter + 1;
		END LOOP writing;
  END PROCEDURE;
  
END def_package_all;
