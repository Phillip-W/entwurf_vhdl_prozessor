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
	
	
	
	--===============================================================================================================
	-- ROTATE AND SHIFT
	--===============================================================================================================
	
	procedure XSLL  (constant A: in data_type; variable R: out data_type;
			 variable Z,CO,N,O: out boolean) is
		variable A_s : integer;
		constant a_length : integer := A'length - 1;
		variable D: std_logic_vector(A_length downto 1) := conv_std_logic_vector(to_unsigned(A,A_Length),A_Length);
		variable T: std_logic_vector(A_length downto 1) = (others=>"0");
		begin
		    T(A_length downto 1) = D(A_length-1 downto 0);
                    R:= conv_integer(T);
                    if D(A_length) = '1' then 
                        CO = TRUE;
                    else
                        CO = FALSE;
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
	
	procedure XSRL(constant A: in data_type; variable R: out data_type;
		       variable Z,CO,N,O: out boolean) is
		constant A_Length : Integer := A'Length -1;
		variable A_copy : std_logic_vector(A_Length downto 0) := conv_std_logic_vector(to_unsigned(A,A_Length),A_Length);
		variable ergebnis : std_logic_vector (A_Length downto 0) := (others=>'0');
		begin
                   ergebnis(A_Length-1 downto 0) := A_copy(A_Length downto 1);
		   R=conv_integer(ergebnis);
	           if A_copy(0) = '1' then
		      CO = TRUE;
		   else
	              CO = FALSE;
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
   
	procedure XSRA(constant A: in data_type; variable R: out data_type;
			variable Z,CO,N,O: out boolean) is  --
		constant A_Length : Integer := A'Length-1;
		alias A_copy : std_logic_vector(A_Length downto 0) := conv_std_logic_vector(to_unsigned(A,A_Length),A_Length);
		variable ergebnis: std_logic_vector(A_Length downto 0);
		begin
                   ergebnis(A_Length-1 downto 0):=A_copy(A_Length downto 1);
                   ergebnis(A_Length) := (others => A_copy(A_Length);
                   R :=conv_integer(ergebnis);
	           if A_copy(0) = '1' then
		      CO = TRUE;
		   else
	              CO = FALSE;
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
	
	procedure ROLC( constant A: in data_type;
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
        
	procedure RORC( constant A: in data_type;
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
