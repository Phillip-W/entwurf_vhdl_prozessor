LIBRARY IEEE;
use std.standard.all;
Use IEEE.numeric_std.ALL;

package body def_package_all is
 

-- ===============================================================================================================
-- Funktionen für die Kernfunktionalität unserer CPU
-- ===============================================================================================================
 
  function INC (constant PC: addr_type)		     -- PC-"increaser" (2.1.3.4; 2.1.3.3)
		return addr_type is
		begin
			return (PC+1)mod 2**addr_width;				   -- Überlauf unseres PC vermeiden
    end INC; 



-- ===============================================================================================================
-- Funktionen für unsere OPCodes
-- ===============================================================================================================
	function "NOT" (constant A:data_type)       -- IO fehlt noch
		return data_type is
		begin
		return -A -1 +2**data_width;
	end "NOT";

	function "AND" (constant A,B:data_type)     -- IO fehlt noch
		return data_type is
		variable r : data_type :=0;
		begin
			for i in 0 to data_width loop
				if ((A/2**i) mod 2)+ ((B/2**i)mod 2)>1 then r:= r+2**i;
				end if;
			end loop;
		return r;
	end "AND";

	function "OR" (constant A,B:data_type)     -- IO fehlt noch
		return data_type is
		begin
		return to_integer(to_unsigned(a, data_width) and to_unsigned(b,data_width)); -- hab lange nach einer eleganteren Methode gesucht, mir wollte aber keine einfallen

	end "OR";
	
	function "XOR" (constant A,B:data_type)   -- IO fehlt noch
		return data_type is
		begin
		return to_integer(to_unsigned(a, data_width) xor to_unsigned(b,data_width)); -- hab lange nach einer eleganteren Methode gesucht, mir wollte aber keine einfallen

	end "XOR";

	function CheckZeroFlag (constant Reg: data_type)
		return boolean is
		begin
		if (Reg = 0) then
		return true;
		else 
		return false; 
		end if;
	end CheckZeroFlag;
	
	procedure ADD (constant O1, O2: in data_type; R: inout data_type; C, Z, O: out boolean) is
	variable ZR: natural;
	begin
		ZR:= O1 + O2;
		if (ZR>=(2**data_width)) then
			O:= true;
			ZR:= ZR- 2**data_width;
			C:=true;
		end if;
		R:= ZR;
		
		Z:= CheckZeroFlag(R);
	end ADD;

	procedure ADDC (constant O1, O2: in data_type; R: inout data_type; C: inout boolean; Z, O: out boolean) is
	variable ZR: natural;
	begin
		ZR:=0;
		if C then 
			ZR:= 2**data_width;
		end if;
		ZR:= O1 + O2 + ZR;
		if (ZR>=(2**data_width)) then
			O:= true;
			ZR:= ZR- 2**data_width;
			C:=true;
		else 
			C:=false;
		end if;
		R:= ZR;
		Z:= CheckZeroFlag(R);
	end ADDC;

	procedure SUB (constant O1, O2: in data_type; R: inout data_type; Z, N: out boolean) is
	variable ZR: integer;
	begin
		ZR:= O1 - O2;
		if (ZR<0) then
			N:= true;
			ZR:= ZR*(-1);
		end if;
		R:= ZR;
		Z:= CheckZeroFlag(R);
	end SUB;
	
	procedure SUBC (constant O1, O2: in data_type; R: inout data_type; C: inout boolean; Z, O, N: out boolean) is
	variable ZR: integer;
	begin	
		ZR:=0;
		if(C) then 
			ZR:= 2**data_width;
		end if;
		ZR:= O1 - O2 - ZR;
		if (ZR<0) then
			N:= true;
			if (ZR<= (-1)*(2**data_width)) then
				ZR:= ZR+ 2**data_width;
				C:= true;
			else
				C:=false;
			end if;	
		else
			C:=false;
		end if;
		R:= ZR;
		Z:= CheckZeroFlag(R);
	end SUBC;
	
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

-- ===============================================================================================================
-- Proceduren / Funktionen für unser IO
-- ===============================================================================================================



	procedure print_tail (variable f:out text) is
		variable l:line;
		begin
			write(l, string'("----------------------------------------------------------------"));
			writeline(f,l);
	end print_tail;

	procedure write_PC_CMD (variable l:inout line; constant PC: in data_type; constant OP: in opcode_type; constant x,y,z: in reg_addr_type) is
		begin
			write(l, integer'image(PC), left, 3);			-- hex_image function muss noch geschrieben werden
			write(l, string'(" | "));
			write(l, integer'image(OP), left, 4);			-- cmd_image function muss noch geschrieben werden
			write(l, string'(" | "));
			write(l, X, left , 1);
			write(l, y, left , 1);
			write(l, z, left , 1); 
			write(l, string'(" | "));
	end write_PC_CMD;

	procedure print_header (variable f: out text) is
		variable l:line;
		begin
			write(l, string'("PC"), left, 3);
			write(l, string'(" | "));
			write(l, string'("Cmd"), left, 4);
			write(l, string'(" | "));
			write(l, string'("XYZ"), left, 3);
			write(l, string'(" | "));
			write(l, string'("P"), left, 3);
			write(l, string'(" | "));
			write(l, string'("R0"), left, 3);
			write(l, string'(" | "));
			write(l, string'("R1"), left, 3);
			write(l, string'(" | "));
			write(l, string'("R2"), left, 3);
			write(l, string'(" | "));
			write(l, string'("R3"), left, 3);
			write(l, string'(" | "));
			write(l, string'("ZCNO"), left, 4);
			writeline(f,l);
	end print_header;

	procedure write_param (variable l:inout line; constant param: in data_type) is
		begin
			write(l, integer'image(param), left , 3);          -- dafür brauchen wir auch noch die transformation integers zu string
			write(l, string'(" | "));
	end write_param;

	procedure write_NoParam (variable l: inout line) is
		begin
			write(l, string'("---"));
			write(l, string'(" | "));
	end write_NoParam;

	procedure write_regs (variable l: inout line; constant r0, r1, r2, r3 : in data_type ) is
	 begin
	    write(l, integer'image(r0), left, 3);			    -- dafür brauchen wir auch noch die transformation integers zu string  (param_image verwenden)
		  write(l, string'(" | "));	
      write(l, integer'image (r1), left, 3);			    
			write(l, string'(" | "));	
      write(l, integer'image (r2), left, 3);			    
			write(l, string'(" | "));	
      write(l, integer'image (r3), left, 3);			    
			write(l, string'(" | "));						
	end write_regs;
  
	procedure write_flags (variable l: inout line; constant Zero, Carry, Negative, Overflow : in boolean ) is
		begin
			write(l, boolean'image(Zero), left, 3);			    -- dafür brauchen wir auch noch die transformation von boolean zu Characters/string (T, F)
			write(l, string'(" | "));	
      write(l, boolean'image (Carry), left, 3);			    
			write(l, string'(" | "));	
      write(l, boolean'image (Negative), left, 3);			    
			write(l, string'(" | "));	
      write(l, boolean'image (Overflow), left, 3);			    
			write(l, string'(" | "));						
	end write_flags;
      
end def_package_all;
