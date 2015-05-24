LIBRARY IEEE;
Use IEEE.numeric_std.ALL;
package body def_package_all is
 

-- ===============================================================================================================
-- Funktionen für die Kernfunktionalität unserer CPU
-- ===============================================================================================================
 
  function INC (constant PC: addr_type)		-- PC-"increaser" (2.1.3.4; 2.1.3.3)
		return addr_type is
		begin
			return (PC+1)mod 2**addr_width;				  -- Überlauf unseres PC vermeiden
    end INC; 



-- ===============================================================================================================
-- Funktionen für unsere OPCodes
-- ===============================================================================================================
	function "NOT" (constant A:data_type)
		return data_type is
		begin
		return -A -1 +2**data_width;
	end "NOT";

	function "AND" (constant A,B:data_type)
		return data_type is
		variable r : data_type :=0;
		begin
			for i in 0 to data_width loop
				if ((A/2**i) mod 2)+ ((B/2**i)mod 2)>1 then r:= r+2**i;
				end if;
			end loop;
		return r;
	end "AND";

	function "OR" (constant A,B:data_type) 
		return data_type is
		begin
		return to_integer(to_unsigned(a, data_width) and to_unsigned(b,data_width)); -- hab lange nach einer eleganteren Methode gesucht, mir wollte aber keine einfallen

	end "OR";
	
	function "XOR" (constant A,B:data_type)
		return data_type is
		begin
		return to_integer(to_unsigned(a, data_width) xor to_unsigned(b,data_width)); -- hab lange nach einer eleganteren Methode gesucht, mir wollte aber keine einfallen

	end "XOR";


-- ===============================================================================================================
-- Funktionen für unser IO
-- ===============================================================================================================

	procedure print_tail (variable f:out text) is
		variable l:line;
		begin
			write(l, string'("----------------------------------------------------------------"));
			writeline(f,l);
	end print_tail;

	procedure write_PC_CMD (variable l:inout line; constant PC in data_type; constant OP: in opcode_type; constant x,y,z: in reg_addr_type) is
		begin
			write(l, hex_image (PC), left, 3);
			write(l, string'(" | "));
			write(l, cmd_image (OP), left, 4);
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

	procedure write_param (variable l:inout line; constant param in data_type) is
		begin
			write(l, param, left , 3);
			write(l, string'(" | "));
	end write_param;

	procedure write_NoParam (variable l: inout line) is
		begin
			write(l, string'("---"));
			write(l, string'(" | "));
	end write_NoParam;

	-- procedure write_regs (variable l: inout line; constant r1, r2, r3 : in ??? ) is
		-- begin
			-- Das muss noch jemand schreiben
	-- end write_regs;
      
end def_package_all;
