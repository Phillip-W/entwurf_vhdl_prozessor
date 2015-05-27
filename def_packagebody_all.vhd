LIBRARY IEEE;
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

end def_package_all;

