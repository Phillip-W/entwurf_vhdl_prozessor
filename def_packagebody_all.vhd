package body def_package_all is
  
  function INC (constant PC: addr_type)		-- PC-"increaser" (2.1.3.4; 2.1.3.3)
		return addr_type is
		begin
			return (PC+1)mod 2**addr_width;				  -- Überlauf unseres PC vermeiden
    end INC; 

	function "NOT" (constant A:data_type)
		return data_type is
		begin
		return -A -1 +2**data_width;
	end "NOT";
      
end def_package_all;
