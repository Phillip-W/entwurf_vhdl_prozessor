package body mem_package_all is
	procedure init_memory( Par: inout Boolean;  OP: inout data_type; variable Memoryfile : in text; variable Memory : inout mem_type ) is

        	variable ml : line;
        	variable v : data_type ;
        	variable i : addr_type := 0;
		-- variable dml: line;
				
        begin
            outest:LOOP --- read line by line
               exit when endfile (Memoryfile);
               readline (Memoryfile , ml);
		-- read values in each line
		exit outest when
			i=2**addr_width-1;
				inputDecode(ml, Par, OP, Memory, i);
				Memory(i):=OP;
				if Par then
					i:=i+2;
				else i:=i+1;
				end if;
	       end LOOP outest;
        end init_memory;
end mem_package_all;
