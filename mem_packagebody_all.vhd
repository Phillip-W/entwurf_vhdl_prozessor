package body mem_package_all is
	procedure init_memory( Par: inout Boolean;  OP: inout data_type; variable Memoryfile : in text; variable Memory : out mem_type ) is

        	variable ml : line;
        	variable success : boolean ;
        	variable v : data_type ;
        	variable i : addr_type := 0;
		-- variable dml: line;
				
        begin
            outest:LOOP --- read line by line
               exit when endfile (Memoryfile);
               readline (Memoryfile , ml);
               success := TRUE ;
		-- read values in each line
		while success LOOP
			if Par then
				read(ml, v, success);
				if success then
					Memory(i):=v;
		exit outest when
			i=2**addr_width-1;
					i:= i+1;
				end if;
			else 
				inputDecode(ml, Par, OP);
				Memory(i):=OP;
				success := False ;
				i:=i+1;
			end if;
		end LOOP;
             end LOOP outest;
        end init_memory;
end mem_package_all;
