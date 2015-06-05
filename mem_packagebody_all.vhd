LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;
Use work.def_package_all.all;

package body mem_package_all is
	procedure init_memory( variable Memoryfile  : in text;
			                   variable Memory: out mem_type) is
         variable ml  :line;
		     variable success : boolean;
	     	 variable v   : data_type;
		     variable i   : addr_type := 0 ;
         begin 
         outest: loop   ----- read line by line
         exit when endfile  (Memoryfile);
         readline (f , ml);
         
         success := TRUE ;
         ----read values in each line
         while success loop
             read (ml, v, success);
	     if success then
	        Memory (i) := v ;
                exit outest when
	                i := 2**addr_width -1;
		i := i + 1;
	      end if;
          end loop;
        end loop;
  end init_memory;
				    
end mem_package_all;
