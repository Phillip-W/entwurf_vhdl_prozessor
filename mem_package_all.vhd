LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;
Use work.def_package_all.all;

package mem_package_all is
         file MemoryFile   :   Text is in "Memory";
	       procedure init_memory( variable Memoryfile  : in text;
			                           variable Memory: out mem_type) ; ---Funktion um unseren Speicher zu beschreiben 
end mem_package_all;
