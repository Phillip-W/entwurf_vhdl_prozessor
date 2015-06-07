LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;
USE work.def_package_all.ALL;
USE std.textio.ALL;

PACKAGE mem_package_all IS
	FILE MemoryFile : Text IS IN "Memory.txt";
	PROCEDURE init_memory ( Par: inout Boolean;  OP: inout data_type;
		VARIABLE Memoryfile : IN text;
		VARIABLE Memory : OUT mem_type
	);
END mem_package_all;
