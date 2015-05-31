LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;
Use work.def_package_all.all;

package body mem_package_all is
  function init_memory return mem_type is
    begin
      return -- hier unseren Ablauf eingeben (Befehle, wie sie der reihe nach ausgeführt werden sollen (immer mit * (2**reg_addr_width)**3 hinten dran
          (   0 => code_nop * (2**reg_addr_width)**3,
              1 => code_stop * (2**reg_addr_width)**3,
              13 => code_sll * (2**reg_addr_width)**3,
	      14 => code_srl * (2**reg_addr_width)**3,
      	      15 => code_sra * (2**reg_addr_width)**3,
	      16 => code_rol * (2**reg_addr_width)**3,
	      17 => code_rolc * (2**reg_addr_width)**3,
	      18 => code_ror * (2**reg_addr_width)**3,
	      19 => code_rorc * (2**reg_addr_width)**3,
              others => 0);
  end init_memory;        
end mem_package_all;
