LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;
Use work.def_package_all.all;

package body mem_package_all is
  function init_memory return mem_type is
    begin
      return -- hier unseren Ablauf eingeben (Befehle, wie sie der reihe nach ausgeführt werden sollen (immer mit * (2**reg_addr_width)**3 hinten dran
          (   0 => code_nop * (2**reg_addr_width)**3,
              1 => code_stop * (2**reg_addr_width)**3,
              others => 0);
  end init_memory;        
end mem_package_all;
