LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;

PACKAGE cpu_defs_pack IS
  
  --Hier alle Konstanten definieren
  constant data_width       :positive :=12;   --1.1.1
  constant address_width    :positive :=12;   --1.1.2
  constant opcode_width     :positive :=6;    --3.1.1
  
  --Hier alle Datentypen, Subtyps definieren
  subtype addr_type IS
    natural range 0 to 2**address_width-1;
  subtype opcode_type IS
    natural range 0 to 2**opcode_width-1;
  
  --Hier alle OPCodes definieren
  constant code_nope  : opcode_type:=0;       --3.3.1.1
  constant code_stop  : opcode_type:=1;       --3.3.1.2
  constant code_add   : opcode_type:=2;       --3.3.1.3
  
END cpu_defs_pack;
